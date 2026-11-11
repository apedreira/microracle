################################################################################
# timeKillFun.R
# Nested growth/inactivation model for time-kill curves (Pedreira et al., 2024,
# Foods 13(12), 1831, doi:10.3390/foods13121831), Equation (3):
#
#   dN/dt = a0/(a0 + (1-a0) exp(-mu t)) * mu/ln10 * (1 - 10^(N - Nm))
#           - K/ln10 * m * t^(m-1)
#
# N is log10 CFU/mL. Parameters: mu (h^-1), Nm (log10 CFU/mL), a0 (-),
# K (h^-m), m (-).
#
# Numerical approach: writing x = N ln10 and y = exp(-x) = 1/X turns the
# equation into a LINEAR first-order ODE in y:
#
#   dy/dt = (K m t^(m-1) - mu alpha(t)) y + mu alpha(t) exp(-xm)
#
# with alpha(t) the lag factor. Its solution only needs one cumulative
# integral, so no ODE solver is called, the t^(m-1) singularity at t = 0
# disappears (the death term enters as K t^m), and every evaluation is fully
# vectorised. This makes the repeated evaluations of the fitting fast.
################################################################################

LN10 <- log(10)

# ------------------------------------------------------------------------------
# Behaviours (Table 2 of the paper). "free" are estimated; "fixed" are forced.
# np = number of estimated parameters used in the AIC.
# ------------------------------------------------------------------------------
TK_BEHAVIOURS <- list(
  "Logistic" =
    list(free = c("mu", "Nm"),                  fixed = c(a0 = 1, K = 0, m = 1)),
  "Logistic + Lag" =
    list(free = c("mu", "Nm", "a0"),            fixed = c(K = 0, m = 1)),
  "Chick-Watson" =
    list(free = c("K"),                         fixed = c(mu = 0, Nm = 10, a0 = 1, m = 1)),
  "Hom" =
    list(free = c("K", "m"),                    fixed = c(mu = 0, Nm = 10, a0 = 1)),
  "Logistic + Chick-Watson" =
    list(free = c("mu", "Nm", "K"),             fixed = c(a0 = 1, m = 1)),
  "Logistic + Hom" =
    list(free = c("mu", "Nm", "K", "m"),        fixed = c(a0 = 1)),
  "Logistic + Lag + Chick-Watson" =
    list(free = c("mu", "Nm", "a0", "K"),       fixed = c(m = 1)),
  "Logistic + Lag + Hom" =
    list(free = c("mu", "Nm", "a0", "K", "m"),  fixed = c())
)
TK_BEHAVIOUR_NAMES <- names(TK_BEHAVIOURS)
TK_AUTO <- "Auto (lowest AIC)"
TK_PARAM_NAMES <- c("mu", "Nm", "a0", "K", "m")

# Lower bounds that keep the model well defined (a0 = 0 means "never leaves
# lag"; m = 0 makes the Hom term a step at t = 0).
TK_EPS_A0 <- 1e-8
TK_MIN_M  <- 1e-3
# Search ranges used by the optimiser (wide but physically plausible; they
# avoid compensating solutions such as very fast growth + very fast killing)
TK_MAX_MU <- 6       # h^-1
TK_MAX_K  <- 1000    # h^-m
TK_NM_RANGE <- c(0, 13)
# Resolution floor for the AIC: residuals below this (log10 units) are treated
# as noise. Without it, curves that are almost entirely below the detection
# limit can be fitted "exactly" and ln(WLS) -> -Inf favours the largest model.
TK_RESOLUTION <- 0.05

# Apply the fixed values of a behaviour to a full parameter vector
tk_apply_behaviour <- function(p, behaviour) {
  p <- p[TK_PARAM_NAMES]
  if (!is.null(behaviour) && behaviour %in% TK_BEHAVIOUR_NAMES) {
    fx <- TK_BEHAVIOURS[[behaviour]]$fixed
    p[names(fx)] <- fx
  }
  p
}

# ------------------------------------------------------------------------------
# Simulation
#   times : output times (h), >= 0
#   N0    : initial log10 CFU/mL (value at t = 0)
#   p     : named vector with mu, Nm, a0, K, m
#   n_grid: points of the internal integration grid
# Returns log10 CFU/mL at `times`.
# ------------------------------------------------------------------------------
tk_simulate <- function(times, N0, p, n_grid = 801) {
  mu <- max(unname(p["mu"]), 0)
  Nm <- unname(p["Nm"])
  a0 <- min(max(unname(p["a0"]), TK_EPS_A0), 1)
  K  <- max(unname(p["K"]), 0)
  m  <- max(unname(p["m"]), TK_MIN_M)

  t_max <- max(times, 0)
  if (t_max == 0) return(rep(N0, length(times)))

  # Quadratic grid: denser near t = 0, where t^m changes fastest when m < 1
  s <- t_max * seq(0, 1, length.out = n_grid)^2
  s <- sort(unique(c(s, times)))

  x0 <- N0 * LN10
  xm <- Nm * LN10

  # A(s) = integral of the lag factor alpha from 0 to s
  if (mu > 0) {
    A     <- s + log(a0 + (1 - a0) * exp(-mu * s)) / mu
    alpha <- a0 / (a0 + (1 - a0) * exp(-mu * s))
  } else {
    A     <- s
    alpha <- rep(1, length(s))
  }
  D <- K * s^m                         # integral of the Hom/Chick-Watson term

  # log Phi(s) = D(s) - mu A(s);  y(s) = Phi(s) * (y0 + I(s))
  logPhi <- D - mu * A
  # integrand of I: mu alpha exp(-xm) / Phi  (computed in log scale)
  if (mu > 0) {
    log_f <- log(mu) + log(alpha) - xm - logPhi
    # cumulative trapezoid in a numerically safe way (scaled by its maximum)
    shift <- max(log_f)
    f     <- exp(log_f - shift)
    I_scaled <- c(0, cumsum(diff(s) * (head(f, -1) + tail(f, -1)) / 2))
    log_term <- log(exp(-x0 - shift) + I_scaled) + shift
  } else {
    log_term <- rep(-x0, length(s))
  }
  log_y <- logPhi + log_term
  N_grid <- -log_y / LN10

  N_grid[match(times, s)]
}

# ------------------------------------------------------------------------------
# Objective: weighted least squares in log10 units, sigma = 0.5 (paper, Eq. 4)
# Censored points (data at or below the detection limit) only penalise the
# model when it predicts counts ABOVE the limit.
# ------------------------------------------------------------------------------
tk_wls <- function(N_model, N_data, censored, sigma = 0.5) {
  r <- ifelse(censored, pmax(N_model - N_data, 0), N_model - N_data)
  sum((r / sigma)^2)
}

tk_aic <- function(wls_sum, nd, np, sigma = 0.5) {
  wls_eff <- max(wls_sum, nd * (TK_RESOLUTION / sigma)^2)
  nd * log(wls_eff / nd) + 2 * (np + 1)
}

# Transformations so the optimiser works unconstrained
.tk_to_par <- function(name, v) {
  switch(name,
         mu = qlogis(min(max(v / TK_MAX_MU, 1e-6), 1 - 1e-6)),
         Nm = qlogis(min(max((v - TK_NM_RANGE[1]) / diff(TK_NM_RANGE), 1e-6), 1 - 1e-6)),
         a0 = qlogis(min(max(v, 1e-6), 1 - 1e-6)),
         K  = qlogis(min(max(v / TK_MAX_K, 1e-9), 1 - 1e-6)),
         m  = qlogis(min(max(v, TK_MIN_M), 1 - 1e-6)))
}
.tk_from_par <- function(name, z) {
  switch(name,
         mu = TK_MAX_MU * plogis(z),
         Nm = TK_NM_RANGE[1] + diff(TK_NM_RANGE) * plogis(z),
         a0 = plogis(z),
         K  = TK_MAX_K * plogis(z),
         m  = TK_MIN_M + (1 - TK_MIN_M) * plogis(z))
}

# Starting values guessed from the data
.tk_starts <- function(t, N, behaviour) {
  N0 <- N[1]
  slope_up   <- max(c(diff(N) / diff(t), 0), na.rm = TRUE)
  slope_down <- max(c(-diff(N) / diff(t), 0), na.rm = TRUE)
  base <- c(mu = min(max(slope_up * LN10, 0.5), TK_MAX_MU * 0.9),
            Nm = min(max(max(N) + 0.1, N0 + 0.5), TK_NM_RANGE[2] - 0.5),
            a0 = 0.5,
            K  = max(slope_down * LN10, 0.5),
            m  = 0.5)
  free <- TK_BEHAVIOURS[[behaviour]]$free
  starts <- list(
    base,
    replace(base, c("mu", "K", "m", "a0"), c(1.5, 2, 0.8, 0.05)),
    replace(base, c("mu", "K", "m", "a0"), c(0.8, 20, 0.3, 0.9))
  )
  lapply(starts, function(s) s[free])
}

# ------------------------------------------------------------------------------
# Fit one behaviour to one curve
#   t, N     : times and log10 counts (first point must be t = 0)
#   censored : logical vector (TRUE = at/below detection limit)
# ------------------------------------------------------------------------------
tk_fit_behaviour <- function(t, N, censored, behaviour, sigma = 0.5,
                             n_grid = 301) {
  beh  <- TK_BEHAVIOURS[[behaviour]]
  free <- beh$free
  np   <- length(free)
  nd   <- length(N)
  N0   <- N[1]

  build_p <- function(z) {
    p <- c(mu = 0, Nm = 10, a0 = 1, K = 0, m = 1)
    for (i in seq_along(free)) p[free[i]] <- .tk_from_par(free[i], z[i])
    tk_apply_behaviour(p, behaviour)
  }
  obj <- function(z) {
    p <- build_p(z)
    Nmod <- tk_simulate(t, N0, p, n_grid)
    val <- tk_wls(Nmod, N, censored, sigma)
    if (!is.finite(val)) 1e12 else val
  }

  best <- NULL
  for (st in .tk_starts(t, N, behaviour)) {
    z0 <- mapply(.tk_to_par, names(st), st)
    fit <- if (length(z0) == 1) {
      # one parameter: bounded 1-D search on the transformed scale
      optim(z0, obj, method = "Brent", lower = -25, upper = 15)
    } else {
      f1 <- optim(z0, obj, method = "Nelder-Mead",
                  control = list(maxit = 150 * length(z0), reltol = 1e-6))
      optim(f1$par, obj, method = "BFGS", control = list(maxit = 100))
    }
    if (is.null(best) || fit$value < best$value) best <- fit
  }

  p_hat <- build_p(best$par)
  wls   <- max(best$value, 1e-12)
  list(behaviour = behaviour, params = p_hat, free = free,
       np = np, nd = nd, wls = wls, aic = tk_aic(wls, nd, np, sigma))
}

# ------------------------------------------------------------------------------
# Fit one curve: either a given behaviour or every behaviour ranked by AIC.
# Behaviours with np >= nd - 1 are skipped (not enough data).
# ------------------------------------------------------------------------------
tk_fit_curve <- function(t, N, censored, behaviour = TK_AUTO, sigma = 0.5) {
  candidates <- if (identical(behaviour, TK_AUTO)) TK_BEHAVIOUR_NAMES else behaviour
  fits <- list()
  for (b in candidates) {
    if (length(TK_BEHAVIOURS[[b]]$free) >= length(N) - 1) next
    fits[[b]] <- tk_fit_behaviour(t, N, censored, b, sigma)
  }
  if (length(fits) == 0) return(NULL)
  aics <- vapply(fits, function(f) f$aic, numeric(1))
  ranking <- data.frame(
    Behaviour = names(fits),
    np        = vapply(fits, function(f) f$np, numeric(1)),
    WLS       = vapply(fits, function(f) f$wls, numeric(1)),
    AIC       = aics,
    row.names = NULL
  )
  ranking <- ranking[order(ranking$AIC), ]
  ranking$dAIC <- ranking$AIC - min(ranking$AIC)
  list(best = fits[[ranking$Behaviour[1]]], ranking = ranking, all = fits)
}
