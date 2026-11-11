# Preset parameters of the Selection & Extinction planes model.
# Kill rates, survival fractions and their confidence intervals are returned as
# text in scientific notation ("e" lowercase) to be shown as such in the tables.
presetDataModel3 <- function() {
  sci <- function(x) formatC(x, format = "e", digits = 2)

  ##############################
  ### SETUP OF THE EXPERIMENT ##
  ##############################
  setup <- list(
    T_g = 24 * 60,          # Duration of the growth periods (min)
    T_k = 10,               # Duration of the killing periods (min)
    K   = as.character(1e9),# Carrying capacity (CFU/mL), text for scientific notation
    X_0 = as.character(1e6),# Total inoculum (CFU/mL)
    X_g = as.character(1),  # Growth limit (cell density below which cells cannot divide)
    r_0 = 10,               # Initial ratio of S2 to S1 cells (X_2(0)/X_1(0))
    D   = 100               # Dilution factor
  )

  ##############################
  ###    BACTERIAL TRAITS     ##
  ##############################
  C <- c(0, 30, 40, 50)     # Bactericide concentrations (any unit, only for legends)

  mu  <- c(S1 = 1.2, S2 = 1.2 * 0.8)
  k_num  <- list(S1 = c(0, 2.56e-1, 7.9e-1, 1.48e0),
                 S2 = c(0, 6.33e-2, 3.74e-1, 9.99e-1))
  sf_num <- list(S1 = c(7.5e-2, 3.55e-4, 5.69e-8, 1.69e-9),
                 S2 = c(5.4e-1, 2.2e-2, 4.46e-5, 3.94e-9))

  ##############################
  ### CI for BACTERIAL TRAITS ##
  ##############################
  # Matrices with columns [lower, upper]
  ci_mu <- lapply(mu, function(m) cbind(0.9 * m, 1.1 * m))
  ci_k  <- lapply(k_num, function(k) matrix(sci(cbind(0.9 * k, 1.1 * k)), ncol = 2))
  ci_SF <- lapply(sf_num, function(sf) matrix(sci(exp(cbind(1.1 * log(sf), 0.9 * log(sf)))), ncol = 2))

  c(setup, list(
    C  = C,
    mu = as.list(mu),
    k  = lapply(k_num, sci),
    SF = lapply(sf_num, sci),
    ci = list(mu = ci_mu, k = ci_k, SF = ci_SF)
  ))
}
