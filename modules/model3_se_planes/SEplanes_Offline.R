# ==============================================================================
# STANDALONE OFFLINE SCRIPT: SEplanes (SELECTION & EXTINCTION PLANES)
# https://microracle.shinyapps.io/Microracle/
# ==============================================================================
# This script automatically reads data from 'SEplanes_Parameters.tsv'  
# ==============================================================================

# TSV file name (ensure it is in the same directory)
DATA_FILE <- "SEplanes_Parameters.tsv"

# ------------------------------------------------------------------------------
# CHOOSE PLOT MODE HERE
# Options: "by_k" (Kill rate) or "by_SF" (Survival fraction)
# ------------------------------------------------------------------------------
MODEL_MODE <- "by_k" 
#MODEL_MODE <- "by_SF" 

if (!file.exists(DATA_FILE)) {
  stop(paste("File not found:", DATA_FILE, "- Please place it in the current working directory."))
}

# ------------------------------------------------------------------------------
# 1. PARSE THE TSV
# ------------------------------------------------------------------------------
parse_SEplanes_data <- function(filename) {
  lines <- readLines(filename)
  
  # 1. General Parameters
  idx_mu <- grep("--- BACTERIAL TRAITS: GROWTH RATE", lines)
  params_lines <- lines[1:(idx_mu - 1)]
  params_lines <- params_lines[params_lines != ""]
  params_df <- read.table(text = params_lines, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  
  p <- as.list(setNames(params_df$Value, params_df$Parameter))
  # Numeric conversion of parameters (Plot_mode is no longer here)
  p$X_0 <- as.numeric(p$X_0); p$K <- as.numeric(p$K); p$X_e <- as.numeric(p$X_e)
  p$r_0 <- as.numeric(p$r_0); p$T_g <- as.numeric(p$T_g); p$D <- as.numeric(p$D)
  if ("T_k" %in% names(p)) p$T_k <- as.numeric(p$T_k)
  
  # 2. Growth Rates (mu)
  idx_dyn <- grep("--- BACTERIAL TRAITS: (KILL RATES|SURVIVAL FRACTIONS)", lines)
  mu_lines <- lines[(idx_mu + 1):(idx_dyn - 1)]
  mu_lines <- mu_lines[mu_lines != ""]
  mu_df <- read.table(text = mu_lines, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  
  # 3. Dynamic Data (k or SF)
  dyn_lines <- lines[(idx_dyn + 1):length(lines)]
  dyn_lines <- dyn_lines[dyn_lines != ""]
  dyn_df <- read.table(text = dyn_lines, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  
  return(list(p = p, mu = mu_df, dyn = dyn_df))
}

# Load extracted data
data_parsed <- parse_SEplanes_data(DATA_FILE)
p_mod <- data_parsed$p
mu_df <- data_parsed$mu
dyn_df <- data_parsed$dyn

# Extract Growth parameters (mu and CIs)
mu_S1 <- mu_df$mu[1]; mu_S2 <- mu_df$mu[2]
ci_mu_S1 <- if(!any(is.na(mu_df[1, c("CI_lower", "CI_upper")]))) as.numeric(mu_df[1, c("CI_lower", "CI_upper")]) else NULL
ci_mu_S2 <- if(!any(is.na(mu_df[2, c("CI_lower", "CI_upper")]))) as.numeric(mu_df[2, c("CI_lower", "CI_upper")]) else NULL

# Colors and concentrations
fixed_palette <- c("#1B4F72", "#F1C40F", "#7D3C98", "#17A589", "#C0392B", "#85C1E9", "#196F3D", "#F39C12", "#A569BD", "#E6B0AA")
colors_vec <- rep(fixed_palette, length.out = nrow(dyn_df))

# Base arguments for plotting
args_plot <- list(
  T_g=p_mod$T_g, X_0=p_mod$X_0, r_0=p_mod$r_0, K=p_mod$K, D=p_mod$D, X_g=p_mod$X_e,
  mu_S1=mu_S1, mu_S2=mu_S2, ci_mu_S1=ci_mu_S1, ci_mu_S2=ci_mu_S2,
  cols=colors_vec, C=dyn_df$C
)

# Extract dynamic data according to the USER DEFINED mode
if (MODEL_MODE == "by_k") {
  args_plot$T_k <- p_mod$T_k
  args_plot$k_S1 <- dyn_df$s1_k; args_plot$k_S2 <- dyn_df$s2_k
  has_ci_s1 <- !any(is.na(dyn_df[, c("s1_CI_lower", "s1_CI_upper")]))
  has_ci_s2 <- !any(is.na(dyn_df[, c("s2_CI_lower", "s2_CI_upper")]))
  args_plot$ci_k_S1 <- if(has_ci_s1) as.matrix(dyn_df[, c("s1_CI_lower", "s1_CI_upper")]) else NULL
  args_plot$ci_k_S2 <- if(has_ci_s2) as.matrix(dyn_df[, c("s2_CI_lower", "s2_CI_upper")]) else NULL
} else if (MODEL_MODE == "by_SF") {
  args_plot$SF_S1 <- dyn_df$s1_sf; args_plot$SF_S2 <- dyn_df$s2_sf 
  has_ci_s1 <- !any(is.na(dyn_df[, c("s1_CI_lower", "s1_CI_upper")]))
  has_ci_s2 <- !any(is.na(dyn_df[, c("s2_CI_lower", "s2_CI_upper")]))
  args_plot$ci_SF_S1 <- if(has_ci_s1) as.matrix(dyn_df[, c("s1_CI_lower", "s1_CI_upper")]) else NULL
  args_plot$ci_SF_S2 <- if(has_ci_s2) as.matrix(dyn_df[, c("s2_CI_lower", "s2_CI_upper")]) else NULL
} else {
  stop("Invalid MODEL_MODE. Please set it to 'by_k' or 'by_SF'.")
}

# ------------------------------------------------------------------------------
# 2. PLOTTING FUNCTIONS 
# ------------------------------------------------------------------------------
draw_legend_custom <- function(labels, colors, title_local, is_first = FALSE) {
  par(mar = c(0, 1, 0, 0)) 
  plot(0, 0, type = "n", axes = FALSE, xlim = c(0, 100), ylim = c(0, 100), xlab = "", ylab = "")
  if (is_first) text(x = 0, y = 92, labels = "Legend", font = 2, cex = 1.8, adj = 0)
  text(x = 0, y = 72, labels = title_local, font = 1, cex = 1.2, adj = 0)
  n <- length(labels); if (n == 0) return()
  for (i in 1:n) {
    c_idx <- (i - 1) %% 3 + 1; r_idx <- ceiling(i / 3); y <- 55 - (r_idx - 1) * 15
    if(y < 0) break ; x <- c(2, 35, 68)[c_idx]
    rect(x, y - 2, x + 5, y + 4, col = colors[i], border = NA)
    text(x = x + 8, y = y, labels = labels[i], col = "black", cex = 1.1, adj = 0)
  }
}

# ------------------------------------------------------------------------------
# 3. MODEL FUNCTIONS
# ------------------------------------------------------------------------------
Plot_SE_Plane_k <- function(inputs, type = "all") {
  T_g <- inputs$T_g; T_k <- inputs$T_k
  X_0 <- inputs$X_0; r_0 <- inputs$r_0
  K <- inputs$K; D <- 1/inputs$D; X_g <- inputs$X_g
  mu_S1 <- inputs$mu_S1; mu_S2 <- inputs$mu_S2
  k_S1  <- inputs$k_S1; k_S2  <- inputs$k_S2
  ci_mu_S1 <- inputs$ci_mu_S1; ci_mu_S2 <- inputs$ci_mu_S2
  ci_k_S1  <- inputs$ci_k_S1; ci_k_S2  <- inputs$ci_k_S2
  cols <- inputs$cols
  nC <- length(k_S1); C_lbl <- inputs$C
  X_S1_0 <- X_0 / (1 + r_0); X_S2_0 <- X_0 - X_S1_0
  
  add_alpha <- function(col, alpha=1){
    if(missing(col)) stop("Please provide a color")
    apply(sapply(col, col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=alpha))  
  }
  
  sel_leg_info <- NULL; ext_leg_info <- NULL
  
  # (2) SELECTION PLANE PLOT
  if (type %in% c("selection", "all")) {
    FC <- 1 - mu_S2 / mu_S1
    if (!is.null(ci_mu_S2) && !is.null(ci_mu_S1)) {
      min_FC <- 1 - ci_mu_S2[2] / ci_mu_S1[1]
      max_FC <- 1 - ci_mu_S2[1] / ci_mu_S1[2]
    } else {
      min_FC <- numeric(0); max_FC <- numeric(0)
    }
    
    par(mar=c(5, 5, 4, 8) + 0.1, xpd=FALSE)
    plot(c(0, 1), c(0, 1), type="n", xlab="", ylab="", xlim=c(0, 1), ylim=c(0, 1), axes=FALSE, frame.plot=TRUE, xaxs="i", yaxs="i")
    title(main="Selection plane", cex.main=1.5)
    title(xlab="Fitness cost", ylab="Survival advantage", cex.lab=1.7)
    lines(seq(0, 1, length.out=100), seq(0, 1, length.out=100), col="black", lwd=2)
    text(0.02, 0.98, expression(paste("S2 is selected")), cex=1.5, adj=c(0, 1))
    text(0.98, 0.02, expression(paste("S1 is selected")), cex=1.5, adj=c(1, 0))
    text(0.5, 0.55, "Equilibrium", cex=1.5, srt=45, adj=c(0.5, 0))
    
    for (iC in 1:nC) {
      SA <- 1 - (k_S2[iC] * T_k - log(D)) / (k_S1[iC] * T_k - log(D))
      min_SA <- numeric(0); max_SA <- numeric(0)
      if (!is.null(ci_k_S2) && !is.null(ci_k_S1)) {
        if(nrow(ci_k_S2) >= iC && nrow(ci_k_S1) >= iC){
          min_SA <- 1 - (ci_k_S2[iC, 2] * T_k - log(D)) / (ci_k_S1[iC, 1] * T_k - log(D))
          max_SA <- 1 - (ci_k_S2[iC, 1] * T_k - log(D)) / (ci_k_S1[iC, 2] * T_k - log(D))
        }
      }
      aux_x <- c(min_FC, max_FC, max_FC, min_FC); aux_y <- c(min_SA, min_SA, max_SA, max_SA)
      curr_col <- cols[iC]
      if (length(aux_x) == length(aux_y) && length(aux_x) == 4) {
        polygon(aux_x, aux_y, col=add_alpha(curr_col, 0.4), border=curr_col, lwd=2)
      } else if (length(aux_x) == 4) {
        lines(seq(min_FC, max_FC, length.out=100), rep(SA, 100), col=curr_col, lwd=2)
      } else if (length(aux_y) == 4) {
        lines(rep(FC, 100), seq(min_SA, max_SA, length.out=100), col=curr_col, lwd=2)
      }
      points(FC, SA, pch=15, col=curr_col, cex=2)
    }
    axis(1, at=seq(0, 1, 0.2), labels=seq(0, 1, 0.2), cex.axis=1.5); axis(2, at=seq(0, 1, 0.2), labels=seq(0, 1, 0.2), cex.axis=1.5)
    box(); sel_leg_info <- list(lbl=C_lbl, col=cols)
  }
  
  # (3) EXTINCTION PLANE PLOT
  if (type %in% c("extinction", "all")) {
    c_s1_iso <- "#6baed6"; c_s1_cmp <- "#9ecae1"; c_s1_sel <- "#eff3ff"
    c_s2_iso <- "#fb6a4a"; c_s2_cmp <- "#fc9272"; c_s2_sel <- "#fee5d9"
    c_t_iso  <- "#756bb1"; c_t_cmp  <- "#bcbddc"
    
    if (!is.null(ci_k_S1)) { xm <- min(ci_k_S1[, 1]); xM <- max(ci_k_S1[, 2]) } else { xm <- 0.9 * min(k_S1); xM <- 1.1 * max(k_S1) }
    if (!is.null(ci_k_S2)) { ym <- min(ci_k_S2[, 1]); yM <- max(ci_k_S2[, 2]) } else { ym <- 0.9 * min(k_S2); yM <- 1.1 * max(k_S2) }
    
    tsat <- log(K / X_0) / ((X_S1_0 / X_0) * mu_S1 + (X_S2_0 / X_0) * mu_S2)
    ext_S1_isol <- min((mu_S1 * T_g + log(D)) / T_k, log(K * D / X_g) / T_k)
    ext_S2_isol <- min((mu_S2 * T_g + log(D)) / T_k, log(K * D / X_g) / T_k)
    ext_S1 <- (mu_S1 * tsat + log(D) - log(X_g / X_S1_0)) / T_k
    ext_S2 <- (mu_S2 * tsat + log(D) - log(X_g / X_S2_0)) / T_k
    
    par(mar=c(10, 5, 4, 2) + 0.1, xpd=FALSE)
    xm <- min(c(xm, 0.9 * ext_S1_isol, 0.9 * ext_S1)); xM <- max(c(xM, 1.1 * ext_S1_isol))
    ym <- min(c(ym, 0.9 * ext_S2_isol, 0.9 * ext_S2)); yM <- max(c(yM, 1.1 * ext_S2_isol))
    
    plot(c(xm, xM), c(ym, yM), type="n", xlab="", ylab="", xlim=c(xm, xM), ylim=c(ym, yM), axes=FALSE, frame.plot=TRUE, xaxs="i", yaxs="i")
    title(main="Extinction plane", cex.main=1.5); title(xlab=expression(paste("Kill rate of S1")), ylab=expression(paste("Kill rate of S2")), cex.lab=1.7)
    x <- seq(xm, xM, length.out=100); y <- seq(ym, yM, length.out=100)
    
    # LAYER 1: POLYGONS
    xaux <- ext_S1_isol; yaux <- ext_S2_isol
    polygon(c(xaux, xM, xM, xaux), c(ym, ym, min(ext_S2, yaux), min(ext_S2, yaux)), col=c_s1_iso, border=NA)
    polygon(c(xm, min(ext_S1, xaux), min(ext_S1, xaux), xm), c(yaux, yaux, yM, yM), col=c_s2_iso, border=NA)
    polygon(c(xaux, xM, xM, xaux), c(yaux, yaux, yM, yM), col=c_t_iso, border=NA)
    
    lgd <- c(); fill_cols <- c()
    if (ext_S1 < ext_S1_isol && ext_S2 < ext_S2_isol) {
      lgd <- c('S1 Extinction (Isolation)', 'S2 Extinction (Isolation)', 'Total Extinction (Isolation)', 'S1 Extinction (Competition)', 'S2 Extinction (Competition)', 'Total Extinction (Competition)')
      fill_cols <- c(c_s1_iso, c_s2_iso, c_t_iso, c_s1_cmp, c_s2_cmp, c_t_cmp)
      xaux_c <- ext_S1; yaux_c <- ext_S2
      polygon(c(xaux_c, ext_S1_isol, ext_S1_isol, xaux_c), c(ym, ym, yaux_c, yaux_c), col=c_s1_cmp, border=NA)
      polygon(c(xm, xaux_c, xaux_c, xm), c(yaux_c, yaux_c, ext_S2_isol, ext_S2_isol), col=c_s2_cmp, border=NA)
      polygon(c(xaux_c, ext_S1_isol, ext_S1_isol, xaux_c), c(yaux_c, yaux_c, yM, yM), col=c_t_cmp, border=NA)
      polygon(c(ext_S1_isol, xM, xM, ext_S1_isol), c(yaux_c, yaux_c, ext_S2_isol, ext_S2_isol), col=c_t_cmp, border=NA)
    } else if (ext_S1 < ext_S1_isol) {
      lgd <- c('S1 Extinction (Isolation)', 'S2 Extinction (Isolation)', 'Total Extinction (Isolation)', 'S1 Extinction (Competition)', 'Total Extinction (Competition)')
      fill_cols <- c(c_s1_iso, c_s2_iso, c_t_iso, c_s1_cmp, c_t_cmp)
      xaux_c <- ext_S1; yaux_c <- ext_S2_isol
      polygon(c(xaux_c, ext_S1_isol, ext_S1_isol, xaux_c), c(ym, ym, yaux_c, yaux_c), col=c_s1_cmp, border=NA)
      polygon(c(xaux_c, ext_S1_isol, ext_S1_isol, xaux_c), c(yaux_c, yaux_c, yM, yM), col=c_t_cmp, border=NA)
    } else if (ext_S2 < ext_S2_isol) {
      lgd <- c('S1 Extinction (Isolation)', 'S2 Extinction (Isolation)', 'Total Extinction (Isolation)', 'S2 Extinction (Competition)', 'Total Extinction (Competition)')
      fill_cols <- c(c_s1_iso, c_s2_iso, c_t_iso, c_s2_cmp, c_t_cmp)
      xaux_c <- ext_S1_isol; yaux_c <- ext_S2
      polygon(c(xm, xaux_c, xaux_c, xm), c(yaux_c, yaux_c, ext_S2_isol, ext_S2_isol), col=c_s2_cmp, border=NA)
      polygon(c(xaux_c, xM, xM, xaux_c), c(yaux_c, yaux_c, ext_S2_isol, ext_S2_isol), col=c_t_cmp, border=NA)
    } else {
      lgd <- c('S1 Extinction (Isolation)', 'S2 Extinction (Isolation)', 'Total Extinction (Isolation)')
      fill_cols <- c(c_s1_iso, c_s2_iso, c_t_iso)
    }
    
    yint_S1 <- xaux * (1 - FC) + FC * log(D) / T_k
    xint_S2 <- (yaux - log(D) * FC / T_k) / (1 - FC)
    xint <- min(xaux, xint_S2)
    xx   <- seq(xm, xint, length.out=100)
    xint_0 <- FC * log(D) / (T_k * (FC - 1))
    
    if (xint == xaux) {
      polygon(c(xint_0, xaux, xaux), c(0, 0, yint_S1), col=c_s1_sel, border=NA)
      polygon(c(0, xint_0, xaux, xaux, 0), c(0, 0, yint_S1, yaux, yaux), col=c_s2_sel, border=NA)
    } else {
      polygon(c(xint_0, xaux, xaux, xint), c(0, 0, yaux, yaux), col=c_s1_sel, border=NA)
      polygon(c(0, xint_0, xint, 0), c(0, 0, yaux, yaux), col=c_s2_sel, border=NA)
    }
    
    # LAYER 2: LINES
    lines(x, rep(ext_S2_isol, 100), col="black", lwd=2); lines(rep(ext_S1_isol, 100), y, col="black", lwd=2)
    if (ext_S1 < ext_S1_isol && ext_S2 < ext_S2_isol) {
      lines(x, rep(ext_S2, 100), col="black", lty=2, lwd=1); lines(rep(ext_S1, 100), y, col="black", lty=2, lwd=1)
    } else if (ext_S1 < ext_S1_isol) {
      lines(rep(ext_S1, 100), y, col="black", lty=2, lwd=1)
    } else if (ext_S2 < ext_S2_isol) {
      lines(x, rep(ext_S2, 100), col="black", lty=2, lwd=1)
    }
    lines(xx, xx * (1 - FC) + FC * log(D) / T_k, col="black", lwd=2)
    
    # LAYER 3: DATA & TEXT
    slope_eq <- 1 - FC; angle_eq <- atan(slope_eq) * 180 / pi
    mid_x <- (xm + xint) / 2; mid_y <- mid_x * slope_eq + (FC * log(D) / T_k)
    text(mid_x, mid_y, "Equilibrium", cex=1.2, srt=angle_eq, adj=c(0.5, -0.5))
    for (iC in 1:nC) {
      points(k_S1[iC], k_S2[iC], pch=15, col=cols[iC], cex=2)
      if (!is.null(ci_k_S1) && !is.null(ci_k_S2)) {
        if(length(ci_k_S1) == 2*nC && length(ci_k_S2) == 2*nC) {
          polygon(c(ci_k_S1[iC, 1], ci_k_S1[iC, 2], ci_k_S1[iC, 2], ci_k_S1[iC, 1]),
                  c(ci_k_S2[iC, 1], ci_k_S2[iC, 1], ci_k_S2[iC, 2], ci_k_S2[iC, 2]),
                  col=add_alpha(cols[iC], 0.4), border=cols[iC], lwd=2)
        }
      }
    }
    axis(1, cex.axis=1.5); axis(2, cex.axis=1.5); box()
    lgd <- c(lgd, "S1 Selection", "S2 Selection"); fill_cols <- c(fill_cols, c_s1_sel, c_s2_sel)
    ext_leg_info <- list(lbl=lgd, col=fill_cols)
  }
  return(list(sel_leg = sel_leg_info, ext_leg = ext_leg_info))
}

Plot_SE_Plane_SF <- function(inputs, type = "all") {
  T_g <- inputs$T_g; X_0 <- inputs$X_0; r_0 <- inputs$r_0
  K <- inputs$K; X_g <- inputs$X_g;  D <- 1 / inputs$D
  mu_S1 <- inputs$mu_S1; mu_S2 <- inputs$mu_S2
  SF_S1 <- inputs$SF_S1; SF_S2 <- inputs$SF_S2
  ci_SF_S1 <- inputs$ci_SF_S1; ci_SF_S2 <- inputs$ci_SF_S2
  ci_mu_S1 <- inputs$ci_mu_S1; ci_mu_S2 <- inputs$ci_mu_S2
  logSF_S1 <- log(SF_S1); logSF_S2 <- log(SF_S2)
  ci_logSF_S1 <- if(!is.null(ci_SF_S1)) log(ci_SF_S1) else NULL
  ci_logSF_S2 <- if(!is.null(ci_SF_S2)) log(ci_SF_S2) else NULL
  cols <- inputs$cols; nC <- length(SF_S1); C_lbl <- inputs$C
  X_S1_0 <- X_0 / (1 + r_0); X_S2_0 <- X_0 - X_S1_0
  
  add_alpha <- function(col, alpha=1){
    if(missing(col)) stop("Please provide a color")
    apply(sapply(col, col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=alpha))  
  }
  
  sel_leg_info <- NULL; ext_leg_info <- NULL
  
  # (2) SELECTION PLANE PLOT
  if (type %in% c("selection", "all")) {
    FC <- 1 - mu_S2 / mu_S1
    min_FC <- numeric(0); max_FC <- numeric(0)
    if (!is.null(ci_mu_S2) && !is.null(ci_mu_S1)) {
      min_FC <- 1 - ci_mu_S2[2] / ci_mu_S1[1]; max_FC <- 1 - ci_mu_S2[1] / ci_mu_S1[2]
    }
    
    par(mar=c(5, 5, 4, 2) + 0.1, xpd=FALSE)
    plot(c(0, 1), c(0, 1), type="n", xlab="", ylab="", xlim=c(0, 1), ylim=c(0, 1), axes=FALSE, frame.plot=TRUE, xaxs="i", yaxs="i")
    title(main="Selection plane", cex.main=1.5); title(xlab="Fitness cost", ylab="Survival advantage", cex.lab=1.7)
    lines(seq(0, 1, length.out=100), seq(0, 1, length.out=100), col="black", lwd=2)
    text(0.02, 0.98, expression(paste("S2 is selected")), cex=1.5, adj=c(0, 1))
    text(0.98, 0.02, expression(paste("S1 is selected")), cex=1.5, adj=c(1, 0))
    text(0.5, 0.55, "Equilibrium", cex=1.5, srt=45, adj=c(0.5, 0))
    
    for (iC in 1:nC) {
      SA <- 1 - (logSF_S2[iC] + log(D)) / (logSF_S1[iC] + log(D))
      min_SA <- numeric(0); max_SA <- numeric(0)
      if (!is.null(ci_logSF_S2) && !is.null(ci_logSF_S1) && nrow(ci_logSF_S2)>=iC && nrow(ci_logSF_S1)>=iC) {
        min_SA <- 1 - (ci_logSF_S2[iC, 2] + log(D)) / (ci_logSF_S1[iC, 1] + log(D))
        max_SA <- 1 - (ci_logSF_S2[iC, 1] + log(D)) / (ci_logSF_S1[iC, 2] + log(D))
      }
      curr_col <- cols[iC]
      if (length(min_FC)>0 && length(min_SA)>0) polygon(c(min_FC, max_FC, max_FC, min_FC), c(min_SA, min_SA, max_SA, max_SA), col=add_alpha(curr_col, 0.4), border=curr_col, lwd=2)
      else if (length(min_FC)>0) lines(seq(min_FC, max_FC, length.out=100), rep(SA, 100), col=curr_col, lwd=2)
      else if (length(min_SA)>0) lines(rep(FC, 100), seq(min_SA, max_SA, length.out=100), col=curr_col, lwd=2)
      points(FC, SA, pch=15, col=curr_col, cex=2)
    }
    axis(1, at=seq(0, 1, 0.2), cex.axis=1.5); axis(2, at=seq(0, 1, 0.2), cex.axis=1.5); box()
    sel_leg_info <- list(lbl=C_lbl, col=cols)
  }
  
  # (3) EXTINCTION PLANE PLOT
  if (type %in% c("extinction", "all")) {
    c_s1_iso <- "#6baed6"; c_s1_cmp <- "#9ecae1"; c_s1_sel <- "#eff3ff"
    c_s2_iso <- "#fb6a4a"; c_s2_cmp <- "#fc9272"; c_s2_sel <- "#fee5d9"
    c_t_iso  <- "#756bb1"; c_t_cmp  <- "#bcbddc"
    
    if (!is.null(ci_SF_S1)) { xm <- min(ci_SF_S1[, 1]); xM <- max(ci_SF_S1[, 2]) } else { xm <- 0.1 * min(SF_S1); xM <- min(1, 10 * max(SF_S1)) }
    if (!is.null(ci_SF_S2)) { ym <- min(ci_SF_S2[, 1]); yM <- max(ci_SF_S2[, 2]) } else { ym <- 0.1 * min(SF_S2); yM <- min(1, 10 * max(SF_S2)) }
    
    tsat <- log(K / X_0) / ((X_S1_0 / X_0) * mu_S1 + (X_S2_0 / X_0) * mu_S2)
    ext_S1_isol <- max(exp(- mu_S1 * T_g) / D, X_g / (D * K))
    ext_S2_isol <- max(exp(- mu_S2 * T_g) / D, X_g / (D * K))
    ext_S1 <- (exp(- mu_S1 * tsat) * X_g) / (X_S1_0 * D)
    ext_S2 <- (exp(- mu_S2 * tsat) * X_g) / (X_S2_0 * D)
    
    par(mar=c(10, 5, 4, 2) + 0.1, xpd=FALSE)
    plot(c(xm, xM), c(ym, yM), type="n", log="xy", xlab="", ylab="", xlim=c(xm, xM), ylim=c(ym, yM), axes=FALSE, frame.plot=TRUE, xaxs="i", yaxs="i")
    title(main="Extinction plane", cex.main=1.5); title(xlab=expression(paste("Survival fraction of S1")), ylab=expression(paste("Survival fraction of S2  ")), cex.lab=1.7)
    x <- 10^seq(log10(xm), log10(xM), length.out=100); y <- 10^seq(log10(ym), log10(yM), length.out=100)
    
    # LAYER 1: POLYGONS
    polygon(c(xm, ext_S1_isol, ext_S1_isol, xm), c(max(ext_S2, ext_S2_isol), max(ext_S2, ext_S2_isol), yM, yM), col=c_s1_iso, border=NA)
    polygon(c(max(ext_S1, ext_S1_isol), xM, xM, max(ext_S1, ext_S1_isol)), c(ym, ym, ext_S2_isol, ext_S2_isol), col=c_s2_iso, border=NA)
    polygon(c(xm, ext_S1_isol, ext_S1_isol, xm), c(ym, ym, ext_S2_isol, ext_S2_isol), col=c_t_iso, border=NA)
    
    lgd <- c(); fill_cols <- c()
    if (ext_S1 > ext_S1_isol && ext_S2 > ext_S2_isol) {
      lgd <- c('S1 Extinction (Isolation)', 'S2 Extinction (Isolation)', 'Total Extinction (Isolation)', 'S1 Extinction (Competition)', 'S2 Extinction (Competition)', 'Total Extinction (Competition)')
      fill_cols <- c(c_s1_iso, c_s2_iso, c_t_iso, c_s1_cmp, c_s2_cmp, c_t_cmp)
      xaux <- ext_S1; yaux <- ext_S2
      polygon(c(ext_S1_isol, ext_S1, ext_S1, ext_S1_isol), c(yaux, yaux, yM, yM), col=c_s1_cmp, border=NA)
      polygon(c(xaux, xM, xM, xaux), c(ext_S2_isol, ext_S2_isol, ext_S2, ext_S2), col=c_s2_cmp, border=NA)
      polygon(c(xm, xaux, xaux, xm), c(ext_S2_isol, ext_S2_isol, ext_S2, ext_S2), col=c_t_cmp, border=NA)
      polygon(c(ext_S1_isol, ext_S1, ext_S1, ext_S1_isol), c(ym, ym, ext_S2_isol, ext_S2_isol), col=c_t_cmp, border=NA)
    } else if (ext_S1 > ext_S1_isol) {
      lgd <- c('S1 Extinction (Isolation)', 'S2 Extinction (Isolation)', 'Total Extinction (Isolation)', 'S1 Extinction (Competition)', 'Total Extinction (Competition)')
      fill_cols <- c(c_s1_iso, c_s2_iso, c_t_iso, c_s1_cmp, c_t_cmp)
      xaux <- ext_S1; yaux <- ext_S2_isol
      polygon(c(ext_S1_isol, xaux, xaux, ext_S1_isol), c(yaux, yaux, yM, yM), col=c_s1_cmp, border=NA)
      polygon(c(ext_S1_isol, xaux, xaux, ext_S1_isol), c(ym, ym, yaux, yaux), col=c_t_cmp, border=NA)
    } else if (ext_S2 > ext_S2_isol) {
      lgd <- c('S1 Extinction (Isolation)', 'S2 Extinction (Isolation)', 'Total Extinction (Isolation)', 'S2 Extinction (Competition)', 'Total Extinction (Competition)')
      fill_cols <- c(c_s1_iso, c_s2_iso, c_t_iso, c_s2_cmp, c_t_cmp)
      xaux <- ext_S1_isol; yaux <- ext_S2
      polygon(c(xaux, xM, xM, xaux), c(ext_S2_isol, ext_S2_isol, yaux, yaux), col=c_s2_cmp, border=NA)
      polygon(c(xm, xaux, xaux, xm), c(ext_S2_isol, ext_S2_isol, yaux, yaux), col=c_t_cmp, border=NA)
    } else {
      lgd <- c('S1 Extinction (Isolation)', 'S2 Extinction (Isolation)', 'Total Extinction (Isolation)')
      fill_cols <- c(c_s1_iso, c_s2_iso, c_t_iso)
      xaux <- ext_S1_isol; yaux <- ext_S2_isol
    }
    
    yint_S1 <- xaux^(1-FC) / (D^FC); xint_S2 <- (yaux * D^FC)^(1/(1-FC))
    xint <- max(xaux, xint_S2)
    xx   <- 10^seq(log10(xint), log10(xM), length.out=100)
    xint_M <- (yM * D^FC)^(1/(1-FC)); yint_M <- xM^(1-FC) / (D^FC)
    
    if (xint == xaux) {
      if (yint_M < yM) {
        polygon(c(xaux, xM, xM, xaux), c(yaux, yaux, yint_M, yint_S1), col=c_s1_sel, border=NA)
        polygon(c(xaux, xM, xM, xaux), c(yint_S1, yint_M, yM, yM), col=c_s2_sel, border=NA)
      } else if (yint_M == yM) {
        polygon(c(xaux, xM, xM, xaux), c(yaux, yaux, yM, yint_S1), col=c_s1_sel, border=NA)
        polygon(c(xaux, xM, xaux), c(yint_S1, yM, yM), col=c_s2_sel, border=NA)
      } else {
        polygon(c(xaux, xM, xM, xint_M, xaux), c(yaux, yaux, yM, yM, yint_S1), col=c_s1_sel, border=NA)
        polygon(c(xaux, xint_M, xaux), c(yint_S1, yM, yM), col=c_s2_sel, border=NA)
      }
    } else {
      if (yint_M <= yM) {
        polygon(c(xint, xM, xM), c(yaux, yaux, yint_M), col=c_s1_sel, border=NA)
        polygon(c(xaux, xint, xM, xM, xaux), c(yaux, yaux, yint_M, yM, yM), col=c_s2_sel, border=NA)
      } else if (yint_M == yM) {
        polygon(c(xint, xM, xM), c(yaux, yaux, yM), col=c_s1_sel, border=NA)
        polygon(c(xaux, xint, xM, xaux), c(yaux, yaux, yM, yM), col=c_s2_sel, border=NA)
      } else {
        polygon(c(xint, xM, xM, xint_M), c(yaux, yaux, yM, yM), col=c_s1_sel, border=NA)
        polygon(c(xaux, xint, xint_M, xaux), c(yaux, yaux, yM, yM), col=c_s2_sel, border=NA)
      }
    }
    
    # LAYER 2: LINES
    lines(x, rep(ext_S2_isol, 100), col="black", lwd=2); lines(rep(ext_S1_isol, 100), y, col="black", lwd=2)
    if (ext_S1 > ext_S1_isol && ext_S2 > ext_S2_isol) {
      lines(x, rep(ext_S2, 100), col="black", lty=2, lwd=1); lines(rep(ext_S1, 100), y, col="black", lty=2, lwd=1)
    } else if (ext_S1 > ext_S1_isol) {
      lines(rep(ext_S1, 100), y, col="black", lty=2, lwd=1)
    } else if (ext_S2 > ext_S2_isol) {
      lines(x, rep(ext_S2, 100), col="black", lty=2, lwd=1)
    }
    lines(xx, xx^(1 - FC) / (D^FC), col="black", lwd=2)
    
    # LAYER 3: DATA & TEXT
    slope_eq <- 1 - FC; angle_eq <- atan(slope_eq) * 180 / pi
    log_mx <- (log10(xint) + log10(xM)) / 2; mid_x <- 10^log_mx; mid_y <- mid_x^(1 - FC) / (D^FC)
    text(mid_x, mid_y, "Equilibrium", cex=1.2, srt=angle_eq, adj=c(0.5, -0.5))
    
    for (iC in 1:nC) {
      points(SF_S1[iC], SF_S2[iC], pch=15, col=cols[iC], cex=2)
      if (!is.null(ci_SF_S1) && !is.null(ci_SF_S2) && nrow(ci_SF_S1)>=iC && nrow(ci_SF_S2)>=iC) {
        polygon(c(ci_SF_S1[iC, 1], ci_SF_S1[iC, 2], ci_SF_S1[iC, 2], ci_SF_S1[iC, 1]), c(ci_SF_S2[iC, 1], ci_SF_S2[iC, 1], ci_SF_S2[iC, 2], ci_SF_S2[iC, 2]), col=add_alpha(cols[iC], 0.4), border=cols[iC], lwd=2)
      }
    }
    
    ticks <- c(1e-12, 1e-10, 1e-8, 1e-6, 1e-4, 1e-2, 1)
    axis(1, at=ticks, labels=ticks, cex.axis=1.5); axis(2, at=ticks, labels=ticks, cex.axis=1.5); box()
    lgd <- c(lgd, "S1 Selection", "S2 Selection"); fill_cols <- c(fill_cols, c_s1_sel, c_s2_sel)
    ext_leg_info <- list(lbl=lgd, col=fill_cols)
  }
  return(list(sel_leg = sel_leg_info, ext_leg = ext_leg_info))
}
# ------------------------------------------------------------------------------
# 4. PLOT EXECUTION
# ------------------------------------------------------------------------------
layout(matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE), heights = c(0.82, 0.18))

if (MODEL_MODE == "by_k") { leg_data <- Plot_SE_Plane_k(args_plot) 
} else { leg_data <- Plot_SE_Plane_SF(args_plot) }

if (!is.null(leg_data)) {
  draw_legend_custom(leg_data$sel_leg$lbl, leg_data$sel_leg$col, title_local = "Drug concentration (arbitrary units)", is_first = TRUE)
  draw_legend_custom(leg_data$ext_leg$lbl, leg_data$ext_leg$col, title_local = "Extinction/ Selection areas", is_first = FALSE)
}
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)