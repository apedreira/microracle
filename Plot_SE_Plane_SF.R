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
  
  cols <- inputs$cols
  col_ext_S1 <- inputs$col_ext_S1; col_ext_S2 <- inputs$col_ext_S2
  col_SR_S1  <- inputs$col_SR_S1; col_SR_S2  <- inputs$col_SR_S2
  col_ext_T  <- inputs$col_ext_T
  
  nC <- length(SF_S1)
  C_lbl <- inputs$C
  X_S1_0 <- X_0 / (1 + r_0); X_S2_0 <- X_0 - X_S1_0
  
  add_alpha <- function(col, alpha=1){
    if(missing(col)) stop("Please provide a color")
    apply(sapply(col, col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=alpha))  
  }
  
  sel_leg_info <- NULL
  ext_leg_info <- NULL
  
  if (type %in% c("selection", "all")) {
    FC <- 1 - mu_S2 / mu_S1
    min_FC <- numeric(0); max_FC <- numeric(0)
    if (!is.null(ci_mu_S2) && !is.null(ci_mu_S1)) {
      min_FC <- 1 - ci_mu_S2[2] / ci_mu_S1[1]; max_FC <- 1 - ci_mu_S2[1] / ci_mu_S1[2]
    }
    
    par(mar=c(5, 5, 4, 2) + 0.1, xpd=FALSE)
    plot(c(0, 1), c(0, 1), type="n", xlab="", ylab="", xlim=c(0, 1), ylim=c(0, 1), axes=FALSE, frame.plot=TRUE, xaxs="i", yaxs="i")
    
    title(main="Selection plane", cex.main=1.5)
    title(xlab="Fitness cost", ylab="Survival advantage", cex.lab=1.7)
    
    lines(seq(0, 1, length.out=100), seq(0, 1, length.out=100), col="black", lwd=2)
    
    # --- ETIQUETAS ACTIVADAS PARA SELECTION PLANE ---
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
    
    # EJES Y CAJA AL FINAL
    axis(1, at=seq(0, 1, 0.2), cex.axis=1.5); axis(2, at=seq(0, 1, 0.2), cex.axis=1.5)
    box()
    
    sel_leg_info <- list(lbl=C_lbl, col=cols)
  }
  
  if (type %in% c("extinction", "all")) {
    
    # Paleta
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
    
    title(main="Extinction plane", cex.main=1.5)
    title(xlab=expression(paste("Survival fraction of S1")), ylab=expression(paste("Survival fraction of S2  ")), cex.lab=1.7)
    
    x <- 10^seq(log10(xm), log10(xM), length.out=100); y <- 10^seq(log10(ym), log10(yM), length.out=100)
    
    # -------------------------------------------------------------------------
    # CAPA 1: POLÍGONOS
    # -------------------------------------------------------------------------
    
    # 1.1 Isolation
    polygon(c(xm, ext_S1_isol, ext_S1_isol, xm), c(max(ext_S2, ext_S2_isol), max(ext_S2, ext_S2_isol), yM, yM), col=c_s1_iso, border=NA)
    polygon(c(max(ext_S1, ext_S1_isol), xM, xM, max(ext_S1, ext_S1_isol)), c(ym, ym, ext_S2_isol, ext_S2_isol), col=c_s2_iso, border=NA)
    polygon(c(xm, ext_S1_isol, ext_S1_isol, xm), c(ym, ym, ext_S2_isol, ext_S2_isol), col=c_t_iso, border=NA)
    
    lgd <- c(); fill_cols <- c()
    
    # 1.2 Competition
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
    
    # 1.3 Selección
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
    
    # -------------------------------------------------------------------------
    # CAPA 2: LÍNEAS
    # -------------------------------------------------------------------------
    
    # 2.1 Isolation
    lines(x, rep(ext_S2_isol, 100), col="black", lwd=2); lines(rep(ext_S1_isol, 100), y, col="black", lwd=2)
    
    # 2.2 Competition
    if (ext_S1 > ext_S1_isol && ext_S2 > ext_S2_isol) {
      lines(x, rep(ext_S2, 100), col="black", lty=2, lwd=1); lines(rep(ext_S1, 100), y, col="black", lty=2, lwd=1)
    } else if (ext_S1 > ext_S1_isol) {
      lines(rep(ext_S1, 100), y, col="black", lty=2, lwd=1)
    } else if (ext_S2 > ext_S2_isol) {
      lines(x, rep(ext_S2, 100), col="black", lty=2, lwd=1)
    }
    
    # 2.3 Equilibrio
    lines(xx, xx^(1 - FC) / (D^FC), col="black", lwd=2)
    
    # -------------------------------------------------------------------------
    # CAPA 3: DATOS Y TEXTOS
    # -------------------------------------------------------------------------
    slope_eq <- 1 - FC; angle_eq <- atan(slope_eq) * 180 / pi
    log_mx <- (log10(xint) + log10(xM)) / 2
    mid_x <- 10^log_mx; mid_y <- mid_x^(1 - FC) / (D^FC)
    text(mid_x, mid_y, "Equilibrium", cex=1.2, srt=angle_eq, adj=c(0.5, -0.5))
    
    # --- ETIQUETAS "S1/S2 SELECTED" ELIMINADAS EN EXTINCTION ---
    
    for (iC in 1:nC) {
      points(SF_S1[iC], SF_S2[iC], pch=15, col=cols[iC], cex=2)
      if (!is.null(ci_SF_S1) && !is.null(ci_SF_S2) && nrow(ci_SF_S1)>=iC && nrow(ci_SF_S2)>=iC) {
        polygon(c(ci_SF_S1[iC, 1], ci_SF_S1[iC, 2], ci_SF_S1[iC, 2], ci_SF_S1[iC, 1]), c(ci_SF_S2[iC, 1], ci_SF_S2[iC, 1], ci_SF_S2[iC, 2], ci_SF_S2[iC, 2]), col=add_alpha(cols[iC], 0.4), border=cols[iC], lwd=2)
      }
    }
    
    # -------------------------------------------------------------------------
    # CAPA 4: MARCO Y EJES (AL FINAL)
    # -------------------------------------------------------------------------
    ticks <- c(1e-12, 1e-10, 1e-8, 1e-6, 1e-4, 1e-2, 1)
    axis(1, at=ticks, labels=ticks, cex.axis=1.5); axis(2, at=ticks, labels=ticks, cex.axis=1.5)
    box()
    
    # Leyendas
    lgd <- c(lgd, "S1 Selection", "S2 Selection")
    fill_cols <- c(fill_cols, c_s1_sel, c_s2_sel)
    ext_leg_info <- list(lbl=lgd, col=fill_cols)
  }
  return(list(sel_leg = sel_leg_info, ext_leg = ext_leg_info))
}