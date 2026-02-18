Plot_SE_Plane_k <- function(inputs, type = "all") {
  # (1) Initialise variables:
  T_g <- inputs$T_g; T_k <- inputs$T_k
  X_0 <- inputs$X_0; r_0 <- inputs$r_0
  K <- inputs$K; D <- 1/inputs$D; X_g <- inputs$X_g
  
  mu_S1 <- inputs$mu_S1; mu_S2 <- inputs$mu_S2
  k_S1  <- inputs$k_S1; k_S2  <- inputs$k_S2
  
  ci_mu_S1 <- inputs$ci_mu_S1; ci_mu_S2 <- inputs$ci_mu_S2
  ci_k_S1  <- inputs$ci_k_S1; ci_k_S2  <- inputs$ci_k_S2
  
  cols <- inputs$cols
  col_ext_S1 <- inputs$col_ext_S1; col_ext_S2 <- inputs$col_ext_S2
  col_SR_S1  <- inputs$col_SR_S1; col_SR_S2  <- inputs$col_SR_S2
  col_ext_T  <- inputs$col_ext_T
  
  nC <- length(k_S1)
  C_lbl <- inputs$C
  
  X_S1_0 <- X_0 / (1 + r_0)
  X_S2_0 <- X_0 - X_S1_0
  
  add_alpha <- function(col, alpha=1){
    if(missing(col)) stop("Please provide a color")
    apply(sapply(col, col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=alpha))  
  }
  
  # Variables para almacenar info de leyendas
  sel_leg_info <- NULL
  ext_leg_info <- NULL
  
  # ===========================================================================
  # (2) First plot (SELECTION PLANE)
  # ===========================================================================
  if (type %in% c("selection", "all")) {
    
    FC <- 1 - mu_S2 / mu_S1
    
    if (!is.null(ci_mu_S2) && !is.null(ci_mu_S1)) {
      min_FC <- 1 - ci_mu_S2[2] / ci_mu_S1[1]
      max_FC <- 1 - ci_mu_S2[1] / ci_mu_S1[2]
    } else {
      min_FC <- numeric(0); max_FC <- numeric(0)
    }
    
    par(mar=c(5, 5, 4, 8) + 0.1, xpd=FALSE)
    
    plot(c(0, 1), c(0, 1), type="n", xlab="", ylab="", xlim=c(0, 1), ylim=c(0, 1), 
         axes=FALSE, frame.plot=TRUE, xaxs="i", yaxs="i")
    
    # TITULO AÑADIDO
    title(main="Selection plane", cex.main=1.5)
    title(xlab="Fitness cost", ylab="Survival advantage", cex.lab=1.7)
    
    lines(seq(0, 1, length.out=100), seq(0, 1, length.out=100), col="black", lwd=2)
    
    # --- ETIQUETAS ACTIVADAS PARA SELECTION PLANE ---
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
      
      aux_x <- c(min_FC, max_FC, max_FC, min_FC)
      aux_y <- c(min_SA, min_SA, max_SA, max_SA)
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
    
    # EJES Y CAJA AL FINAL
    axis(1, at=seq(0, 1, 0.2), labels=seq(0, 1, 0.2), cex.axis=1.5)
    axis(2, at=seq(0, 1, 0.2), labels=seq(0, 1, 0.2), cex.axis=1.5)
    box()
    
    sel_leg_info <- list(lbl=C_lbl, col=cols)
  }
  
  # ===========================================================================
  # (3) Second plot (EXTINCTION PLANE)
  # ===========================================================================
  if (type %in% c("extinction", "all")) {
    
    # Paleta
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
    
    plot(c(xm, xM), c(ym, yM), type="n", xlab="", ylab="", xlim=c(xm, xM), ylim=c(ym, yM), 
         axes=FALSE, frame.plot=TRUE, xaxs="i", yaxs="i")
    
    title(main="Extinction plane", cex.main=1.5)
    title(xlab=expression(paste("Kill rate of S1")), ylab=expression(paste("Kill rate of S2")), cex.lab=1.7)
    
    x <- seq(xm, xM, length.out=100); y <- seq(ym, yM, length.out=100)
    
    # -------------------------------------------------------------------------
    # CAPA 1: POLÍGONOS (FONDO)
    # -------------------------------------------------------------------------
    
    # 1.1 Polígonos Isolation
    xaux <- ext_S1_isol; yaux <- ext_S2_isol
    polygon(c(xaux, xM, xM, xaux), c(ym, ym, min(ext_S2, yaux), min(ext_S2, yaux)), col=c_s1_iso, border=NA)
    polygon(c(xm, min(ext_S1, xaux), min(ext_S1, xaux), xm), c(yaux, yaux, yM, yM), col=c_s2_iso, border=NA)
    polygon(c(xaux, xM, xM, xaux), c(yaux, yaux, yM, yM), col=c_t_iso, border=NA)
    
    lgd <- c(); fill_cols <- c()
    
    # 1.2 Polígonos Competition
    if (ext_S1 < ext_S1_isol && ext_S2 < ext_S2_isol) {
      lgd <- c('S1 Extinction (Isolation)', 'S2 Extinction (Isolation)', 'Total Extinction (Isolation)', 'S1 Extinction (Competition)', 'S2 Extinction (Competition)', 'Total Extinction (Competition)')
      fill_cols <- c(c_s1_iso, c_s2_iso, c_t_iso, c_s1_cmp, c_s2_cmp, c_t_cmp)
      
      xaux <- ext_S1; yaux <- ext_S2
      polygon(c(xaux, ext_S1_isol, ext_S1_isol, xaux), c(ym, ym, yaux, yaux), col=c_s1_cmp, border=NA)
      polygon(c(xm, xaux, xaux, xm), c(yaux, yaux, ext_S2_isol, ext_S2_isol), col=c_s2_cmp, border=NA)
      polygon(c(xaux, ext_S1_isol, ext_S1_isol, xaux), c(yaux, yaux, yM, yM), col=c_t_cmp, border=NA)
      polygon(c(ext_S1_isol, xM, xM, ext_S1_isol), c(yaux, yaux, ext_S2_isol, ext_S2_isol), col=c_t_cmp, border=NA)
      
    } else if (ext_S1 < ext_S1_isol) {
      lgd <- c('S1 Extinction (Isolation)', 'S2 Extinction (Isolation)', 'Total Extinction (Isolation)', 'S1 Extinction (Competition)', 'Total Extinction (Competition)')
      fill_cols <- c(c_s1_iso, c_s2_iso, c_t_iso, c_s1_cmp, c_t_cmp)
      xaux <- ext_S1; yaux <- ext_S2_isol
      polygon(c(xaux, ext_S1_isol, ext_S1_isol, xaux), c(ym, ym, yaux, yaux), col=c_s1_cmp, border=NA)
      polygon(c(xaux, ext_S1_isol, ext_S1_isol, xaux), c(yaux, yaux, yM, yM), col=c_t_cmp, border=NA)
      
    } else if (ext_S2 < ext_S2_isol) {
      lgd <- c('S1 Extinction (Isolation)', 'S2 Extinction (Isolation)', 'Total Extinction (Isolation)', 'S2 Extinction (Competition)', 'Total Extinction (Competition)')
      fill_cols <- c(c_s1_iso, c_s2_iso, c_t_iso, c_s2_cmp, c_t_cmp)
      xaux <- ext_S1_isol; yaux <- ext_S2
      polygon(c(xm, xaux, xaux, xm), c(yaux, yaux, ext_S2_isol, ext_S2_isol), col=c_s2_cmp, border=NA)
      polygon(c(xaux, xM, xM, xaux), c(yaux, yaux, ext_S2_isol, ext_S2_isol), col=c_t_cmp, border=NA)
      
    } else {
      lgd <- c('S1 Extinction (Isolation)', 'S2 Extinction (Isolation)', 'Total Extinction (Isolation)')
      fill_cols <- c(c_s1_iso, c_s2_iso, c_t_iso)
      xaux <- ext_S1_isol; yaux <- ext_S2_isol
    }
    
    # 1.3 Polígonos Selección
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
    
    # -------------------------------------------------------------------------
    # CAPA 2: LÍNEAS (ENCIMA DE POLÍGONOS)
    # -------------------------------------------------------------------------
    
    # 2.1 Líneas Isolation (Sólidas)
    lines(x, rep(ext_S2_isol, 100), col="black", lwd=2)
    lines(rep(ext_S1_isol, 100), y, col="black", lwd=2)
    
    # 2.2 Líneas Competition (Discontinuas)
    if (ext_S1 < ext_S1_isol && ext_S2 < ext_S2_isol) {
      lines(x, rep(ext_S2, 100), col="black", lty=2, lwd=1)
      lines(rep(ext_S1, 100), y, col="black", lty=2, lwd=1)
    } else if (ext_S1 < ext_S1_isol) {
      lines(rep(ext_S1, 100), y, col="black", lty=2, lwd=1)
    } else if (ext_S2 < ext_S2_isol) {
      lines(x, rep(ext_S2, 100), col="black", lty=2, lwd=1)
    }
    
    # 2.3 Línea de Equilibrio
    lines(xx, xx * (1 - FC) + FC * log(D) / T_k, col="black", lwd=2)
    
    # -------------------------------------------------------------------------
    # CAPA 3: DATOS Y TEXTO
    # -------------------------------------------------------------------------
    slope_eq <- 1 - FC
    angle_eq <- atan(slope_eq) * 180 / pi
    mid_x <- (xm + xint) / 2
    mid_y <- mid_x * slope_eq + (FC * log(D) / T_k)
    text(mid_x, mid_y, "Equilibrium", cex=1.2, srt=angle_eq, adj=c(0.5, -0.5))
    
    # --- ETIQUETAS "S1/S2 SELECTED" ELIMINADAS EN EXTINCTION ---
    
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
    
    # -------------------------------------------------------------------------
    # CAPA 4: MARCO Y EJES (AL FINAL)
    # -------------------------------------------------------------------------
    axis(1, cex.axis=1.5); axis(2, cex.axis=1.5)
    box()
    
    # Leyendas
    lgd <- c(lgd, "S1 Selection", "S2 Selection")
    fill_cols <- c(fill_cols, c_s1_sel, c_s2_sel)
    ext_leg_info <- list(lbl=lgd, col=fill_cols)
  }
  
  return(list(sel_leg = sel_leg_info, ext_leg = ext_leg_info))
}