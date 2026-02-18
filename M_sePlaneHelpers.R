################################################################################
# sePlaneHelpers.R
# Translation of MATLAB logic (Plot_SE_Plane_k.m and Plot_SE_Plane_SF.m)
# into R for calculating Selection and Extinction Planes.
################################################################################

# Define Colors based on Run_SE_Plane.m
SE_COLORS <- list(
  ext_S1 = rgb(183, 145, 47, maxColorValue = 255),
  ext_S2 = rgb(206, 92, 92, maxColorValue = 255),
  SR_S1  = rgb(206, 92, 92, maxColorValue = 255),
  SR_S2  = rgb(183, 145, 47, maxColorValue = 255),
  ext_T  = rgb(101, 115, 126, maxColorValue = 255),
  boxes  = c(
    rgb(183, 145, 47, maxColorValue=255), rgb(39, 183, 222, maxColorValue=255),
    rgb(237, 106, 90, maxColorValue=255), rgb(129, 23, 27, maxColorValue=255),
    rgb(51, 115, 87, maxColorValue=255),  rgb(76, 57, 87, maxColorValue=255),
    rgb(227, 158, 84, maxColorValue=255), rgb(72, 166, 167, maxColorValue=255),
    rgb(41, 115, 178, maxColorValue=255)
  )
)

#' Calculate Selection Plane Data
#' Corresponds to Section (2) of the MATLAB scripts.
#' @param inputs List containing model parameters
#' @param mode String: "kill_rate" or "survival_fraction"
getSelectionPlaneData <- function(inputs, mode) {
  
  # Unpack parameters
  T_k <- if(mode == "kill_rate") inputs$T_k else NULL # Only needed for k mode
  D   <- inputs$D
  mu_S1 <- inputs$mu_S1
  mu_S2 <- inputs$mu_S2
  
  # Calculate Fitness Cost (FC)
  FC <- 1 - mu_S2/mu_S1
  
  # Bounds on FC (Assumed empty/NULL based on premisas default behavior for now, 
  # can be expanded if CIs are passed)
  min_FC <- NULL
  max_FC <- NULL
  
  # Prepare experimental points
  nC <- length(inputs$trait_S1) # trait is either k or SF
  points_df <- data.frame(
    FC = numeric(nC),
    SA = numeric(nC),
    Color = character(nC),
    Label = character(nC),
    stringsAsFactors = FALSE
  )
  
  # Color cycling
  box_cols <- rep(SE_COLORS$boxes, length.out = nC)
  
  for(i in 1:nC) {
    trait1 <- inputs$trait_S1[i]
    trait2 <- inputs$trait_S2[i]
    
    # Calculate Survival Advantage (SA)
    if(mode == "kill_rate") {
      # Formula from Plot_SE_Plane_k.m lines 78-79
      # SA = 1 - (k_S2(iC)*T_k - log(D))/(k_S1(iC)*T_k - log(D));
      num <- (trait2 * T_k - log(D))
      den <- (trait1 * T_k - log(D))
      SA <- 1 - num/den
    } else {
      # Formula from Plot_SE_Plane_SF.m lines 79-80
      # SA = 1 - (logSF_S2(iC) + log(D))/(logSF_S1(iC) + log(D));
      num <- (log(trait2) + log(D))
      den <- (log(trait1) + log(D))
      SA <- 1 - num/den
    }
    
    points_df$FC[i] <- FC
    points_df$SA[i] <- SA
    points_df$Color[i] <- box_cols[i]
    points_df$Label[i] <- if(!is.null(inputs$C)) paste("C =", inputs$C[i]) else paste("Exp", i)
  }
  
  return(list(
    points = points_df,
    FC = FC,
    equil_line = data.frame(x = seq(0, 1, length.out = 100), y = seq(0, 1, length.out = 100))
  ))
}

#' Calculate Extinction Plane Data
#' Corresponds to Section (3) of the MATLAB scripts.
getExtinctionPlaneData <- function(inputs, mode) {
  
  # Unpack
  T_g <- inputs$T_g
  T_k <- inputs$T_k
  X_0 <- inputs$X_0
  r_0 <- inputs$r_0
  K   <- inputs$K
  D   <- inputs$D
  X_g <- inputs$X_g
  mu_S1 <- inputs$mu_S1
  mu_S2 <- inputs$mu_S2
  
  # Initial concentrations
  X_S1_0 <- X_0 / (1 + r_0)
  X_S2_0 <- X_0 - X_S1_0
  
  # Fitness cost
  FC <- 1 - mu_S2/mu_S1
  
  # --- 1. Bounds Calculation ---
  
  # Saturation time
  # tsat = log(K/X_0)/((X_S1_0/X_0)*mu_S1 + (X_S2_0/X_0)*mu_S2);
  tsat <- log(K/X_0) / ((X_S1_0/X_0)*mu_S1 + (X_S2_0/X_0)*mu_S2)
  
  if (mode == "kill_rate") {
    # From Plot_SE_Plane_k.m Section 3.1
    ext_S1_isol <- min((mu_S1*T_g + log(D))/T_k, log(K*D/X_g)/T_k)
    ext_S2_isol <- min((mu_S2*T_g + log(D))/T_k, log(K*D/X_g)/T_k)
    ext_S1_comp <- (mu_S1*tsat + log(D) - log(X_g/X_S1_0))/T_k
    ext_S2_comp <- (mu_S2*tsat + log(D) - log(X_g/X_S2_0))/T_k
    
    # Plot limits (Default handling if CIs missing)
    vals_S1 <- inputs$trait_S1
    vals_S2 <- inputs$trait_S2
    xm <- 0.9 * min(vals_S1); xM <- 1.1 * max(vals_S1)
    ym <- 0.9 * min(vals_S2); yM <- 1.1 * max(vals_S2)
    
    # Update limits based on bounds
    xm <- min(c(xm, 0.9*ext_S1_isol, 0.9*ext_S1_comp))
    xM <- max(c(xM, 1.1*ext_S1_isol))
    ym <- min(c(ym, 0.9*ext_S2_isol, 0.9*ext_S2_comp))
    yM <- max(c(yM, 1.1*ext_S2_isol))
    
  } else {
    # From Plot_SE_Plane_SF.m Section 3.1
    ext_S1_isol <- max(exp(-mu_S1*T_g)/D, X_g/(D*K))
    ext_S2_isol <- max(exp(-mu_S2*T_g)/D, X_g/(D*K))
    ext_S1_comp <- (exp(-mu_S1*tsat)*X_g)/(X_S1_0*D)
    ext_S2_comp <- (exp(-mu_S2*tsat)*X_g)/(X_S2_0*D)
    
    # Limits for Log Scale plotting
    vals_S1 <- inputs$trait_S1
    vals_S2 <- inputs$trait_S2
    xm <- 0.1 * min(vals_S1); xM <- min(1, 10 * max(vals_S1))
    ym <- 0.1 * min(vals_S2); yM <- min(1, 10 * max(vals_S2))
    
    # Update limits
    xm <- min(c(xm, 0.1*ext_S1_isol))
    xM <- min(1, max(c(xM, 10*ext_S1_isol, 10*ext_S1_comp)))
    ym <- min(c(ym, 0.1*ext_S2_isol))
    yM <- min(1, max(c(yM, 10*ext_S2_isol, 10*ext_S2_comp)))
  }
  
  # --- 2. Define Regions (Polygons) ---
  polygons <- list()
  
  # (3.3.a) Extinction in isolation regions
  # Note: The logic for vertices depends on whether it's Kill Rate (Linear, > kills) 
  # or SF (Log, < SF kills). 
  
  # Bounds
  xaux <- ext_S1_isol
  yaux <- ext_S2_isol
  eS1 <- ext_S1_comp
  eS2 <- ext_S2_comp
  
  if (mode == "kill_rate") {
    # Kill Rate Geometry (Plot_SE_Plane_k.m)
    
    # Extinction S1 (Isolation)
    polygons[[length(polygons)+1]] <- list(
      x = c(xaux, xM, xM, xaux),
      y = c(ym, ym, min(eS2, yaux), min(eS2, yaux)),
      color = SE_COLORS$ext_S1, name = "Ext. S1 (isol.)"
    )
    # Extinction S2 (Isolation)
    polygons[[length(polygons)+1]] <- list(
      x = c(xm, min(eS1, xaux), min(eS1, xaux), xm),
      y = c(yaux, yaux, yM, yM),
      color = SE_COLORS$ext_S2, name = "Ext. S2 (isol.)"
    )
    # Total Extinction (Isolation)
    polygons[[length(polygons)+1]] <- list(
      x = c(xaux, xM, xM, xaux),
      y = c(yaux, yaux, yM, yM),
      color = SE_COLORS$ext_T, name = "Ext. T (isol.)"
    )
    
    # Competition logic (Case 1, 2, 3)
    if (eS1 < xaux && eS2 < yaux) {
      # Case 1
      polygons[[length(polygons)+1]] <- list(
        x = c(eS1, xaux, xaux, eS1), y = c(ym, ym, eS2, eS2),
        color = SE_COLORS$ext_S1, name = "Ext. S1 (comp)"
      )
      polygons[[length(polygons)+1]] <- list(
        x = c(xm, eS1, eS1, xm), y = c(eS2, eS2, yaux, yaux),
        color = SE_COLORS$ext_S2, name = "Ext. S2 (comp)"
      )
      # Total comp overlaps
      polygons[[length(polygons)+1]] <- list(
        x = c(eS1, xaux, xaux, eS1), y = c(eS2, eS2, yM, yM),
        color = SE_COLORS$ext_T, name = "Ext. T (comp)"
      )
      polygons[[length(polygons)+1]] <- list(
        x = c(xaux, xM, xM, xaux), y = c(eS2, eS2, yaux, yaux),
        color = SE_COLORS$ext_T, name = "Ext. T (comp)"
      )
    } else if (eS1 < xaux) {
      # Case 2
      polygons[[length(polygons)+1]] <- list(
        x = c(eS1, xaux, xaux, eS1), y = c(ym, ym, yaux, yaux),
        color = SE_COLORS$ext_S1, name = "Ext. S1 (comp)"
      )
      polygons[[length(polygons)+1]] <- list(
        x = c(eS1, xaux, xaux, eS1), y = c(yaux, yaux, yM, yM),
        color = SE_COLORS$ext_T, name = "Ext. T (comp)"
      )
    } else if (eS2 < yaux) {
      # Case 3
      polygons[[length(polygons)+1]] <- list(
        x = c(xm, xaux, xaux, xm), y = c(eS2, eS2, yaux, yaux),
        color = SE_COLORS$ext_S2, name = "Ext. S2 (comp)"
      )
      polygons[[length(polygons)+1]] <- list(
        x = c(xaux, xM, xM, xaux), y = c(eS2, eS2, yaux, yaux),
        color = SE_COLORS$ext_T, name = "Ext. T (comp)"
      )
    }
    
    # Equilibrium Line & Selection Range
    # yint_S1 = xaux*(1 - FC) + FC*log(D)/T_k;
    yint_S1 <- xaux*(1 - FC) + FC*log(D)/T_k
    # xint_S2 = (yaux - log(D)*FC/T_k)/(1 - FC);
    xint_S2 <- (yaux - log(D)*FC/T_k)/(1 - FC)
    
    xint <- min(xaux, xint_S2)
    xint_0 <- FC*log(D)/(T_k*(FC - 1))
    
    # Selection Range Polygons
    if (xint == xaux) {
      polygons[[length(polygons)+1]] <- list(
        x = c(xint_0, xaux, xaux), y = c(0, 0, yint_S1),
        color = SE_COLORS$SR_S1, name = "S1 Selected"
      )
      polygons[[length(polygons)+1]] <- list(
        x = c(0, xint_0, xaux, xaux, 0), y = c(0, 0, yint_S1, yaux, yaux),
        color = SE_COLORS$SR_S2, name = "S2 Selected"
      )
    } else {
      polygons[[length(polygons)+1]] <- list(
        x = c(xint_0, xaux, xaux, xint), y = c(0, 0, yaux, yaux),
        color = SE_COLORS$SR_S1, name = "S1 Selected"
      )
      polygons[[length(polygons)+1]] <- list(
        x = c(0, xint_0, xint, 0), y = c(0, 0, yaux, yaux),
        color = SE_COLORS$SR_S2, name = "S2 Selected"
      )
    }
    
    # Equilibrium Line Data
    xx <- seq(xm, xint, length.out=100)
    yy <- xx*(1 - FC) + FC*log(D)/T_k
    eq_line <- data.frame(x=xx, y=yy)
    
  } else {
    # SF Logic (Plot_SE_Plane_SF.m)
    # Be careful with max/min logic here as SF implies smaller is better (survival)
    # but the plot axes are often log scale.
    
    # Regions for extinction in isolation (3.3.a)
    polygons[[length(polygons)+1]] <- list(
      x = c(xm, xaux, xaux, xm), y = c(max(eS2, yaux), max(eS2, yaux), yM, yM),
      color = SE_COLORS$ext_S1, name = "Ext. S1 (isol.)"
    )
    polygons[[length(polygons)+1]] <- list(
      x = c(max(eS1, xaux), xM, xM, max(eS1, xaux)), y = c(ym, ym, yaux, yaux),
      color = SE_COLORS$ext_S2, name = "Ext. S2 (isol.)"
    )
    polygons[[length(polygons)+1]] <- list(
      x = c(xm, xaux, xaux, xm), y = c(ym, ym, yaux, yaux),
      color = SE_COLORS$ext_T, name = "Ext. T (isol.)"
    )
    
    # Competition (Case 1, 2, 3)
    # Note: Logic inverted > compared to kill rates
    if(eS1 > xaux && eS2 > yaux) {
      # Case 1
      polygons[[length(polygons)+1]] <- list(
        x = c(xaux, eS1, eS1, xaux), y = c(yaux, yaux, yM, yM),
        color = SE_COLORS$ext_S1, name = "Ext. S1 (comp)"
      )
      polygons[[length(polygons)+1]] <- list(
        x = c(eS1, xM, xM, eS1), y = c(yaux, yaux, eS2, eS2),
        color = SE_COLORS$ext_S2, name = "Ext. S2 (comp)"
      )
      # Total
      polygons[[length(polygons)+1]] <- list(
        x = c(xm, eS1, eS1, xm), y = c(yaux, yaux, eS2, eS2),
        color = SE_COLORS$ext_T, name = "Ext. T (comp)"
      )
      polygons[[length(polygons)+1]] <- list(
        x = c(xaux, eS1, eS1, xaux), y = c(ym, ym, yaux, yaux),
        color = SE_COLORS$ext_T, name = "Ext. T (comp)"
      )
    } else if (eS1 > xaux) {
      # Case 2
      polygons[[length(polygons)+1]] <- list(
        x = c(xaux, eS1, eS1, xaux), y = c(yaux, yaux, yM, yM),
        color = SE_COLORS$ext_S1, name = "Ext. S1 (comp)"
      )
      polygons[[length(polygons)+1]] <- list(
        x = c(xaux, eS1, eS1, xaux), y = c(ym, ym, yaux, yaux),
        color = SE_COLORS$ext_T, name = "Ext. T (comp)"
      )
    } else if (eS2 > yaux) {
      # Case 3
      polygons[[length(polygons)+1]] <- list(
        x = c(xaux, xM, xM, xaux), y = c(yaux, yaux, eS2, eS2),
        color = SE_COLORS$ext_S2, name = "Ext. S2 (comp)"
      )
      polygons[[length(polygons)+1]] <- list(
        x = c(xm, xaux, xaux, xm), y = c(yaux, yaux, eS2, eS2),
        color = SE_COLORS$ext_T, name = "Ext. T (comp)"
      )
    }
    
    # Equilibrium Line & Selection Range (3.4)
    yint_S1 <- xaux^(1-FC)/(D^FC)
    xint_S2 <- (yaux*D^FC)^(1/(1-FC))
    xint <- max(xaux, xint_S2)
    
    xint_M <- (yM*D^FC)^(1/(1-FC))
    yint_M <- xM^(1-FC)/(D^FC)
    
    # Selection Range Logic (Complex polygons)
    if(xint == xaux) {
      if(yint_M < yM) {
        polygons[[length(polygons)+1]] <- list(x = c(xaux, xM, xM, xaux), y = c(yaux, yaux, yint_M, yint_S1), color = SE_COLORS$SR_S1)
        polygons[[length(polygons)+1]] <- list(x = c(xaux, xM, xM, xaux), y = c(yint_S1, yint_M, yM, yM), color = SE_COLORS$SR_S2)
      } else if (yint_M == yM) {
        polygons[[length(polygons)+1]] <- list(x = c(xaux, xM, xM, xaux), y = c(yaux, yaux, yM, yint_S1), color = SE_COLORS$SR_S1)
        polygons[[length(polygons)+1]] <- list(x = c(xaux, xM, xaux), y = c(yint_S1, yM, yM), color = SE_COLORS$SR_S2)
      } else {
        polygons[[length(polygons)+1]] <- list(x = c(xaux, xM, xM, xint_M, xaux), y = c(yaux, yaux, yM, yM, yint_S1), color = SE_COLORS$SR_S1)
        polygons[[length(polygons)+1]] <- list(x = c(xaux, xint_M, xaux), y = c(yint_S1, yM, yM), color = SE_COLORS$SR_S2)
      }
    } else {
      if (yint_M <= yM) {
        polygons[[length(polygons)+1]] <- list(x = c(xint, xM, xM), y = c(yaux, yaux, yint_M), color = SE_COLORS$SR_S1)
        polygons[[length(polygons)+1]] <- list(x = c(xaux, xint, xM, xM, xaux), y = c(yaux, yaux, yint_M, yM, yM), color = SE_COLORS$SR_S2)
      } else if (yint_M == yM) {
        polygons[[length(polygons)+1]] <- list(x = c(xint, xM, xM), y = c(yaux, yaux, yM), color = SE_COLORS$SR_S1)
        polygons[[length(polygons)+1]] <- list(x = c(xaux, xint, xM, xaux), y = c(yaux, yaux, yM, yM), color = SE_COLORS$SR_S2)
      } else {
        polygons[[length(polygons)+1]] <- list(x = c(xint, xM, xM, xint_M), y = c(yaux, yaux, yM, yM), color = SE_COLORS$SR_S1)
        polygons[[length(polygons)+1]] <- list(x = c(xaux, xint, xint_M, xaux), y = c(yaux, yaux, yM, yM), color = SE_COLORS$SR_S2)
      }
    }
    
    xx <- seq(xint, xM, length.out=100)
    yy <- xx^(1 - FC)/(D^FC)
    eq_line <- data.frame(x=xx, y=yy)
  }
  
  return(list(
    polygons = polygons,
    isol_lines = list(x = xaux, y = yaux),
    comp_lines = list(x = eS1, y = eS2),
    limits = list(x = c(xm, xM), y = c(ym, yM)),
    eq_line = eq_line
  ))
}