dataValidatorModel3 <- function(data_mu, data_k, data_sf, mode, X_0, X_g, r_0, T_g, T_k, K, D) {
  
  msg <- "OK"
  
  # --- ROBUST CLEANING FUNCTION ---
  # Converts any input (scientific text, factors, lists) into a real numeric value
  force_numeric <- function(col_data) {
    v <- unlist(col_data)
    v <- as.character(v)
    v <- as.numeric(v)
    return(v)
  }
  
  # Initialize clean copies of data
  clean_mu <- data_mu
  
  # Clean scalar inputs
  X_0_val <- force_numeric(X_0)
  X_g_val <- force_numeric(X_g)
  r_0_val <- force_numeric(r_0)
  T_g_val <- force_numeric(T_g)
  T_k_val <- force_numeric(T_k)
  K_val   <- force_numeric(K)
  D_val   <- force_numeric(D)
  
  clean_dynamic <- NULL 
  
  # ============================================================================
  # 1. GENERAL PARAMETERS
  # ============================================================================
  
  if (is.na(T_g_val) || T_g_val <= 0) {
    msg <- showNotification("The Duration of the growth periods (Tg) must be defined and Tg > 0", type = "error")
    return(list(msg = msg, data_mu = clean_mu, data_dynamic = NULL))
  }
  
  if (mode == 'by_k') {
    if (is.na(T_k_val) || T_k_val <= 0) {
      msg <- showNotification("The Duration of the killing periods (Tk) must be defined and Tk > 0", type = "error")
      return(list(msg = msg, data_mu = clean_mu, data_dynamic = NULL))
    }
  }
  
  if (is.na(K_val) || K_val <= 0) {
    msg <- showNotification("The Carrying capacity (K) must be defined and K > 0", type = "error")
    return(list(msg = msg, data_mu = clean_mu, data_dynamic = NULL))
  }
  
  if (is.na(r_0_val) || r_0_val <= 0) {
    msg <- showNotification("The Initial mixing ratio (r0) must be defined and r0 > 0", type = "error")
    return(list(msg = msg, data_mu = clean_mu, data_dynamic = NULL))
  }
  
  if (is.na(X_0_val) || X_0_val <= 0 || X_0_val >= K_val) {
    msg <- showNotification("The Initial population (X0) must be defined and 0 < X0 < K", type = "error")
    return(list(msg = msg, data_mu = clean_mu, data_dynamic = NULL))
  }
  
  if (is.na(D_val) || D_val <= 0) {
    msg <- showNotification("The Dilution rate (D) must be defined and 0 < D <= 1", type = "error")
    return(list(msg = msg, data_mu = clean_mu, data_dynamic = NULL))
  }
  
  # ============================================================================
  # REQUIREMENT 1: X_g
  # ============================================================================
  if (is.na(X_g_val)) {
    msg <- showNotification("The growth limit (X_g) must be defined", type = "error")
    return(list(msg = msg, data_mu = clean_mu, data_dynamic = NULL))
  }
  
  limit_s1 <- X_0_val / (1 + r_0_val)
  limit_s2 <- (r_0_val * X_0_val) / (1 + r_0_val)
  
  if (X_g_val <= 0 || X_g_val >= limit_s1 || X_g_val >= limit_s2) {
    msg <- showNotification("The growth limit (X_g) must be defined and 0 < X_g < X_0/(1+r_0) AND 0 < X_g < r_0*X_0/(1 + r_0))", type = "error")
    return(list(msg = msg, data_mu = clean_mu, data_dynamic = NULL))
  }
  
  # ============================================================================
  # REQUIREMENTS 2 & 3: GROWTH RATES (MU)
  # ============================================================================
  mu_S1 <- force_numeric(clean_mu[1, 2])
  mu_S2 <- force_numeric(clean_mu[2, 2])
  
  # Strain S1 Validation (Error)
  if (is.na(mu_S1) || mu_S1 <= 0) {
    msg <- showNotification("Growth rate for Strain S1 (mu_S1) must be defined and mu_S1 > 0)", type = "error")
    return(list(msg = msg, data_mu = clean_mu, data_dynamic = NULL))
  }
  
  # Confidence Intervals (CIs) Validation for S1 (Warning)
  ci_s1_low <- force_numeric(clean_mu[1, 1])
  ci_s1_high <- force_numeric(clean_mu[1, 3])
  
  # Logic: If any CI value exists, verify if it is incomplete or invalid
  has_ci_s1 <- !is.na(ci_s1_low) || !is.na(ci_s1_high)
  is_bad_s1 <- is.na(ci_s1_low) || is.na(ci_s1_high) || ci_s1_low > mu_S1 || mu_S1 > ci_s1_high
  
  if (has_ci_s1 && is_bad_s1) {
    showNotification("Confidence intervals not provided (or invalid) for ci_mu_S1. Ignoring during plotting", type = "warning")
    clean_mu[1, c(1,3)] <- NA # Clear partial or erroneous data
  }
  
  # Strain S2 Validation (Error)
  if (is.na(mu_S2) || mu_S2 <= 0 || mu_S2 > mu_S1) {
    msg <- showNotification("Growth rate for Strain S2 (mu_S2) must be defined and 0 < mu_S2 <= mu_S1)", type = "error")
    return(list(msg = msg, data_mu = clean_mu, data_dynamic = NULL))
  }
  
  # Confidence Intervals (CIs) Validation for S2 (Warning)
  ci_s2_low <- force_numeric(clean_mu[2, 1])
  ci_s2_high <- force_numeric(clean_mu[2, 3])
  
  has_ci_s2 <- !is.na(ci_s2_low) || !is.na(ci_s2_high)
  is_bad_s2 <- is.na(ci_s2_low) || is.na(ci_s2_high) || ci_s2_low > mu_S2 || mu_S2 > ci_s2_high
  
  if (has_ci_s2 && is_bad_s2) {
    showNotification("Confidence intervals not provided (or invalid) for ci_mu_S2. Ignoring during plotting", type = "warning")
    clean_mu[2, c(1,3)] <- NA
  }
  
  # ============================================================================
  # DYNAMIC VALIDATION - REQUIREMENTS 4 & 5
  # ============================================================================
  
  if (mode == 'by_k') {
    clean_k <- data_k
    
    # CONVERT ALL TO NUMERIC
    # Col 1 is Text (C), Cols 2-7 are Numeric
    clean_k[[1]] <- as.character(unlist(clean_k[[1]]))
    for(i in 2:7) clean_k[[i]] <- force_numeric(clean_k[[i]])
    
    # 4.1. SIZE CHECK (Complete rows for C, k1, k2)
    count_C  <- sum(!is.na(clean_k[[1]]) & clean_k[[1]] != "")
    count_k1 <- sum(!is.na(clean_k[[3]]))
    count_k2 <- sum(!is.na(clean_k[[6]]))
    
    if (count_C != count_k1 || count_C != count_k2 || count_C == 0) {
      msg <- showNotification("Provide killing rates (k) and antimicrobial concentrations (C) for each of the experiments and strains", type = "error")
      return(list(msg = msg, data_mu = clean_mu, data_dynamic = NULL))
    }
    
    valid_rows <- (!is.na(clean_k[[1]]) & clean_k[[1]] != "") & !is.na(clean_k[[3]]) & !is.na(clean_k[[6]])
    df_check <- clean_k[valid_rows, ]
    
    # 4.2. COMPARATIVE LOGIC: k_S2 <= k_S1
    if (any(df_check[[6]] > df_check[[3]])) {
      msg <- showNotification("The kill rate for strain S1 should be higher or equal than for S2 in all the experiments", type = "error")
      return(list(msg = msg, data_mu = clean_mu, data_dynamic = NULL))
    }
    
    # 4.3. Confidence Intervals (CIs) (Warning)
    min_s1 <- clean_k[[2]]; val_s1 <- clean_k[[3]]; max_s1 <- clean_k[[4]]
    min_s2 <- clean_k[[5]]; val_s2 <- clean_k[[6]]; max_s2 <- clean_k[[7]]
    
    # Logic: Detect rows where CI (partial or total) is present but invalid.
    # Invalid if: Missing min/max, min < 0, or inconsistent range.
    
    has_ci_s1 <- !is.na(min_s1) | !is.na(max_s1)
    is_bad_s1 <- is.na(min_s1) | is.na(max_s1) | min_s1 < 0 | min_s1 > val_s1 | val_s1 > max_s1
    bad_s1_idx <- which(has_ci_s1 & is_bad_s1)
    
    has_ci_s2 <- !is.na(min_s2) | !is.na(max_s2)
    is_bad_s2 <- is.na(min_s2) | is.na(max_s2) | min_s2 < 0 | min_s2 > val_s2 | val_s2 > max_s2
    bad_s2_idx <- which(has_ci_s2 & is_bad_s2)
    
    if (length(bad_s1_idx) > 0 || length(bad_s2_idx) > 0) {
      showNotification("Ignoring killing rate confidence intervals (invalid or incomplete values).", type = "warning")
      if(length(bad_s1_idx) > 0) clean_k[bad_s1_idx, c(2,4)] <- NA
      if(length(bad_s2_idx) > 0) clean_k[bad_s2_idx, c(5,7)] <- NA
    }
    clean_dynamic <- clean_k
    
  } else if (mode == 'by_SF') {
    clean_sf <- data_sf
    
    clean_sf[[1]] <- as.character(unlist(clean_sf[[1]]))
    for(i in 2:7) clean_sf[[i]] <- force_numeric(clean_sf[[i]])
    
    # 5.1. SIZE CHECK
    count_C   <- sum(!is.na(clean_sf[[1]]) & clean_sf[[1]] != "")
    count_sf1 <- sum(!is.na(clean_sf[[3]]))
    count_sf2 <- sum(!is.na(clean_sf[[6]]))
    
    if (count_C != count_sf1 || count_C != count_sf2 || count_C == 0) {
      msg <- showNotification("Provide survival fraction (SF) and antimicrobial concentrations (C) for each of the experiments and strains", type = "error")
      return(list(msg = msg, data_mu = clean_mu, data_dynamic = NULL))
    }
    
    valid_rows <- (!is.na(clean_sf[[1]]) & clean_sf[[1]] != "") & !is.na(clean_sf[[3]]) & !is.na(clean_sf[[6]])
    df_check <- clean_sf[valid_rows, ]
    
    # 5.2. COMPARATIVE LOGIC: sf_S1 <= sf_S2
    if (any(df_check[[3]] > df_check[[6]])) {
      msg <- showNotification("The survival fraction for strain S1 should be lower or equal than for S2 in all the experiments", type = "error")
      return(list(msg = msg, data_mu = clean_mu, data_dynamic = NULL))
    }
    
    # 5.3. Confidence Intervals (CIs) (Warning)
    min_s1 <- clean_sf[[2]]; val_s1 <- clean_sf[[3]]; max_s1 <- clean_sf[[4]]
    min_s2 <- clean_sf[[5]]; val_s2 <- clean_sf[[6]]; max_s2 <- clean_sf[[7]]
    
    has_ci_s1 <- !is.na(min_s1) | !is.na(max_s1)
    is_bad_s1 <- is.na(min_s1) | is.na(max_s1) | min_s1 < 0 | min_s1 > val_s1 | val_s1 > max_s1
    bad_s1_idx <- which(has_ci_s1 & is_bad_s1)
    
    has_ci_s2 <- !is.na(min_s2) | !is.na(max_s2)
    is_bad_s2 <- is.na(min_s2) | is.na(max_s2) | min_s2 < 0 | min_s2 > val_s2 | val_s2 > max_s2
    bad_s2_idx <- which(has_ci_s2 & is_bad_s2)
    
    if (length(bad_s1_idx) > 0 || length(bad_s2_idx) > 0) {
      showNotification("Ignoring survival fraction confidence intervals (invalid or incomplete values).", type = "warning")
      if(length(bad_s1_idx) > 0) clean_sf[bad_s1_idx, c(2,4)] <- NA
      if(length(bad_s2_idx) > 0) clean_sf[bad_s2_idx, c(5,7)] <- NA
    }
    clean_dynamic <- clean_sf
  }
  
  return(list(msg = "OK", data_mu = clean_mu, data_dynamic = clean_dynamic))
}