### Model_SubMIC_Carvacrol: (Pedreira et al. 20220) A predictive microbiology
### model to determine the effect of subinhibitory concentrations of Carvacrol
### on the foodborne species E. coli and B. cereus (single experiment).

library(deSolve)

Ys       <- 0.2306
aa       <- 0.0000
bb       <- 77.7600

submic_ddac <- function(time_exp, c_exp, param, y_exp, disc_pts) {
  sampling_times = length(time_exp[!is.na(time_exp)])
  init_exp_time =  time_exp[1]
  final_exp_time =  time_exp[sampling_times]
  init_exp_drug =  c_exp[1]
  final_exp_drug =  c_exp[sampling_times]
  
  # Create a sorted vector with experimental and modeling times 
  # (created in base of Discretization points). Repeated times are removed.
  time_aux = seq(from = init_exp_time,
                 to =  final_exp_time ,
                 by = final_exp_time / disc_pts)
  time_mod = unique(sort(c(time_aux, time_exp)))
  
  # Create a sorted vector with experimental and modeling drug concentrations.
  # taking into account if drug remains equal, decreases or increases over
  # the time. Repeated concentrations are removed.
  if (init_exp_drug == final_exp_drug) {
    drug_aux = rep(final_exp_drug, times = length(time_mod))
    drug_mod = drug_aux
  } else if (init_exp_drug > final_exp_drug) {
    drug_aux = seq(final_exp_drug, init_exp_drug, length.out = length(time_mod))
    drug_mod = rev(unique((c(drug_aux, c_exp))))[-1]
  } else if (init_exp_drug < final_exp_drug) {
    drug_aux = seq(init_exp_drug, final_exp_drug, length.out = length(time_mod))
    drug_mod = unique((c(drug_aux, c_exp)))[-1]
  }
  
  
  # Destructuring user-settable parameter vector
  # ka0 <- unlist(p[1])
  ka0 <- param[1]
  kg0 <- param[2]
  kd0 <- param[3]
  kd1  <- param[4]
  ki  <- param[5]
  IC50_a   <- param[6]
  IC50_g   <- param[7]
  EC50_d  <- param[8]
  gamma_a  <- param[9]
  gamma_g  <- param[10]
  gamma_d  <- param[11]
  
  # Non user-settable parameters
  Ys       <- 0.2306
  aa       <- 0.0000
  bb       <- 77.7600
  
  
  # Initial population size (extracted from y measurements):
  # (X0 is N0)
  X0 <- y_exp[1]
  #Set initial status of all cell population as live
  Xl = X0
  
  #print(paste0(typeof(ka0), " P1"))
  #print(unlist(p))
  print(paste0("PARAMETROS: ", param))
  
  
  
  # Model  -----------------------------------------------------------------------
  Model <- function (t, y, parms) {
    with(as.list(y), {
      kg    <- kg0 * (IC50_g ^ gamma_g) / (drug ^ gamma_g + IC50_g ^ gamma_g)
      ka    <- ka0 * (IC50_a ^ gamma_a) / (drug ^ gamma_a + IC50_a ^ gamma_a)
      kd    <- kd0 + kd1 * (drug ^ gamma_d) / (drug ^ gamma_d + EC50_d^gamma_d)
      dXl   <- (-ka * Xl - kd * Xl)
      dX    <- (ka * Xl + (kg / (ki + X / (bb * 10 ^ 10))) * S * X - kd *
                  X)
      dS    <- -(Ys / (bb * 10 ^ 10)) * (kg / (ki + X / (bb * 10 ^ 10))) * S*X
      dXd   <- (kd * Xl + kd * X)
      
      return(list(c(dXl, dX, dS, dXd)))
    })
  }
  
  # Initiate storage variables (numeric for each time, vector
  # agglutinates all solutions)
  sol <- c()
  sol_vec <- c()
  
  # Apply the model the for each time
  
  for (i in 1:length(time_mod)) {
    drug = drug_mod[i]
    out <-  ode(
      y = c(
        Xl = X0,
        X = 0,
        S = 1,
        Xd = 0
      ),
      times = time_mod,
      func = Model,
      parms = NULL
    )
    Xlv = out[, 2]
    Xv = out[, 3]
    sol <- (Xlv[i] + Xv[i])
    sol_vec <- c(sol_vec, sol)
  }
  
  result <- list(time_mod, sol_vec)
  return(result)
}
