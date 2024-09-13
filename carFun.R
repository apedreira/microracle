### Model_SubMIC_Carvacrol: (Pedreira et al. 20220) A predictive microbiology 
### model to determine the effect of subinhibitory concentrations of Carvacrol 
### on the foodborne species E. Coli and B. Cereus (single experiment).

submiC3 <- function(time_exp, c_exp, p, y_exp, disc_pts) {
  
  sampling_times = length(time_exp[!is.na(time_exp)])
  init_exp_time =  time_exp[1]
  final_exp_time =  time_exp[sampling_times]
  init_exp_drug =  c_exp[1]
  final_exp_drug =  c_exp[sampling_times]

  # Create a sorted vector with experimental and modeling times (repeated times are deleted)
  time_aux = seq(from = init_exp_time, to =  final_exp_time , by = final_exp_time /disc_pts)
  time_mod = unique(sort(c(time_aux, time_exp)))
  
  # Create a sorted vector with experimental and modeling drug concentrations (repeated times are deleted)
  # taking into account if drug remains equal, decreases or increases over the time.
  if (init_exp_drug == final_exp_drug){
    drug_aux = rep(final_exp_drug, times=length(time_mod))
    drug_mod = drug_aux
  } else if (init_exp_drug > final_exp_drug) {
    drug_aux= seq(final_exp_drug, init_exp_drug, length.out=length(time_mod))
    drug_mod= rev(unique((c(drug_aux, c_exp))))[-1]

 
  } else if (init_exp_drug < final_exp_drug) {
    drug_aux = seq(init_exp_drug, final_exp_drug, length.out=length(time_mod))
    drug_mod = unique((c(drug_aux, c_exp)))[-1]
  }
  
  # Destructuring parameter vector
  mu_g0 = p[1]
  mu_d0 = p[2]
  k_S = p[3]
  k_g = p[4]
  k_d = p[5]
  
  # Initial condition (extracted from y measurements):
  N0 = y_exp[1]
  
  # Initiate storage variables (numeric for each time, vector agglutinates all solutions)
  sol = 0
  sol_vec = c()
  
  # Apply the model the for each time
    for(i in 1:length(time_mod)){
    sol = N0*exp(mu_g0*exp(-k_g*drug_mod[i])*(1-exp(-k_S*time_mod[i]))/k_S-mu_d0*exp(k_d*drug_mod[i])*time_mod[i])
    sol_vec = c(sol_vec,sol)
  }
  
  result = list(time_mod, sol_vec)
  return(result)
}

