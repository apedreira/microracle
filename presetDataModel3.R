##############################
### SETUP OF THE EXPERIMENT ##
##############################

# Duration of the growth periods (min): 
T_g = 24 * 60 

# Duration of the killing periods (min):
T_k <- 10 

# Carrying capacity (CFU/mL): 
K = 1e9
# Cast to character to enable scientific notation
K = as.character(K)

# Bactericide concentrations to test (any unit, only for legends):
C = c(0, 30, 40, 50, 60, 75) 

# Total inoculum (CFU/mL): 
X_0 = 1e6
# Cast to character to enable scientific notation
X_0 = as.character(X_0)

# Growth limit (Cell density below which cells cannot divide): 
X_g = 1
# Cast to character to enable scientific notation
X_g = as.character(X_g)

# Initial ratio of S2 to S1 cells (X_2(0)/X_1(0)):
r_0 = 10 

# Dilution factor: 
D = 100

##############################
###    BACTERIAL TRAITS     ##
##############################

#Growth rate of strain S1
mu_S1 = 1.2 

#Growth rate of strain S2
mu_S2 = mu_S1 * 0.8 

# --- KILL RATES (K) ---
# 1. Definimos numéricos para cálculos
k_s1_num <- c(0, 2.56 * 10^-1, 7.9  * 10^-1, 1.48 * 10^0)
k_s2_num <- c(0, 6.33 * 10^-2, 3.74 * 10^-1, 9.99 * 10^-1)

# 2. Convertimos a TEXTO con formato científico ("e" minúscula)
# CAMBIO: format = "e"
k_S1 <- formatC(k_s1_num, format = "e", digits = 2)
k_S2 <- formatC(k_s2_num, format = "e", digits = 2)

# --- SURVIVAL FRACTIONS (SF) ---
# 1. Definimos numéricos para cálculos
sf_s1_num <- c(7.5  * 10^-2, 3.55 * 10^-4, 5.69 * 10^-8, 1.69 * 10^-9)
sf_s2_num <- c(5.4  * 10^-1, 2.2  * 10^-2, 4.46 * 10^-5, 3.94 * 10^-9)

# 2. Convertimos a TEXTO con formato científico ("e" minúscula)
# CAMBIO: format = "e"
SF_S1 <- formatC(sf_s1_num, format = "e", digits = 2)
SF_S2 <- formatC(sf_s2_num, format = "e", digits = 2)

# Bactericide concentrations for tables
C = c(0,30,40,50)


sampleDataModel3 <- function() {
  presetData = NULL
  presetData = list(T_g, T_k, K, C, X_0, X_g, r_0, D, mu_S1, mu_S2, k_S1, k_S2, SF_S1, SF_S2, C)
  return(presetData)
}


##############################
### CI for BACTERIAL TRAITS ##
##############################

# Confidence intervals for the growth rates (Numeric is fine here)
ci_mu_S1 = cbind(0.9*mu_S1, 1.1*mu_S1)
ci_mu_S2 = cbind(0.9*mu_S2, 1.1*mu_S2)

# --- CIs for KILL RATES (Text) ---
# Calculamos numéricamente
ci_k_s1_num = cbind(0.9*k_s1_num, 1.1*k_s1_num)
ci_k_s2_num = cbind(0.9*k_s2_num, 1.1*k_s2_num)

# Formateamos a texto con "e" minúscula
# CAMBIO: format = "e"
ci_k_S1 = matrix(formatC(ci_k_s1_num, format = "e", digits = 2), ncol = 2)
ci_k_S2 = matrix(formatC(ci_k_s2_num, format = "e", digits = 2), ncol = 2)

# --- CIs for SURVIVAL FRACTIONS (Text) ---
# Calculamos numéricamente
ci_sf_s1_num = exp(cbind(1.1 * log(sf_s1_num), 0.9 * log(sf_s1_num)))
ci_sf_s2_num = exp(cbind(1.1 * log(sf_s2_num), 0.9 * log(sf_s2_num)))

# Formateamos a texto con "e" minúscula
# CAMBIO: format = "e"
ci_SF_S1 = matrix(formatC(ci_sf_s1_num, format = "e", digits = 2), ncol = 2)
ci_SF_S2 = matrix(formatC(ci_sf_s2_num, format = "e", digits = 2), ncol = 2)

presetCIModel3 <- function() {
  presetCI = NULL
  presetCI = list(ci_mu_S1, ci_mu_S2, ci_k_S1, ci_k_S2, ci_SF_S1, ci_SF_S2 )
  return(presetCI)
}