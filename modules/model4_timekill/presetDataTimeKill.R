.tk_row <- function(name, behaviour, mu, Nm = 10, a0 = 1, K = 0, m = 1) {
  data.frame(Curve = name, Behaviour = behaviour,
             mu = mu, Nm = Nm, a0 = a0, K = K, m = m,
             stringsAsFactors = FALSE)
}

TK_PRESETS <- list(
  # ----------------------------- Bacillus cereus -----------------------------
  "B. cereus | BAC + EUG" = rbind(
    .tk_row("Control",             "Logistic",                      1.90, 9.60),
    .tk_row("BAC 8 mg/L",          "Logistic + Lag + Chick-Watson", 1.39, 8.02, 0.01, 0.73, 1),
    .tk_row("EUG 800 mg/L",        "Logistic + Lag",                1.90, 7.82, 0.19),
    .tk_row("BAC 8 + EUG 800",     "Hom",                           0,    K = 5.38, m = 0.28)
  ),
  "B. cereus | BAC + CAR" = rbind(
    .tk_row("Control",             "Logistic",                      1.87, 9.27),
    .tk_row("BAC 8 mg/L",          "Logistic + Lag + Chick-Watson", 1.04, 9.03, 0.05, 0.70, 1),
    .tk_row("CAR 50 mg/L",         "Logistic + Lag",                0.961, 8.13, 0.0056),
    .tk_row("BAC 8 + CAR 50",      "Chick-Watson",                  0,    K = 4.02, m = 1)
  ),
  "B. cereus | DDAC + CAR" = rbind(
    .tk_row("Control",             "Logistic",                      1.64, 9.69),
    .tk_row("DDAC 1 mg/L",         "Logistic + Hom",                1.64, 8.34, 1, 4.70, 0.001),
    .tk_row("CAR 100 mg/L",        "Logistic + Hom",                1.64, 7.97, 1, 2.51, 0.42),
    .tk_row("DDAC 1 + CAR 100",    "Chick-Watson",                  0,    K = 471.91, m = 1)
  ),
  "B. cereus | DDAC + EUG" = rbind(
    .tk_row("Control",             "Logistic",                      1.50, 9.65),
    .tk_row("DDAC 0.5 mg/L",       "Logistic",                      1.37, 8.54),
    .tk_row("EUG 800 mg/L",        "Logistic",                      1.11, 8.06),
    .tk_row("DDAC 0.5 + EUG 800",  "Hom",                           0,    K = 3.79, m = 0.43)
  ),
  # ---------------------------- Escherichia coli -----------------------------
  "E. coli | BAC + CAR" = rbind(
    .tk_row("Control",             "Logistic",                      2.03, 9.24),
    .tk_row("BAC 4 mg/L",          "Logistic + Lag",                1.87, 8.68, 0.0000503),
    .tk_row("CAR 200 mg/L",        "Logistic + Lag",                0.62, 8.14, 0.21),
    .tk_row("BAC 4 + CAR 200",     "Hom",                           0,    K = 2.66, m = 0.58)
  ),
  "E. coli | DDAC + CAR" = rbind(
    .tk_row("Control",             "Logistic",                      2.02, 9.38),
    .tk_row("DDAC 1 mg/L",         "Logistic + Lag + Hom",          1.74, 8.74, 0.01, 1.59, 0.59),
    .tk_row("CAR 200 mg/L",        "Logistic + Lag",                0.51, 7.90, 0.21),
    .tk_row("DDAC 1 + CAR 200",    "Chick-Watson",                  0,    K = 3.42, m = 1)
  ),
  "E. coli | BAC + EUG" = rbind(
    .tk_row("Control",             "Logistic",                      2.05, 9.32),
    .tk_row("BAC 16 mg/L",         "Logistic + Hom",                1.23, 9.27, 1, 5.12, 0.39),
    .tk_row("EUG 800 mg/L",        "Logistic",                      0.40, 8.63),
    .tk_row("BAC 16 + EUG 800",    "Hom",                           0,    K = 2.64, m = 0.44)
  ),
  "E. coli | DDAC + EUG" = rbind(
    .tk_row("Control",             "Logistic",                      1.95, 9.36),
    .tk_row("DDAC 0.5 mg/L",       "Logistic",                      1.62, 9.27),
    .tk_row("EUG 800 mg/L",        "Logistic + Lag",                0.81, 8.27, 0.23),
    .tk_row("DDAC 0.5 + EUG 800",  "Hom",                           0,    K = 1.62, m = 0.77)
  )
)

TK_PRESET_DATA <- list(
  "B. cereus | BAC + EUG" = list(
    Time = c(0, 2, 4, 8, 12, 16, 24),
    "Control" = c(6.0840, 7.9566, 8.9724, 9.6039, 9.7794, 9.7865, 9.3805),
    "BAC 8 mg/L" = c(6.1927, 5.9328, 5.0022, 6.6418, 7.0163, 7.4497, 7.9777),
    "EUG 800 mg/L" = c(6.1066, 6.9510, 7.9747, 7.9708, 7.7853, 7.7733, 7.6643),
    "BAC 8 mg/L + EUG 800 mg/L" = c(6.1383, 3.2974, 2.8653, 1.9542, 1.9542, 1.9542, 1.9542)
  ),
  "B. cereus | BAC + CAR" = list(
    Time = c(0, 2, 4, 8, 12, 16, 24),
    "Control" = c(6.1852, 7.9058, 8.8921, 9.0743, 9.4559, 9.4874, 9.1644),
    "BAC 4 mg/L" = c(6.3661, 6.0601, 5.6110, 6.3557, 7.1856, 7.1982, 8.3318),
    "CAR 50 mg/L" = c(6.3832, 6.3490, 6.4876, 7.4180, 8.0875, 8.0086, 8.2480),
    "BAC 4 mg/L + CAR 50 mg/L" = c(6.1742, 2.7132, 1.9542, 1.9542, 1.9542, 1.9542, 1.9542)
  ),
  "B. cereus | DDAC + CAR" = list(
    Time = c(0, 2, 4, 8, 12, 16, 24),
    "Control" = c(6.1548, 7.5119, 8.9129, 9.2745, 9.7061, 9.9311, 9.9049),
    "DDAC 1 mg/L" = c(6.4008, 5.4497, 7.5336, 8.0601, 8.0798, 8.2389, 8.9209),
    "CAR 100 mg/L" = c(6.0000, 5.9138, 6.9345, 7.7597, 7.8643, 7.9165, 8.1222),
    "DDAC 1 mg/L + CAR 100 mg/L" = c(6.1066, 1.9542, 1.9542, 1.9542, 1.9542, 1.9542, 1.9542)
  ),
  "B. cereus | DDAC + EUG" = list(
    Time = c(0, 2, 4, 8, 12, 16, 24),
    "Control" = c(6.2882, 7.7324, 8.7533, 9.3388, 9.7769, 9.8441, 9.6690),
    "DDAC 0.5 mg/L" = c(6.1461, 7.4960, 8.0407, 8.3627, 8.5740, 8.6418, 8.7482),
    "EUG 800 mg/L" = c(6.1151, 6.9646, 7.7794, 8.1414, 8.1703, 7.9437, 8.1184),
    "DDAC 0.5 mg/L + EUG 800 mg/L" = c(6.0923, 3.6917, 3.4205, 1.9542, 1.9542, 1.9542, 1.9542)
  ),
  "E. coli | BAC + CAR" = list(
    Time = c(0, 2, 4, 8, 12, 16, 24),
    "Control" = c(6.0401, 7.9420, 8.8741, 9.0531, 9.4003, 9.3467, 9.2956),
    "BAC 4 mg/L" = c(5.9646, 6.1703, 5.9845, 8.0563, 8.4194, 8.5643, 9.0434),
    "CAR 200 mg/L" = c(6.0287, 6.3560, 6.4764, 7.4122, 8.0870, 7.9978, 8.2422),
    "BAC 4 mg/L + CAR 200 mg/L" = c(5.9208, 3.9486, 3.6721, 1.9542, 1.9542, 1.9542, 1.9542)
  ),
  "E. coli | DDAC + CAR" = list(
    Time = c(0, 2, 4, 8, 12, 16, 24),
    "Control" = c(6.2019, 7.9004, 9.2439, 9.3212, 9.4802, 9.4372, 9.2882),
    "DDAC 1 mg/L" = c(6.4063, 5.4819, 5.7584, 7.8643, 8.3096, 8.7559, 8.8865),
    "CAR 200 mg/L" = c(6.2848, 6.5502, 6.5311, 7.3793, 7.7027, 7.8173, 8.0065),
    "DDAC 1 mg/L + CAR 200 mg/L" = c(6.2707, 3.2825, 2.0479, 1.9542, 1.9542, 1.9542, 1.9542)
  ),
  "E. coli | BAC + EUG" = list(
    Time = c(0, 2, 4, 8, 12, 16, 24),
    "Control" = c(6.1589, 7.9395, 8.8741, 9.3314, 9.4883, 9.3570, 9.2937),
    "BAC 16 mg/L" = c(5.9614, 4.1083, 4.2644, 5.3267, 6.3181, 7.9558, 9.1599),
    "EUG 800 mg/L" = c(6.1668, 6.2338, 7.2911, 7.5378, 7.9868, 8.3760, 8.7174),
    "BAC 16 mg/L + EUG 800 mg/L" = c(5.9286, 4.3589, 3.8195, 3.0969, 2.6797, 1.9542, 1.9542)
  ),
  "E. coli | DDAC + EUG" = list(
    Time = c(0, 2, 4, 8, 12, 16, 24),
    "Control" = c(6.0737, 7.9542, 8.8741, 9.3979, 9.6500, 9.5966, 8.9890),
    "DDAC 0.5 mg/L" = c(5.9574, 7.5543, 8.4890, 9.0798, 9.3723, 9.3903, 9.2937),
    "EUG 800 mg/L" = c(6.0569, 6.1599, 7.0177, 7.9226, 8.1228, 8.5662, 8.1533),
    "DDAC 0.5 mg/L + EUG 800 mg/L" = c(6.0716, 4.5873, 4.3862, 2.5441, 1.9542, 1.9542, 1.9542)
  )
)

# Parameter and data tables for a preset.
presetDataTimeKill <- function(preset_name, n_rows = 30) {
  pars <- TK_PRESETS[[preset_name]]
  obs  <- TK_PRESET_DATA[[preset_name]]
  curve_names <- setdiff(names(obs), "Time")
  pars$Curve <- curve_names
  df <- data.frame(Time = pad(obs$Time, n_rows))
  for (i in seq_along(curve_names)) df[[paste0("C", i)]] <- pad(obs[[curve_names[i]]], n_rows)
  list(params = pars, data = df)
}
