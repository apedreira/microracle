library(shiny)
library(rhandsontable)
library(zip)

# --- FUNCTION TO DOWNLOAD .ZIP (DATA in .tsv + MODEL & PLOT CODE) ---
downloadZipSEplanes <- function(input, rv) {
  downloadHandler(
    filename = function() {
      paste0("SEplanes_DataAndCode_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      # 1. Move to a temporary directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd)) 
      
      tsv_name <- "SEplanes_Parameters.tsv"
      
      r_script_source <- "SEplanes_Offline.R" 
      r_script_target <- "SEplanes_Offline.R"
      
      # 2. WRITE THE TSV (Plot_mode removed)
      params_df <- data.frame(
        Parameter = c("X_0", "K", "X_e", "r_0", "T_g", "D", "T_k"),
        Value = c(input$X_0, input$K, input$X_g, input$r_0, input$T_g, input$D, ifelse(input$model3Mode == "by_k", input$T_k, NA))
      )
      write.table(params_df, tsv_name, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
      
      cat("\n--- BACTERIAL TRAITS: GROWTH RATE (MU) ---\n", file = tsv_name, append = TRUE)
      cur_mu <- if(!is.null(input$model3table_mu)) hot_to_r(input$model3table_mu) else rv$data_mu
      cur_mu_out <- cbind(Strain = c("Strain 1", "Strain 2"), cur_mu)
      colnames(cur_mu_out)[colnames(cur_mu_out) == "Mu"] <- "mu"
      colnames(cur_mu_out)[colnames(cur_mu_out) == "CI_Lower"] <- "CI_lower"
      colnames(cur_mu_out)[colnames(cur_mu_out) == "CI_Upper"] <- "CI_upper"
      write.table(cur_mu_out, tsv_name, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE, append = TRUE)
      
      cat("\n", file = tsv_name, append = TRUE)
      if (input$model3Mode == 'by_k') {
        cat("--- BACTERIAL TRAITS: KILL RATES (k) at defined antimicrobial concentration (C) ---\n", file = tsv_name, append = TRUE)
        cur_k <- if(!is.null(input$model3table_k)) hot_to_r(input$model3table_k) else rv$data_k
        cur_k <- cur_k[!is.na(cur_k$C) & cur_k$C != "", ] 
        colnames(cur_k)[colnames(cur_k) == "s1_min"] <- "s1_CI_lower"; colnames(cur_k)[colnames(cur_k) == "s1_max"] <- "s1_CI_upper"
        colnames(cur_k)[colnames(cur_k) == "s2_min"] <- "s2_CI_lower"; colnames(cur_k)[colnames(cur_k) == "s2_max"] <- "s2_CI_upper"
        write.table(cur_k, tsv_name, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE, append = TRUE)
      } else {
        cat("--- BACTERIAL TRAITS: SURVIVAL FRACTIONS (SF) ---\n", file = tsv_name, append = TRUE)
        cur_sf <- if(!is.null(input$model3table_sf)) hot_to_r(input$model3table_sf) else rv$data_sf
        cur_sf <- cur_sf[!is.na(cur_sf$C) & cur_sf$C != "", ] 
        colnames(cur_sf)[colnames(cur_sf) == "s1_min"] <- "s1_CI_lower"; colnames(cur_sf)[colnames(cur_sf) == "s1_max"] <- "s1_CI_upper"
        colnames(cur_sf)[colnames(cur_sf) == "s2_min"] <- "s2_CI_lower"; colnames(cur_sf)[colnames(cur_sf) == "s2_max"] <- "s2_CI_upper"
        write.table(cur_sf, tsv_name, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE, append = TRUE)
      }
      
      # 3. COPY THE R SCRIPT TO THE TEMPORARY DIRECTORY
      file.copy(from = file.path(owd, r_script_source), to = r_script_target, overwrite = TRUE)
      
      # 4. CREATE THE ZIP
      zip::zipr(zipfile = file, files = c(tsv_name, r_script_target))
    },
    contentType = "application/zip"
  )
}