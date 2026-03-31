library(shiny)
library(plotly)
library(rhandsontable)
library(shinycssloaders)
library(shinyBS)
library(shinyjs)
library(shinyalert) 

# ==============================================================================
# UI DEFINITION
# ==============================================================================
model3UI <- function() {
  tabPanel(
    title = "Selection & Extinction Planes", 
    value = "PEplane",                         
    useShinyjs(),
    
    tags$head(
      tags$style(HTML("
        /* --- TAB STYLES (BUTTONS) --- */
        #sidebar_tabs > li > a {
          background-color: #f8f9fa;
          color: #333 !important;
          border: 1px solid #ddd;
          border-radius: 4px;
          margin-bottom: 5px; 
          font-weight: 500;
          font-size: 0.9em;   
          text-align: center;
        }
        #sidebar_tabs > li > a:hover {
          background-color: #e2e6ea;
          border-color: #adb5bd;
        }
        #sidebar_tabs > li.active > a, 
        #sidebar_tabs > li.active > a:focus, 
        #sidebar_tabs > li.active > a:hover {
          background-color: #0fc1a3 !important;
          color: white !important;
          border: 1px solid #0fc1a3;
          font-weight: bold;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .nav-pills { display: flex; flex-wrap: wrap; }
        .nav-pills > li { flex: 1; min-width: 100px; }
        .nav-pills > li > a { margin: 2px; padding: 8px 5px; }
        
        /* --- HANDSONTABLE HEADER STYLES --- */
        .handsontable th {
            vertical-align: middle;
            text-align: center;
            font-weight: bold;
            color: #333;
            background-color: #f0f0f0; 
            border-bottom: 1px solid #ccc;
        }
        
        /* --- PARAM DESCRIPTION STYLES --- */
        .param-desc {
          display: block;
          color: #6c757d;
          font-size: 0.78em;
          margin-top: -4px;
          margin-bottom: 4px;
          line-height: 1.3;
        }
      "))
    ),
    
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(12,
                 radioButtons("model3Mode", "Plot mode:",
                              c("By Kill rate (k)" = "by_k", 
                                "By Survival fraction (SF)" = "by_SF"),
                              inline = TRUE
                 )
          ),
          column(12,
                 align = "center", style = "margin-bottom: 20px;",
                 actionButton("run", "Run", class = "btn-success"),
                 actionButton("reset", "Reset", class = "btn-warning")
          )
        ),
        hr(),
        
        tabsetPanel(
          id = "sidebar_tabs",
          type = "pills", 
          
          tabPanel(
            title = tags$span(icon("flask"), "Experimental parameters"),
            br(),
            fluidRow(
              
              # --- Row 1: X_0 and K ---
              column(6,
                     tags$label(HTML("X<span class='supsub'><br/>0</span>"), class = "control-label"),
                     tags$small("Initial population (CFUs/mL)", class = "param-desc"),
                     textInput("X_0", label = NULL, width = "100%")
              ),
              column(6,
                     tags$label("K", class = "control-label"),
                     tags$small("Carrying capacity (CFUs/mL)", class = "param-desc"),
                     textInput("K", label = NULL, width = "100%")
              ),
              
              # --- Row 2: X_e and r_0 ---
              column(6,
                     tags$label(HTML("X<span class='supsub'>e<br/></span>"), class = "control-label"),
                     tags$small("Extinction limit (CFUs/mL)", class = "param-desc"),
                     textInput("X_g", label = NULL, width = "100%")
              ),
              column(6,
                     tags$label(HTML("T:W<span class='supsub'><br/>0</span> (-)"), class = "control-label"),
                     tags$small("Initial mixing ratio (Strain 1 : Strain 2)", class = "param-desc"),
                     numericInput("r_0", label = NULL, value = NULL, step = 0.01, width = "100%")
              ),
              
              # --- Row 3: T_g and D ---
              column(6,
                     tags$label(HTML("t<span class='supsub'><br/>g</span>"), class = "control-label"),
                     tags$small("Duration of growth periods (minutes)", class = "param-desc"),
                     numericInput("T_g", label = NULL, value = NULL, min = 1, width = "100%")
              ),
              column(6,
                     tags$label("D (-)", class = "control-label"),
                     tags$small("Dilution factor (1/value)", class = "param-desc"),
                     numericInput("D", label = NULL, value = NULL, step = 1, min = 1, width = "100%")
              ),
              
              # --- Row 4: T_k (conditional) ---
              column(6,
                     conditionalPanel(
                       condition = "input.model3Mode == 'by_k'",
                       tags$label(HTML("t<span class='supsub'><br/>k</span>"), class = "control-label"),
                       tags$small("Duration of killing periods (minutes)", class = "param-desc"),
                       numericInput("T_k", label = NULL, value = NULL, min = 1, width = "100%")
                     )
              )
            )
          ),
          
          tabPanel(
            title = tags$span(icon("bacteria"), "Bacterial traits"),
            br(),
            
            # --- Growth rate block ---
            tags$small(
              "Confidence Intervals (C.I) are optional. If omitted or illogical, the error bars will be ignored during calculation and plotting.",
              class = "param-desc"
            ),
            div(
              h5("Growth rate (μ)", style = "display:inline-block; font-weight: bold; color: #31708f;"),
              actionLink("help_mu", "", icon = icon("info-circle"), style = "margin-left: 5px; color: #31708f;")
            ),
            rHandsontableOutput("model3table_mu", height = "100px"),
            
            # --- Kill rates / Survival fractions blocks (dynamic) ---
            uiOutput("model3_dynamicTable_k"),
            uiOutput("model3_dynamicTable_sf")
          )
        ),
        
        # --- DOWNLOADS (AT THE BOTTOM OF SIDEBAR) ---
        hr(),
        div(align = "center",
            downloadButton("download_rcode", "Download Data & Code (.R)", class = "btn-info", style = "width: 80%;")
        )
      ), 
      
      mainPanel(
        tabsetPanel(
          tabPanel("Results (Selection & Extinction)",
                   # ACTUALIZADO: withSpinner con el GIF personalizado
                   withSpinner(plotOutput("plot_results", height = "750px"), 
                               image = "https://github.com/apedreira/microracle/blob/main/var/img/customLoading.gif?raw=true"),
                   br()
          ),
          tabPanel(
            "Reference", HTML("<br> <p>
          Martínez-López, N., Nordholt, N., Schreiber, F. & García, M. R. (2022). Conditions for Bacterial Selection and Extinction Driven by Growth-Kill Trade-Off in Cyclic Antimicrobial Treatments.
          <i>arXiv</i>. doi: <a href='https://doi.org/10.48550/arXiv.2602.14645' target='_blank'>https://doi.org/10.48550/arXiv.2602.14645</a> </p>") 
          )
        )
      )
    )
  )
}

# ==============================================================================
# SERVER LOGIC
# ==============================================================================
model3Server <- function(input, output, session) {
  
  rv <- reactiveValues(data_mu = NULL, data_k = NULL, data_sf = NULL, plot_inputs = NULL)
  
  # --- INITIAL DATA LOAD ---
  observe({
    req(is.null(rv$data_mu)) 
    presetData <- sampleDataModel3()
    presetCI <- presetCIModel3()
    
    updateNumericInput(session, "T_g", value = presetData[[1]])
    updateNumericInput(session, "T_k", value = presetData[[2]])
    updateTextInput(session, "K", value = presetData[[3]])
    updateTextInput(session, "X_0", value = presetData[[5]])
    updateTextInput(session, "X_g", value = presetData[[6]])
    updateNumericInput(session, "r_0", value = presetData[[7]])
    updateNumericInput(session, "D", value = presetData[[8]])
    
    C_vals <- presetData[[4]]
    len_data <- length(unlist(presetData[[11]])) 
    total_len <- len_data + 6 
    C_col <- c(C_vals, rep(NA, max(0, total_len - length(C_vals))))[1:total_len]
    
    rv$data_mu <- data.frame(
      CI_Lower = c(unlist(presetCI[[1]][, 1]), unlist(presetCI[[2]][, 1])), 
      Mu       = c(unlist(presetData[[9]]),    unlist(presetData[[10]])),   
      CI_Upper = c(unlist(presetCI[[1]][, 2]), unlist(presetCI[[2]][, 2]))  
    )
    rv$data_k <- data.frame(
      C      = C_col,
      s1_min = c(unlist(presetCI[[3]][, 1]), rep(NA, 6)),
      s1_k   = c(unlist(presetData[[11]]), rep(NA, 6)),
      s1_max = c(unlist(presetCI[[3]][, 2]), rep(NA, 6)),
      s2_min = c(unlist(presetCI[[4]][, 1]), rep(NA, 6)),
      s2_k   = c(unlist(presetData[[12]]), rep(NA, 6)),
      s2_max = c(unlist(presetCI[[4]][, 2]), rep(NA, 6))
    )
    rv$data_sf <- data.frame(
      C      = C_col,
      s1_min = c(unlist(presetCI[[5]][, 1]), rep(NA, 6)),
      s1_sf  = c(unlist(presetData[[13]]), rep(NA, 6)),
      s1_max = c(unlist(presetCI[[5]][, 2]), rep(NA, 6)),
      s2_min = c(unlist(presetCI[[6]][, 1]), rep(NA, 6)),
      s2_sf  = c(unlist(presetData[[14]]), rep(NA, 6)),
      s2_max = c(unlist(presetCI[[6]][, 2]), rep(NA, 6))
    )
  })
  
  # --- TABLE RENDERING ---
  output$model3table_mu <- renderRHandsontable({
    req(rv$data_mu)
    rhandsontable(rv$data_mu, 
                  rowHeaders = c("Strain 1", "Strain 2"), 
                  colHeaders = c(HTML("C.I (Lower)"), HTML("μ<br/> (h<sup>-1</sup>)"), HTML("C.I (Upper)")),
                  rowHeaderWidth = 100) %>% 
      hot_validate_numeric(col = c(1, 2, 3), min = 0)
  })
  
  output$model3table_k <- renderRHandsontable({
    req(rv$data_k)
    row_labs <- paste("Exp.#", 1:nrow(rv$data_k))
    headers <- c("<b>Drug<br>concentration</b><br>", "<br><small>C.I Lower</small>", "<b>Strain 1</b><br>k", "<br><small>C.I Upper</small>", "<br><small>C.I Lower</small>", "<b>Strain 2</b><br>k", "<br><small>C.I Upper</small>")
    
    rhandsontable(rv$data_k, maxRows = 100, rowHeaders = row_labs, colHeaders = headers, rowHeaderWidth = 80) %>% 
      hot_cols(halign = "center") %>% 
      hot_col(col=1:7, type="text") %>% 
      hot_col(col=c(1,4), renderer="function(instance, td, row, col, prop, value, cellProperties) { Handsontable.renderers.TextRenderer.apply(this, arguments); td.style.borderRight = '3px solid #666'; }")
  })
  
  output$model3table_sf <- renderRHandsontable({
    req(rv$data_sf)
    row_labs <- paste("Exp.#", 1:nrow(rv$data_sf))
    headers <- c("<b>Drug<br>concentration</b><br>", "<br><small>C.I Lower</small>", "<b>Strain 1</b><br>SF", "<br><small>C.I High</small>", "<br><small>C.I Low</small>", "<b>Strain 2</b><br>SF", "<br><small>C.I High</small>")
    
    rhandsontable(rv$data_sf, maxRows = 100, rowHeaders = row_labs, colHeaders = headers, rowHeaderWidth = 80) %>% 
      hot_cols(halign = "center") %>% 
      hot_col(col=1:7, type="text") %>% 
      hot_col(col=c(1,4), renderer="function(instance, td, row, col, prop, value, cellProperties) { Handsontable.renderers.TextRenderer.apply(this, arguments); td.style.borderRight = '3px solid #666'; }")
  })
  
  output$model3_dynamicTable_k <- renderUI({
    req(input$model3Mode)
    if(input$model3Mode == 'by_k') tagList(br(), div(h5(style="display:inline-block;font-weight:bold;color:#31708f;","Kill Rates (S1 & S2)"), actionLink("help_k","",icon=icon("info-circle"),style="margin-left:5px;color:#31708f;")), rHandsontableOutput("model3table_k", height="300px")) else NULL
  })
  output$model3_dynamicTable_sf <- renderUI({
    req(input$model3Mode)
    if(input$model3Mode == 'by_SF') tagList(br(), div(h5(style="display:inline-block;font-weight:bold;color:#31708f;","Survival Fractions (S1 & S2)"), actionLink("help_sf","",icon=icon("info-circle"),style="margin-left:5px;color:#31708f;")), rHandsontableOutput("model3table_sf", height="300px")) else NULL
  })
  
  # --- HELP INFORMATION MODALS ---
  observeEvent(input$help_mu, {
    shinyalert(
      title = "Info: Growth Rate (μ)",
      text = HTML(paste0(
        "<div style='text-align:left;'>",
        "<b>Strain Definitions:</b><br>",
        "&bull; <b>Strain 1 (S1):</b> Represents the sensitive or Wild Type strain.<br>",
        "&bull; <b>Strain 2 (S2):</b> Represents the resistant or Mutant strain.<br><br>",
        "<b>Input Rules:</b><br>",
        "&bull; <b>μ Values:</b> Must be non-negative numeric values (min<sup>-1</sup>).<br>",
        "&bull; <b>Confidence Intervals (C.I.):</b> Optional fields [Lower, Upper]. <br>",
        "If C.I. values are omitted, invalid (e.g. text), or illogical (Lower > Mean), ",
        "the error bars will be ignored in the plots and calculations.",
        "</div>"
      )),
      type = "info",
      html = TRUE
    )
  })
  
  observeEvent(input$help_k, {
    shinyalert(
      title = "Info: Kill Rate (k)",
      text = HTML(paste0(
        "<div style='text-align:left;'>",
        "<b>Strain Definitions:</b><br>",
        "&bull; <b>Strain 1 (S1):</b> Represents the sensitive or Wild Type strain.<br>",
        "&bull; <b>Strain 2 (S2):</b> Represents the resistant or Mutant strain.<br><br>",
        "<b>Input Rules:</b><br>",
        "&bull; <b>k Values:</b> Must be non-negative numeric values (min<sup>-1</sup>).<br>",
        "&bull; <b>Confidence Intervals (C.I.):</b> Optional fields [Lower, Upper]. <br>",
        "If C.I. values are omitted, invalid, or out of range (Lower > Mean > Upper), ",
        "the error bars will be ignored for that data point.",
        "</div>"
      )),
      type = "info",
      html = TRUE
    )
  })
  
  observeEvent(input$help_sf, {
    shinyalert(
      title = "Info: Survival Fraction (SF)",
      text = HTML(paste0(
        "<div style='text-align:left;'>",
        "<b>Strain Definitions:</b><br>",
        "&bull; <b>Strain 1 (S1):</b> Represents the sensitive or Wild Type strain.<br>",
        "&bull; <b>Strain 2 (S2):</b> Represents the resistant or Mutant strain.<br><br>",
        "<b>Input Rules:</b><br>",
        "&bull; <b>SF Values:</b> Must be numeric values between 0 and 1.<br>",
        "&bull; <b>Confidence Intervals (C.I.):</b> Optional fields [Lower, Upper]. <br>",
        "If C.I. values are omitted, invalid, or out of range (Lower > Mean > Upper), ",
        "the error bars will be ignored for that data point.",
        "</div>"
      )),
      type = "info",
      html = TRUE
    )
  })
  
  observeEvent(input$model3table_mu, { rv$data_mu <- hot_to_r(input$model3table_mu) })
  observeEvent(input$model3table_k, { rv$data_k <- hot_to_r(input$model3table_k) })
  observeEvent(input$model3table_sf, { rv$data_sf <- hot_to_r(input$model3table_sf) })
  
  # --- RUN ACTION ---
  observeEvent(input$run, {
    cur_mu <- if(!is.null(input$model3table_mu)) hot_to_r(input$model3table_mu) else rv$data_mu
    cur_k  <- if(!is.null(input$model3table_k))  hot_to_r(input$model3table_k)  else rv$data_k
    cur_sf <- if(!is.null(input$model3table_sf)) hot_to_r(input$model3table_sf) else rv$data_sf
    
    validation <- dataValidatorModel3(data_mu=cur_mu, data_k=cur_k, data_sf=cur_sf, mode=input$model3Mode, X_0=input$X_0, X_g=input$X_g, r_0=input$r_0, T_g=input$T_g, T_k=input$T_k, K=input$K, D=input$D)
    
    if (validation$msg == "OK") {
      clean_mu <- validation$data_mu; clean_dynamic <- validation$data_dynamic
      
      rows_to_keep <- if(input$model3Mode == 'by_k') !is.na(clean_dynamic$s1_k) & !is.na(clean_dynamic$s2_k) else !is.na(clean_dynamic$s1_sf) & !is.na(clean_dynamic$s2_sf)
      clean_dynamic <- clean_dynamic[rows_to_keep, , drop = FALSE]
      if (nrow(clean_dynamic) == 0) { showNotification("No valid data rows found.", type="error"); return() }
      
      fn <- function(x) as.numeric(as.character(x))
      fixed_palette <- c(
        "#1B4F72", "#F1C40F", "#7D3C98", "#17A589", "#C0392B", 
        "#85C1E9", "#196F3D", "#F39C12", "#A569BD", "#E6B0AA"
      )
      nC <- nrow(clean_dynamic)
      colors_vec <- if (nC <= length(fixed_palette)) fixed_palette[1:nC] else rep(fixed_palette, length.out = nC)
      
      col_ext_S1_val <- "#167288"; col_ext_S2_val <- "#b45248"; col_SR_S1_val <- "#8cdaec"; col_SR_S2_val <- "#d6cfa2"; col_ext_T_val <- "#643c6a"
      
      plot_args <- list(
        T_g=fn(input$T_g), X_0=fn(input$X_0), r_0=fn(input$r_0), K=fn(input$K), D=fn(input$D), X_g=fn(input$X_g),
        mu_S1=clean_mu$Mu[1], mu_S2=clean_mu$Mu[2],
        ci_mu_S1=if(!any(is.na(clean_mu[1, c(1,3)]))) c(clean_mu[1,1], clean_mu[1,3]) else NULL,
        ci_mu_S2=if(!any(is.na(clean_mu[2, c(1,3)]))) c(clean_mu[2,1], clean_mu[2,3]) else NULL,
        cols=colors_vec,
        col_ext_S1=col_ext_S1_val, col_ext_S2=col_ext_S2_val, col_SR_S1=col_SR_S1_val, col_SR_S2=col_SR_S2_val, col_ext_T=col_ext_T_val,
        C=clean_dynamic$C
      )
      
      if (input$model3Mode == 'by_k') {
        plot_args$T_k <- fn(input$T_k); plot_args$k_S1 <- clean_dynamic$s1_k; plot_args$k_S2 <- clean_dynamic$s2_k
        has_ci_s1 <- !any(is.na(clean_dynamic$s1_min)) && !any(is.na(clean_dynamic$s1_max)); has_ci_s2 <- !any(is.na(clean_dynamic$s2_min)) && !any(is.na(clean_dynamic$s2_max))
        plot_args$ci_k_S1 <- if(has_ci_s1) as.matrix(clean_dynamic[, c("s1_min", "s1_max")]) else NULL
        plot_args$ci_k_S2 <- if(has_ci_s2) as.matrix(clean_dynamic[, c("s2_min", "s2_max")]) else NULL
        rv$plot_inputs <- list(mode="by_k", args=plot_args)
      } else {
        plot_args$SF_S1 <- clean_dynamic$s1_sf; plot_args$SF_S2 <- clean_dynamic$s2_sf
        has_ci_s1 <- !any(is.na(clean_dynamic$s1_min)) && !any(is.na(clean_dynamic$s1_max)); has_ci_s2 <- !any(is.na(clean_dynamic$s2_min)) && !any(is.na(clean_dynamic$s2_max))
        plot_args$ci_SF_S1 <- if(has_ci_s1) as.matrix(clean_dynamic[, c("s1_min", "s1_max")]) else NULL
        plot_args$ci_SF_S2 <- if(has_ci_s2) as.matrix(clean_dynamic[, c("s2_min", "s2_max")]) else NULL
        rv$plot_inputs <- list(mode="by_SF", args=plot_args)
      }
    } else { rv$plot_inputs <- NULL }
  })
  
  # --- CUSTOM LEGEND HELPER FUNCTION ---
  draw_legend_custom <- function(labels, colors, title_local, is_first = FALSE) {
    par(mar = c(0, 1, 0, 0))
    plot(0, 0, type = "n", axes = FALSE, xlim = c(0, 100), ylim = c(0, 100), xlab = "", ylab = "")
    
    if (is_first) {
      text(x = 0, y = 92, labels = "Legend", font = 2, cex = 1.8, adj = 0)
    }
    y_subtitle <- 72
    text(x = 0, y = y_subtitle, labels = title_local, font = 1, cex = 1.2, adj = 0)
    
    n <- length(labels)
    if (n == 0) return()
    
    cols_num <- 3 
    rows_num <- ceiling(n / cols_num)
    x_starts <- c(2, 35, 68)
    y_items_start <- 55 
    y_step <- 15 
    box_w <- 5
    
    for (i in 1:n) {
      c_idx <- (i - 1) %% cols_num + 1
      r_idx <- ceiling(i / cols_num)
      y <- y_items_start - (r_idx - 1) * y_step
      
      if(y < 0) break 
      
      x <- x_starts[c_idx]
      rect(x, y - 2, x + box_w, y + 4, col = colors[i], border = NA)
      text(x = x + box_w + 3, y = y, labels = labels[i], col = "black", cex = 1.1, adj = 0)
    }
  }
  
  # --- MAIN PLOT RENDERING ---
  output$plot_results <- renderPlot({
    req(rv$plot_inputs)
    
    layout(matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE), heights = c(0.82, 0.18))
    
    tryCatch({
      leg_data <- NULL
      if (rv$plot_inputs$mode == "by_k") {
        leg_data <- Plot_SE_Plane_k(rv$plot_inputs$args)
      } else {
        leg_data <- Plot_SE_Plane_SF(rv$plot_inputs$args)
      }
      
      if (!is.null(leg_data)) {
        draw_legend_custom(leg_data$sel_leg$lbl, leg_data$sel_leg$col, 
                           title_local = "Drug concentration (arbitrary units)", 
                           is_first = TRUE)
        draw_legend_custom(leg_data$ext_leg$lbl, leg_data$ext_leg$col, 
                           title_local = "Extinction/ Selection areas", 
                           is_first = FALSE)
      }
      
    }, error = function(e) {
      par(mfrow=c(1,1)); plot(1, 1, type="n"); text(1, 1, paste("Error:", e$message), col="red")
    })
    
    par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
  })
  
  # --- DONWLOAD DATA (.TSV) & OFFLINE MODEL (.R)  ---
  output$download_rcode <- downloadZipSEplanes(input, rv)
  
  observeEvent(input$reset, { rv$data_mu <- NULL; rv$plot_inputs <- NULL })
}