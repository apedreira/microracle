# Model parameters, in the order expected by submic_ddac()
M2_PARAM_IDS <- c("ka0", "kg0", "kd0", "kd1", "ki", "IC50_a", "IC50_g", "EC50_d",
                  "gamma_a", "gamma_g", "gamma_d")
# Rows of the editable data tables
M2_TABLE_ROWS <- 100

model2UI <- function(id, model) {
  ns <- NS(id)
  model_page(
    model,
    sidebar = tagList(
      fluidRow(
        column(
          10,
          div(
            class = "species-choice",
            radioButtons(ns("strain"), "Preset data for:",
                         c("Escherichia coli" = "ecoli", "Bacillus cereus" = "bcereus"),
                         inline = TRUE
            )
          ),
          bsTooltip(ns("strain"),
                    "Predefined data and parameters for E.coli and B.cereus.",
                    "bottom",
                    options = list(container = "body")
          )
        ),
        column(
          3,
          numericInput(
            inputId = ns("ka0"),
            HTML("<span class='nobr'>k<span class='supsub'>0<br/>a</span>&nbsp;&nbsp;</span><span>(h<sup>-1</sup>)</span>"),
            value = NULL,
            step = 0.00001,
            width = "100%"
          ),
          bsTooltip(ns("ka0"),
                    "Adaptation rate without disinfectant",
                    "right",
                    options = list(container = "body")
          ),
          numericInput(
            inputId = ns("kg0"),
            HTML("<span class='nobr'>k<span class='supsub'>0<br/>g</span>&nbsp;&nbsp;</span><span>(AU h<sup>-1</sup>)</span>"),
            value = NULL,
            step = 0.00001,
            width = "100%"
          ),
          bsTooltip(ns("kg0"),
                    "Growth rate without disinfectant",
                    "right",
                    options = list(container = "body")
          ),
          numericInput(
            inputId = ns("kd0"),
            HTML("<span class='nobr'>k<span class='supsub'>0<br/>d</span>&nbsp;&nbsp;</span><span>(h<sup>-1</sup>)</span>"),
            value = NULL,
            step = 0.00001,
            width = "100%"
          ),
          bsTooltip(ns("kd0"),
                    "Death rate without disinfectant",
                    "right",
                    options = list(container = "body")
          ),
          numericInput(
            inputId = ns("kd1"),
            HTML("<span class='nobr'>k<span class='supsub'>d<br/>*</span>&nbsp;&nbsp;</span><span>(h<sup>-1</sup>)</span>"),
            value = NULL,
            step = 0.0001,
            width = "100%"
          ),
          bsTooltip(ns("kd1"),
                    "Scaling of disinfectant effect on death",
                    "right",
                    options = list(container = "body")
          )
        ),
        column(
          3,
          numericInput(
            inputId = ns("ki"),
            HTML("<span class='nobr'>k<span class='supsub'><br/>i</span>&nbsp;&nbsp;</span><span>(AU)</span>"),
            value = NULL,
            step = 0.000001,
            width = "100%"
          ),
          bsTooltip(ns("ki"),
                    "Inhibition constant due to cell density",
                    "right",
                    options = list(container = "body")
          ),
          numericInput(
            inputId = ns("IC50_a"),
            HTML("<span class='nobr'>IC<span class='supsub'>50,a<br/></span>&nbsp;&nbsp;</span><span>(mg L<sup>-1</sup>)</span>"),
            value = NULL,
            step = 0.000001,
            width = "100%"
          ),
          bsTooltip(ns("IC50_a"),
                    "Half maximal inhibitory concentration of adaptation rate",
                    "right",
                    options = list(container = "body")
          ),
          numericInput(
            inputId = ns("IC50_g"),
            HTML("<span class='nobr'>IC<span class='supsub'>50,g<br/></span>&nbsp;&nbsp;</span><span>(mg L<sup>-1</sup>)</span>"),
            value = NULL,
            step = 0.000001,
            width = "100%"
          ),
          bsTooltip(ns("IC50_g"),
                    "Half maximal inhibitory concentration of growth rate",
                    "right",
                    options = list(container = "body")
          ),
          numericInput(
            inputId = ns("EC50_d"),
            HTML("<span class='nobr'>EC<span class='supsub'>50,d<br/></span>&nbsp;&nbsp;</span><span>(mg L<sup>-1</sup>)</span>"),
            value = NULL,
            step = 0.000001,
            width = "100%"
          ),
          bsTooltip(ns("EC50_d"),
                    "Half maximal effective concentration on death",
                    "right",
                    options = list(container = "body")
          )
        ),
        column(
          3, numericInput(
            inputId = ns("gamma_a"),
            HTML("<span class='nobr'>γ<span class='supsub'>a<br/></span>&nbsp;&nbsp;</span>"),
            value = NULL,
            step = 0.000001,
            width = "100%"
          ),
          bsTooltip(ns("gamma_a"),
                    "Effect shape of disinfectant over adaptation rate",
                    "right",
                    options = list(container = "body")
          ),
          numericInput(
            inputId = ns("gamma_g"),
            HTML("<span class='nobr'>γ<span class='supsub'>g<br/></span>&nbsp;&nbsp;</span>"),
            value = NULL,
            step = 0.000001,
            width = "100%"
          ),
          bsTooltip(ns("gamma_g"),
                    "Effect shape of disinfectant over growth rate",
                    "right",
                    options = list(container = "body")
          ),
          numericInput(
            inputId = ns("gamma_d"),
            HTML("<span class='nobr'>γ<span class='supsub'>d<br/></span>&nbsp;&nbsp;</span>"),
            value = NULL,
            step = 0.000001,
            width = "100%"
          ),
          bsTooltip(ns("gamma_d"),
                    "Effect shape of disinfectant over death rate",
                    "right",
                    options = list(container = "body")
          )
        ),
      ),
      sliderInput(ns("discTimes"),
                  label = "Discretization points",
                  step = 10,
                  value = 100,
                  min = 10,
                  max = 200
      ),
      bsTooltip(ns("discTimes"),
                "Number of intermediate points for modelling",
                "bottom",
                options = list(container = "body")
      ),
      # Run/Reset buttons for the active experiment tab
      model_actions(
        conditionalPanel(
          condition = "input.experimentTabs == 'Single experiment'",
          ns = ns,
          div(
            align = "center",
            actionButton(ns("runSingle"), "Run", style = "margin-right: 16px;", class = "btn-success"),
            actionButton(ns("resetSingle"), "Reset", class = "btn-warning")
          )
        ),
        conditionalPanel(
          condition = "input.experimentTabs == 'Multiple experiment'",
          ns = ns,
          div(
            align = "center",
            actionButton(ns("runMultiple"), "Run", style = "margin-right: 16px;", class = "btn-success"),
            actionButton(ns("resetMultiple"), "Reset", class = "btn-warning")
          )
        )
      )
    ),
    tabs = list(
      tabPanel(
        "Plot", loading_spinner(plotlyOutput(ns("plot"), height = "400px"))
      ),
      tabPanel(
        "Model",
        br(),
        p("Each experiment is simulated by solving the following system, where C is the DDAC concentration (mg/L):"),
        p("$$\\frac{dX_l}{dt} = -(k_a + k_d)\\, X_l$$"),
        p("$$\\frac{dX}{dt} = k_a X_l + \\frac{k_g}{k_i + X/X_m}\\, S X - k_d X$$"),
        p("$$\\frac{dS}{dt} = -\\frac{Y_s}{X_m}\\, \\frac{k_g}{k_i + X/X_m}\\, S X$$"),
        p("with the rates depending on the concentration as"),
        p("$$k_a = k_a^0 \\frac{IC_{50,a}^{\\gamma_a}}{C^{\\gamma_a} + IC_{50,a}^{\\gamma_a}}, \\qquad k_g = k_g^0 \\frac{IC_{50,g}^{\\gamma_g}}{C^{\\gamma_g} + IC_{50,g}^{\\gamma_g}}, \\qquad k_d = k_d^0 + k_d^{*} \\frac{C^{\\gamma_d}}{C^{\\gamma_d} + EC_{50,d}^{\\gamma_d}}$$"),
        tags$ul(
          tags$li(HTML("<b>X<sub>l</sub></b>: cells that have not adapted yet (lag phase). At t = 0 it holds the whole inoculum, N<sub>0</sub>.")),
          tags$li(HTML("<b>X</b>: adapted cells, which grow and die. It starts at 0.")),
          tags$li(HTML("<b>S</b>: normalised substrate, from 1 at t = 0.")),
          tags$li(HTML("The plotted count is X<sub>l</sub> + X (CFU/mL)."))
        ),
        uiOutput(ns("fixedText"))
      )
    ),
    tables = tabsetPanel(
      id = ns("experimentTabs"),
      type = "tabs",
      tabPanel(
        "Single experiment",
        br(),
        sidebarPanel(
          width = 4,
          rHandsontableOutput(ns("tableSingle"), height = "400px")
        )
      ),
      tabPanel(
        "Multiple experiment",
        br(),
        sidebarPanel(
          width = 12,
          fluidRow(
            column(
              width = 12,
              fluidRow(
                column(
                  width = 2,
                  textInput(ns("colHeader1"), "Exp#1 name:", value = "Control", width = "40%")
                ),
                column(
                  width = 2,
                  textInput(ns("colHeader2"), "Exp#2 name:", value = "0.50", width = "40%")
                ),
                column(
                  width = 2,
                  textInput(ns("colHeader3"), "Exp#3 name:", value = "Exp3", width = "40%")
                ),
                column(
                  width = 2,
                  textInput(ns("colHeader4"), "Exp#4 name:", value = "Exp4", width = "40%")
                ),
                column(
                  width = 2,
                  textInput(ns("colHeader5"), "Exp#5 name:", value = "Exp5", width = "40%")
                ),
                column(
                  width = 2,
                  textInput(ns("colHeader6"), "Exp#6 name:", value = "Exp6", width = "40%")
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  rHandsontableOutput(ns("tableMultiple"), height = "400px")
                )
              )
            )
          )
        )
      )
    )
  )
}

model2Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # All observers are registered once, at the top level of the server. Nesting
    # them inside observe()/observeEvent() registers a new copy on every
    # re-execution, so a single click on Run/Reset ends up firing N times.

    # Preset data and parameters for the selected strain
    presetData <- reactive({
      presetDataDDAC(input$strain)
    })

    # Preset parameter values, in the order expected by submic_ddac()
    presetParams <- function() {
      unlist(presetData()$params[M2_PARAM_IDS])
    }

    # Current parameter values, in the order expected by submic_ddac().
    # NULL when any of them is empty.
    modelParams <- function() {
      params <- unlist(lapply(M2_PARAM_IDS, function(pid) input[[pid]]))
      if (length(params) != length(M2_PARAM_IDS) || anyNA(params)) return(NULL)
      params
    }

    # Restore equation parameters and discretization points to preset values.
    # Use updateXInput to avoid triggering automatic rendering when updating parameters
    resetParams <- function() {
      preset <- presetData()
      for (pid in M2_PARAM_IDS) {
        updateNumericInput(session, pid, value = preset$params[[pid]])
      }
      updateSliderInput(session, "discTimes", value = 100)
    }

    # Preset data frame for the single experiment (second curve of the preset)
    singlePreset <- function() {
      preset <- presetData()
      cv <- preset$curves[[2]]
      data.frame(
        Time = pad(preset$time, M2_TABLE_ROWS),
        y = pad(cv$y, M2_TABLE_ROWS),
        drug = pad(rep(cv$drug, length(preset$time)), M2_TABLE_ROWS)
      )
    }

    # Preset data frame for the multiple experiment (one pair of columns per curve)
    multiplePreset <- function() {
      preset <- presetData()
      df <- data.frame(exp1_t = pad(preset$time, M2_TABLE_ROWS))
      for (i in seq_along(preset$curves)) {
        cv <- preset$curves[[i]]
        df[[paste0("exp", i, "_y")]] <- pad(cv$y, M2_TABLE_ROWS)
        df[[paste0("exp", i, "_d")]] <- pad(rep(cv$drug, length(preset$time)), M2_TABLE_ROWS)
      }
      df
    }

    # Column headers for the multiple experiment table, built from the experiment names
    colHeaders <- function() {
      input_count <- 1
      headers <- character(13)
      headers[1] <- "Time (h)"
      for (i in seq(from = 2, to = 12, by = 2)) {
        headers[i] <- paste0(input[[paste0("colHeader", input_count)]], " CFU/mL")
        headers[i + 1] <- paste0(input[[paste0("colHeader", input_count)]], " DDAC (mg/L)")
        input_count <- input_count + 1
      }
      return(headers)
    }

    # Reactive values for the table data frames. 'version' forces the table to be
    # redrawn on Reset even when the stored data frame has not changed.
    datavalues <- reactiveValues(data = NULL, version = 0)
    outputValues <- reactiveValues(data = NULL, version = 0)

    # Create a handsontable for the data and validate that content is numerical
    output$tableSingle <- renderRHandsontable({
      datavalues$version
      req(datavalues$data)
      rhandsontable(datavalues$data, maxRows = 100, colHeaders = c("Time (h)", "CFU/mL", "DDAC concentration (mg/L)")) %>%
        hot_validate_numeric(col = c(1, 2, 3), min = 0)
    })

    # Create handsontable using dynamic input values for column headers
    output$tableMultiple <- renderRHandsontable({
      outputValues$version
      req(outputValues$data)
      rhandsontable(outputValues$data, maxRows = 100, colHeaders = colHeaders()) %>%
        hot_validate_numeric(col = c(1:13), min = 0)
    })

    # Fixed constants of the selected strain, in the Model tab
    output$fixedText <- renderUI({
      fixed <- presetData()$fixed
      xm <- fixed$bb * 1e10
      xm_exp <- floor(log10(xm))
      p(HTML(sprintf(
        "The parameters in the sidebar are described in their tooltips. For the selected strain, Y<sub>s</sub> = %s and X<sub>m</sub> = %s·10<sup>%d</sup> CFU/mL are fixed.
         The system is solved at the sampling times plus the number of <b>Discretization points</b> chosen in the sidebar.",
        format(fixed$Ys), format(signif(xm / 10^xm_exp, 4)), xm_exp
      )))
    })

    ### ----------------------------##
    ### SIMULATION                  ##
    ### ----------------------------##

    # Simulate the stored single experiment and plot it
    simulateSingle <- function(params) {
      # Ensure Time, Drug, and CFU/mL columns have matching lengths (excluding NA values)
      validation <- dataValidatorSingle(datavalues$data)
      if (!validation$ok) {
        showNotification(validation$msg, type = "error")
        return()
      }

      # Retrieve discretization points
      discTimes <- input$discTimes

      # Extract experimental data from data frame
      exp_df <- datavalues$data
      y_exp <- unlist(exp_df$y)
      c_exp <- unlist(exp_df$drug)
      time_exp <- unlist(exp_df$Time)

      # Execute modelling function and store output
      submic_ddac_func <- submic_ddac(time_exp, c_exp, params, y_exp, discTimes,
                                      fixed = isolate(presetData()$fixed))
      time_mod <- submic_ddac_func[[1]]
      y_mod <- submic_ddac_func[[2]]

      # Plot results (markers for experimental data, lines for model output)
      output$plot <- renderPlotly({
        plot_ly(exp_df,
                x = ~Time, y = log10(exp_df$y), name = "Observed", type = "scatter",
                mode = "markers", color = "white"
        ) %>%
          add_lines(name = "Expected", x = time_mod, y = log10(y_mod), mode = "line") %>%
          plot_layout_log10()
      })
    }

    # Simulate the stored multiple experiment and plot it
    simulateMultiple <- function(params) {
      headers <- colHeaders()

      validation <- dataValidatorMultiple(outputValues$data)
      if (!validation$ok) {
        showNotification(validation$msg, type = "error")
        return()
      }
      validated_subsetDF <- validation$data

      # Retrieve discretization points and the fixed constants of the strain
      discTimes <- input$discTimes
      fixed <- isolate(presetData()$fixed)

      # Plot results
      output$plot <- renderPlotly({
        # Initialize a scatter plot
        p <- plot_ly(type = "scatter", mode = "markers")

        # Initialize loop counters
        l <- 2
        exp_name <- 2

        # Unlist Time column from data frame
        time_exp <- unlist(validated_subsetDF[1])

        # Iterate over data frame to dynamically add traces and lines to plot
        while (l < (length(validated_subsetDF) + 1)) {
          y_exp <- unlist(validated_subsetDF[l])
          drug_conc <- unlist(validated_subsetDF[l + 1])

          # Execute model function and store results
          submic_ddac_func <- submic_ddac(time_exp, drug_conc, params, y_exp, discTimes, fixed = fixed)
          time_mod <- submic_ddac_func[[1]]
          y_mod <- log10(submic_ddac_func[[2]])

          # Retrieve column name for legend, removing the "CFU/mL" suffix
          currentColName <- headers[exp_name]
          legendName <- substr(currentColName, 1, nchar(currentColName) - 6)

          # Trace plot: Markers represent experimental data, lines represent model predictions
          p <- add_trace(p, x = unlist(validated_subsetDF[1]), y = log10(y_exp), mode = "markers", name = paste0(legendName, " (observed)"), marker = list(color = plot_color(l / 2)))
          p <- add_lines(p, x = time_mod, y = y_mod, mode = "line", name = paste0(legendName, " (expected)"), line = list(color = plot_color(l / 2)))
          l <- l + 2
          exp_name <- exp_name + 2
        }
        plot_layout_log10(p)
      })
    }

    # Parameters for Run: notify when some of them are missing
    runParams <- function() {
      params <- modelParams()
      if (is.null(params)) showNotification("Fill in all the model parameters.", type = "error")
      params
    }

    # Load preset data and parameters when the strain changes (and on start-up).
    # Nothing is simulated here: the plot is only computed on Run/Reset, since
    # some simulations take a while.
    observeEvent(input$strain, {
      resetParams()
      datavalues$data <- singlePreset()
      datavalues$version <- datavalues$version + 1
      outputValues$data <- multiplePreset()
      outputValues$version <- outputValues$version + 1
    })

    # On start-up only, plot the preset of the single experiment (the open tab).
    # Lower priority so it runs after the preset has been loaded above.
    observeEvent(input$strain, simulateSingle(presetParams()), once = TRUE, priority = -1)

    ### ----------------------------##
    ### CODE FOR SINGLE EXPERIMENT  ##
    ### ----------------------------##

    # Update data frame values and plot only when clicking "Run". Prevents automatic rendering.
    observeEvent(input$runSingle, {
      req(input$tableSingle)
      datavalues$data <- hot_to_r(input$tableSingle)
      params <- runParams()
      if (!is.null(params)) simulateSingle(params)
    })

    # Reset data frame, handsontable, parameters and plot to the preset
    observeEvent(input$resetSingle, {
      datavalues$data <- singlePreset()
      datavalues$version <- datavalues$version + 1
      resetParams()
      simulateSingle(presetParams())
    })

    ### -------------------------------##
    ### CODE FOR MULTIPLE EXPERIMENT   ##
    ### -------------------------------##

    # Apply handsontable changes to data frame when clicking "Run"
    observeEvent(input$runMultiple, {
      req(input$tableMultiple)
      outputValues$data <- hot_to_r(input$tableMultiple)
      params <- runParams()
      if (!is.null(params)) simulateMultiple(params)
    })

    # Reset data frame, handsontable, parameters and plot to the preset
    observeEvent(input$resetMultiple, {
      outputValues$data <- multiplePreset()
      outputValues$version <- outputValues$version + 1
      resetParams()
      simulateMultiple(presetParams())
    })
  })
}
