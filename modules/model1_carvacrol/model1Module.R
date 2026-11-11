################################################################################
# Model 1: carvacrol at sub-MIC concentrations
# Pedreira et al. (2024) Journal of Food Engineering 361, 111734
#
# Requires: carFun.R, presetDataCar.R, dataValidator.R and helpers.R
################################################################################

# Model parameters, in the order expected by submiC3()
M1_PARAM_IDS <- c("mug0", "mud0", "kS", "kG", "kD")
# Rows of the editable data tables
M1_TABLE_ROWS <- 100

model1UI <- function(id, model) {
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
          5,
          numericInput(
            inputId = ns("kS"),
            HTML("<span class='nobr'>k<span class='supsub'><br/>S</span>&nbsp;&nbsp;</span><span>(h<sup>-1</sup>)</span>"),
            value = NULL,
            step = 0.00001,
            width = "100%"
          ),
          bsTooltip(ns("kS"),
                    "Positive constant characterising the rate of the state transition.",
                    "right",
                    options = list(container = "body")
          ),
          numericInput(
            inputId = ns("kG"),
            HTML("<span class='nobr'>k<span class='supsub'><br/>g</span>&nbsp;&nbsp;</span><span>(h<sup>-1</sup>)</span>"),
            value = NULL,
            step = 0.0000001,
            width = "100%"
          ),
          bsTooltip(ns("kG"),
                    "Rate of growth as antimicrobial concentration increases",
                    "right",
                    options = list(container = "body")
          ),
          numericInput(
            inputId = ns("kD"),
            HTML("<span class='nobr'>k<span class='supsub'><br/>d</span>&nbsp;&nbsp;</span><span>(h<sup>-1</sup>)</span>"),
            value = NULL,
            step = 0.00000001,
            width = "100%"
          ),
          bsTooltip(ns("kD"),
                    "Rate of decay as antimicrobial concentration increases",
                    "right",
                    options = list(container = "body")
          ),
        ),
        column(
          5, numericInput(
            inputId = ns("mug0"),
            HTML("<span class='nobr'>μ<span class='supsub'>0<br/>g</span>&nbsp;&nbsp;</span><span>(h<sup>-1</sup>)</span>"),
            value = NULL,
            step = 0.0001,
            width = "100%"
          ),
          bsTooltip(ns("mug0"),
                    "Maximum growth rate in absence of antimicrobial",
                    "right",
                    options = list(container = "body")
          )
        ),
        column(
          5, numericInput(
            inputId = ns("mud0"),
            HTML("<span class='nobr'>μ<span class='supsub'>0<br/>m</span>&nbsp;&nbsp;</span><span>(h<sup>-1</sup>)</span>"),
            value = NULL,
            step = 0.000001,
            width = "100%"
          ),
          bsTooltip(ns("mud0"),
                    "Maximum death rate in absence of antimicrobial",
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
        p("Closed-form solution used to simulate each experiment, with N the cell concentration (CFU/mL) and C the carvacrol concentration (mg/L):"),
        p("$$N(t) = N_0 \\exp\\left( \\mu_g^0\\, e^{-k_g C}\\, \\frac{1 - e^{-k_S t}}{k_S} \\; - \\; \\mu_m^0\\, e^{k_d C}\\, t \\right)$$"),
        p("Growth slows down over time (state transition, rate k", tags$sub("S"), ") and with the antimicrobial concentration, while death increases with the concentration:"),
        tags$ul(
          tags$li(HTML("<b>μ<sub>g</sub><sup>0</sup></b> (h<sup>-1</sup>): maximum growth rate in absence of antimicrobial.")),
          tags$li(HTML("<b>μ<sub>m</sub><sup>0</sup></b> (h<sup>-1</sup>): death rate in absence of antimicrobial.")),
          tags$li(HTML("<b>k<sub>S</sub></b> (h<sup>-1</sup>): rate of the state transition.")),
          tags$li(HTML("<b>k<sub>g</sub></b>: decrease of the growth rate as the concentration increases.")),
          tags$li(HTML("<b>k<sub>d</sub></b>: increase of the death rate as the concentration increases.")),
          tags$li(HTML("<b>N<sub>0</sub></b> (CFU/mL): first count of each experiment."))
        ),
        p(HTML("The model is solved at the sampling times plus the number of <b>Discretization points</b> chosen in the sidebar."))
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
                  textInput(ns("colHeader2"), "Exp#2 name:", value = "Exp2", width = "40%")
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

model1Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # All observers are registered once, at the top level of the server. Nesting
    # them inside observe()/observeEvent() registers a new copy on every
    # re-execution, so a single click on Run/Reset ends up firing N times.

    # Preset data and parameters for the selected strain
    presetData <- reactive({
      presetDataCar(input$strain)
    })

    # Preset parameter values, in the order expected by submiC3()
    presetParams <- function() {
      unlist(presetData()$params[M1_PARAM_IDS])
    }

    # Current parameter values, in the order expected by submiC3().
    # NULL when any of them is empty.
    modelParams <- function() {
      params <- unlist(lapply(M1_PARAM_IDS, function(pid) input[[pid]]))
      if (length(params) != length(M1_PARAM_IDS) || anyNA(params)) return(NULL)
      params
    }

    # Restore equation parameters and discretization points to preset values.
    # Use updateXInput to avoid triggering automatic rendering when updating parameters
    resetParams <- function() {
      preset <- presetData()
      for (pid in M1_PARAM_IDS) {
        updateNumericInput(session, pid, value = preset$params[[pid]])
      }
      updateSliderInput(session, "discTimes", value = 100)
    }

    # Preset data frame for the single experiment (second curve of the preset)
    singlePreset <- function() {
      preset <- presetData()
      cv <- preset$curves[[2]]
      data.frame(
        Time = pad(preset$time, M1_TABLE_ROWS),
        y = pad(cv$y, M1_TABLE_ROWS),
        drug = pad(rep(cv$drug, length(preset$time)), M1_TABLE_ROWS)
      )
    }

    # Preset data frame for the multiple experiment (one pair of columns per curve)
    multiplePreset <- function() {
      preset <- presetData()
      df <- data.frame(exp1_t = pad(preset$time, M1_TABLE_ROWS))
      for (i in seq_along(preset$curves)) {
        cv <- preset$curves[[i]]
        df[[paste0("exp", i, "_y")]] <- pad(cv$y, M1_TABLE_ROWS)
        df[[paste0("exp", i, "_d")]] <- pad(rep(cv$drug, length(preset$time)), M1_TABLE_ROWS)
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
        headers[i + 1] <- paste0(input[[paste0("colHeader", input_count)]], " Carvacrol (mg/L)")
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
      rhandsontable(datavalues$data, maxRows = 100, colHeaders = c("Time (h)", "CFU/mL", "Carvacrol concentration (mg/L)")) %>%
        hot_validate_numeric(col = c(1, 2, 3), min = 0)
    })

    # Create handsontable using dynamic input values for column headers
    output$tableMultiple <- renderRHandsontable({
      outputValues$version
      req(outputValues$data)
      rhandsontable(outputValues$data, maxRows = 100, colHeaders = colHeaders()) %>%
        hot_validate_numeric(col = c(1:13), min = 0)
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
      submic_car_func <- submiC3(time_exp, c_exp, params, y_exp, discTimes)
      time_mod <- submic_car_func[[1]]
      y_mod <- submic_car_func[[2]]

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

      # Retrieve discretization points
      discTimes <- input$discTimes

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
          submic_car_func <- submiC3(time_exp, drug_conc, params, y_exp, discTimes)
          time_mod <- submic_car_func[[1]]
          y_mod <- log10(submic_car_func[[2]])

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
