################################################################################
# Model 4: time-kill curves of QAC-EOC combinations
# Pedreira et al. (2024) Foods 13(12), 1831. doi:10.3390/foods13121831
#
# Requires: timeKillFun.R, presetDataTimeKill.R and helpers.R (sourced in app.R)
################################################################################

M4_MAX_CURVES <- 6
# Preset selectors: species and QAC–EOC pair. Values match the names of
# TK_PRESETS ("<species> | <pair>").
M4_SPECIES <- c("Escherichia coli" = "E. coli",
                "Bacillus cereus"  = "B. cereus")
M4_COMBOS <- c("Didecyldimethylammonium chloride (DDAC) + Carvacrol" = "DDAC + CAR",
               "Didecyldimethylammonium chloride (DDAC) + Eugenol"   = "DDAC + EUG",
               "Benzalkonium chloride (BAC) + Carvacrol"             = "BAC + CAR",
               "Benzalkonium chloride (BAC) + Eugenol"               = "BAC + EUG")
m4_preset_name <- function(species, combo) paste(species, "|", combo)

# ==============================================================================
# HELPERS (no Shiny dependencies, easy to test)
# ==============================================================================

# Keep the data table with exactly one value column per curve
m4_sync_columns <- function(data, n_curves) {
  value_cols <- setdiff(names(data), "Time")
  n_rows <- nrow(data)
  out <- data.frame(Time = data$Time)
  for (i in seq_len(n_curves)) {
    out[[paste0("C", i)]] <- if (i <= length(value_cols)) {
      as.numeric(data[[value_cols[i]]])
    } else {
      rep(NA_real_, n_rows)
    }
  }
  out
}

# Validate both tables and return the curves ready for simulation/fitting.
#   need_data = TRUE requires at least 3 points per curve (fitting)
m4_prepare <- function(params, data, units, lod, censor, need_data) {
  fail <- function(msg) list(ok = FALSE, msg = msg)
  
  if (is.null(params) || nrow(params) == 0) return(fail("Add at least one curve to the parameter table."))
  if (nrow(params) > M4_MAX_CURVES) return(fail(paste("A maximum of", M4_MAX_CURVES, "curves can be used.")))
  if (any(is.na(params$Curve) | trimws(params$Curve) == "")) return(fail("Give every curve a name (column 'Curve')."))
  if (any(duplicated(trimws(params$Curve)))) return(fail("Curve names must be different."))
  if (!all(params$Behaviour %in% c(TK_AUTO, TK_BEHAVIOUR_NAMES))) return(fail("Choose a behaviour from the list for every curve."))
  if (!is.numeric(lod) || is.na(lod)) return(fail("Enter a detection limit."))
  
  if (!need_data) {
    num <- as.matrix(params[, TK_PARAM_NAMES])
    if (any(is.na(num))) return(fail("Fill in all model parameters (μ, Nm, a0, K and m) before simulating."))
    if (any(num < 0)) return(fail("Model parameters cannot be negative."))
    if (any(params$a0 > 1) || any(params$m > 1)) return(fail("a0 and m must be between 0 and 1."))
  }
  
  time <- data$Time
  has_time <- !is.na(time)
  value_cols <- paste0("C", seq_len(nrow(params)))
  values <- as.matrix(data[, value_cols, drop = FALSE])
  
  if (any(!has_time & rowSums(!is.na(values)) > 0)) return(fail("Some rows have counts but no time. Check the 'Time (h)' column."))
  if (!any(has_time)) return(fail("Enter the sampling times in the 'Time (h)' column."))
  t_all <- time[has_time]
  if (any(t_all < 0)) return(fail("Times cannot be negative."))
  if (t_all[1] != 0) return(fail("The first time must be 0 h (initial count)."))
  if (any(diff(t_all) <= 0)) return(fail("Times must be in increasing order, without repetitions."))
  
  curves <- vector("list", nrow(params))
  for (i in seq_len(nrow(params))) {
    v <- values[has_time, i]
    keep <- !is.na(v)
    t <- t_all[keep]
    v <- v[keep]
    name <- params$Curve[i]
    
    if (length(t) == 0 || t[1] != 0) return(fail(paste0("Curve '", name, "' needs a count at t = 0 h.")))
    if (units == "cfu") {
      if (any(v < 0)) return(fail(paste0("Curve '", name, "': counts cannot be negative.")))
      N <- ifelse(v > 0, log10(v), -Inf)
    } else {
      N <- v
    }
    if (!is.finite(N[1])) return(fail(paste0("Curve '", name, "': the initial count must be above zero.")))
    if (N[1] <= lod) return(fail(paste0("Curve '", name, "': the initial count must be above the detection limit.")))
    
    cens <- if (censor) N <= lod + 1e-9 else rep(FALSE, length(N))
    if (!censor && any(!is.finite(N))) return(fail(paste0("Curve '", name, "' has zero counts. Enable censoring or enter values above zero.")))
    N[cens] <- lod
    if (need_data && length(N) < 3) return(fail(paste0("Curve '", name, "' needs at least 3 points (t = 0 and two more) to be fitted.")))
    
    curves[[i]] <- list(name = name, t = t, N = N, censored = cens)
  }
  list(ok = TRUE, curves = curves)
}

# Simulation for plotting
m4_simulate_curves <- function(params, curves, t_end) {
  lapply(seq_along(curves), function(i) {
    p <- unlist(params[i, TK_PARAM_NAMES])
    p <- tk_apply_behaviour(p, params$Behaviour[i])
    tt <- sort(unique(c(seq(0, t_end, length.out = 300), curves[[i]]$t)))
    list(t = tt, N = tk_simulate(tt, curves[[i]]$N[1], p))
  })
}

# Format a parameter as in Table 4 of the paper ("–" when it does not apply)
m4_format_params <- function(behaviour, p) {
  p <- unlist(p)
  growth <- unname(p["mu"]) > 0
  decay  <- unname(p["K"]) > 0
  fmt <- function(x) formatC(unname(x), format = "fg", digits = 3, flag = "#")
  c(mu = fmt(p["mu"]),
    Nm = if (growth) fmt(p["Nm"]) else "–",
    a0 = if (growth) fmt(p["a0"]) else "–",
    K  = fmt(p["K"]),
    m  = if (decay) fmt(p["m"]) else "–")
}

# Headers of the parameter table, with a tooltip on each one. The table is
# re-rendered often, so the Bootstrap tooltips are created on first hover
# (see M4_TOOLTIP_JS) instead of with bsTooltip.
M4_PARAM_HEADERS <- list(
  list("Curve", "Name of the curve. It labels the plot and the matching column of the data table."),
  list("Behaviour", "Sets which parameters are fixed and which are estimated (see the Model tab). 'Auto (lowest AIC)' chooses it by AIC when fitting."),
  list("μ (h<sup>-1</sup>)", "μ (h⁻¹): maximum growth rate. 0 means no growth."),
  list("N<sub>m</sub> (log<sub>10</sub>)", "Nm (log10 CFU/mL): maximum count reached by the population. Only used when μ > 0."),
  list("a<sub>0</sub>", "a0 (–): lag level, from 0 to 1. 1 means no lag phase. Only used when μ > 0."),
  list("K (h<sup>-m</sup>)", "K (h⁻ᵐ): inactivation rate. 0 means no inactivation."),
  list("m", "m (–): Hom coefficient, from 0 to 1. m = 1 is Chick–Watson (log-linear decay); m < 1 gives tailing. Only used when K > 0.")
)
m4_param_headers <- function() {
  vapply(M4_PARAM_HEADERS, function(h) {
    sprintf("<span class=\"m4-tip\" title=\"%s\" style=\"cursor:help;border-bottom:1px dotted;\">%s</span>",
            htmltools::htmlEscape(h[[2]], attribute = TRUE), h[[1]])
  }, character(1))
}
# Widths (px) of the row numbers and the vertical scrollbar of the data table
M4_ROW_HEADER_WIDTH <- 50
M4_SCROLLBAR_WIDTH <- 20
# Column width (px) that fits the longest text in one line. Handsontable's own
# auto-sizing wraps long HTML headers, so widths are set explicitly.
m4_col_width <- function(..., min_chars = 6) {
  n <- max(nchar(c(...), type = "width"), min_chars, na.rm = TRUE)
  round(n * 7.5 + 24)
}
M4_TOOLTIP_JS <- "
$(document).on('mouseenter', '.m4-tip', function() {
  var el = $(this);
  if (!el.data('bs.tooltip')) el.tooltip({container: 'body', placement: 'top'});
  el.tooltip('show');
});
$(document).on('mouseleave mousedown', '.m4-tip', function() {
  if ($(this).data('bs.tooltip')) $(this).tooltip('hide');
});
"

m4_plot <- function(curves, sims, lod) {
  floor_y <- min(lod - 1, min(unlist(lapply(curves, function(cv) cv$N))) - 0.5)
  p <- plot_ly()
  for (i in seq_along(curves)) {
    cv <- curves[[i]]
    col <- plot_color(i)
    if (!is.null(sims)) {
      y <- sims[[i]]$N
      y[y < floor_y] <- NA
      p <- add_lines(p, x = sims[[i]]$t, y = y, name = paste0(cv$name, " (model)"),
                     line = list(color = col), legendgroup = cv$name)
    }
    obs <- !cv$censored
    if (length(cv$N) > 1 || any(obs)) {
      p <- add_markers(p, x = cv$t[obs], y = cv$N[obs], name = paste0(cv$name, " (observed)"),
                       marker = list(color = col, size = 8), legendgroup = cv$name)
    }
    if (any(cv$censored)) {
      p <- add_markers(p, x = cv$t[cv$censored], y = cv$N[cv$censored],
                       name = paste0(cv$name, " (≤ detection limit)"),
                       marker = list(color = col, size = 9, symbol = "triangle-down-open"),
                       legendgroup = cv$name)
    }
  }
  t_max <- max(unlist(lapply(curves, function(cv) cv$t)), unlist(lapply(sims, function(s) s$t)))
  p %>% plot_layout_log10(
    shapes = list(list(type = "line", x0 = 0, x1 = t_max, y0 = lod, y1 = lod,
                       line = list(dash = "dash", color = "#888888", width = 1))),
    annotations = list(list(x = t_max, y = lod, text = "Detection limit", showarrow = FALSE,
                            xanchor = "right", yanchor = "bottom", font = list(size = 11, color = "#888888")))
  )
}

# ==============================================================================
# UI
# ==============================================================================
model4UI <- function(id, model) {
  ns <- NS(id)
  model_page(
    model,
    tabs_id = ns("tabs"),
    sidebar = tagList(
      div(class = "species-choice",
          radioButtons(ns("species"), "Preset data for:",
                       M4_SPECIES, inline = TRUE)),
      selectInput(ns("combo"), "Antimicrobial combination:",
                  choices = M4_COMBOS, width = "100%"),
      bsTooltip(ns("combo"),
                "Loads the experimental curves and the parameters of Table 4 of the reference for this QAC–EOC pair.",
                "right", options = list(container = "body")),
      radioButtons(ns("units"), "Units of the counts in the data table:",
                   c("log10 CFU/mL" = "log", "CFU/mL" = "cfu"), inline = TRUE),
      numericInput(ns("lod"), "Detection limit (log10 CFU/mL)", value = 2, step = 0.5, width = "100%"),
      checkboxInput(ns("censor"), "Treat counts at or below the detection limit as censored", value = TRUE),
      radioButtons(ns("fitMode"), "When fitting:",
                   c("Choose the behaviour of each curve by AIC" = "aic",
                     "Keep the behaviour set in the table" = "table")),
      bsTooltip(ns("censor"),
                "When ticked, those points only penalise the fit if the model predicts counts above the limit.",
                "right", options = list(container = "body")),
      model_actions(
        actionButton(ns("simulate"), "Simulate", class = "btn-success"),
        actionButton(ns("fit"), "Fit parameters", class = "btn-primary"),
        actionButton(ns("reset"), "Reset", class = "btn-warning")
      ),
      br(),
      tags$small(class = "param-desc",
                 HTML("<b>Simulate</b> draws the curves with the parameters of the table.
                       <b>Fit parameters</b> estimates them from the data. When choosing by AIC,
                       the eight behaviours are fitted to every curve and the one with the lowest
                       Akaike criterion is written to the table."))
    ),
    tabs = list(
      tabPanel("Plot",
               loading_spinner(plotlyOutput(ns("plot"), height = "450px"))),
      tabPanel("Fit results",
               br(),
               uiOutput(ns("fitEmpty")),
               tableOutput(ns("fitTable")),
               uiOutput(ns("compareUI")),
               tableOutput(ns("compareTable")),
               uiOutput(ns("downloadUI"))),
      tabPanel("Model",
               br(),
               p("Nested model for growth and inactivation (Equation 3 of the reference). N is the log", tags$sub("10"), " of CFU/mL:"),
               p("$$\\frac{dN}{dt} = \\frac{a_0}{a_0 + (1-a_0)\\,e^{-\\mu t}}\\,\\frac{\\mu}{\\ln 10}\\left(1 - 10^{\\,N - N_m}\\right) - \\frac{K}{\\ln 10}\\, m\\, t^{\\,m-1}$$"),
               tags$ul(
                 tags$li(HTML("<b>μ</b> (h<sup>-1</sup>): growth rate.")),
                 tags$li(HTML("<b>N<sub>m</sub></b> (log<sub>10</sub> CFU/mL): maximum count.")),
                 tags$li(HTML("<b>a<sub>0</sub></b> (–): lag level, from 0 to 1; 1 means no lag phase.")),
                 tags$li(HTML("<b>K</b> (h<sup>-m</sup>): inactivation rate.")),
                 tags$li(HTML("<b>m</b> (–): Hom coefficient, from 0 to 1; m = 1 is Chick–Watson (log-linear decay)."))
               ),
               p("Each behaviour fixes some parameters and estimates the rest:"),
               tableOutput(ns("behaviourTable")),
               p(HTML("Fitting minimises the weighted least squares of log<sub>10</sub> counts (σ = 0.5)
                 and compares behaviours with AIC = n<sub>d</sub>·ln(WLS/n<sub>d</sub>) + 2(n<sub>p</sub> + 1)
                 (Equation 4 of the reference). Parameters are estimated separately for each curve, at constant
                 antimicrobial concentration, so the model does not describe how they change with the dose."))
         )
    ),
    tables = div(
      class = "m4-tables",
      tags$script(HTML(M4_TOOLTIP_JS)),
      h4("Curves and parameters"),
      tags$small(class = "param-desc",
                 "One row per curve (up to 6). The behaviour column sets which parameters are used when simulating and which are estimated when fitting."),
      rHandsontableOutput(ns("params")),
      br(),
      h4("Experimental data"),
      tags$small(class = "param-desc",
                 HTML("Paste or type the counts. Each column corresponds to one row of the table above.
                       The first time must be 0 h. Empty cells are ignored, so curves may have different sampling times.")),
      rHandsontableOutput(ns("data"), height = "400px")
    )
  )
}

# ==============================================================================
# SERVER
# ==============================================================================
model4Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  
    rv <- reactiveValues(
      params = NULL, data = NULL,
      params_version = 0, data_version = 0,
      curves = NULL, sims = NULL, fits = NULL
    )
  
    load_preset <- function(name) {
      pr <- presetDataTimeKill(name)
      rv$params <- pr$params
      rv$data <- pr$data
      rv$fits <- NULL
      rv$params_version <- rv$params_version + 1
      rv$data_version <- rv$data_version + 1
    }
  
    run_simulation <- function(params, data) {
      prep <- m4_prepare(params, data, input$units, input$lod,
                         input$censor, need_data = FALSE)
      if (!prep$ok) {
        showNotification(prep$msg, type = "error")
        return(invisible(FALSE))
      }
      t_end <- max(c(unlist(lapply(prep$curves, function(cv) cv$t)), 24))
      rv$curves <- prep$curves
      rv$sims <- m4_simulate_curves(params, prep$curves, t_end)
      invisible(TRUE)
    }
  
    # --- Preset loading and reset ----------------------------------------------
    current_preset <- reactive({
      req(input$species, input$combo)
      m4_preset_name(input$species, input$combo)
    })
    # Changing the preset only loads its data: the plot is only computed on
    # Simulate/Fit/Reset, since some simulations take a while
    observeEvent(current_preset(), load_preset(current_preset()))
    # On start-up only, plot the preset. Lower priority so it runs after the
    # preset has been loaded above.
    observeEvent(current_preset(), run_simulation(rv$params, rv$data),
                 once = TRUE, priority = -1)
    observeEvent(input$reset, {
      load_preset(current_preset())
      run_simulation(rv$params, rv$data)
    })
  
    # --- Tables ----------------------------------------------------------------
    # They are only re-rendered when their version changes, so typing in a cell
    # never triggers a redraw of the table being edited.
    output$params <- renderRHandsontable({
      rv$params_version
      params <- isolate(rv$params)
      req(params)
      rhandsontable(params, maxRows = M4_MAX_CURVES,
                    colHeaders = m4_param_headers(),
                    rowHeaders = NULL, stretchH = "none", manualColumnResize = TRUE,
                    colWidths = c(m4_col_width("Curve", params$Curve, min_chars = 12),
                                  m4_col_width(TK_AUTO, TK_BEHAVIOUR_NAMES),
                                  rep(m4_col_width("Nm (log10)"), 5))) %>%
        hot_col(2, type = "dropdown", source = c(TK_AUTO, TK_BEHAVIOUR_NAMES),
                strict = TRUE, allowInvalid = FALSE) %>%
        hot_col(c(3, 4, 6), format = "0.00", halign = "htCenter") %>%
        hot_col(c(5, 7), format = "0.0000", halign = "htCenter") %>%
        hot_validate_numeric(col = c(3, 4, 6), min = 0) %>%
        hot_validate_numeric(col = c(5, 7), min = 0, max = 1) %>%
        hot_context_menu(allowColEdit = FALSE)
    })
  
    output$data <- renderRHandsontable({
      rv$data_version
      data <- isolate(rv$data)
      params <- isolate(rv$params)
      req(data, params)
      unit_lab <- if (isolate(input$units) == "cfu") "CFU/mL" else "log<sub>10</sub> CFU/mL"
      headers <- c("Time (h)", paste0(params$Curve, "<br><small>", unit_lab, "</small>"))
      unit_txt <- if (isolate(input$units) == "cfu") "CFU/mL" else "log10 CFU/mL"
      widths <- c(m4_col_width("Time (h)"),
                  vapply(params$Curve, m4_col_width, numeric(1), unit_txt))
      # Table only as wide as its columns (+ row numbers and vertical scrollbar),
      # so the scrollbar sits next to the last column
      rhandsontable(data, maxRows = 100, colHeaders = headers, stretchH = "none",
                    manualColumnResize = TRUE, colWidths = unname(widths),
                    rowHeaderWidth = M4_ROW_HEADER_WIDTH,
                    width = sum(widths) + M4_ROW_HEADER_WIDTH + M4_SCROLLBAR_WIDTH) %>%
        hot_col(seq_len(ncol(data))[-1], halign = "htCenter") %>%
        hot_validate_numeric(col = seq_len(ncol(data)), min = 0) %>%
        hot_context_menu(allowColEdit = FALSE)
    })
  
    # Store edits; redraw the data table only when curves are added, removed or renamed
    observeEvent(input$params, {
      new_params <- hot_to_r(input$params)
      old_params <- rv$params
      rv$params <- new_params
      structure_changed <- is.null(old_params) ||
        nrow(new_params) != nrow(old_params) ||
        !identical(as.character(new_params$Curve), as.character(old_params$Curve))
      if (structure_changed) {
        rv$data <- m4_sync_columns(rv$data, nrow(new_params))
        rv$data_version <- rv$data_version + 1
      }
    })
    observeEvent(input$data, {
      rv$data <- m4_sync_columns(hot_to_r(input$data), nrow(rv$params))
    })
    observeEvent(input$units, {
      rv$data_version <- rv$data_version + 1
    }, ignoreInit = TRUE)
  
    # --- Simulate --------------------------------------------------------------
    observeEvent(input$simulate, {
      run_simulation(rv$params, rv$data)
      updateTabsetPanel(session, "tabs", selected = "Plot")
    })
  
    # --- Fit -------------------------------------------------------------------
    observeEvent(input$fit, {
      params <- rv$params
      prep <- m4_prepare(params, rv$data, input$units, input$lod,
                         input$censor, need_data = TRUE)
      if (!prep$ok) {
        showNotification(prep$msg, type = "error")
        return()
      }
    
      fits <- vector("list", length(prep$curves))
      withProgress(message = "Fitting curves", value = 0, {
        for (i in seq_along(prep$curves)) {
          cv <- prep$curves[[i]]
          incProgress(1 / length(prep$curves), detail = cv$name)
          behaviour <- if (input$fitMode == "aic") TK_AUTO else params$Behaviour[i]
          fits[[i]] <- tk_fit_curve(cv$t, cv$N, cv$censored, behaviour)
        }
      })
    
      skipped <- vapply(fits, is.null, logical(1))
      if (any(skipped)) {
        showNotification(
          paste0("Not enough points to fit the chosen behaviour for: ",
                 paste(params$Curve[skipped], collapse = ", "),
                 ". Add data or choose a behaviour with fewer parameters."),
          type = "warning", duration = 10)
      }
    
      # Write the estimates into the parameter table
      for (i in which(!skipped)) {
        params[i, TK_PARAM_NAMES] <- as.list(fits[[i]]$best$params[TK_PARAM_NAMES])
        params$Behaviour[i] <- fits[[i]]$best$behaviour
      }
      rv$params <- params
      rv$params_version <- rv$params_version + 1
      rv$fits <- fits
    
      t_end <- max(c(unlist(lapply(prep$curves, function(cv) cv$t)), 24))
      rv$curves <- prep$curves
      rv$sims <- m4_simulate_curves(params, prep$curves, t_end)
      updateTabsetPanel(session, "tabs", selected = "Fit results")
    })
  
    # --- Outputs ---------------------------------------------------------------
    output$plot <- renderPlotly({
      req(rv$curves)
      m4_plot(rv$curves, rv$sims, isolate(input$lod))
    })
  
    fit_summary <- reactive({
      req(rv$fits)
      rows <- lapply(seq_along(rv$fits), function(i) {
        f <- rv$fits[[i]]
        if (is.null(f)) return(NULL)
        b <- f$best
        fp <- m4_format_params(b$behaviour, b$params)
        data.frame(Curve = rv$params$Curve[i], Behaviour = b$behaviour,
                   `μ (h⁻¹)` = fp["mu"], `Nm (log10)` = fp["Nm"], a0 = fp["a0"],
                   `K (h⁻ᵐ)` = fp["K"], m = fp["m"],
                   np = b$np, nd = b$nd, AIC = round(b$aic, 2),
                   check.names = FALSE, row.names = NULL)
      })
      do.call(rbind, rows)
    })
  
    output$fitEmpty <- renderUI({
      if (is.null(rv$fits)) {
        p("Paste the experimental counts in the data table and press ", tags$b("Fit parameters"), ".")
      }
    })
  
    output$fitTable <- renderTable({
      fit_summary()
    }, striped = TRUE, hover = TRUE, align = "l")
  
    output$compareUI <- renderUI({
      req(rv$fits)
      fitted <- which(!vapply(rv$fits, is.null, logical(1)))
      tagList(
        hr(),
        h5("Behaviours compared for one curve", style = "font-weight:bold;"),
        selectInput(ns("compareCurve"), NULL, choices = setNames(fitted, rv$params$Curve[fitted]))
      )
    })
  
    output$compareTable <- renderTable({
      req(rv$fits, input$compareCurve)
      f <- rv$fits[[as.integer(input$compareCurve)]]
      req(f)
      r <- f$ranking
      r$np <- as.integer(r$np)
      r$WLS <- signif(r$WLS, 3)
      r$AIC <- round(r$AIC, 2)
      r$dAIC <- round(r$dAIC, 2)
      names(r) <- c("Behaviour", "np", "WLS", "AIC", "ΔAIC")
      r
    }, striped = TRUE, hover = TRUE, align = "l")
  
    output$downloadUI <- renderUI({
      req(rv$fits)
      tagList(br(), downloadButton(ns("download"), "Download results (CSV)", class = "btn-info"))
    })
  
    output$download <- downloadHandler(
      filename = function() paste0("microracle_timekill_fit_", Sys.Date(), ".csv"),
      content = function(file) {
        rows <- list()
        for (i in seq_along(rv$fits)) {
          f <- rv$fits[[i]]
          if (is.null(f)) next
          for (b in f$ranking$Behaviour) {
            fb <- f$all[[b]]
            rows[[length(rows) + 1]] <- data.frame(
              curve = rv$params$Curve[i], behaviour = b,
              selected = identical(b, f$best$behaviour),
              t(fb$params[TK_PARAM_NAMES]), np = fb$np, nd = fb$nd,
              wls = fb$wls, aic = fb$aic, row.names = NULL)
          }
        }
        write.csv(do.call(rbind, rows), file, row.names = FALSE)
      }
    )
  
    output$behaviourTable <- renderTable({
      rows <- lapply(TK_BEHAVIOUR_NAMES, function(b) {
        beh <- TK_BEHAVIOURS[[b]]
        cell <- function(pn) {
          if (pn %in% beh$free) return("estimated")
          v <- beh$fixed[pn]
          if (pn %in% c("Nm", "a0") && beh$fixed["mu"] %in% 0) return("–")
          if (pn == "m" && identical(unname(beh$fixed["K"]), 0)) return("–")
          format(v)
        }
        data.frame(Behaviour = b, `μ` = cell("mu"), Nm = cell("Nm"), a0 = cell("a0"),
                   K = cell("K"), m = cell("m"), np = length(beh$free),
                   check.names = FALSE)
      })
      do.call(rbind, rows)
    }, striped = TRUE, align = "l")
  })
}
