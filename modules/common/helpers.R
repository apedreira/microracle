################################################################################
# Helpers shared by the model modules
################################################################################

# Loading animation of the outputs: a <canvas> animated by www/loading.js
# (cells dividing + "Working..."), shown by shinycssloaders while the
# output is recalculating. shinycssloaders 1.1.0 has no 'custom.html', so the
# canvas goes in the 'caption' and the spinner image is a hidden 1px GIF.
loading_spinner <- function(ui) {
  withSpinner(
    ui,
    image = "data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7",
    image.width = 0,
    image.height = 0,
    caption = tags$canvas(class = "bacilli-loader", width = 680, height = 520)
  )
}

# Single palette for every plot (one colour per curve)
PLOT_COLORS <- c("#404040", "#619cff", "#e69f00", "#cc79a7", "#78ac44", "#d55e00")
plot_color <- function(i) PLOT_COLORS[(i - 1) %% length(PLOT_COLORS) + 1]

# Pad a vector with NA up to n elements (empty rows of the editable tables)
pad <- function(x, n) c(x, rep(NA, max(0, n - length(x))))

# Common axes of the growth/inactivation plots: time vs log10 CFU/mL.
# Extra arguments (shapes, annotations...) are passed on to layout().
plot_layout_log10 <- function(p, ...) {
  layout(
    p,
    yaxis = list(showexponent = "all", exponentformat = "E", title = "Log<sub>10</sub> CFU/mL"),
    xaxis = list(title = "Time (h)"),
    ...
  )
}

################################################################################
# Common page template of the models
#   1. Sidebar: preset selector, options and, at the bottom, the actions
#   2. Main panel: Plot · Results · Model tabs (those that apply) + Reference
#   3. Data tables below
# 'model' is the entry of the model in MODELS (models.R).
################################################################################

model_page <- function(model, sidebar, tabs, tables = NULL, tabs_id = NULL) {
  tabPanel(
    title = model$title,
    value = model$value,
    withMathJax(),
    sidebarLayout(
      sidebarPanel(sidebar),
      mainPanel(
        do.call(tabsetPanel, c(list(id = tabs_id), tabs, list(reference_tab(model))))
      )
    ),
    if (!is.null(tables)) fluidRow(style = "padding:16px", column(12, tables))
  )
}

# Block of action buttons, always at the bottom of the sidebar
model_actions <- function(...) {
  div(class = "model-actions", hr(), div(align = "center", ...))
}

# Reference tab, built from the citation in the model registry
reference_tab <- function(model) {
  tabPanel("Reference", br(), p(HTML(model$citation)))
}
