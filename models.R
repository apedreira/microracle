################################################################################
# Model registry
#
# Single description of every model. app.R builds from it the table of the
# Home page, the MODELS menu, the "Try it!" navigation and the Reference tab
# of each model, so a new model only has to be added here.
#
#   id          module id (namespace of its inputs and outputs)
#   value       tab value, used in the URL (?tab=<value>)
#   title       title of the tab in the MODELS menu
#   name        short name in the Home table
#   description description in the Home table (HTML)
#   authors     authors and institutions in the Home table
#   link, link_label  publication linked from the Home table
#   citation    full reference shown in the Reference tab (HTML)
#   ui, server  module functions
################################################################################

MODELS <- list(
  list(
    id = "model1",
    value = "Carvacrol_SubMIC",
    title = "Carvacrol at sub-MIC concentrations",
    name = "Carvacrol sub-MIC",
    description = "Data & Model to predict Carvacrol inactivation of <em>E. coli</em> and <em>B. cereus</em> at sub-MIC levels.",
    authors = "Pedreira et al. (IIM-CSIC)",
    link = "https://doi.org/10.1016/j.jfoodeng.2023.111734",
    link_label = "Journal of Food Engineering",
    citation = "Pedreira, A., Martínez-López, N., Vázquez, J. A., & García, M. R. (2024). Modelling the antimicrobial effect of food preservatives in bacteria: Application to <i>Escherichia coli</i> and <i>Bacillus cereus</i> inhibition with carvacrol.
      <i>Journal of Food Engineering</i>, 361, 111734. doi: <a href='https://doi.org/10.1016/j.jfoodeng.2023.111734' target='_blank'>doi.org/10.1016/j.jfoodeng.2023.111734</a>",
    ui = model1UI,
    server = model1Server
  ),
  list(
    id = "model2",
    value = "DDAC_SubMIC",
    title = "DDAC at sub-MIC concentrations",
    name = "DDAC sub-MIC",
    description = "Data & Model to predict DDAC inactivation of <em>E. coli</em> and <em>B. cereus</em> at sub-MIC levels.",
    authors = "Pedreira et al. (IIM-CSIC)",
    link = "https://doi.org/10.3389/fmicb.2022.758237",
    link_label = "Frontiers in Microbiology",
    citation = "Pedreira, A., Vázquez, J. A., & García, M. R. (2022). Kinetics of Bacterial Adaptation, Growth, and Death at Didecyldimethylammonium Chloride sub-MIC Concentrations.
      <i>Frontiers in Microbiology</i>, 13, 758237. doi: <a href='https://doi.org/10.3389/fmicb.2022.758237' target='_blank'>doi.org/10.3389/fmicb.2022.758237</a>",
    ui = model2UI,
    server = model2Server
  ),
  list(
    id = "model4",
    value = "QACs–EOCs_synergy",
    title = "QACs–EOCs synergy",
    name = "QACs–EOCs synergy",
    description = "Data & Model to fit growth, lag and inactivation in time-kill curves of <em>E. coli</em> and <em>B. cereus</em> exposed to Quaternary Ammonium Compounds (QACs), Essential Oil Constituents and their combinations.",
    authors = "Pedreira et al. (IIM-CSIC / LEPABE)",
    link = "https://doi.org/10.3390/foods13121831",
    link_label = "Foods",
    citation = "Pedreira, A., Fernandes, S., Simões, M., García, M. R., & Vázquez, J. A. (2024).
      Synergistic bactericidal effects of quaternary ammonium compounds with essential oil constituents.
      <i>Foods</i>, 13(12), 1831. doi: <a href='https://doi.org/10.3390/foods13121831' target='_blank'>doi.org/10.3390/foods13121831</a>",
    ui = model4UI,
    server = model4Server
  ),
  list(
    id = "model3",
    value = "PEplane",
    title = "Selection & Extinction Planes",
    name = "Selection & Extinction",
    description = "Analysis of competitive strain dynamics under cyclic treatments based on growth-kill trade-offs.",
    authors = "Martínez-López et al. (IIM-CSIC / BAM)",
    link = "https://arxiv.org/abs/2602.14645",
    link_label = "arXiv (Preprint with Theory)",
    citation = "Martínez-López, N., Nordholt, N., Schreiber, F. & García, M. R. (2026). Conditions for Bacterial Selection and Extinction Driven by Growth-Kill Trade-Off in Cyclic Antimicrobial Treatments.
      <i>arXiv</i>. doi: <a href='https://doi.org/10.48550/arXiv.2602.14645' target='_blank'>https://doi.org/10.48550/arXiv.2602.14645</a>",
    ui = model3UI,
    server = model3Server
  )
)

# Row of the Home table for a model
model_home_row <- function(model) {
  tags$tr(
    tags$td(tags$strong(model$name)),
    tags$td(HTML(model$description)),
    tags$td(model$authors, tags$br(), tags$a(href = model$link, target = "_blank", model$link_label)),
    tags$td(actionButton(paste0("go_", model$id), "Try it!", class = "btn-outline-info btn-sm"))
  )
}
