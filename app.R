####################
###   LIBRARYS   ###
####################

library(shiny)
library(plotly)
library(rhandsontable)
library(shinythemes)
library(shinyBS)
library(shinyjs)
library(shinyalert)


###################################
### AUXILIAR FUNCTIONS AND CODE ###
###################################

source("dataValidator.R")
source("carFun.R")
source("ddacFun.R")
source("model1Module.R")
source("model2Module.R")
source("presetDataCar.R")
source("presetDataDDAC.R")


######################
### USER INTERFACE ###
######################
ui <-
  navbarPage(
    title = div(
      "",
      img(
        src = "logo2.png",
        id = "logo",
        height = "50px",
        style = "position: relative; margin:-15px 0px; padding: 3% 0%;
                  display:right-align;"
      )
    ),
    id = "navbar",
    theme = shinytheme("yeti"),
    collapsible = TRUE,
    
    # HEADER: Metadatos y lógica de iconos de RRSS
    header = tagList(
      tags$head(
        tags$link(rel = "shortcut icon", href = "/icon.ico"),
        tags$style(
          HTML(
            ".shiny-notification { position:fixed; top: 50%; left: 0%; color:black; }",
            ".shiny-notification-warning { color:#8f6830; background-color: #fff3cd; }",
            ".shiny-notification-error { color:#751f26; background-color: #f8d7da; }",
            
            # Estilo de la Navbar
            ".navbar-nav { float: none !important; width: 100%; }",
            ".nav-icon-link { float: right !important; margin-top: 10px; margin-left: 15px; }",
            ".nav-icon-link a { padding: 10px 15px !important; background: transparent !important; }",
            ".navbar-default { background-color:black; border: none; }",
            ".navbar-nav > li > a { color:red; background-color:black; }",
            
            # Hover y activo
            ".navbar-nav > li.active > a,
             .navbar-nav > li:hover > a {
               border-bottom: 8px solid #0fc1a3 !important;
               box-sizing: border-box;
            }",
            
            # Estilo de la tabla
            ".about-table { margin-top: 20px; background-color: #222; color: white; }",
            ".about-table th { background-color: #333; color: #0fc1a3; }",
            ".about-table td { border-top: 1px solid #444 !important; }",
            
            ".svgIcon { filter: brightness(0) invert(1); height: 25px; width: 25px; }"
          )
        ),
        # Script para mover iconos al final de la navbar
        tags$script(HTML("
          $(document).ready(function() {
            $('.navbar-nav').append($('#nav-icons').contents());
          });
        "))
      ),
      shinyjs::useShinyjs(),
      
      # Links directos RRSS
      tags$div(id = "nav-icons", style = "display:none;",
               tags$li(class = "nav-icon-link",
                       tags$a(href = "https://github.com/apedreira/microracle/", target = "_blank",
                              tags$img(class = "svgIcon", src = "github.svg")
                       )
               ),
               tags$li(class = "nav-icon-link",
                       tags$a(href = "https://twitter.com/microracle", target = "_blank",
                              tags$img(class = "svgIcon", src = "x-twitter.svg")
                       )
               )
      )
    ),
    
    # 1. Pestaña ABOUT US
    tabPanel(
      "Home",
      mainPanel(
        width = 12,
        column(width = 2, ""),
        column(
          width = 8,
          h1("Microracle"),
          h4(tags$em("Interactive simulation and analysis of antimicrobial treatments on bacterial dynamics")),
          br(),
          p("Microracle includes the following models and tools:"),
          
          # TABLA DE MODELOS
          tags$table(
            class = "table about-table",
            tags$thead(
              tags$tr(
                tags$th("Model"),
                tags$th("Description"),
                tags$th("Authors"),
                tags$th("Ref"),
                tags$th("Action")
              )
            ),
            #
            tags$tbody(
              tags$tr(
                tags$td(tags$strong("Carvacrol at sub-MIC concentrations")),
                tags$td("Model Simulation & Data for E. coli and B. cereus inactivation at sub-MIC carvacrol concentrations."),
                tags$td("Pedreira, Martínez-López, Vázquez, García (IIM-CSIC)"),
                tags$td(tags$a(href = "https://doi.org/10.1016/j.jfoodeng.2023.111734", target = "_blank", "Read Paper")),
                tags$td(
                  actionButton("go_mod1", "Go to Model", class = "btn-info btn-sm",
                               onclick = "eval(\"$('#navbar a[data-value=\\\"Model 1\\\"]').tab('show')\")")
                )
              ),
              #
              tags$tr(
                tags$td(tags$strong("DDAC at sub-MIC concentrations")),
                tags$td("Model Simulation & Data for DDAC inactivation of E. coli and B. cereus at sub-MIC concentrations."),
                tags$td("Pedreira, Vázquez, García (IIM-CSIC)"),
                tags$td(tags$a(href = "https://doi.org/10.3389/fmicb.2022.758237", target = "_blank", "Read Paper")),
                tags$td(
                  actionButton("go_mod2", "Go to Model", class = "btn-info btn-sm",
                               onclick = "eval(\"$('#navbar a[data-value=\\\"Model 2\\\"]').tab('show')\")")
                )
              ),
              #
              tags$tr(
                tags$td(tags$strong("Selection & Extinction planes")),
                tags$td("Conditions for selection and extinction between two competing strains under cyclic treatment depending on growth-kill trade-off"),
                tags$td("Martínez-López, Pedreira, and García (IIM-CSIC) in collaboration with Nordholt and Schreiber (BAM)"),
                tags$td(tags$a(href = "https://arxiv.org/abs/2602.14645", target = "_blank", "arxiv with theory")),
                tags$td(
                  actionButton("go_mod3", "Go to Analysis", class = "btn-info btn-sm",
                               onclick = "eval(\"$('#navbar a[data-value=\\\"Model 3\\\"]').tab('show')\")")
                )
              )
            )
          ),
          
          hr(),
          h2("Authoring"),
          
          p(
            "Microracle has been developed by researchers from the Biosystems and Bioprocess Engineering Group (Bio2Eng) and the Recycling and Valorization of Waste Materials (REVAL) groups at the IIM-CSIC (Vigo, Spain)."
          ),
          
          # Cleaner list with bold names
          tags$div(
            tags$ul(
              tags$li(tags$strong("Adrián Pedreira:"), " Web development, experimentation, and data collection."),
              tags$li(tags$strong("Nerea Martínez:"), " Development of mathematical models."),
              tags$li(tags$strong("Xosé A. Vázquez:"), " Experimental design."),
              tags$li(tags$strong("Míriam R. García:"), " Development of mathematical models.")
            )
          ),
          h3("Collaborations"),
          
          p("Some models have been developed in collaboration with external researchers:"),
          
          # Readable table with corrected spelling
          tags$table(
            class = "table table-striped table-bordered",
            tags$thead(
              tags$tr(
                tags$th("Model"),
                tags$th("Institution"),
                tags$th("People")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td("Selection and Extinction Planes"),
                tags$td("Federal Institute for Materials Research and Testing (BAM), Berlin, Germany"),
                tags$td("Niclas Nordholt and Frank Schreiber")
              )
            )
          ),
          
          h2("Contact details"),
          
          p(
            "For questions related to the mathematical models, please contact ",
            tags$strong("Míriam R. García"),
            " (miriamr@iim.csic.es)."
          ),
          p(
            "Microracle is in early development. If you encounter any issues, please open a new issue in the ",
            tags$a(href = "https://github.com/apedreira/microracle", target = "_blank", "GitHub repository"),
            " or write to ",
            tags$strong("Adrián Pedreira"),
            " (apedreira@iim.csic.es)."
          ),
          
          h2("Requirements"),
          HTML("<p>Microracle can be freely accessed from <a href='https://microracle.shinyapps.io/Microracle/'>microracle.shinyapps.io/Microracle</a>.</p>")
        ),
        column(width = 2, "")
      )
    ),
    
    # 2. Menú MODELS
    navbarMenu(
      "MODELS",
      model1UI(),
      model2UI()
    )
  )


####################
### SERVER LOGIC ###
####################

server <- function(input, output, session) {
  model1Server(input, output, session)
  model2Server(input, output, session)
}

shinyApp(ui, server)