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
            
            # Mejora visual de las tablas
            ".about-table { margin-top: 20px; background-color: #1a1a1a; color: white; border-radius: 8px; overflow: hidden; }",
            ".about-table th { background-color: #333; color: #0fc1a3; padding: 12px !important; text-transform: uppercase; font-size: 0.85em; }",
            ".about-table td { border-top: 1px solid #444 !important; padding: 12px !important; vertical-align: middle !important; }",
            ".table-hover tbody tr:hover { background-color: #252525 !important; }",
            
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
    
    # 1. Pestaña HOME
    tabPanel(
      "Home",
      mainPanel(
        width = 12,
        column(width = 2, ""),
        column(
          width = 8,
          h1(class = "text-center","Microracle"),
          
          h4(
            class = "text-center",
            "Understanding the effect of antimicrobials on bacterial dynamics "
          ),
          h4(
            class = "text-center",
            "through the integration of theory and data"
          ),
          

          # TABLA DE MODELOS
          tags$table(
            class = "table about-table table-hover",
            tags$thead(
              tags$tr(
                tags$th("Model / Tool"),
                tags$th("Description"),
                tags$th("Authors"),
                tags$th("Reference"),
                tags$th("Access")
              )
            ),
            tags$tbody(
              # Carvacrol
              tags$tr(
                tags$td(tags$strong("Carvacrol sub-MIC")),
                tags$td(HTML("Data & Model to predict Carvacrol inactivation of <em>E. coli</em> and <em>B. cereus</em> at sub-MIC levels.")),
                tags$td("Pedreira et al. (IIM-CSIC)"),
                tags$td(tags$a(href = "https://doi.org/10.1016/j.jfoodeng.2023.111734", target = "_blank", "Journal of Food Engineering")),
                tags$td(
                  actionButton("go_mod1", "Open Model", class = "btn-outline-info btn-sm",
                               onclick = "eval(\"$('#navbar a[data-value=\\\"Model 1\\\"]').tab('show')\")")
                )
              ),
              # DDAC
              tags$tr(
                tags$td(tags$strong("DDAC sub-MIC")),
                tags$td(HTML("Data & Model to predict DDAC inactivation of <em>E. coli</em> and <em>B. cereus</em> at sub-MIC levels.")),
                tags$td("Pedreira et al. (IIM-CSIC)"),
                tags$td(tags$a(href = "https://doi.org/10.3389/fmicb.2022.758237", target = "_blank", "Frontiers in Microbiology")),
                tags$td(
                  actionButton("go_mod2", "Open Model", class = "btn-outline-info btn-sm",
                               onclick = "eval(\"$('#navbar a[data-value=\\\"Model 2\\\"]').tab('show')\")")
                )
              ),
              # Selection Planes
              tags$tr(
                tags$td(tags$strong("Selection & Extinction")),
                tags$td("Analysis of competitive strain dynamics under cyclic treatments based on growth-kill trade-offs."),
                tags$td("Martínez-López et al. (IIM-CSIC / BAM)"),
                tags$td(tags$a(href = "https://arxiv.org/abs/2602.14645", target = "_blank", "arXiv (Preprint with Theory)")),
                tags$td(
                  actionButton("go_mod3", "Open Analysis", class = "btn-outline-info btn-sm",
                               onclick = "eval(\"$('#navbar a[data-value=\\\"Model 3\\\"]').tab('show')\")")
                )
              )
            )
          ),
          
          hr(),
          h2("Authoring"),
          p("Microracle is a collaborative effort developed by the Biosystems and Bioprocess Engineering Group (Bio2Eng) and the Recycling and Valorization of Waste Materials (REVAL) groups at the IIM-CSIC (Vigo, Spain)."),
          
          tags$div(
            tags$ul(
              tags$li(tags$strong("Adrián Pedreira:"), " Web development, experimental assays, and data collection."),
              tags$li(tags$strong("Nerea Martínez:"), " Mathematical modeling and theoretical framework."),
              tags$li(tags$strong("Xosé A. Vázquez:"), " Experimental design."),
              tags$li(tags$strong("Míriam R. García:"), " Web and Theory Supervision.")
            )
          ),
          
          h3("Institutional Collaborations"),
          p("We work with experts to expand the modeling capabilities of Microracle:"),
          
          tags$table(
            class = "table about-table",
            tags$thead(
              tags$tr(
                tags$th("Research Project"),
                tags$th("Institution"),
                tags$th("Collaborators")
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
          
          h2("Contact & Support"),
          p(HTML("For scientific inquiries regarding web content, please contact <strong>Míriam R. García</strong> (miriamr@iim.csic.es).")),
          p(HTML("Microracle is in active development. To report bugs or technical issues, please visit our 
                 <a href='https://github.com/apedreira/microracle' target='_blank'>GitHub repository</a> or contact 
                 <strong>Adrián Pedreira</strong> (apedreira@iim.csic.es).")),
          
          h2("Access"),
          HTML("<p>Microracle is open-source and can be accessed at: <a href='https://microracle.shinyapps.io/Microracle/'>microracle.shinyapps.io/Microracle</a>.</p>")
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