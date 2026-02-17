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
    
    # HEADER: Aquí gestionamos los metadatos y los iconos directos
    header = tagList(
      tags$head(
        tags$link(rel = "shortcut icon", href = "/icon.ico"),
        tags$style(
          HTML(
            ".shiny-notification { position:fixed; top: 50%; left: 0%; color:black; }",
            ".shiny-notification-warning { color:#8f6830; background-color: #fff3cd; }",
            ".shiny-notification-error { color:#751f26; background-color: #f8d7da; }",
            
            # Ajuste de la barra para que acepte floats
            ".navbar-nav { float: none !important; width: 100%; }",
            
            # Estilo para los iconos inyectados a la derecha
            ".nav-icon-link { float: right !important; margin-top: 10px; margin-left: 15px; }",
            ".nav-icon-link a { padding: 10px 15px !important; background: transparent !important; }",
            
            ".navbar-default { background-color:black; border: none; }",
            ".navbar-nav > li > a { color:red; background-color:black; }",
            
            # Hover y activo para About y Models (hijos 1 y 2)
            ".navbar-nav > li.active > a,
             .navbar-nav > li:hover > a {
               border-bottom: 8px solid #0fc1a3 !important;
               box-sizing: border-box;
            }",
            
            ".svgIcon { filter: brightness(0) invert(1); height: 25px; width: 25px; }"
          )
        ),
        # Script para mover los iconos al final de la navbar de forma limpia
        tags$script(HTML("
          $(document).ready(function() {
            $('.navbar-nav').append($('#nav-icons').contents());
          });
        "))
      ),
      shinyjs::useShinyjs(),
      
      # Contenedor oculto con los links directos que moveremos con JS
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
    
    # 1. About us (Pestaña simple a la izquierda)
    tabPanel(
      "About us",
      mainPanel(
        width = 12,
        column(width = 3, ""),
        column(
          width = 6,
          h1("Microracle"),
          h2("Authoring"),
          p("Microracle has been developed by researchers of the Biosystems and Bioprocess Engineering Group (Bio2Eng) and Recycling and Valorization of Waste Materials (REVAL) groups from the IIM-CSIC (Vigo, Spain):"),
          tags$div(tags$ul(
            tags$li(tags$span("Adrián Pedreira: Web developer, experimentation and data collection.")),
            tags$li(tags$span("Nerea Martínez: Development of mathematical models.")),
            tags$li(tags$span("Xosé A. Vázquez: Experimental design")),
            tags$li(tags$span("Míriam R. García: Development of mathematical models."))
          )),
          HTML("<p>For any question related to the mathematical models, please contact with Míriam R. García (miriamr@iim.csic.es).</p>")
        )
      ),
      column(width = 3, "")
    ),
    
    # 2. MODELS (Menú desplegable a la izquierda)
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