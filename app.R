####################
###  LIBRARIES   ###
####################

# All the libraries used by the app and its modules are loaded here, once.
library(shiny)
library(plotly)
library(rhandsontable)
library(shinythemes)
library(shinyBS)
library(shinyjs)
library(shinyalert)
library(shinycssloaders)
library(deSolve)
library(zip)

####################################
### AUXILIARY FUNCTIONS AND CODE ###
####################################

# Each model lives in its own folder under modules/; common/ holds the code
# shared by several models
source("modules/common/helpers.R")
source("modules/common/dataValidator.R")

# Model 1: Carvacrol sub-MIC
source("modules/model1_carvacrol/carFun.R")
source("modules/model1_carvacrol/presetDataCar.R")
source("modules/model1_carvacrol/model1Module.R")

# Model 2: DDAC sub-MIC
source("modules/model2_ddac/ddacFun.R")
source("modules/model2_ddac/presetDataDDAC.R")
source("modules/model2_ddac/model2Module.R")

# Model 3: Selection & Extinction planes
# (SEplanes_Offline.R is not sourced: it is the standalone script of the download)
source("modules/model3_se_planes/Plot_SE_Plane_k.R")
source("modules/model3_se_planes/Plot_SE_Plane_SF.R")
source("modules/model3_se_planes/presetDataModel3.R")
source("modules/model3_se_planes/dataValidatorModel3.R")
source("modules/model3_se_planes/downloadDataModel3.R")
source("modules/model3_se_planes/model3Module.R")

# Model 4: QACs–EOCs synergy (time-kill curves)
source("modules/model4_timekill/timeKillFun.R")
source("modules/model4_timekill/presetDataTimeKill.R")
source("modules/model4_timekill/model4Module.R")

# Model registry (after the modules, it references their ui/server functions)
source("models.R")

# Name of a person linked to their ORCID profile
orcid_link <- function(name, orcid) {
  tags$a(href = paste0("https://orcid.org/", orcid), target = "_blank", name)
}


######################
### USER INTERFACE ###
######################

ui <- navbarPage(
  title = div(
    "",
    img(
      src = "logo2.png",
      id = "logo",
      height = "50px",
      style = "position: relative; margin:-15px 0px; padding: 3% 0%;"
    )
  ),
  id = "navbar",
  theme = shinytheme("yeti"),
  collapsible = TRUE,
  
  # IMPORTANT: Non-tab elements (CSS, JS, useShinyjs) must be placed in the 'header'.
  # This prevents the creation of empty tabs (dead <li> elements) in the navbar.
  header = tagList(
    useShinyjs(),
    tags$head(
      tags$link(rel = "shortcut icon", href = "/icon.ico"),
      
      # 1. JAVASCRIPT: Inject Social Media icons at the end of the navigation bar
      #    Added once when the page loads (not on 'shiny:connected', which fires
      #    again on every reconnection and duplicated the icon).
      tags$script(HTML("
        $(function() {
            var nav = $('.navbar-nav').first();
            if (nav.find('.nav-item-icon').length) return;
            
            // Raw HTML for social media icons
            var ghIcon = '<li class=\"nav-item-icon\"><a href=\"https://github.com/apedreira/microracle/\" target=\"_blank\"><img class=\"svgIcon\" src=\"github.svg\"></a></li>';
            
            // Append icons to the navbar
            nav.append(ghIcon); 
        });
      ")),
      
      # 2. CSS STYLES (all the styles of the app, modules included)
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),

      # 3. Loading animation of the outputs (see loading_spinner() in helpers.R)
      tags$script(src = "loading.js")
    )
  ),
  
  # --- TAB CONTENT ---
  
  # 1. HOME Tab
  tabPanel(
    "Home",
    mainPanel(
      width = 12,
      column(width = 2, ""),
      column(
        width = 8,
        h1(class = "text-center", img(src = "logo_black.png", alt = "Microracle", class = "home-logo")),
        
        h4(
          class = "text-center",
          "Understanding the effect of antimicrobials on bacterial dynamics "
        ),
        h4(
          class = "text-center",
          "through the integration of theory and data"
        ),
        
        # PROJECTS SUMMARY TABLE
        tags$table(
          class = "table about-table table-hover",
          tags$thead(
            tags$tr(
              tags$th("Project"),
              tags$th("Description"),
              tags$th("Reference"),
              tags$th("Access")
            )
          ),
          # One row per model of the registry (models.R)
          tags$tbody(lapply(MODELS, model_home_row))
        ),
        
        hr(),
        h2("Authoring"),
        p(
          "Microracle originated as a joint initiative of the ",
          tags$a(href = "https://bio2eng.csic.es", target = "_blank", "Bio2Eng"),
          " and ",
          tags$a(href = "https://reval.csic.es/", target = "_blank", "REVAL"),
          " research groups at the Spanish National Research Council (",
          tags$a(href = "https://www.csic.es/en/csic", target = "_blank", "IIM-CSIC"),
          "):"
        ),
        
        tags$div(
          tags$ul(
            tags$li(tags$strong(orcid_link("Adrián Pedreira", "0000-0002-7843-4111"), ":"), " Web development, experimental assays, and data collection."),
            tags$li(tags$strong(orcid_link("Nerea Martínez-López", "0000-0002-3976-9883"), ":"), " Mathematical modeling and theoretical framework."),
            tags$li(tags$strong(orcid_link("Xosé A. Vázquez", "0000-0002-1122-4726"), ":"), " Experimental design."),
            tags$li(tags$strong(orcid_link("Míriam R. García", "0000-0001-9538-6388"), ":"), " Web and Theory Supervision.")
          )
        ),
        
        p("Microracle continues to grow through collaborations with international experts:"),
        
        tags$table(
          class = "table about-table",
          tags$thead(
            tags$tr(
              tags$th("Project"),
              tags$th("Institution"),
              tags$th("Collaborators")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td("Selection and Extinction Planes"),
              tags$td("Federal Institute for Materials Research and Testing (BAM), Berlin, Germany"),
              tags$td(orcid_link("Niclas Nordholt", "0000-0001-5788-0801"), " and ",
                      orcid_link("Frank Schreiber", "0000-0003-1957-6328"))
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td("QACs–EOCs synergy"),
              tags$td("Laboratory for Process Engineering, Environment, Biotechnology and Energy (LEPABE)–Universidade de Porto, Porto, Portugal"),
              tags$td(orcid_link("Susana Fernandes", "0000-0002-9132-5382"), " and ",
                      orcid_link("Manuel Simões", "0000-0002-3355-4398"))
            )
          )
        ),
        
        h2("Contact & Support"),
        p(HTML("For scientific inquiries regarding web content, please contact <strong>Míriam R. García</strong> (miriamr@iim.csic.es) or <strong>Adrián Pedreira</strong> (apedreira@iim.csic.es).")),
        p(HTML("Microracle is in active development. To report bugs or technical issues, please visit our 
                  <a href='https://github.com/apedreira/microracle' target='_blank'>GitHub repository</a> or contact 
                  <strong>Adrián Pedreira</strong> (apedreira@iim.csic.es)."))
        
      ),
      column(width = 2, "")
    )
  ),
  
  do.call(navbarMenu, c(list("MODELS"), lapply(MODELS, function(m) m$ui(m$id, m))))
)


####################
### SERVER LOGIC ###
####################

server <- function(input, output, session) {
  # Initialize module servers
  for (m in MODELS) m$server(m$id)
  
  # --- NAVIGATION FROM HOME BUTTONS ---
  lapply(MODELS, function(m) {
    observeEvent(input[[paste0("go_", m$id)]], {
      updateNavbarPage(session, "navbar", selected = m$value)
    })
  })
  
  # --- URL <-> TAB SYNCHRONISATION ---
  # Tab in the URL (?tab=<value>); no tab means Home
  url_tab <- function() {
    query <- parseQueryString(session$clientData$url_search)
    if (is.null(query$tab)) "Home" else query$tab
  }
  
  # Open the tab of the URL: on start-up and when using the browser's back/forward buttons
  observe({
    updateNavbarPage(session, "navbar", selected = url_tab())
  })
  
  # Write the open tab in the URL, so the link can be shared and "back" works.
  # Only when it differs, to avoid adding history entries when the change came from the URL.
  observeEvent(input$navbar, {
    if (!identical(isolate(url_tab()), input$navbar)) {
      updateQueryString(paste0("?tab=", input$navbar), mode = "push")
    }
  }, ignoreInit = TRUE)
}

# Run the Application
shinyApp(ui, server)
