####################
###  LIBRARIES   ###
####################

library(shiny)
library(plotly)
library(rhandsontable)
library(shinythemes)
library(shinyBS)
library(shinyjs)
library(shinyalert)

####################################
### AUXILIARY FUNCTIONS AND CODE ###
####################################

source("dataValidator.R")
source("dataValidatorModel3.R")
source("carFun.R")
source("ddacFun.R")
source("model1Module.R")
source("model2Module.R")
source("model3Module.R")
source("presetDataCar.R")
source("presetDataDDAC.R")
source("presetDataModel3.R")
source("Plot_SE_Plane_k.R")
source("Plot_SE_Plane_SF.R")


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
      tags$script(HTML("
        $(document).on('shiny:connected', function() {
            var nav = $('.navbar-nav');
            
            // Raw HTML for social media icons
            var xIcon = '<li class=\"nav-item-icon\"><a href=\"https://twitter.com/microracle\" target=\"_blank\"><img class=\"svgIcon\" src=\"x-twitter.svg\"></a></li>';
            var ghIcon = '<li class=\"nav-item-icon\"><a href=\"https://github.com/apedreira/microracle/\" target=\"_blank\"><img class=\"svgIcon\" src=\"github.svg\"></a></li>';
            
            // Append icons to the navbar
            nav.append(ghIcon); 
            nav.append(xIcon);
        });
      ")),
      
      # 2. CSS STYLES
      tags$style(
        HTML(
          "
    /* --- Navbar Global Appearance --- */
    .navbar-default {
        background-color: black !important;
        box-sizing: border-box;
        border: none;
    }
    .navbar-brand {
        margin-top: 4px;
    }
    .navbar-nav { 
        float: none !important; 
    }
    .navbar-nav > li { 
        float: left; /* Align standard tabs to the left */
    } 
    .navbar-nav > li > a {
        font-weight: 500;
        box-sizing: border-box;
    }

    /* --- Interactive States & Indicators --- */
    .navbar-nav > li.active > a,
    .navbar-nav > li.dropdown:hover > a,
    .navbar-nav > li > a:hover {
        border-bottom: 10px solid #0fc1a3;
        background-color: #333; 
    }
    
    /* Disable bottom border for the last two items (Social Icons) */
    .navbar-nav > li:nth-last-child(1) > a:hover,
    .navbar-nav > li:nth-last-child(2) > a:hover,
    .navbar-nav > li:nth-last-child(1).active > a,
    .navbar-nav > li:nth-last-child(2).active > a {
        border-bottom: none !important;
    }

    /* --- Dropdown Menus --- */
    .dropdown-menu > li > a {
        color: white !important;
        background-color: black !important;
    }
    .dropdown-menu > li > a:hover {
        border-left: 4px solid #0fc1a3;
        background-color: #333 !important;
        text-decoration: none;
    }

    /* --- Social Media Icons --- */
    .nav-item-icon {
        float: right !important;
    }
    .nav-item-icon > a {
        padding-top: 15px !important;
        padding-bottom: 15px !important;
    }
    .svgIcon {
        filter: brightness(0) invert(1); /* Forces pure white color */
        height: 25px;
        width: 25px;
        vertical-align: middle;
        transition: opacity 0.2s ease;
    }
    .svgIcon:hover {
        opacity: 0.8;
    }

    /* --- UI Components & Notifications --- */
    .shiny-notification { 
        position: fixed; 
        top: 50%; 
        left: 0%; 
        color: black; 
    }
    .shiny-notification-warning { color: #8f6830; background-color: #fff3cd; }
    .shiny-notification-error   { color: #751f26; background-color: #f8d7da; }
    
    /* --- Typography & Form Utilities --- */
    label { margin-bottom: 10px; font-weight: bold; }
    .control-label { font-size: 110%; }
    .nobr { white-space: nowrap; }
    .supsub { 
        display: inline-block; 
        margin: -9em 0; 
        vertical-align: -0.55em; 
        line-height: 1.35em; 
        font-size: 70%; 
        text-align: left; 
    }
    input[name='strainID'] + span { font-style: italic; }

    /* --- About Table Styling --- */
    .about-table { 
        margin-top: 20px; 
        background-color: #1a1a1a; 
        color: white; 
        border-radius: 8px; 
        overflow: hidden; 
    }
    .about-table th { 
        background-color: #333; 
        color: #0fc1a3; 
        padding: 12px !important; 
        text-transform: uppercase; 
        font-size: 0.85em; 
    }
    .about-table td { 
        border-top: 1px solid #444 !important; 
        padding: 12px !important; 
        vertical-align: middle !important; 
    }
    .table-hover tbody tr:hover { 
        background-color: #252525 !important; 
    }
    "
        )
      )
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
        h1(class = "text-center","Microracle"),
        
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
              tags$th("Authors"),
              tags$th("Reference"),
              tags$th("Access")
            )
          ),
          tags$tbody(
            # Project: Carvacrol
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
            # Project: DDAC
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
            # Project: Selection Planes
            tags$tr(
              tags$td(tags$strong("Selection & Extinction")),
              tags$td("Analysis of competitive strain dynamics under cyclic treatments based on growth-kill trade-offs."),
              tags$td("Martínez-López et al. (IIM-CSIC / BAM)"),
              tags$td(tags$a(href = "https://arxiv.org/abs/2602.14645", target = "_blank", "arXiv (Preprint with Theory)")),
              tags$td(
                actionButton("go_mod3", "Open Analysis", class = "btn-outline-info btn-sm")
              )
            )
          )
        ),
        
        hr(),
        h2("Authoring"),
        p("Microracle originated as a joint initiative of the Bio2Eng and REVAL research groups at the IIM‑CSIC (Vigo, Spain):"),
        
        tags$div(
          tags$ul(
            tags$li(tags$strong("Adrián Pedreira:"), " Web development, experimental assays, and data collection."),
            tags$li(tags$strong("Nerea Martínez-López:"), " Mathematical modeling and theoretical framework."),
            tags$li(tags$strong("Xosé A. Vázquez:"), " Experimental design."),
            tags$li(tags$strong("Míriam R. García:"), " Web and Theory Supervision.")
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
  
  navbarMenu(
    "MODELS",
    model1UI(),
    model2UI(),
    model3UI()
  )
)


####################
### SERVER LOGIC ###
####################

server <- function(input, output, session) {
  # Initialize module servers
  model1Server(input, output, session)
  model2Server(input, output, session)
  model3Server(input, output, session)
  
  # --- NAVIGATION FROM HOME BUTTONS ---
  observeEvent(input$go_mod3, {
    updateNavbarPage(session, "navbar", selected = "model3")
  })
  observeEvent(input$go_mod1, {
    updateNavbarPage(session, "navbar", selected = "model1") # Asumiendo que el value es "model1"
  })
  observeEvent(input$go_mod2, {
    updateNavbarPage(session, "navbar", selected = "model2") # Asumiendo que el value es "model2"
  })
  # ---------------------------------------
  
  # URL Parsing Logic
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$tab)) {
      updateNavbarPage(session, "navbar", selected = query$tab)
    }
  })
}

# Run the Application
shinyApp(ui, server)