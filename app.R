####################
###   LIBRARIES  ###
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
ui =
  navbarPage(
    title = div("",
                img(
                  src = "logo2.png",
                  id = "logo",
                  height = "50px",
                  style = "position: relative; margin:-15px 0px; padding: 3% 0%;
                  display:right-align;")
    ),
    id="navbar",
    theme = shinytheme("yeti"),
    # Themes impede the type argument in Shiny shownotification
    # therefore, css must be provided to solve the problem and change colors
    tags$head(
      #Favicon
      tags$link(rel = "shortcut icon", href = "/icon.ico"),
      #Necessary for avoid notifications stay so more time, float right rrss 
      #icons in navbar and aligned sub and superscripts
     
      tags$style( 
        HTML(".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(0%);
             color:black;
             }",
             ".shiny-notification-warning {
               color:#8f6830;
               background-color: #fff3cd;

             }",
             ".shiny-notification-error {
               color:#751f26;
               background-color: #f8d7da;

             }",
   
             "label {margin-bottom:10px, font-weight:bold}",
             ".control-label { font-size:110%;}",
             ".action-button {padding:8px 16px; margin-right: 16px;
             font-size:120%}",
            ".navbar-nav {float: none !important;}",
             ".navbar-nav > li:nth-child() {float: right;right: 150px;}",
             ".navbar-nav > li:nth-child(4) {float: right;, margin-top: 10px}",
            ".navbar-nav > li:nth-child(5) {float: right;, margin-top: 10px}",
            ".navbar-nav > li:nth-child(5) a:nth-child(1) {display:none}",
            ".navbar-nav > li:nth-child(4) a:nth-child(1) {display:none}",
            ".nobr {white-space: nowrap;}",
            ".navbar-default {
            background-color:black;
            box-sizing: border-box;
            }",
            ".navbar-nav > li > a {
            color:red;
            background-color:black;
             box-sizing: border-box;
            }",
            ".navbar-nav > li.active > a,
            .navbar-nav > li.dropdown:hover > a,
            .navbar-nav > li:nth-child(6):hover > a{
             border-bottom: 8px solid #0fc1a3;
                box-sizing: border-box;
            }",
            ".dropdown-menu li {
            background-color:black;
            box-sizing: border-box;
            }",
            ".dropdown-menu li.active,
            .dropdown-menu li:hover {
            border-right: 8px solid #0fc1a3;
            }",
            ".supsub {
              display: inline-block;
              margin: -9em 0;
              vertical-align: -0.55em;
              line-height: 1.35em;
              font-size: 70%;
              text-align: left;
            }",
            "input[name='strainID'] + span {
              font-style: italic; 
            }",
            ".svgIcon {
            filter: brightness(0) invert(1);
            height: 25px;
            width: 25px
            }",
            "#navbar > li:nth-child(1) > a:nth-child(1) {
            display: none;
            }",
            "#navbar > li:nth-child(2) > a:nth-child(1) {
             display: none;
            }"
        )
      )
    ),
    shinyjs::useShinyjs(),
    collapsible = TRUE,
    navbarMenu("MODELS",
               model1UI(),
               model2UI()
    ),
    tabPanel(tags$a(tags$img( class = "svgIcon", src = "x-twitter.svg"), 
                    href = "https://twitter.com/microracle", target ="_blank"),
             ),
    tabPanel(tags$a(tags$img(class = "svgIcon", src = "github.svg"), 
                    href = "https://github.com/apedreira/microracle/", 
                    target ="_blank"),
    ),
   tabPanel("About us",
            mainPanel(
              width = 12,
              column(width = 3, ""),
              column(
                width = 6,
                h1("Microracle"),
                # HMTL("
                #   <p>Microracle allows users to employ the bacterial 
                #  inactivation models developed in our group with their own 
                # experimental data. Alternatively, the user can simply load the
                # available experimental data for <i>Escherichia coli</i> and 
                #<i>Bacillus cereus</i> and modify the model parameters to test 
                # how they affect the model output.
                # Currently, the following models are available:</p>"),
                h2("Authoring"),
                p(
                  "Microracle has been developed by researchers of the
                  Biosystems and Bioprocess Engineering Group (Bio2Eng) and 
                  Recycling and Valorization of Waste Materials (REVAL) groups 
                  from the IIM-CSIC (Vigo, Spain):"
                ),
                tags$div(tags$ul(
                  tags$li(
                  tags$span("Adrián Pedreira: Web developer, experimentation and 
                            data collection.")),
                  tags$li(tags$span("Nerea Martínez: Development of
                                    mathematical models.")),
                  tags$li(tags$span("Xosé A. Vázquez: Experimental design")),
                  tags$li(tags$span("Míriam R. García: Development of 
                                    mathematical models.")))
                  ),
                
                HTML("<p>For any question related to the mathematical models,
                please contact with Míriam R. García (miriamr@iim.csic.es). 
                Microracle is a project in early development, so you may 
                encounter some problems on the web. To report any bug or a 
                problem, please generate a new issue in the 
                <a href='https://github.com/apedreira/microracle'target='_blank'
                >GitHub repository</a> or contact with Adrián Pedreira 
                (apedreira@iim.csic.es).</p>"),
              h2("Requirements"),
              HTML("<p>Microracle can be freely accessed from 
              <a href='https://microracle.shinyapps.io/Microracle/'
              target='_blank'>microracle.shinyapps.io/Microracle</a>. 
              No requirements other than internet connection from a web browser
              are necessary.</p>"
              )
            )
            ),
            column(width = 3, "")
            ))


####################
### SERVER LOGIC ###
####################

server = function(input, output, session) {
  model1Server(input, output, session)
  model2Server(input, output, session)
}

shinyApp(ui, server)

