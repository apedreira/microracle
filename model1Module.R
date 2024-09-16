library(shinycssloaders)


model1UI <- function() {

tabPanel(
  "Carvacrol at sub-MIC concentrations",
  sidebarLayout(sidebarPanel(
    fluidRow(
      column(10, 
             radioButtons("model1StrainID", "Preset data for:",
                          c("Escherichia coli" = "ecoli", 
                            "Bacillus cereus" = "bcereus"), inline=TRUE),
             bsTooltip("strainData",
                       "Predefined data and parameters for E.coli and
                       B.cereus.",
                       "bottom",
                       options = list(container = "body")
             )),
      
      column(5,
             numericInput(inputId = "kS",
                          HTML("<span class='nobr'>k
                               <span class='supsub'><br/>S</span>&nbsp;&nbsp;
                               </span><span>(h<sup>-1</sup>)</span>"),
                          value = NULL,
                          step = 0.00001,
                          width = '100%'),
             
             bsTooltip("kS",
                       "Positive constant characterising the rate
                       of the state transition.",
                       "right",
                       options = list(container = "body")),
             numericInput(inputId = "kG",
                          HTML("<span class='nobr'>k<span class='supsub'>
                               <br/>g</span>&nbsp;&nbsp;</span><span>
                               (h<sup>-1</sup>)</span>"),
                          value = NULL,
                          step = 0.0000001,
                          width = '100%'),
             bsTooltip("kG",
                       "Rate of growth as antimicrobial concentration increases",
                       "right",
                       options = list(container = "body")),
             numericInput(inputId = "kD",
                          HTML("<span class='nobr'>k<span class='supsub'>
                               <br/>d</span>&nbsp;&nbsp;</span><span>
                               (h<sup>-1</sup>)</span>"),
                          value = NULL,
                          step = 0.00000001,
                          width = '100%'),
             bsTooltip("kD",
                       "Rate of decay as antimicrobial concentration increases",
                       "right",
                       options = list(container = "body")),
             
      ),
      
      column(5, numericInput(inputId = "mug0",
                             HTML("<span class='nobr'>μ<span class='supsub'>0
                                  <br/>g</span>&nbsp;&nbsp;</span><span>
                                  (h<sup>-1</sup>)</span>"),
                             value = NULL,
                             step = 0.0001,
                             width = '100%'),
             bsTooltip("mug0",
                       "Maximum growth rate in absence of antimicrobial",
                       "right",
                       options = list(container = "body"))),
      column(5, numericInput(inputId = "mud0",
                             HTML("<span class='nobr'>μ<span class='supsub'>0
                                  <br/>m</span>&nbsp;&nbsp;</span><span>
                                  (h<sup>-1</sup>)</span>"),
                             value = NULL,
                             step = 0.000001,
                             width = '100%'),
             bsTooltip("mud0",
                       "Maximum death rate in absence of antimicrobial",
                       "right",
                       options = list(container = "body"))),
    ),
    sliderInput('model1DiscTimes',
                label = "Discretization points",
                step = 10,
                value = 100,
                min = 10,
                max = 200),
    bsTooltip("model1DiscTimes",
              "Number of intermediate points for modelling",
              "bottom",
              options = list(container = "body"))
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel(
        "Plot",  withSpinner(plotlyOutput("model1PlotSingle", height = "400px"),
         image="https://github.com/apedreira/microracle/blob/main/var/img/customLoading.gif?raw=true")
      ),
      tabPanel(
        "Reference", HTML("<br> <p>
         Pedreira, A., Vázquez, J. A., & García, M. R. (2022).
         Modelling the antimicrobial effect of food preservatives in bacteria: 
         Application to <i>Escherichia coli</i> and <i>Bacillus cereus</i> 
         inhibition with carvacrol.
        <i>Journal of Food Engineering</i>, 361, 111734. doi: 
        <a href='https://doi.org/10.1016/j.jfoodeng.2023.111734' 
        target='_blank'>doi.org/10.1016/j.jfoodeng.2023.111734</a></p>
      ")
      )
    )
  )
  ),
  fluidRow(style="padding:16px",
           tabsetPanel(
             id = "model1_tabsetPanel",
             type = "tabs",
             tabPanel(
               "Single experiment",
               column(12, align="center", style="padding:16px",
                      actionButton("runSingle", "Run", class = "btn-success"),
                      actionButton("resetSingle", "Reset",
                                   class = "btn-warning"),
               ),
               sidebarPanel(width=4,
                            rHandsontableOutput("table", height = "400px"))
             ),
             tabPanel(
               "Multiple experiment",
               column(12, align="center", style="padding:16px",
                      actionButton("runMultiple", "Run",
                                   style="padding:8px 16px; margin-right: 16px;
                                   font-size:120%",class = "btn-success"),
                      actionButton("resetMultiple", "Reset",
                                   style="padding:8px 16px; font-size:120%",
                                   class = "btn-success", class = "btn-warning")
               ),
               sidebarPanel(width=12,
                            fluidRow(
                              column(width = 12,
                                     
                                     fluidRow(
                                       column(width = 2,
                                              textInput("colHeader1", 
                                                        "Exp#1 name:",
                                                        value = "Control", 
                                                        width='40%')
                                       ),
                                       column(width = 2,
                                              textInput("colHeader2",
                                                        "Exp#2 name:", 
                                                        value = "Exp2", 
                                                        width='40%')
                                       ),
                                       column(width = 2,
                                              textInput("colHeader3", 
                                                        "Exp#3 name:", 
                                                        value = "Exp3", 
                                                        width='40%')
                                       ),
                                       column(width = 2,
                                              textInput("colHeader4", 
                                                        "Exp#4 name:", 
                                                        value = "Exp4",
                                                        width='40%')
                                       ),
                                       column(width = 2,
                                              textInput("colHeader5", 
                                                        "Exp#5 name:", 
                                                        value = "Exp5", 
                                                        width='40%')
                                       )
                                     ),
                                     fluidRow(
                                       column(width = 12,
                                              rHandsontableOutput("table2", 
                                                              height = "400px")
                                       )
                                     )
                              )
                            ))
             )),
  ),
  
)}

model1Server <- function(input, output, session) {
  observe({
    simpleDataCar = simpleDataCar(input$model1StrainID)
    #Wrap parameters with "updateXInput" to avoid trigger automatic rendering
    updateNumericInput(session, "kS", value = simpleDataCar[4])
    updateNumericInput(session, "kG", value = simpleDataCar[5])
    updateNumericInput(session, "kD", value = simpleDataCar[6])
    updateNumericInput(session, "mug0", value = simpleDataCar[7])
    updateNumericInput(session, "mud0", value = simpleDataCar[8])
    updateSliderInput(session, "model1DiscTimes", value = 100)
    
    #Create a data frame by calling simpleDataCarCar external function
    # and set predefined experimental and modelling values for the initial plot
    exp_df = data.frame(Time = unlist(simpleDataCar[1]),
                        y = simpleDataCar[[2]][[2]],
                        drug = unlist(simpleDataCar[[3]][[2]]))
    texto = input$model1StrainID
    
    #Make dataframe reactive to changes
    datavalues = reactiveValues(data = exp_df)

    #Create a handsontable with the dataframe content and validate just 
    #numerical content
    output$table = renderRHandsontable({
      rhandsontable(datavalues$data, maxRows = 100, colHeaders = c("Time (h)",
                                                                   "CFU/mL",
                                                                   "Carvacrol
                                                                   concentration
                                                                   (mg/L)"))%>%
        hot_validate_numeric(col = c(1,2,3), min = 0)
    })
    
    
    
    
    alreadyOpened = FALSE
    #Observe selected tabset (Single/ Muliple experiment)
    observeEvent(input$model1_tabsetPanel,{
      
      ###----------------------------##
      ### CODE FOR SINGLE EXPERIMENT ##
      ###----------------------------##   
      
      #A variable to check if single experiment was already executed. Prevents
      # loss previous plot when switching among single/ multiple experiment by 
      #blocking render the empty plot dataframe.
      
      alreadyExecuted = FALSE
      
      
      if(input$model1_tabsetPanel == "Single experiment"){
        
        alreadyExecuted = TRUE
        
        if (TRUE) {
          #Render empty plot frame
          output$model1PlotSingle = renderPlotly({
            plot_ly(x = c(0), y = c(0), type = 'scatter',
                    mode = 'markers', color ="white") %>% 
              layout(yaxis=list(showexponent= "all", exponentformat='E',
                                title="Log<sub>10</sub> CFU/mL"),
                     xaxis = list(title = "Time (h)"))
          })}
        
        
        
        #Uptate dataframe values and plot just when clicking "Run" button. 
        # Avoid automatic rendering
        observeEvent(
          input$runSingle,
          # 
          {
            datavalues$data = hot_to_r(input$table)
            validation_msg = dataValidatorSingle(datavalues$data)
            
            #The columns Time, Drug and CFU/mL should have the same length
            #(without counting the NA) otherwise it will give an error 
            
            if(validation_msg == "OK") {
              #Uptate dataframe extracting values from the handsontable 
              datavalues$data = hot_to_r(input$table)
              
              # Packing equation parameters in a vector
              p1 = input$mug0
              p2 = input$mud0
              p3 = input$kS
              p4 = input$kG
              p5 = input$kD
              
              params = c(p1,p2,p3,p4,p5)
              
              #Get discretization times value
              discTimes = input$model1DiscTimes
              
              #Extracts experimental data from dataframe
              y_exp = unlist(datavalues$data$y)
              c_exp = unlist(datavalues$data$drug)
              time_exp = unlist(datavalues$data$Time)
              
              #Call modelling function. Save model output in two vectors
              submic_car_func = submiC3(time_exp, c_exp, params, y_exp,
                                        discTimes)
              time_mod = submic_car_func[[1]]
              y_mod = submic_car_func[[2]]
              
              #Plot results (dots as experimental data, lines as model output)
              output$model1PlotSingle = renderPlotly({
                plot_ly(datavalues$data, x = ~Time, 
                        y = log10(datavalues$data$y), name = "Observed", 
                        type = 'scatter',
                        mode = 'markers', color = "white") %>% 
                  add_lines(name = "Expected",x = time_mod, y = log10(y_mod), 
                            mode = 'line') %>% 
                  layout(yaxis = list(showexponent = "all", exponentformat ='E',
                                      title ="Log<sub>10</sub> CFU/mL"),
                         xaxis = list(title = "Time (h)"))
              })}
          })
        
        
        
        #Reset dataframe, handsontable and plot to default when clicking 
        #"Reset" button
        observeEvent(
          input$resetSingle,
          {datavalues$data = exp_df
          output$table = renderRHandsontable({
            rhandsontable(datavalues$data, maxRows = 100, 
                          colHeaders = c("Time (h)","CFU/mL",
                                         "Carvacrol concentration (mg/L)"))%>%
              hot_validate_numeric(col = c(1,2,3), min = 0)
          })
          
          simpleDataCar = simpleDataCar(input$model1StrainID)
          updateNumericInput(session, "kS", value = simpleDataCar[4])
          updateNumericInput(session, "kG", value = simpleDataCar[5])
          updateNumericInput(session, "kD", value = simpleDataCar[6])
          updateNumericInput(session, "mug0", value = simpleDataCar[7])
          updateNumericInput(session, "mud0", value = simpleDataCar[8])
          updateSliderInput(session, "model1DiscTimes", value = 100)
          
          #Render empty plot frame
          output$model1PlotSingle = renderPlotly({
            plot_ly(x = c(0), y = c(0), type = 'scatter',
                    mode = 'markers', color ="white") %>% 
              layout(yaxis=list(showexponent= "all", exponentformat='E',
                                title="Log<sub>10</sub> CFU/mL"),
                     xaxis = list(title = "Time (h)"))
          })
          
          })
        
      }
      
      ###-------------------------------##
      ### CODE FOR MULTIPLE EXPERIMENT  ##
      ###-------------------------------##  
      
      
      else if (input$model1_tabsetPanel == "Multiple experiment"){
        
        colHeaders = c("Time (h)",
                       "Control CFU/mL",
                       "Control Carvacrol (mg/L)",
                       "Exp#2 CFU/mL",
                       "Exp#2 Carvacrol (mg/L)",
                       "Exp#3 CFU/mL",
                       "Exp#3 Carvacrol (mg/L)",
                       "Exp#4 CFU/mL",
                       "Exp#4 Carvacrol (mg/L)",
                       "Exp#5 CFU/mL",
                       "Exp#5 Carvacrol (mg/L)",
                       "Exp#6 CFU/mL",
                       "Exp#6 Carvacrol (mg/L)")
        
        #Create a data frame and set predefined values for initial plot
        preset_df = data.frame(exp1_t= unlist(simpleDataCar[1]),
                               exp1_y = simpleDataCar[[2]][[1]],
                               exp1_d = unlist(simpleDataCar[[3]][[1]]),
                               exp2_y = simpleDataCar[[2]][[2]],
                               exp2_d = unlist(simpleDataCar[[3]][[2]]),
                               exp3_y = simpleDataCar[[2]][[3]],
                               exp3_d = unlist(simpleDataCar[[3]][[3]]),
                               exp4_y = simpleDataCar[[2]][[4]],
                               exp4_d = unlist(simpleDataCar[[3]][[4]]),
                               exp5_y = simpleDataCar[[2]][[5]],
                               exp5_d = unlist(simpleDataCar[[3]][[5]])
        )
        
        #Render empty plot frame
        output$model1PlotSingle=renderPlotly({
          plot_ly(x=c(0), y=c(0), name = "Observed", type = 'scatter',
                  mode = 'markers', color ="white", ) %>% 
            layout(yaxis=list(showexponent= "all", exponentformat='E',
                              title="Log<sub>10</sub> CFU/mL"),
                   xaxis = list(title = "Time (h)"))
        })
        
        #Make dataframe reactive to changes
        outputValues = reactiveValues(data=preset_df)
        
        
        colHeaders = function(){
          input_count = 1
          headers = character(12)
          headers[1] = "Time (h)"
          for(i in seq(from = 2, to = 10, by = 2)){
            headers[i] = paste0(input[[paste0("colHeader", input_count )]],
                                " CFU/mL")
            headers[i+1] = paste0(input[[paste0("colHeader", input_count )]], 
                                  " Carvacrol (mg/L)")
            input_count  = input_count  + 1
          }
          return(headers)
        }
        
        #outputValues$data=hot_to_r(input$table2)
        
        #Create a handsontable with the dataframe content, using inputs values
        #to set colheaders
        output$table2 = renderRHandsontable({
          rhandsontable(outputValues$data,maxRows = 100, 
                        colHeaders = colHeaders())%>%
            hot_validate_numeric(col = c(1:11), min = 0)
        })
        
        
        #A loop that observe changes in inputs values and set col names in df
        #and handsontable
        for (i in 1:5) {
          observeEvent(input[[paste0("colHeader", i)]], {
            colnames(preset_df)[i] = input[[paste0("colHeader", i)]]
          })
        }
        
        
        #Apply changes in handsontable to dataframe when clicking in Run 
        observeEvent(input$runMultiple,
                     {outputValues$data = hot_to_r(input$table2)
                     colHeaders = colHeaders()
                     validation = dataValidatorMultiple(outputValues$data)
                     validation_msg = validation[[1]]
                     validated_subsetDF = validation[[2]]

                     
                     if(validation_msg == "OK") {
                       #Packing parameters in a vector
                       p1 = input$mug0
                       p2 = input$mud0
                       p3 = input$kS
                       p4 = input$kG
                       p5 = input$kD
                       params = c(p1,p2,p3,p4,p5)
                       
                       #Get discretization times value
                       discTimes = input$model1DiscTimes
                       
                       #Plot results
                       output$model1PlotSingle = renderPlotly({
                         
                         #Initializate a scatter plot
                         p=plot_ly(type="scatter", mode="markers")
                         
                         #Set counters for loop
                         l=2
                         exp_name = 2
                         
                         #Create a palette with 10 colours for traces and lines
                         plotColors = c("#404040","#619cff","#e69f00","#cc79a7",
                                        "#78ac44","#d55e00","#0072b2","#ffce00",
                                        "#751056","#01665e")
                         
                         
                         #Unlist Time col from dataframe
                         time_exp = unlist(validated_subsetDF[1])
                         
                         #Iterate over dataframe and dinamically add traces 
                         #and lines to plot
                         while (l<(length(validated_subsetDF)+1)){
                           y_exp = unlist(validated_subsetDF[l])
                           drug_conc = unlist(validated_subsetDF[l+1])
                           
                           #Call model function. Save model output in two 
                           #vectors
                           submic_car_func = submiC3(time_exp, drug_conc,
                                                     params, y_exp, discTimes)
                           time_mod = submic_car_func[[1]]
                           y_mod = log10(submic_car_func[[2]])
                           #Get col name from dataframe for include in 
                           #the legend, remove the "CFU/mL" part
                           currentColName = colHeaders[exp_name]
                           legendName =  substr(currentColName, 1,
                                                nchar(currentColName) - 6)
                           #Trace plot. Markers depict experimental points,
                           #lines model prediction
                           p = add_trace(p, x = unlist(validated_subsetDF[1]), 
                                         y = log10(y_exp),  mode = "markers", 
                                         name = paste0(legendName, 
                                                       " (observed)"), 
                                         marker = list(color = plotColors [l-1]))
                           p = add_lines(p, x = time_mod ,y = y_mod, 
                                         mode="line",
                                         name = paste0(legendName, 
                                                       " (expected)"), 
                                         line = list(color = plotColors[l-1]))
                           l = l+2
                           exp_name =  exp_name + 2
                         }
                         p%>%
                           layout(yaxis=list(showexponent = "all",
                                             exponentformat = 'E', 
                                             title="Log<sub>10</sub> CFU/mL"),
                                  xaxis = list(title = "Time (h)"))
                       })
                     }
                     })
        #Reset dataframe, handsontable and plot to default when clicking
        #"Reset" button
        observeEvent(
          input$resetMultiple,
          {outputValues$data = preset_df
          
          
          output$table2 = renderRHandsontable({
            rhandsontable(outputValues$data, maxRows = 100)%>%
              hot_validate_numeric(col = c(1,2,3), min = 0)
          })
          output$table2 = renderRHandsontable({
            rhandsontable(outputValues$data, maxRows = 100, colHeaders = c("Time (h)",
                                                                           "Control CFU/mL",
                                                                           "Control Carvacrol (mg/L)",
                                                                           "Exp#2 CFU/mL",
                                                                           "Exp#2 Carvacrol (mg/L)",
                                                                           "Exp#3 CFU/mL",
                                                                           "Exp#3 Carvacrol (mg/L)",
                                                                           "Exp#4 CFU/mL",
                                                                           "Exp#4 Carvacrol (mg/L)",
                                                                           "Exp#5 CFU/mL",
                                                                           "Exp#5 Carvacrol (mg/L)",
                                                                           "Exp#6 CFU/mL",
                                                                           "Exp#6 Carvacrol (mg/L)"))%>%
              hot_validate_numeric(col = c(1,2,3), min = 0)
          })
          
          
          simpleDataCar = simpleDataCar(input$model1StrainID)
          updateNumericInput(session, "kS", value = simpleDataCar[4])
          updateNumericInput(session, "kG", value = simpleDataCar[5])
          updateNumericInput(session, "kD", value = simpleDataCar[6])
          updateNumericInput(session, "mug0", value = simpleDataCar[7])
          updateNumericInput(session, "mud0", value = simpleDataCar[8])
          updateSliderInput(session, "model1DiscTimes", value = 100)
          
          #Render empty plot frame
          output$model1PlotSingle = renderPlotly({
            plot_ly(x = c(0), y = c(0), type = 'scatter',
                    mode = 'markers', color ="white") %>% 
              layout(yaxis=list(showexponent= "all", exponentformat='E', 
                                title="Log<sub>10</sub> CFU/mL"),
                     xaxis = list(title = "Time (h)"))
          })
          })
      }
    })
  })
}
