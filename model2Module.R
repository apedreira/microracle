library(shinycssloaders)

model2UI <- function() {
  tabPanel(
    "DDAC at sub-MIC concentrations",
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(
            10,
            radioButtons("model2StrainID", "Preset data for:",
                         #c("Escherichia coli" = "ecoli",
                         "Bacillus cereus" = "bcereus"),
            inline = TRUE
          ),
          c("Escherichia coli" = "ecoli"),
          inline = TRUE
        ),
        bsTooltip(
          "strainData",
          "Predefined data and parameters for E.coli and B.cereus.",
          "bottom",
          options = list(container = "body")
        )
      ),
      
      column(
        3,
        numericInput(
          inputId = "ka0",
          HTML(
            "<span class='nobr'>k
                               <span class='supsub'>0<br/>a</span>&nbsp;&nbsp;
                               </span><span>(h<sup>-1</sup>)</span>"
          ),
          value = NULL,
          step = 0.00001,
          width = '100%'
        ),
        
        bsTooltip(
          "ka0",
          "Adaptation rate without disinfectant",
          "right",
          options = list(container = "body")
        ),
        numericInput(
          inputId = "kg0",
          HTML(
            "<span class='nobr'>k<span class='supsub'>0
                               <br/>g</span>&nbsp;&nbsp;</span>
                               <span>(AU h<sup>-1</sup>)</span>"
          ),
          value = NULL,
          step = 0.00001,
          width = '100%'
        ),
        bsTooltip(
          "kg0",
          "Growth rate without disinfectant",
          "right",
          options = list(container = "body")
        ),
        numericInput(
          inputId = "kd0",
          HTML(
            "<span class='nobr'>k<span class='supsub'>0
                               <br/>d</span>&nbsp;&nbsp;</span>
                               <span>(h<sup>-1</sup>)</span>"
          ),
          value = NULL,
          step =  0.00001,
          width = '100%'
        ),
        bsTooltip(
          "kd0",
          "Death rate without disinfectant",
          "right",
          options = list(container = "body")
        ),
        numericInput(
          inputId = "kd1",
          HTML(
            "<span class='nobr'>k<span class='supsub'>d
                               <br/>*</span>&nbsp;&nbsp;</span>
                               <span>(h<sup>-1</sup>)</span>"
          ),
          value = NULL,
          step = 0.0001,
          width = '100%'
        ),
        bsTooltip(
          "kd1",
          "Scaling of disinfectant effect on death",
          "right",
          options = list(container = "body")
        )
      ),
      
      column(
        3,
        numericInput(
          inputId = "ki",
          HTML(
            "<span class='nobr'>k<span class='supsub'>
                               <br/>i</span>&nbsp;&nbsp;</span>
                               <span>(AU)</span>"
          ),
          value = NULL,
          step = 0.000001,
          width = '100%'
        ),
        bsTooltip(
          "ki",
          "Inhibition constant due to cell density",
          "right",
          options = list(container = "body")
        ),
        
        numericInput(
          inputId = "IC50_a",
          HTML(
            "<span class='nobr'>IC
                                      <span class='supsub'>50,a<br/>
                                      </span>&nbsp;&nbsp;</span>
                                      <span>(mg L<sup>-1</sup>)</span>"
          ),
          value = NULL,
          step = 0.000001,
          width = '100%'
        ),
        bsTooltip(
          "IC50_a",
          "Half maximal inhibitory concentration
                       of adaptation rate",
          "right",
          options = list(container = "body")
        ),
        numericInput(
          inputId = "IC50_g",
          HTML(
            "<span class='nobr'>IC<span
                                  class='supsub'>50,g<br/></span>&nbsp;&nbsp;
                                  </span><span>(mg L<sup>-1</sup>)</span>"
          ),
          value = NULL,
          step = 0.000001,
          width = '100%'
        ),
        bsTooltip(
          "IC50_g",
          "Half maximal inhibitory concentration of growth rate",
          "right",
          options = list(container = "body")
        ),
        numericInput(
          inputId = "EC50_d",
          HTML(
            "<span class='nobr'>EC<span class='supsub'>50,
                                  d<br/></span>&nbsp;&nbsp;</span>
                                  <span>(mg L<sup>-1</sup>)</span>"
          ),
          value = NULL,
          step = 0.000001,
          width = '100%'
        ),
        bsTooltip(
          "EC50_d",
          "Half maximal effective concentration on death",
          "right",
          options = list(container = "body")
        )
      ),
      column(
        3,
        numericInput(
          inputId = "gamma_a",
          HTML(
            "<span class='nobr'>γ<span class='supsub'>a
                                  <br/></span>&nbsp;&nbsp;</span>"
          ),
          value = NULL,
          step = 0.000001,
          width = '100%'
        ),
        bsTooltip(
          "gamma_a",
          "Effect shape of disinfectant over adaptation rate",
          "right",
          options = list(container = "body")
        ),
        numericInput(
          inputId = "gamma_g",
          HTML(
            "<span class='nobr'>γ<span class='supsub'>g
                                  <br/></span>&nbsp;&nbsp;</span>"
          ),
          value = NULL,
          step = 0.000001,
          width = '100%'
        ),
        bsTooltip(
          "gamma_g",
          "Effect shape of disinfectant over growth rate",
          "right",
          options = list(container = "body")
        ),
        numericInput(
          inputId = "gamma_d",
          HTML(
            "<span class='nobr'>γ<span class='supsub'>d
                                  <br/></span>&nbsp;&nbsp;</span>"
          ),
          value = NULL,
          step = 0.000001,
          width = '100%'
        ),
        bsTooltip(
          "gamma_d",
          "Effect shape of disinfectant over death rate",
          "right",
          options = list(container = "body")
        )
      ),
    ),
    
    sliderInput(
      'model2DiscTimes',
      label = "Discretization points",
      step = 10,
      value = 100,
      min = 10,
      max = 200
    ),
    bsTooltip(
      "model2DiscTimes",
      "Number of intermediate points for modelling",
      "bottom",
      options = list(container = "body")
    )
  ),
  
  mainPanel(tabsetPanel(
    tabPanel(
      "Plot",
      withSpinner(plotlyOutput("model2PlotSingle", height = "400px"),
                  image = "https://github.com/apedreira/microracle/blob/main/var/img/customLoading.gif?raw=true")
    ),
    tabPanel(
      "Reference",
      HTML(
        "<br> <p>
         Pedreira, A., Vázquez, J. A., & García, M. R. (2022).
         Kinetics of Bacterial Adaptation, Growth, and Death at
         Didecyldimethylammonium Chloride sub-MIC Concentrations.
         <i>Frontiers in Microbiology</i>, 13, 758237.
                          doi: <a href='https://doi.org/10.3389/fmicb.2022.758237'
                          target='_blank'>doi.org/10.3389/fmicb.2022.758237</a> </p>"
      )
    )
  ))
  
  ,
  
  fluidRow(style = "padding:16px",
           tabsetPanel(
             id = "model2_tabsetPanel",
             type = "tabs",
             tabPanel(
               "Single experiment",
               column(
                 12,
                 align = "center",
                 style = "padding:16px",
                 actionButton("model2RunSingle", "Run", class = "btn-success"),
                 actionButton("model2ResetSingle", "Reset", class = "btn-warning"),
               ),
               sidebarPanel(
                 width = 4,
                 rHandsontableOutput("model2_single_table", height = "400px")
               )
             ),
             tabPanel(
               "Multiple experiment",
               column(
                 12,
                 align = "center",
                 style = "padding:16px",
                 actionButton(
                   "model2RunMultiple",
                   "Run",
                   style = "padding:8px 16px; margin-right: 16px;font-size:120%",
                   class = "btn-success"
                 ),
                 actionButton(
                   "model2ResetMultiple",
                   "Reset",
                   style = "padding:8px 16px; font-size:120%",
                   class = "btn-success",
                   class = "btn-warning"
                 )
               ),
               sidebarPanel(width = 12,
                            fluidRow(column(
                              width = 12,
                              
                              fluidRow(
                                column(
                                  width = 2,
                                  textInput(
                                    "model2ColHeader1",
                                    "Exp#1 name:",
                                    value = "Control",
                                    width = '40%'
                                  )
                                ),
                                column(
                                  width = 2,
                                  textInput(
                                    "model2ColHeader2",
                                    "Exp#2 name:",
                                    value = "0.50",
                                    width = '40%'
                                  )
                                ),
                                column(
                                  width = 2,
                                  textInput(
                                    "model2ColHeader3",
                                    "Exp#3 name:",
                                    value = "Exp3",
                                    width = '40%'
                                  )
                                ),
                                column(
                                  width = 2,
                                  textInput(
                                    "model2ColHeader4",
                                    "Exp#4 name:",
                                    value = "Exp4",
                                    width = '40%'
                                  )
                                ),
                                column(
                                  width = 2,
                                  textInput(
                                    "model2ColHeader5",
                                    "Exp#5 name:",
                                    value = "Exp5",
                                    width = '40%'
                                  )
                                ),
                                column(
                                  width = 2,
                                  textInput(
                                    "model2ColHeader6",
                                    "Exp#6 name:",
                                    value = "Exp6",
                                    width = '40%'
                                  )
                                )
                              ),
                              fluidRow(column(
                                width = 12,
                                rHandsontableOutput("model2_mult_table", 
                                                    height = "400px")
                              ))
                            )))
             )
           ),)
}

model2Server <- function(input, output, session) {
  observe({
    simpleDataDDAC = simpleDataDDAC(input$model2StrainID)
    #Wrap parameters with "updateXInput" to avoid trigger automatic rendering
    updateNumericInput(session, "ka0", value = simpleDataDDAC[4])
    updateNumericInput(session, "kg0", value = simpleDataDDAC[5])
    updateNumericInput(session, "kd0", value = simpleDataDDAC[6])
    updateNumericInput(session, "kd1", value = simpleDataDDAC[7])
    updateNumericInput(session, "ki", value = simpleDataDDAC[8])
    updateNumericInput(session, "IC50_a", value = simpleDataDDAC[9])
    updateNumericInput(session, "IC50_g", value = simpleDataDDAC[10])
    updateNumericInput(session, "EC50_d", value = simpleDataDDAC[11])
    updateNumericInput(session, "gamma_a", value = simpleDataDDAC[12])
    updateNumericInput(session, "gamma_g", value = simpleDataDDAC[13])
    updateNumericInput(session, "gamma_d", value = simpleDataDDAC[14])
    updateSliderInput(session, "model2DiscTimes", value = 100)
    
    #Create a data frame by calling simpleDataDDAC external function
    # and set predefined experimental and modelling values for the initial plot
    exp_df = data.frame(
      Time = unlist(simpleDataDDAC[1]),
      y = simpleDataDDAC[[2]][[2]],
      drug = unlist(simpleDataDDAC[[3]][[2]])
    )
    texto = input$model2StrainID
    
    #Make dataframe reactive to changes
    datavalues = reactiveValues(data = exp_df)
    # print(texto)
    
    #Create a handsontable with the dataframe content and validate just 
    #numerical content
    output$model2_single_table = renderRHandsontable({
      rhandsontable(
        datavalues$data,
        maxRows = 100,
        colHeaders = c("Time (h)", "CFU/mL", "DDAC concentration (mg/L)")
      ) %>%
        hot_validate_numeric(col = c(1, 2, 3), min = 0)
    })
    
    alreadyOpened = FALSE
    #Observe selected tabset (Single/ Muliple experiment)
    observeEvent(input$model2_tabsetPanel, {
      ###----------------------------##
      ### CODE FOR SINGLE EXPERIMENT ##
      ###----------------------------##
      
      #A variable to check if single experiment was already executed. Prevents
      # loss previous plot when switching among single/ multiple experiment by
      #blocking render the empty plot dataframe.
      
      alreadyExecuted = TRUE
      
      
      if (input$model2_tabsetPanel == "Single experiment") {
        alreadyExecuted = TRUE
        
        if (TRUE) {
          #Render empty plot frame
          output$model2PlotSingle = renderPlotly({
            plot_ly(
              x = c(0),
              y = c(0),
              type = 'scatter',
              mode = 'markers',
              color = "white"
            ) %>%
              layout(
                yaxis = list(
                  showexponent = "all",
                  exponentformat = 'E',
                  title = "Log<sub>10</sub> CFU/mL"
                ),
                xaxis = list(title = "Time (h)")
              )
          })
        }
        
        
        
        #Uptate dataframe values and plot just when clicking "Run" button. 
        # to avoid automatic rendering
        observeEvent(input$model2RunSingle,
                     #
                     {
                       datavalues$data = hot_to_r(input$model2_single_table)
                       validation_msg = dataValidatorSingle(datavalues$data)
                       
                       if (validation_msg == "OK") {
                         #Uptate dataframe extracting values from the handsontable
                         datavalues$data = hot_to_r(input$model2_single_table)
                         
                         # Packing equation parameters in a vector
                         p1 = input$ka0
                         p2 = input$kg0
                         p3 = input$kd0
                         p4 = input$kd1
                         p5 = input$ki
                         p6 = input$IC50_a
                         p7 = input$IC50_g
                         p8 = input$EC50_d
                         p9 = input$gamma_a
                         p10 = input$gamma_g
                         p11 = input$gamma_d
                         params = c(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)
                         
                         #Get discretization times value
                         discTimes = input$model2DiscTimes
                         
                         
                         #Extracts experimental data from dataframe
                         y_exp = unlist(datavalues$data$y)
                         c_exp = unlist(datavalues$data$drug)
                         time_exp = unlist(datavalues$data$Time)
                         
                         #Call modelling function. Save model output in 
                         #two vectors
                         submic_ddac_func = submic_ddac(time_exp, c_exp, params,
                                                        y_exp, discTimes)
                         time_mod =  submic_ddac_func[[1]]
                         y_mod =  submic_ddac_func[[2]]
                         
                         #Plot results (dots as experimental data, 
                         # lines as model output)
                         output$model2PlotSingle = renderPlotly({
                           plot_ly(
                             datavalues$data,
                             x = ~ Time,
                             y = log10(datavalues$data$y),
                             name = "Observed",
                             type = 'scatter',
                             mode = 'markers',
                             color = "white"
                           ) %>%
                             add_lines(
                               name = "Expected",
                               x = time_mod,
                               y = log10(y_mod),
                               mode = 'line'
                             ) %>%
                             layout(
                               yaxis = list(
                                 showexponent = "all",
                                 exponentformat = 'E',
                                 title = "Log<sub>10</sub> CFU/mL"
                               ),
                               xaxis = list(title = "Time (h)")
                             )
                         })
                       }
                     })
        
        #Reset dataframe, handsontable and plot to default when clicking 
        #"Reset" button
        observeEvent(input$model2ResetSingle,
                     {
                       datavalues$data = exp_df
                       output$model2_single_table = renderRHandsontable({
                         rhandsontable(
                           datavalues$data,
                           maxRows = 100,
                           colHeaders = c("Time (h)", "CFU/mL",
                                          "DDAC concentration (mg/L)")
                         ) %>%
                           hot_validate_numeric(col = c(1, 2, 3), min = 0)
                       })
                       
                       simpleDataDDAC = simpleDataDDAC(input$model2StrainID)
                       updateNumericInput(session, "ka0",
                                          value = simpleDataDDAC[4])
                       updateNumericInput(session, "kg0", 
                                          value = simpleDataDDAC[5])
                       updateNumericInput(session, "kd0", 
                                          value = simpleDataDDAC[6])
                       updateNumericInput(session, "kd1",
                                          value = simpleDataDDAC[7])
                       updateNumericInput(session, "ki", 
                                          value = simpleDataDDAC[8])
                       updateNumericInput(session, "IC50_a", 
                                          value = simpleDataDDAC[9])
                       updateNumericInput(session, "IC50_g", 
                                          value = simpleDataDDAC[10])
                       updateNumericInput(session, "EC50_d", 
                                          value = simpleDataDDAC[11])
                       updateNumericInput(session, "gamma_a",
                                          value = simpleDataDDAC[12])
                       updateNumericInput(session, "gamma_g", 
                                          value = simpleDataDDAC[13])
                       updateNumericInput(session, "gamma_d", 
                                          value = simpleDataDDAC[14])
                       updateSliderInput(session, "model2DiscTimes", value=100)
                       
                       #Render empty plot frame
                       output$model2PlotSingle = renderPlotly({
                         plot_ly(
                           x = c(0),
                           y = c(0),
                           type = 'scatter',
                           mode = 'markers',
                           color = "white"
                         ) %>%
                           layout(
                             yaxis = list(
                               showexponent = "all",
                               exponentformat = 'E',
                               title = "Log<sub>10</sub> CFU/mL"
                             ),
                             xaxis = list(title = "Time (h)")
                           )
                       })
                       
                     })
        
        
        
      }
      
      ###-------------------------------##
      ### CODE FOR MULTIPLE EXPERIMENT  ##
      ###-------------------------------##
      
      
      else if (input$model2_tabsetPanel == "Multiple experiment") {
        colHeaders = c(
          "Time (h)",
          "Control CFU/mL",
          "Control DDAC (mg/L)",
          "Exp#2 CFU/mL",
          "Exp#2 DDAC (mg/L)",
          "Exp#3 CFU/mL",
          "Exp#3 DDAC (mg/L)",
          "Exp#4 CFU/mL",
          "Exp#4 DDAC (mg/L)",
          "Exp#5 CFU/mL",
          "Exp#5 DDAC (mg/L)",
          "Exp#6 CFU/mL",
          "Exp#6 DDAC (mg/L)"
        )
        
        #Create a data frame and set predefined values for initial plot
        preset_df = data.frame(
          exp1_t = unlist(simpleDataDDAC[1]),
          exp1_y = simpleDataDDAC[[2]][[1]],
          exp1_d = unlist(simpleDataDDAC[[3]][[1]]),
          exp2_y = simpleDataDDAC[[2]][[2]],
          exp2_d = unlist(simpleDataDDAC[[3]][[2]]),
          exp3_y = simpleDataDDAC[[2]][[3]],
          exp3_d = unlist(simpleDataDDAC[[3]][[3]]),
          exp4_y = simpleDataDDAC[[2]][[4]],
          exp4_d = unlist(simpleDataDDAC[[3]][[4]]),
          exp5_y = simpleDataDDAC[[2]][[5]],
          exp5_d = unlist(simpleDataDDAC[[3]][[5]]),
          exp6_y = simpleDataDDAC[[2]][[6]],
          exp6_d = unlist(simpleDataDDAC[[3]][[6]])
        )
        
        #Render empty plot frame
        output$model2PlotSingle = renderPlotly({
          plot_ly(
            x = c(0),
            y = c(0),
            name = "Observed",
            type = 'scatter',
            mode = 'markers',
            color = "white",
            
          ) %>%
            layout(
              yaxis = list(
                showexponent = "all",
                exponentformat = 'E',
                title = "Log<sub>10</sub> CFU/mL"
              ),
              xaxis = list(title = "Time (h)")
            )
        })
        
        #Make dataframe reactive to changes
        outputValues = reactiveValues(data = preset_df)
        
        
        colHeaders = function() {
          input_count = 1
          headers = character(14)
          headers[1] = "Time (h)"
          for (i in seq(from = 2,
                        to = 14,
                        by = 2)) {
            headers[i] = paste0(input[[paste0("model2ColHeader",
                                              input_count)]], " CFU/mL")
            headers[i + 1] = paste0(input[[paste0("model2ColHeader", 
                                                  input_count)]], " DDAC (mg/L)")
            input_count  = input_count  + 1
          }
          return(headers)
        }
        
        
        
        #outputValues$data=hot_to_r(input$table2)
        
        #Create a handsontable with the dataframe content, using inputs values
        #to set colheaders
        output$model2_mult_table = renderRHandsontable({
          rhandsontable(outputValues$data,
                        maxRows = 100,
                        colHeaders = colHeaders()) %>%
            hot_validate_numeric(col = c(1:13), min = 0)
        })
        
        
        #A loop that observe changes in inputs values and set col names in df 
        #and handsontable
        for (i in 1:6) {
          observeEvent(input[[paste0("model2ColHeader", i)]], {
            colnames(preset_df)[i] = input[[paste0("model2ColHeader", i)]]
          })
        }
        
        
        #Apply changes in handsontable to dataframe when clicking in Run
        observeEvent(input$model2RunMultiple,
                     {
                       outputValues$data = hot_to_r(input$model2_mult_table)
                       colHeaders = colHeaders()
                       
                       
                       # empty_cols = emptyColumns(outputValues$data)
                       # print(paste0("Columnas valeiras: ", empty_cols))
                       # otherdata = subset(outputValues$data, select = -c(1))
                       
                       # print(otherdata)
                       # print(ncol(outputValues$data))
                       # print(ncol(otherdata))
                       validation = dataValidatorMultiple(outputValues$data)
                       validation_msg = validation[[1]]
                       validated_subsetDF = validation[[2]]
                       #print(paste0("En app: ", validation_msg))
                       print(validated_subsetDF)
                       #getEmptyColumns(outputValues$data)
                       
                       if (validation_msg == "OK") {
                         # Packing equation parameters in a vector
                         p1 = input$ka0
                         p2 = input$kg0
                         p3 = input$kd0
                         p4 = input$kd1
                         p5 = input$ki
                         p6 = input$IC50_a
                         p7 = input$IC50_g
                         p8 = input$EC50_d
                         p9 = input$gamma_a
                         p10 = input$gamma_g
                         p11 = input$gamma_d
                         params = c(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)
                         
                         #Get discretization times value
                         discTimes = input$model2DiscTimes
                         
                         #Plot results
                         output$model2PlotSingle = renderPlotly({
                           #Initializate a scatter plot
                           p = plot_ly(type = "scatter", mode = "markers")
                           
                           #Set counters for loop
                           l = 2
                           exp_name = 2
                           color = 1
                           
                           #Create a palette with 10 colours for traces and lines
                           plotColors = c(
                             "#404040",
                             "#ffce00",
                             "#619cff",
                             "#e69f00",
                             "#cc79a7",
                             "#78ac44",
                             "#d55e00",
                             "#0072b2",
                             "#751056",
                             "#01665e"
                           )
                           
                           
                           #Unlist Time col from dataframe
                           time_exp = unlist(validated_subsetDF[1])
                           
                           #Iterate over dataframe and dinamically add traces a
                           #and lines to plot
                           while (l < (length(validated_subsetDF) + 1)) {
                             y_exp = unlist(validated_subsetDF[l])
                             drug_conc = unlist(validated_subsetDF[l + 1])
                             
                             #Call model function. Save model output in two vectors
                             submic_ddac_func = submic_ddac(time_exp, drug_conc,
                                                            params, y_exp,
                                                            discTimes)
                             time_mod = submic_ddac_func[[1]]
                             y_mod = log10(submic_ddac_func[[2]])
                             #Get col name from dataframe for include in the 
                             #legend, remove the "CFU/mL" part
                             currentColName = colHeaders[exp_name]
                             legendName =  substr(currentColName, 1,
                                                  nchar(currentColName) - 6)
                             # Trace plot. Markers depict experimental points
                             # while lines model prediction
                             
                             p = add_trace(
                               p,
                               x = unlist(validated_subsetDF[1]),
                               y = log10(y_exp),
                               mode = "markers",
                               name = paste0(legendName, " (observed)"),
                               marker = list(color = plotColors [color])
                             )
                             p = add_lines(
                               p,
                               x = time_mod ,
                               y = y_mod,
                               mode = "line",
                               name = paste0(legendName, " (expected)"),
                               line = list(color = plotColors[color])
                             )
                             l = l + 2
                             color = color + 1
                             exp_name =  exp_name + 2
                           }
                           p %>%
                             layout(
                               yaxis = list(
                                 showexponent = "all",
                                 exponentformat = 'E',
                                 title = "Log<sub>10</sub> CFU/mL"
                               ),
                               xaxis = list(title = "Time (h)")
                             )
                         })
                       }
                     })
        
        
        #Reset dataframe, handsontable and plot to default when clicking
        #"Reset" button
        observeEvent(input$model2ResetMultiple,
                     {
                       outputValues$data = preset_df
                       output$model2_mult_table = renderRHandsontable({
                         rhandsontable(outputValues$data, maxRows = 100) %>%
                           hot_validate_numeric(col = c(1, 2, 3), min = 0)
                       })
                       output$model2_mult_table = renderRHandsontable({
                         rhandsontable(
                           outputValues$data,
                           maxRows = 100,
                           colHeaders = c(
                             "Time (h)",
                             "Control CFU/mL",
                             "Control DDAC (mg/L)",
                             "Exp#2 CFU/mL",
                             "Exp#2 DDAC (mg/L)",
                             "Exp#3 CFU/mL",
                             "Exp#3 DDAC (mg/L)",
                             "Exp#4 CFU/mL",
                             "Exp#4 DDAC (mg/L)",
                             "Exp#5 CFU/mL",
                             "Exp#5 DDAC (mg/L)",
                             "Exp#6 CFU/mL",
                             "Exp#6 DDAC (mg/L)"
                           )
                         ) %>%
                           hot_validate_numeric(col = c(1, 2, 3), min = 0)
                       })
                       
                       
                       simpleDataDDAC = simpleDataDDAC(input$model2StrainID)
                       updateNumericInput(session, "ka0",
                                          value = simpleDataDDAC[4])
                       updateNumericInput(session, "kg0", 
                                          value = simpleDataDDAC[5])
                       updateNumericInput(session, "kd0", 
                                          value = simpleDataDDAC[6])
                       updateNumericInput(session, "kd1", 
                                          value = simpleDataDDAC[7])
                       updateNumericInput(session, "ki", 
                                          value = simpleDataDDAC[8])
                       updateNumericInput(session, "IC50_a",
                                          value = simpleDataDDAC[9])
                       updateNumericInput(session, "IC50_g", 
                                          value = simpleDataDDAC[10])
                       updateNumericInput(session, "EC50_d", 
                                          value = simpleDataDDAC[11])
                       updateNumericInput(session, "gamma_a",
                                          value = simpleDataDDAC[12])
                       updateNumericInput(session, "gamma_g", 
                                          value = simpleDataDDAC[13])
                       updateNumericInput(session, "gamma_d",
                                          value = simpleDataDDAC[14])
                       updateSliderInput(session, "model2DiscTimes",
                                         value = 100)
                       
                       
                       #Render empty plot frame
                       output$model2PlotSingle = renderPlotly({
                         plot_ly(
                           x = c(0),
                           y = c(0),
                           type = 'scatter',
                           mode = 'markers',
                           color = "white"
                         ) %>%
                           layout(
                             yaxis = list(
                               showexponent = "all",
                               exponentformat = 'E',
                               title = "Log<sub>10</sub> CFU/mL"
                             ),
                             xaxis = list(title = "Time (h)")
                           )
                       })
                     })
      }
    })
  })
}
