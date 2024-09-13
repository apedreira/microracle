openModal = function(id) {
  showModal(
    modalDialog(
      id = "miModal",
      title = "Data input",
      footer = modalButton("Close"),
      easyClose = TRUE,
      size = "l",
      fluidRow(
        column(width = 12,
               rHandsontableOutput("table2", height = "400px"),
               column(width=6,
                      textInput("colHeader1", "Header 1:", value = "Exp1"),
                      textInput("colHeader2", "Header 2:", value = "Exp2"),
                      textInput("colHeader3", "Header 3:", value = "Exp3")  ),
               column(width=6,
                      textInput("colHeader4", "Header 4:", value = "Exp4"),
                      textInput("colHeader5", "Header 5:", value = "Exp5"),
                      textInput("colHeader6", "Header 6:", value = "Exp6"))
        )
      )
    )
  )
}