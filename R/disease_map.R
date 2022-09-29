disease_map_ui <- fluidPage(
  textInput("text_disease_map", "What's your name?", ""),
  textOutput("text_disease_map"),
  verbatimTextOutput("code_disease_map")
)

disease_map_server <- function(input, output, session) {
  output$text_disease_map <- renderText({
    input$text_disease_map
  })
  output$code_disease_map <- renderPrint({
    summary(1:10)
  })
}
