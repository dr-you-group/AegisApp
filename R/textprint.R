textprint_ui <- fluidPage(
  textInput("text", "What's your name?", ""),
  textOutput("text"),
  verbatimTextOutput("code")
)

textprint_server <- function(input, output, session) {
  output$text <- renderText({
    input$text
  })
  output$code <- renderPrint({
    summary(1:10)
  })
}
