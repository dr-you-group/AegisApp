text_ui <- function(id) {
  textOutput(NS(id, "text"))
  verbatimTextOutput(NS(id, "print"))
}

text_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$text <- renderText("hello!")
    output$print <- renderPrint("hello!")
  })
}
