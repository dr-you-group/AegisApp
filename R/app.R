library(shiny)

AegisApp <- function(...) {
  months <- c(
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"
  )

  ui <- navbarPage(
    "AegisApp",
    tabPanel(
      "Pick a month",
      selectInput("month", "What's your favourite month?", choices = months)
    ),
    tabPanel("Feedback", func1_UI("tab1"))
  )

  server <- function(input, output, session) {
    func1_Server("tab1", reactive(input$month))
  }

  shinyApp(ui, server, ...)
}
