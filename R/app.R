library(shiny)
library(AegisFunc)

AegisApp <- function(...) {
  months <- c(
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"
  )
  animals <- c("dog", "cat", "mouse", "bird", "other", "I hate animals")

  ui <- navbarPage(
    title = "AegisApp",
    tabPanel(
      title = "Func_input",
      selectInput("month", "What's your favourite month?", choices = months)
    ),
    tabPanel(
      title = "Func_feedback",
      func_ui("feed")
    ),
    tabPanel(
      title = "Inputs",
      selectInput("month", "What's your favourite month?", choices = months),
      textInput("name", "What's your name?", ""),
      passwordInput("password", "What's your password?"),
      textAreaInput("story", "Tell me about yourself", rows = 3),
      numericInput("num", "Number one", value = 0, min = 0, max = 100),
      sliderInput("num2", "Number two", value = 50, min = 0, max = 100),
      sliderInput("rng", "Range", value = c(10, 20), min = 0, max = 100),
      dateInput("dob", "When were you born?"),
      dateRangeInput("holiday", "When do you want to go on vacation next?"),
      selectInput("state", "What's your favourite state?", state.name),
      radioButtons("animal", "What's your favourite animal?", animals),
      radioButtons("rb", "Choose one:",
                   choiceNames = list(
                     icon("angry"),
                     icon("smile"),
                     icon("sad-tear")
                   ),
                   choiceValues = list("angry", "happy", "sad")
      ),
      selectInput("state", "What's your favourite state?", state.name, multiple = TRUE),
      checkboxGroupInput("animal", "What animals do you like?", animals),
      checkboxInput("cleanup", "Clean up?", value = TRUE),
      checkboxInput("shutdown", "Shutdown?"),
      actionButton("click", "Click me!"),
      actionButton("drink", "Drink me!", icon = icon("cocktail"))
    ),
    tabPanel(
      title = "Text",
      text_ui("text")
    ),
    tabPanel(
      title = "Table",
      table_ui("table")
    ),
    tabPanel(
      title = "Plot",
      plot_ui("plot")
    ),
    tabPanel(
      title = "Text Print",
      textprint_ui
    # ),
    # tabPanel(
    #   title = "Get cohort",
    #   cohort_menu,
    #   parameter_tabs
    # ),
    # tabPanel(
    #   title = "Disease Map",
    #   plot_ui("plot_map")
    # ),
    # tabPanel(
    #   title = "Disease Cluster",
    #   plot_ui("plot_cluster")
    )
  )

  server <- function(input, output, session) {
    func_server("feed", reactive(input$month))

    text_server("text")
    table_server("table")
    plot_server("plot")

    textprint_server(input, output, session)

    # plot_server("plot_map")
    # plot_server("plot_cluster")
  }

  shinyApp(ui, server, ...)
}
