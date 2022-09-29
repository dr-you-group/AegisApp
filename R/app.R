library(shiny)
library(AegisFunc)

AegisApp <- function(...) {
  ui <- navbarPage(
    title = "AegisApp",

    # tabPanel(
    #   title = "Inputs",
    #   selectInput("select1", "What's your favourite month?", choices = months),
    #   selectInput("select2", "What's your favourite state?", state.name),
    #   selectInput("select3", "What's your favourite state?", state.name, multiple = TRUE),
    #   textInput("text1", "What's your name?", ""),
    #   passwordInput("password1", "What's your password?"),
    #   textAreaInput("textArea1", "Tell me about yourself", rows = 3),
    #   numericInput("numeric1", "Number one", value = 0, min = 0, max = 100),
    #   sliderInput("slider1", "Number two", value = 50, min = 0, max = 100),
    #   sliderInput("slider2", "Range", value = c(10, 20), min = 0, max = 100),
    #   dateInput("date1", "When were you born?"),
    #   dateRangeInput("dateRange1", "When do you want to go on vacation next?"),
    #   radioButtons("radio1", "What's your favourite animal?", animals),
    #   radioButtons("radio2", "Choose one:",
    #                choiceNames = list(
    #                  icon("angry"),
    #                  icon("smile"),
    #                  icon("sad-tear")
    #                ),
    #                choiceValues = list("angry", "happy", "sad")
    #   ),
    #   checkboxGroupInput("checkboxGroup1", "What animals do you like?", animals),
    #   checkboxInput("checkbox1", "Clean up?", value = TRUE),
    #   checkboxInput("checkbox2", "Shutdown?"),
    #   actionButton("action1", "Click me!"),
    #   actionButton("action2", "Drink me!", icon = icon("cocktail"))
    # ),
    # tabPanel(
    #   title = "Text",
    #   text_ui("text")
    # ),
    # tabPanel(
    #   title = "Table",
    #   table_ui("table")
    # ),
    # tabPanel(
    #   title = "Plot",
    #   plot_ui("plot")
    # ),
    # tabPanel(
    #   title = "Func_input",
    #   selectInput("month", "What's your favourite month?", choices = months)
    # ),
    # tabPanel(
    #   title = "Func_feedback",
    #   func_ui("feed")
    # ),
    # tabPanel(
    #   title = "Text Print",
    #   textprint_ui
    # ),

    tabPanel(
      title = "Get cohort",
      cohort_ui
    ),
    tabPanel(
      title = "Disease Map",
      disease_map_ui
    ),
    tabPanel(
      title = "Disease Cluster",
      disease_cluster_ui
    )
  )

  server <- function(input, output, session) {
    # text_server("text")
    # table_server("table")
    # plot_server("plot")
    # func_server("feed", reactive(input$month))
    # textprint_server(input, output, session)

    cohort_server(input, output, session)
    disease_map_server(input, output, session)
    disease_cluster_server(input, output, session)
  }

  shinyApp(ui, server, ...)
}
