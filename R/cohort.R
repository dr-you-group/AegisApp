months <- c(
  "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December"
)
animals <- c("dog", "cat", "mouse", "bird", "other", "I hate animals")

cohort_ui <- fluidPage(
  textInput("text_cohort", "What's your name?", ""),
  textOutput("text_cohort"),
  # Inputs
  selectInput("select1", "What's your favourite month?", choices = months),
  selectInput("select2", "What's your favourite state?", state.name),
  selectInput("select3", "What's your favourite state?", state.name, multiple = TRUE),
  textInput("text1", "What's your name?", ""),
  passwordInput("password1", "What's your password?"),
  textAreaInput("textArea1", "Tell me about yourself", rows = 3),
  numericInput("numeric1", "Number one", value = 0, min = 0, max = 100),
  sliderInput("slider1", "Number two", value = 50, min = 0, max = 100),
  sliderInput("slider2", "Range", value = c(10, 20), min = 0, max = 100),
  dateInput("date1", "When were you born?"),
  dateRangeInput("dateRange1", "When do you want to go on vacation next?"),
  radioButtons("radio1", "What's your favourite animal?", animals),
  radioButtons("radio2", "Choose one:",
               choiceNames = list(
                 icon("angry"),
                 icon("smile"),
                 icon("sad-tear")
               ),
               choiceValues = list("angry", "happy", "sad")
  ),
  checkboxGroupInput("checkboxGroup1", "What animals do you like?", animals),
  checkboxInput("checkbox1", "Clean up?", value = TRUE),
  checkboxInput("checkbox2", "Shutdown?"),
  actionButton("action1", "Click me!"),
  actionButton("action2", "Drink me!", icon = icon("cocktail"))
)

cohort_server <- function(input, output, session) {
  output$text_cohort <- renderText({
    input$text_cohort
  })
}

# parameter_tabs <- tabsetPanel(
#   id = "params",
#   type = "hidden",
#   tabPanel("normal",
#            numericInput("mean", "mean", value = 1),
#            numericInput("sd", "standard deviation", min = 0, value = 1)
#   ),
#   tabPanel("uniform",
#            numericInput("min", "min", value = 0),
#            numericInput("max", "max", value = 1)
#   ),
#   tabPanel("exponential",
#            numericInput("rate", "rate", value = 1, min = 0),
#   )
# )
