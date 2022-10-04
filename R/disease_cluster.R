disease_cluster_ui <- fluidPage(
  textInput("text_disease_cluster", "What's your name?", ""),
  textOutput("text_disease_cluster"),
  verbatimTextOutput("code_disease_cluster")
)

disease_cluster_server <- function(input, output, session) {
  output$text_disease_cluster <- renderText({
    input$text_disease_cluster
  })
  output$code_disease_cluster <- renderPrint({
    summary(1:10)
  })

  # render.table <- eventReactive(input$submit_table, {})
  # observeEvent(input$submit_plot, {})
  # observeEvent(input$submit_cluster, {})
}
