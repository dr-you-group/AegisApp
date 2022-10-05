disease_cluster_ui <- fluidPage(s
  plotOutput("disease_cluster", width = "400px")
)

disease_cluster_server <- function(input, output, session) {

  # table <- eventReactive(input$get_table, {...})
  # output$table <- renderDataTable(table())
  # plot <- eventReactive(input$get_plot, {...})
  # output$plot <- renderPlot(plot())
  # observeEvent(input$do_any, {})
}
