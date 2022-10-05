disease_map_ui <- fluidPage(
  plotOutput("disease_map", width = "400px")
)

disease_map_server <- function(input, output, session) {

  # table <- eventReactive(input$get_table, {...})
  # output$table <- renderDataTable(table())
  # plot <- eventReactive(input$get_plot, {...})
  # output$plot <- renderPlot(plot())
  # observeEvent(input$do_any, {})
}
