disease_map_ui <- fluidPage(
  actionButton("plot_disease_map", "Plot disease map"),

  plotOutput("disease_map", width = "400px")
)

disease_map_server <- function(input, output, session) {
  disease_map <- eventReactive(input$plot_disease_map, {
    plot(1:5)
  })

  output$disease_map <- renderPlot(
    disease_map(),
    res = 96
  )

  # table <- eventReactive(input$get_table, {...})
  # output$table <- renderDataTable(table())
  # plot <- eventReactive(input$get_plot, {...})
  # output$plot <- renderPlot(plot())
  # observeEvent(input$do_any, {})
}
