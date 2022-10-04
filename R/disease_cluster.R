disease_cluster_ui <- fluidPage(
  actionButton("plot_disease_cluster", "Plot disease cluster"),

  plotOutput("disease_cluster", width = "400px")
)

disease_cluster_server <- function(input, output, session) {
  disease_cluster <- eventReactive(input$plot_disease_cluster, {
    plot(6:10)
  })

  output$disease_cluster <- renderPlot(
    disease_cluster(),
    res = 96
  )

  # table <- eventReactive(input$get_table, {...})
  # output$table <- renderDataTable(table())
  # plot <- eventReactive(input$get_plot, {...})
  # output$plot <- renderPlot(plot())
  # observeEvent(input$do_any, {})
}
