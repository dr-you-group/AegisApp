disease_cluster_ui <- fluidPage(
  actionButton("print_disease_cluster", "Print"),
  actionButton("plot_disease_cluster", "Plot disease cluster"),

  plotOutput("disease_cluster", width = "400px")
)

disease_cluster_server <- function(input, output, session) {
  observeEvent(input$print_disease_cluster, {
    params <- list()

    message("disease_cluster_params: ", toString(params))

  })

  disease_cluster <- eventReactive(input$plot_disease_cluster, {
    # Calculate disease cluster
    param <- base::list()
    param$table <- table_adj()

    deriv <- calculate_disease_cluster(param)


    # Merge geo data with derivatives
    param <- base::list()
    param$geo <- geo()
    param$deriv <- deriv$arranged_table

    data <- merge_geo_with_deriv(param)


    # Plot disease map
    param <- base::list()
    param$data <- data
    param$stats <- deriv$stats

    plot <- get_leaflet_map(param)

    plot
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
