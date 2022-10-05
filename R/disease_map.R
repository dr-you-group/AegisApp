disease_map_ui <- fluidPage(
  actionButton("print_disease_map", "Print"),
  actionButton("plot_disease_map", "Plot disease map"),

  plotOutput("disease_map", width = "400px")
)

disease_map_server <- function(input, output, session) {
  observeEvent(input$print_disease_map, {
    params <- list()

    message("disease_map_params: ", toString(params))

  })

  disease_map <- eventReactive(input$plot_disease_map, {
    # Generate graph file
    param <- base::list()
    param$geo <- geo()

    graph_file_path <- trans_geo_to_graph(param)


    # Calculate disease map
    param <- base::list()
    param$table <- table_adj()
    param$graph_file_path <- graph_file_path

    deriv <- calculate_disease_map(param)


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
