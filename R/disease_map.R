disease_map_ui <- fluidPage(
  selectInput("map_color_type", "Color type", choices = c("colorQuantile", "colorBin", "colorNumeric", "colorFactor")),
  conditionalPanel(
    condition = "input.map_color_type == 'colorQuantile'",

    selectInput("map_palette", "Palette", choices = c("Reds", "Greens")),
    textInput("map_domain", "Domain", value = ""),
    numericInput("map_n", "n", value = 9, min = 1, max = 9),
    textInput("map_na_color", "NA color", value = "#808080"),
    selectInput("map_alpha", "Alpha", choices = c(TRUE, FALSE), selected = FALSE),
    selectInput("map_reverse", "Reverse", choices = c(TRUE, FALSE), selected = FALSE),
    selectInput("map_right", "Right", choices = c(TRUE, FALSE), selected = FALSE)
  ),
  conditionalPanel(
    condition = "input.map_color_type == 'colorBin'",

    selectInput("map_palette", "Palette", choices = c("Reds", "Greens")),
    textInput("map_domain", "Domain", value = ""),
    numericInput("map_bins", "Bins", value = 7, min = 1, max = 9),
    selectInput("map_pretty", "Pretty", choices = c(TRUE, FALSE), selected = TRUE),
    textInput("map_na_color", "NA color", value = "#808080"),
    selectInput("map_alpha", "Alpha", choices = c(TRUE, FALSE), selected = FALSE),
    selectInput("map_reverse", "Reverse", choices = c(TRUE, FALSE), selected = FALSE),
    selectInput("map_right", "Right", choices = c(TRUE, FALSE), selected = FALSE)
  ),
  conditionalPanel(
    condition = "input.map_color_type == 'colorNumeric'",

    selectInput("map_palette", "Palette", choices = c("Reds", "Greens")),
    textInput("map_domain", "Domain", value = ""),
    textInput("map_na_color", "NA color", value = "#808080"),
    selectInput("map_alpha", "Alpha", choices = c(TRUE, FALSE), selected = FALSE),
    selectInput("map_reverse", "Reverse", choices = c(TRUE, FALSE), selected = FALSE)
  ),
  conditionalPanel(
    condition = "input.map_color_type == 'colorFactor'",

    selectInput("map_palette", "Palette", choices = c("Reds", "Greens")),
    textInput("map_domain", "Domain", value = ""),
    textInput("map_levels", "Levels", value = ""),
    selectInput("map_ordered", "Ordered", choices = c(TRUE, FALSE), selected = FALSE),
    textInput("map_na_color", "NA color", value = "#808080"),
    selectInput("map_alpha", "Alpha", choices = c(TRUE, FALSE), selected = FALSE),
    selectInput("map_reverse", "Reverse", choices = c(TRUE, FALSE), selected = FALSE)
  ),

  actionButton("print_disease_map", "Print"),
  actionButton("plot_disease_map", "Plot disease map"),

  plotOutput("disease_map", width = "400px")
)

disease_map_server <- function(input, output, session) {
  observeEvent(input$print_disease_map, {
    params <- list()
    params$color_type <- input$map_color_type
    params$palette <- input$map_palette
    params$domain <- input$map_domain
    params$bins <- input$map_bins
    params$pretty <- input$map_pretty
    params$n <- input$map_n
    params$levels <- input$map_levels
    params$ordered <- input$map_ordered
    params$na_color <- input$map_na_color
    params$alpha <- input$map_alpha
    params$reverse <- input$map_reverse
    params$right <- input$map_right

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
    param$color$type <- input$map_color_type
    param$color$param <- base::list(
      palette <- input$map_palette,
      domain <- input$map_domain,
      bins <- as.numeric(input$map_bins),
      pretty <- input$map_pretty,
      n <- as.numeric(input$map_n),
      levels <- input$map_levels,
      ordered <- input$map_ordered,
      na.color <- input$map_na_color,
      alpha <- input$map_alpha,
      reverse <- input$map_reverse,
      right <- input$map_right,
    )

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
