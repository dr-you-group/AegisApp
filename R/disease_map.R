disease_map_ui <- fluidPage(
  titlePanel(
    # app title/description
    "Plot disease map"
  ),
  sidebarLayout(
    sidebarPanel(
      # inputs
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
      actionButton("plot_disease_map", "Plot disease map")
    ),
    mainPanel(
      # outputs
      leafletOutput("disease_map", width = "400px"),
      hidden(
        p(id = "work_disease_map", "Processing...")
      )
    )
  )
)

disease_map_server <- function(input, output, session, transfer) {
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

  observe({
    disable("plot_disease_map")

    if (length(transfer$table_adj()) > 0 & length(transfer$geo()@data) > 0) {
      enable("plot_disease_map")
    }
  })

  disease_map <- eventReactive(input$plot_disease_map, {
    disable("plot_disease_map")
    show("work_disease_map")

    # Generate graph file
    geo <- transfer$geo()

    graph_file_path <- trans_geo_to_graph(
      geo = geo
    )


    # Calculate disease map
    table <- transfer$table_adj()
    graph_file_path <- graph_file_path

    deriv <- calculate_disease_map(
      table = table,
      graph_file_path = graph_file_path
    )


    # Merge geo data with derivatives
    geo <- transfer$geo()
    deriv <- deriv$arranged_table

    data <- merge_geo_with_deriv(
      geo = geo,
      deriv = deriv
    )


    # Plot disease map
    data <- data
    stats <- deriv$stats
    color_type <- input$map_color_type
    color_param <- base::list(
      palette = input$map_palette,
      domain = if(trimws(input$map_domain) == ""){NULL},
      bins = as.numeric(input$map_bins),
      pretty = as.logical(input$map_pretty),
      n = as.numeric(input$map_n),
      levels = if(trimws(input$map_levels) == ""){NULL},
      ordered = as.logical(input$map_ordered),
      na.color = if(trimws(input$map_na_color) == ""){"#808080"},
      alpha = as.logical(input$map_alpha),
      reverse = as.logical(input$map_reverse),
      right = as.logical(input$map_right)
    )

    plot <- get_leaflet_map(
      data = data,
      stats = stats,
      color_type = color_type,
      color_param = color_param
    )

    hide("work_disease_map")
    enable("plot_disease_map")

    plot
  })

  output$disease_map <- renderLeaflet(
    disease_map()
  )
}
