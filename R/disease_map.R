disease_map_ui <- shiny::fluidPage(
  shiny::titlePanel(
    # app title/description
    "Plot disease map"
  ),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      # inputs
      shiny::selectInput("map_color_type", "Color type", choices = c("colorQuantile", "colorBin", "colorNumeric", "colorFactor")),
      shiny::conditionalPanel(
        condition = "input.map_color_type == 'colorQuantile'",
        shiny::selectInput("map_palette", "Palette", choices = c("Reds", "Greens")),
        shiny::textInput("map_domain", "Domain", value = ""),
        shiny::numericInput("map_n", "n", value = 9, min = 1, max = 9),
        shiny::textInput("map_na_color", "NA color", value = "#FFFFFF"),
        shiny::selectInput("map_alpha", "Alpha", choices = c(TRUE, FALSE), selected = FALSE),
        shiny::selectInput("map_reverse", "Reverse", choices = c(TRUE, FALSE), selected = FALSE),
        shiny::selectInput("map_right", "Right", choices = c(TRUE, FALSE), selected = FALSE)
      ),
      shiny::conditionalPanel(
        condition = "input.map_color_type == 'colorBin'",
        shiny::selectInput("map_palette", "Palette", choices = c("Reds", "Greens")),
        shiny::textInput("map_domain", "Domain", value = ""),
        shiny::numericInput("map_bins", "Bins", value = 7, min = 1, max = 9),
        shiny::selectInput("map_pretty", "Pretty", choices = c(TRUE, FALSE), selected = TRUE),
        shiny::textInput("map_na_color", "NA color", value = "#FFFFFF"),
        shiny::selectInput("map_alpha", "Alpha", choices = c(TRUE, FALSE), selected = FALSE),
        shiny::selectInput("map_reverse", "Reverse", choices = c(TRUE, FALSE), selected = FALSE),
        shiny::selectInput("map_right", "Right", choices = c(TRUE, FALSE), selected = FALSE)
      ),
      shiny::conditionalPanel(
        condition = "input.map_color_type == 'colorNumeric'",
        shiny::selectInput("map_palette", "Palette", choices = c("Reds", "Greens")),
        shiny::textInput("map_domain", "Domain", value = ""),
        shiny::textInput("map_na_color", "NA color", value = "#FFFFFF"),
        shiny::selectInput("map_alpha", "Alpha", choices = c(TRUE, FALSE), selected = FALSE),
        shiny::selectInput("map_reverse", "Reverse", choices = c(TRUE, FALSE), selected = FALSE)
      ),
      shiny::conditionalPanel(
        condition = "input.map_color_type == 'colorFactor'",
        shiny::selectInput("map_palette", "Palette", choices = c("Reds", "Greens")),
        shiny::textInput("map_domain", "Domain", value = ""),
        shiny::textInput("map_levels", "Levels", value = ""),
        shiny::selectInput("map_ordered", "Ordered", choices = c(TRUE, FALSE), selected = FALSE),
        shiny::textInput("map_na_color", "NA color", value = "#FFFFFF"),
        shiny::selectInput("map_alpha", "Alpha", choices = c(TRUE, FALSE), selected = FALSE),
        shiny::selectInput("map_reverse", "Reverse", choices = c(TRUE, FALSE), selected = FALSE)
      ),
      shiny::actionButton("print_disease_map", "Print"),
      shiny::actionButton("plot_disease_map", "Plot disease map")
    ),
    shiny::mainPanel(
      # outputs
      leaflet::leafletOutput("disease_map"),
      shinyjs::hidden(
        htmltools::p(id = "work_disease_map", "Processing...")
      )
    )
  )
)

disease_map_server <- function(input, output, session, transfer) {
  shiny::observeEvent(input$print_disease_map, {
    params <- list()
    params$model <- input$model
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

  shiny::observe({
    shinyjs::disable("plot_disease_map")

    if (sum(match(input$model, c("spatial", "spatio-temporal"), nomatch = 0)) > 0 &
        length(transfer$table_adj()) > 0 &
        length(transfer$geo()@data) > 0) {
      shinyjs::enable("plot_disease_map")
    }
  })

  disease_map <- shiny::eventReactive(input$plot_disease_map, {
    shinyjs::disable("plot_disease_map")
    shinyjs::show("work_disease_map")

    # Generate graph file
    geo <- transfer$geo()

    graph_file_path <- AegisFunc::trans_geo_to_graph(
      geo = geo
    )


    # Calculate disease map
    model <- input$model
    table <- transfer$table_adj()
    graph_file_path <- graph_file_path

    deriv_m <- AegisFunc::calculate_disease_map(
      model = model,
      table = table,
      graph_file_path = graph_file_path
    )


    # Merge geo data with derivatives
    geo <- transfer$geo()
    deriv_m_arr <- deriv_m$arranged_table

    data_m <- AegisFunc::merge_geo_with_deriv(
      geo = geo,
      deriv = deriv_m_arr
    )


    # Plot disease map
    data <- data_m
    stats <- deriv_m$stats
    color_type <- input$map_color_type
    color_param <- base::list(
      palette = input$map_palette,
      domain = if(trimws(input$map_domain) == ""){NULL}else{trimws(input$map_domain)},
      bins = as.numeric(input$map_bins),
      pretty = as.logical(input$map_pretty),
      n = as.numeric(input$map_n),
      levels = if(trimws(input$map_levels) == ""){NULL}else{trimws(input$map_levels)},
      ordered = as.logical(input$map_ordered),
      na.color = if(trimws(input$map_na_color) == ""){"#FFFFFF"}else{trimws(input$map_na_color)},
      alpha = as.logical(input$map_alpha),
      reverse = as.logical(input$map_reverse),
      right = as.logical(input$map_right)
    )

    plot_m <- AegisFunc::get_leaflet_map(
      data = data,
      stats = stats,
      color_type = color_type,
      color_param = color_param
    )

    shinyjs::hide("work_disease_map")
    shinyjs::enable("plot_disease_map")

    plot_m
  })

  output$disease_map <- leaflet::renderLeaflet(
    disease_map()
  )
}
