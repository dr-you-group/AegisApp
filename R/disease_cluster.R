disease_cluster_ui <- shiny::fluidPage(
  shiny::titlePanel(
    # app title/description
    "Plot disease cluster"
  ),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      # inputs
      shiny::selectInput("cluster_color_type", "Color type", choices = c("colorQuantile", "colorBin", "colorNumeric", "colorFactor")),
      shiny::conditionalPanel(
        condition = "input.cluster_color_type == 'colorQuantile'",
        shiny::selectInput("cluster_palette", "Palette", choices = c("Reds", "Greens")),
        shiny::textInput("cluster_domain", "Domain", value = ""),
        shiny::numericInput("cluster_n", "n", value = 1, min = 1, max = 9),
        shiny::textInput("cluster_na_color", "NA color", value = "#808080"),
        shiny::selectInput("cluster_alpha", "Alpha", choices = c(TRUE, FALSE), selected = FALSE),
        shiny::selectInput("cluster_reverse", "Reverse", choices = c(TRUE, FALSE), selected = FALSE),
        shiny::selectInput("cluster_right", "Right", choices = c(TRUE, FALSE), selected = FALSE)
      ),
      shiny::conditionalPanel(
        condition = "input.cluster_color_type == 'colorBin'",
        shiny::selectInput("cluster_palette", "Palette", choices = c("Reds", "Greens")),
        shiny::textInput("cluster_domain", "Domain", value = ""),
        shiny::numericInput("cluster_bins", "Bins", value = 7, min = 1, max = 9),
        shiny::selectInput("cluster_pretty", "Pretty", choices = c(TRUE, FALSE), selected = TRUE),
        shiny::textInput("cluster_na_color", "NA color", value = "#808080"),
        shiny::selectInput("cluster_alpha", "Alpha", choices = c(TRUE, FALSE), selected = FALSE),
        shiny::selectInput("cluster_reverse", "Reverse", choices = c(TRUE, FALSE), selected = FALSE),
        shiny::selectInput("cluster_right", "Right", choices = c(TRUE, FALSE), selected = FALSE)
      ),
      shiny::conditionalPanel(
        condition = "input.cluster_color_type == 'colorNumeric'",
        shiny::selectInput("cluster_palette", "Palette", choices = c("Reds", "Greens")),
        shiny::textInput("cluster_domain", "Domain", value = ""),
        shiny::textInput("cluster_na_color", "NA color", value = "#808080"),
        shiny::selectInput("cluster_alpha", "Alpha", choices = c(TRUE, FALSE), selected = FALSE),
        shiny::selectInput("cluster_reverse", "Reverse", choices = c(TRUE, FALSE), selected = FALSE)
      ),
      shiny::conditionalPanel(
        condition = "input.cluster_color_type == 'colorFactor'",
        shiny::selectInput("cluster_palette", "Palette", choices = c("Reds", "Greens")),
        shiny::textInput("cluster_domain", "Domain", value = ""),
        shiny::textInput("cluster_levels", "Levels", value = ""),
        shiny::selectInput("cluster_ordered", "Ordered", choices = c(TRUE, FALSE), selected = FALSE),
        shiny::textInput("cluster_na_color", "NA color", value = "#808080"),
        shiny::selectInput("cluster_alpha", "Alpha", choices = c(TRUE, FALSE), selected = FALSE),
        shiny::selectInput("cluster_reverse", "Reverse", choices = c(TRUE, FALSE), selected = FALSE)
      ),
      shiny::actionButton("print_disease_cluster", "Print"),
      shiny::actionButton("plot_disease_cluster", "Plot disease cluster")
    ),
    shiny::mainPanel(
      # outputs
      leaflet::leafletOutput("disease_cluster", width = "400px"),
      shinyjs::hidden(
        htmltools::p(id = "work_disease_cluster", "Processing...")
      )
    )
  )
)

disease_cluster_server <- function(input, output, session, transfer) {
  shiny::observeEvent(input$print_disease_cluster, {
    params <- list()
    params$color_type <- input$cluster_color_type
    params$palette <- input$cluster_palette
    params$domain <- input$cluster_domain
    params$bins <- input$cluster_bins
    params$pretty <- input$cluster_pretty
    params$n <- input$cluster_n
    params$levels <- input$cluster_levels
    params$ordered <- input$cluster_ordered
    params$na_color <- input$cluster_na_color
    params$alpha <- input$cluster_alpha
    params$reverse <- input$cluster_reverse
    params$right <- input$cluster_right

    message("disease_cluster_params: ", toString(params))
  })

  shiny::observe({
    shinyjs::disable("plot_disease_cluster")

    if (length(transfer$table_adj()) > 0 & length(transfer$geo()@data) > 0) {
      shinyjs::enable("plot_disease_cluster")
    }
  })

  disease_cluster <- shiny::eventReactive(input$plot_disease_cluster, {
    shinyjs::disable("plot_disease_cluster")
    shinyjs::show("work_disease_cluster")

    # Calculate disease cluster
    table <- transfer$table_adj()

    deriv <- AegisFunc::calculate_disease_cluster(
      table = table
    )
    message("len deriv: ", toString(length(deriv)))


    # Merge geo data with derivatives
    geo <- transfer$geo()
    deriv_arr <- deriv$arranged_table

    data <- AegisFunc::merge_geo_with_deriv(
      geo = geo,
      deriv = deriv_arr
    )
    message("len geo: ", toString(length(geo)))
    message("len deriv arrange: ", toString(length(deriv)))


    # Plot disease map
    data <- data
    stats <- deriv$stats
    color_type <- input$cluster_color_type
    color_param <- base::list(
      palette = input$cluster_palette,
      domain = if(trimws(input$cluster_domain) == ""){NULL},
      bins = as.numeric(input$cluster_bins),
      pretty = as.logical(input$cluster_pretty),
      n = as.numeric(input$cluster_n),
      levels = if(trimws(input$cluster_levels) == ""){NULL},
      ordered = as.logical(input$cluster_ordered),
      na.color = if(trimws(input$cluster_na_color) == ""){"#808080"},
      alpha = as.logical(input$cluster_alpha),
      reverse = as.logical(input$cluster_reverse),
      right = as.logical(input$cluster_right)
    )

    plot <- AegisFunc::get_leaflet_map(
      data = data,
      stats = stats,
      color_type = color_type,
      color_param = color_param
    )

    shinyjs::hide("work_disease_cluster")
    shinyjs::enable("plot_disease_cluster")

    plot
  })

  output$disease_cluster <- leaflet::renderLeaflet(
    disease_cluster()
  )
}
