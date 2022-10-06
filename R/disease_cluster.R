disease_cluster_ui <- fluidPage(
  titlePanel(
    # app title/description
    "Plot disease cluster"
  ),
  sidebarLayout(
    sidebarPanel(
      # inputs
      selectInput("cluster_color_type", "Color type", choices = c("colorQuantile", "colorBin", "colorNumeric", "colorFactor")),
      conditionalPanel(
        condition = "input.cluster_color_type == 'colorQuantile'",

        selectInput("cluster_palette", "Palette", choices = c("Reds", "Greens")),
        textInput("cluster_domain", "Domain", value = ""),
        numericInput("cluster_n", "n", value = 9, min = 1, max = 9),
        textInput("cluster_na_color", "NA color", value = "#808080"),
        selectInput("cluster_alpha", "Alpha", choices = c(TRUE, FALSE), selected = FALSE),
        selectInput("cluster_reverse", "Reverse", choices = c(TRUE, FALSE), selected = FALSE),
        selectInput("cluster_right", "Right", choices = c(TRUE, FALSE), selected = FALSE)
      ),
      conditionalPanel(
        condition = "input.cluster_color_type == 'colorBin'",

        selectInput("cluster_palette", "Palette", choices = c("Reds", "Greens")),
        textInput("cluster_domain", "Domain", value = ""),
        numericInput("cluster_bins", "Bins", value = 7, min = 1, max = 9),
        selectInput("cluster_pretty", "Pretty", choices = c(TRUE, FALSE), selected = TRUE),
        textInput("cluster_na_color", "NA color", value = "#808080"),
        selectInput("cluster_alpha", "Alpha", choices = c(TRUE, FALSE), selected = FALSE),
        selectInput("cluster_reverse", "Reverse", choices = c(TRUE, FALSE), selected = FALSE),
        selectInput("cluster_right", "Right", choices = c(TRUE, FALSE), selected = FALSE)
      ),
      conditionalPanel(
        condition = "input.cluster_color_type == 'colorNumeric'",

        selectInput("cluster_palette", "Palette", choices = c("Reds", "Greens")),
        textInput("cluster_domain", "Domain", value = ""),
        textInput("cluster_na_color", "NA color", value = "#808080"),
        selectInput("cluster_alpha", "Alpha", choices = c(TRUE, FALSE), selected = FALSE),
        selectInput("cluster_reverse", "Reverse", choices = c(TRUE, FALSE), selected = FALSE)
      ),
      conditionalPanel(
        condition = "input.cluster_color_type == 'colorFactor'",

        selectInput("cluster_palette", "Palette", choices = c("Reds", "Greens")),
        textInput("cluster_domain", "Domain", value = ""),
        textInput("cluster_levels", "Levels", value = ""),
        selectInput("cluster_ordered", "Ordered", choices = c(TRUE, FALSE), selected = FALSE),
        textInput("cluster_na_color", "NA color", value = "#808080"),
        selectInput("cluster_alpha", "Alpha", choices = c(TRUE, FALSE), selected = FALSE),
        selectInput("cluster_reverse", "Reverse", choices = c(TRUE, FALSE), selected = FALSE)
      ),

      actionButton("print_disease_cluster", "Print"),
      actionButton("plot_disease_cluster", "Plot disease cluster")
    ),
    mainPanel(
      # outputs
      leafletOutput("disease_cluster", width = "400px"),
      hidden(
        p(id = "work_disease_cluster", "Processing...")
      )
    )
  )

  # titlePanel(
  #   # app title/description
  # ),
  # sidebarLayout(
  #   sidebarPanel(
  #     # inputs
  #   ),
  #   mainPanel(
  #     # outputs
  #   )
  # )
)

disease_cluster_server <- function(input, output, session, transfer) {
  observeEvent(input$print_disease_cluster, {
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

  observe({
    disable("plot_disease_cluster")

    if(length(transfer$table_adj()) > 0 & length(transfer$geo()@data) > 0) {
      enable("plot_disease_cluster")
    }
  })

  disease_cluster <- eventReactive(input$plot_disease_cluster, {
    disable("plot_disease_cluster")
    show("work_disease_cluster")

    # Calculate disease cluster
    param <- base::list()
    param$table <- transfer$table_adj()

    deriv <- calculate_disease_cluster(param)


    # Merge geo data with derivatives
    param <- base::list()
    param$geo <- transfer$geo()
    param$deriv <- deriv$arranged_table

    data <- merge_geo_with_deriv(param)


    # Plot disease map
    param <- base::list()
    param$data <- data
    param$stats <- deriv$stats
    param$color$type <- input$cluster_color_type
    param$color$param <- base::list(
      palette <- input$cluster_palette,
      domain <- input$cluster_domain,
      bins <- as.numeric(input$cluster_bins),
      pretty <- input$cluster_pretty,
      n <- as.numeric(input$cluster_n),
      levels <- input$cluster_levels,
      ordered <- input$cluster_ordered,
      na.color <- input$cluster_na_color,
      alpha <- input$cluster_alpha,
      reverse <- input$cluster_reverse,
      right <- input$cluster_right,
    )

    plot <- get_leaflet_map(param)

    hide("work_disease_cluster")
    enable("plot_disease_cluster")

    plot
  })

  output$disease_cluster <- renderLeaflet(
    disease_cluster()
  )

  # table <- eventReactive(input$get_table, {...})
  # output$table <- renderDataTable(table())
  # plot <- eventReactive(input$get_plot, {...})
  # output$plot <- renderPlot(plot())
  # observeEvent(input$do_any, {})
}
