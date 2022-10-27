disease_cluster_ui <- shiny::fluidPage(
  shiny::titlePanel(
    # app title/description
    "Plot disease cluster"
  ),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      # inputs
      shiny::selectInput("cluster_color_type", "Color type", choices = c("colorQuantile", "colorBin", "colorNumeric", "colorFactor")),
      shiny::selectInput("cluster_palette", "Palette", choices = c("Reds", "Greens")),
      shiny::textInput("cluster_domain", "Domain", value = ""),
      shiny::textInput("cluster_na_color", "NA color", value = "#FFFFFF"),
      shiny::selectInput("cluster_alpha", "Alpha", choices = c(TRUE, FALSE), selected = FALSE),
      shiny::selectInput("cluster_reverse", "Reverse", choices = c(TRUE, FALSE), selected = FALSE),
      shiny::conditionalPanel(
        condition = "['colorQuantile', 'colorBin'].includes(input.cluster_color_type)",
        shiny::selectInput("cluster_right", "Right", choices = c(TRUE, FALSE), selected = FALSE)
      ),
      shiny::conditionalPanel(
        condition = "['colorQuantile'].includes(input.cluster_color_type)",
        shiny::numericInput("cluster_n", "n", value = 1, min = 1, max = 9),
      ),
      shiny::conditionalPanel(
        condition = "['colorBin'].includes(input.cluster_color_type)",
        shiny::numericInput("cluster_bins", "Bins", value = 7, min = 1, max = 9),
        shiny::selectInput("cluster_pretty", "Pretty", choices = c(TRUE, FALSE), selected = TRUE),
      ),
      shiny::conditionalPanel(
        condition = "['colorFactor'].includes(input.cluster_color_type)",
        shiny::textInput("cluster_levels", "Levels", value = ""),
        shiny::selectInput("cluster_ordered", "Ordered", choices = c(TRUE, FALSE), selected = FALSE),
      ),
      shiny::conditionalPanel(
        condition = "['colorNumeric'].includes(input.cluster_color_type)"
        # do something
      ),
      shiny::actionButton("print_disease_cluster", "Print"),
      shiny::actionButton("plot_disease_cluster", "Plot disease cluster")
    ),
    shiny::mainPanel(
      # outputs
      leaflet::leafletOutput("disease_cluster"),
      shiny::uiOutput("disease_clusters"),
      shinyjs::hidden(
        htmltools::p(id = "work_disease_cluster", "Processing...")
      ),
      shinyjs::hidden(
        htmltools::p(id = "work_disease_clusters", "Processing... making cluster plots")
      )
    )
  )
)

disease_cluster_server <- function(input, output, session, transfer) {
  shiny::observeEvent(input$print_disease_cluster, {
    params <- list()
    params$model <- input$model
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

    if (sum(match(input$model, c("spatial", "spatio-temporal"), nomatch = 0)) > 0 &
        length(transfer$table_adj()) > 0 &
        length(transfer$geo()@data) > 0) {
      shinyjs::enable("plot_disease_cluster")
    }
  })

  disease_cluster <- shiny::eventReactive(input$plot_disease_cluster, {
    shinyjs::disable("plot_disease_cluster")
    shinyjs::show("work_disease_cluster")

    # Calculate disease cluster
    model <- input$model
    table <- transfer$table_adj()

    deriv_c <- AegisFunc::calculate_disease_cluster(
      model = model,
      table = table
    )


    if(input$model == "spatio-temporal") {
      shinyjs::hide("disease_cluster")
      shinyjs::show("disease_clusters")

      # Merge geo data with derivatives
      years_c <- names(deriv_c)

      data_c <- base::list()

      for(i in 1:length(years_c)) {
        geo <- transfer$geo()
        deriv_c_arr <- deriv_c[years_c[i]][[1]]$arranged_table

        data_c <- append(data_c, list(AegisFunc::merge_geo_with_deriv(
          geo = geo,
          deriv = deriv_c_arr
        )))
      }

      names(data_c) <- years_c

      # Plot disease map
      plot_c <- base::list()

      for(i in 1:length(years_c)) {
        data <- data_c[years_c[i]][[1]]
        stats <- deriv_c[years_c[i]][[1]]$stats
        color_type <- "colorQuantile"
        color_param <- base::list(
          palette = "Reds",
          domain = NULL,
          bins = 7,
          pretty = TRUE,
          n = 9,
          levels = NULL,
          ordered = FALSE,
          na.color = "#FFFFFF",
          alpha = FALSE,
          reverse = FALSE,
          right = FALSE
        )


        plot_c <- append(plot_c, list(AegisFunc::get_leaflet_map(
          data = data,
          stats = stats,
          color_type = color_type,
          color_param = color_param
        )))
      }

      names(plot_c) <- years_c
    } else { # input$model == "spatial"
      shinyjs::hide("disease_clusters")
      shinyjs::show("disease_cluster")

      # Merge geo data with derivatives
      geo <- transfer$geo()
      deriv_c_arr <- deriv_c$arranged_table

      data_c <- AegisFunc::merge_geo_with_deriv(
        geo = geo,
        deriv = deriv_c_arr
      )

      # Plot disease map
      data <- data_c
      stats <- deriv_c$stats
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

      plot_c <- AegisFunc::get_leaflet_map(
        data = data,
        stats = stats,
        color_type = color_type,
        color_param = color_param
      )
    }

    shinyjs::hide("work_disease_cluster")
    shinyjs::enable("plot_disease_cluster")

    plot_c
  })

  output$disease_cluster <- leaflet::renderLeaflet(
    if(input$model == "spatio-temporal") {
      idx <- names(disease_cluster())

      message("renderLeaflet idx: ", idx)
      message("renderLeaflet len: ", length(idx))

      disease_cluster()[[idx[1]]][[1]]
    } else {
      disease_cluster()
    }
  )

  # reference: https://github.com/rstudio/shiny/issues/3348#issuecomment-958814151
  rendered_js_callback_ui <- function(input_id, input_value = "Date.now().toString()") {
    tags$script(
      glue::glue_safe("Shiny.setInputValue(\"{input_id}\", {input_value})")
    )
  }

  shiny::observeEvent(disease_cluster(), {
    shinyjs::show("work_disease_clusters")

    shiny::insertUI(
      session = session,
      selector = "#disease_clusters",
      ui = htmltools::tagList(
        lapply(1:length(names(disease_cluster())), function(i) {
          plotname <- paste("disease_cluster_", i, sep="")

          message("render ui plotname: ", plotname)

          leaflet::leafletOutput(plotname)
        }),
        rendered_js_callback_ui(input_id = "my_input_for_catching_render")
      )
    )
  })

  shiny::observeEvent(input$my_input_for_catching_render, {
    idx <- names(disease_cluster())

    message("observe disease_clusters idx: ", idx)
    message("observe disease_clusters len: ", length(idx))

    for (i in 1:length(idx)) {
      local({
        my_i <- i
        plotname <- paste("disease_cluster_", my_i, sep="")

        message("observe plotname: ", plotname)

        output[[plotname]] <- leaflet::renderLeaflet({
          message("render plotname: ", plotname)

          disease_cluster()[[my_i]]
        })
      })
    }

    shinyjs::hide("work_disease_clusters")
  }, ignoreInit = TRUE)

}
