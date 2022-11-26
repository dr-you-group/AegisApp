spatialServer <- function(id, data) {
# spatialServer <- function(id, data, filter = is.numeric) {
#   stopifnot(is.reactive(data))
#   stopifnot(!is.reactive(filter))

  shiny::moduleServer(id, function(input, output, session) {
    # Logic here
    if(data$type() == "map") {
      # Generate graph file
      graph_file_path <- AegisFunc::trans_geo_to_graph(
        geo = data$geo()
      )

      # Calculate disease map
      deriv <- AegisFunc::calculate_disease_map(
        model = data$model(),
        table = data$table(),
        graph_file_path = graph_file_path
      )
    } else { # if(data$type() == "cluster")
      # Calculate disease cluster
      deriv <- AegisFunc::calculate_disease_cluster(
        model = data$model(),
        table = data$table()
      )
    }

    # Merge geo data with derivatives
    merged <- AegisFunc::merge_geo_with_deriv(
      geo = data$geo(),
      deriv = deriv$arranged_table
    )

    # Plot disease map
    color_type <- data$pal()$color_type
    color_param <- base::list(
      palette = data$pal()$palette,
      domain = if(trimws(data$pal()$domain) == ""){NULL}else{trimws(data$pal()$domain)},
      bins = as.numeric(data$pal()$bins),
      pretty = as.logical(data$pal()$pretty),
      n = as.numeric(data$pal()$n),
      levels = if(trimws(data$pal()$levels) == ""){NULL}else{trimws(data$pal()$levels)},
      ordered = as.logical(data$pal()$ordered),
      na.color = if(trimws(data$pal()$na_color) == ""){"#FFFFFF"}else{trimws(data$pal()$na_color)},
      alpha = as.logical(data$pal()$alpha),
      reverse = as.logical(data$pal()$reverse),
      right = as.logical(data$pal()$right)
    )

    plot <- AegisFunc::get_leaflet_map(
      data = merged,
      stats = deriv$stats,
      color_type = color_type,
      color_param = color_param
    )

    shiny::reactive(plot)

    # Use data param like so:
    # observeEvent(data(), {
    #   updateSelectInput(session, "var", choices = find_vars(data(), filter))
    # })
    #
    # reactive(data()[[input$var]])
  })
}
