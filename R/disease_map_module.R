diseaseMapUI <- function(id) {
  htmltools::tagList(
    # UI here
    shiny::titlePanel(
      # app title/description
      "Plot disease map"
    ),
    leafletUI(id)
  )
}

diseaseMapServer <- function(id, data) {
# diseaseMapServer <- function(id, data, filter = is.numeric) {
#   stopifnot(is.reactive(data))
#   stopifnot(!is.reactive(filter))

  shiny::moduleServer(id, function(input, output, session) {
    # Logic here
    shiny::observeEvent(input$print, {
      params <- list()
      params$color_type <- input$color_type
      params$palette <- input$palette
      params$domain <- input$domain
      params$bins <- input$bins
      params$pretty <- input$pretty
      params$n <- input$n
      params$levels <- input$levels
      params$ordered <- input$ordered
      params$na_color <- input$na_color
      params$alpha <- input$alpha
      params$reverse <- input$reverse
      params$right <- input$right

      message("disease_map_params: ", toString(params))
    })

    shiny::observe({
      shinyjs::disable("plot")

      if (sum(match(data$model(), c("spatial", "spatio-temporal"), nomatch = 0)) > 0 &
          length(data$table()) > 0 &
          length(data$geo()@data) > 0) {
        shinyjs::enable("plot")
      }
    })

    disease_map <- shiny::eventReactive(input$plot, {
      shinyjs::disable("plot")

      pal <- list()
      pal$color_type <- input$color_type
      pal$palette <- input$palette
      pal$domain <- input$domain
      pal$bins <- input$bins
      pal$pretty <- input$pretty
      pal$n <- input$n
      pal$levels <- input$levels
      pal$ordered <- input$ordered
      pal$na_color <- input$na_color
      pal$alpha <- input$alpha
      pal$reverse <- input$reverse
      pal$right <- input$right

      if(data$model() == "spatio-temporal") {
        shinyjs::hide("output_plot")
        shinyjs::show("output_plots")

        plot <- spatioTemporalServer("dzm", list(
          type = shiny::reactive("map"),
          model = shiny::reactive(data$model()),
          table = shiny::reactive(data$table()),
          geo = shiny::reactive(data$geo()),
          pal = shiny::reactive(pal)
        ))
      } else { # if(data$model() == "spatial")
        shinyjs::hide("output_plots")
        shinyjs::show("output_plot")

        plot <- spatialServer("dzm", list(
          type = shiny::reactive("map"),
          model = shiny::reactive(data$model()),
          table = shiny::reactive(data$table()),
          geo = shiny::reactive(data$geo()),
          pal = shiny::reactive(pal)
        ))
      }

      shinyjs::enable("plot")

      plot()
    })

    output$output_plot <- leaflet::renderLeaflet(
      if(data$model() == "spatio-temporal") {
        idx <- names(disease_map())

        message("renderLeaflet idx: ", idx)
        message("renderLeaflet len: ", length(idx))

        disease_map()[idx[1]][[1]]
      } else { # if(data$model() == "spatial")
        disease_map()
      }
    )

    # reference: https://github.com/rstudio/shiny/issues/3348#issuecomment-958814151
    rendered_js_callback_ui <- function(input_id, input_value = "Date.now().toString()") {
      tags$script(
        glue::glue_safe("Shiny.setInputValue(\"{input_id}\", {input_value})")
      )
    }

    shiny::observeEvent(disease_map(), {
      shiny::insertUI(
        session = session,
        selector = "#dzm-output_plots",
        ui = htmltools::tagList(
          lapply(1:length(names(disease_map())), function(i) {
            plotname <- paste("dzm-", "disease_map_", i, sep="")

            message("render ui plotname: ", plotname)

            leaflet::leafletOutput(plotname)
          }),
          rendered_js_callback_ui(input_id = "dzm-my_input_for_catching_render")
        )
      )
    })

    shiny::observeEvent(input$my_input_for_catching_render, {
      idx <- names(disease_map())

      message("observe disease_maps idx: ", idx)
      message("observe disease_maps len: ", length(idx))

      for (i in 1:length(idx)) {
        local({
          my_i <- i
          plotname <- paste("disease_map_", my_i, sep="")

          message("observe plotname: ", plotname)

          output[[plotname]] <- leaflet::renderLeaflet({
            message("render plotname: ", plotname)

            disease_map()[[my_i]]
          })
        })
      }
    }, ignoreInit = TRUE)

    # Use data param like so:
    # observeEvent(data(), {
    #   updateSelectInput(session, "var", choices = find_vars(data(), filter))
    # })
    #
    # reactive(data()[[input$var]])
  })
}
