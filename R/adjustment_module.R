adjustmentUI <- function(id) {
  htmltools::tagList(
    # UI here
    shiny::titlePanel(
      # app title/description
      "Get adjusted table"
    ),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        # inputs
        shiny::selectInput(shiny::NS(id, "mode"), "Adjustment mode", choices = c("std", "crd")),
        shiny::selectInput(shiny::NS(id, "fraction"), "Fraction", choices = c("100000")),
        shiny::selectInput(shiny::NS(id, "conf_level"), "Confidence level", choices = c("0.95")),
        shiny::actionButton(shiny::NS(id, "print_table"), "Print"),
        shiny::actionButton(shiny::NS(id, "get_table"), "Get adjusted table")
      ),
      shiny::mainPanel(
        # outputs
        shiny::dataTableOutput(shiny::NS(id, "table"))
      )
    )
  )
}

adjustmentServer <- function(id, data) {
# adjustmentServer <- function(id, data, filter = is.numeric) {
#   stopifnot(is.reactive(data))
#   stopifnot(!is.reactive(filter))

  shiny::moduleServer(id, function(input, output, session) {
    # Logic here
    shiny::observeEvent(input$print_table, {
      params <- list()
      params$model <- input$model
      params$mode <- input$mode
      params$fraction <- input$fraction
      params$conf_level <- input$conf_level

      message("adj_params: ", toString(params))
    })

    shiny::observe({
      shinyjs::disable("get_table")

      if (sum(match(data$model(), c("spatial", "spatio-temporal"), nomatch = 0)) > 0 &
          length(data$cohort_table()) > 0 &
          length(data$geo()@data) > 0) {
        shinyjs::enable("get_table")
      }
    })

    table <- shiny::eventReactive(input$get_table, {
      shinyjs::disable("get_table")

      # Map cohort table with geo data
      latlong <- data$cohort_table()
      geo <- data$geo()

      geo_map <- AegisFunc::map_latlong_geo(
        latlong = latlong,
        geo = geo
      )

      # Arrange table
      model <- data$model()
      table <- geo_map

      table_arr <- AegisFunc::calculate_count_with_geo_oid(
        model = model,
        table = table
      )

      # Adjustment for age and sex
      model <- data$model()
      table <- table_arr
      mode <- input$mode
      fraction <- input$fraction
      conf_level <- input$conf_level

      table <- AegisFunc::calculate_adjust_age_sex_indirectly(
        model = model,
        table = table,
        mode = mode,
        fraction = fraction,
        conf_level = conf_level
      )

      shinyjs::enable("get_table")

      table
    })

    output$table <- shiny::renderDataTable(
      table(),
      options = list(pageLength = 5)
    )

    list(
      table = shiny::reactive(table())
    )

    # Use data param like so:
    # observeEvent(data(), {
    #   updateSelectInput(session, "var", choices = find_vars(data(), filter))
    # })
    #
    # reactive(data()[[input$var]])
  })
}
