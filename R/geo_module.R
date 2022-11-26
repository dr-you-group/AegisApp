geoUI <- function(id) {
  htmltools::tagList(
    # UI here
    shiny::titlePanel(
      # app title/description
      "Get geo data"
    ),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        # inputs
        shiny::selectInput(shiny::NS(id, "name"), "Source of the geo data", choices = c("KOR", "GADM")),
        shiny::selectInput(shiny::NS(id, "country"), "Country", choices = c("KOR")),
        shiny::selectInput(shiny::NS(id, "level"), "Level of the administrative data", choices = c(2:3)),
        shiny::actionButton(shiny::NS(id, "print_geo"), "Print"),
        shiny::actionButton(shiny::NS(id, "get_geo"), "Get geo data")
      ),
      shiny::mainPanel(
        # outputs
        shiny::dataTableOutput(shiny::NS(id, "geo"))
      )
    )
  )
}

geoServer <- function(id, data) {
# geoServer <- function(id, data, filter = is.numeric) {
#   stopifnot(is.reactive(data))
#   stopifnot(!is.reactive(filter))

  shiny::moduleServer(id, function(input, output, session) {
    # Logic here
    shiny::observeEvent(input$print_geo, {
      params <- list()
      params$name <- input$name
      params$country <- input$country
      params$level <- input$level

      message("geo_params: ", toString(params))
    })

    shiny::observe({
      shinyjs::disable("get_geo")

      if (sum(match(data$model(), c("spatial", "spatio-temporal"), nomatch = 0)) > 0 &
          length(data$cohort_table()) > 0) {
        shinyjs::enable("get_geo")
      }
    })

    geo <- shiny::eventReactive(input$get_geo, {
      shinyjs::disable("get_geo")

      # Get geo data
      name <- input$name
      country <- input$country
      level <- input$level

      geo <- AegisFunc::get_geo_data(
        name = name,
        country = country,
        level = level
      )

      shinyjs::enable("get_geo")

      geo
    })

    output$geo <- shiny::renderDataTable(
      geo()@data,
      options = list(pageLength = 5)
    )

    list(
      geo = shiny::reactive(geo())
    )

    # Use data param like so:
    # observeEvent(data(), {
    #   updateSelectInput(session, "var", choices = find_vars(data(), filter))
    # })
    #
    # reactive(data()[[input$var]])
  })
}
