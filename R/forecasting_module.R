forecastingUI <- function(id) {
  htmltools::tagList(
    # UI here
    shiny::titlePanel(
      # app title/description
      "Plot forecasting"
    ),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        # inputs
        shiny::dateInput(shiny::NS(id, "fore_obs_end_date"), "Observation end date", value = "2008-01-01"),
        shiny::dateInput(shiny::NS(id, "fore_pred_end_date"), "Prediction end date", value = "2009-08-01"),
        shiny::selectInput(shiny::NS(id, "fore_var_type"), "Variables type", choices = c("day,season,month,week", "day,season,month", "day,season,week", "day,season", "day,month,week", "day,month", "day,week", "day")),
        shiny::actionButton(shiny::NS(id, "print"), "Print"),
        shiny::actionButton(shiny::NS(id, "plot"), "Plot forecasting")
      ),
      shiny::mainPanel(
        # outputs
        shiny::plotOutput(shiny::NS(id, "plot"))
      )
    )
  )
}

forecastingServer <- function(id, data) {
# forecastingServer <- function(id, data, filter = is.numeric) {
  # stopifnot(is.reactive(data))
  # stopifnot(!is.reactive(filter))

  shiny::moduleServer(id, function(input, output, session) {
    # Logic here
    shiny::observeEvent(input$print, {
      params <- list()
      params$model <- data$model()
      params$fore_obs_end_date <- input$fore_obs_end_date
      params$fore_pred_end_date <- input$fore_pred_end_date
      params$fore_var_type <- input$fore_var_type

      message("forecasting_params: ", toString(params))
    })

    shiny::observe({
      shinyjs::disable("plot")

      if (sum(match(data$model(), c("temporal"), nomatch = 0)) > 0 &
          length(data$cohort_table()) > 0) {
        shinyjs::enable("plot")
      }
    })

    plot <- shiny::eventReactive(input$plot, {
      shinyjs::disable("plot")

      # Calculate forecasting
      deriv <- temporalServer("fcst", list(
        model = shiny::reactive(data$model()),
        table = shiny::reactive(data$cohort_table()),
        observation_end_date = shiny::reactive(input$fore_obs_end_date),
        prediction_end_date = shiny::reactive(input$fore_pred_end_date),
        variables_type = shiny::reactive(input$fore_var_type)
      ))

      # Plot forecasting
      plot <- plotServer("fcst", list(
        data = shiny::reactive(deriv())
      ))

      shinyjs::enable("plot")

      plot()
    })

    output$plot <- shiny::renderPlot(
      plot()
    )

    # Use data param like so:
    # observeEvent(data(), {
    #   updateSelectInput(session, "var", choices = find_vars(data(), filter))
    # })
    #
    # reactive(data()[[input$var]])
  })
}
