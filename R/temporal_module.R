temporalServer <- function(id, data) {
# temporalServer <- function(id, data, filter = is.numeric) {
#   stopifnot(is.reactive(data))
#   stopifnot(!is.reactive(filter))

  shiny::moduleServer(id, function(input, output, session) {
    # Logic here
    deriv <- AegisFunc::calculate_forecasting(
      model = data$model(),
      table = data$table(),
      observation_end_date = data$observation_end_date(),
      prediction_end_date = data$prediction_end_date(),
      variables_type = data$variables_type()
    )

    shiny::reactive(deriv)

    # Use data param like so:
    # observeEvent(data(), {
    #   updateSelectInput(session, "var", choices = find_vars(data(), filter))
    # })
    #
    # reactive(data()[[input$var]])
  })
}
