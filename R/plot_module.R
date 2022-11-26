plotServer <- function(id, data) {
# plotServer <- function(id, data, filter = is.numeric) {
#   stopifnot(is.reactive(data))
#   stopifnot(!is.reactive(filter))

  shiny::moduleServer(id, function(input, output, session) {
    # Logic here
    plot <- AegisFunc::get_forecasting_plot(
      data = data$data()
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
