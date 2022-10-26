forecasting_ui <- shiny::fluidPage(
  shiny::titlePanel(
    # app title/description
    "Plot forecasting"
  ),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      # inputs
      shiny::dateInput("fore_obs_end_date", "Observation end date", value = "2008-01-01"),
      shiny::dateInput("fore_pred_end_date", "Prediction end date", value = "2009-08-01"),
      shiny::selectInput("fore_var_type", "Variables type", choices = c("day,season,month,week", "day,season,month", "day,season,week", "day,season", "day,month,week", "day,month", "day,week", "day")),
      shiny::actionButton("print_forecasting", "Print"),
      shiny::actionButton("plot_forecasting", "Plot forecasting")
    ),
    shiny::mainPanel(
      # outputs
      shiny::plotOutput("plot_forecasting"),
      shinyjs::hidden(
        htmltools::p(id = "work_forecasting", "Processing...")
      )
    )
  )
)

forecasting_server <- function(input, output, session, transfer) {
  shiny::observeEvent(input$print_forecasting, {
    params <- list()
    params$model <- input$model
    params$fore_obs_end_date <- input$fore_obs_end_date
    params$fore_pred_end_date <- input$fore_pred_end_date
    params$fore_var_type <- input$fore_var_type

    message("forecasting_params: ", toString(params))
  })

  shiny::observe({
    shinyjs::disable("plot_forecasting")

    if (sum(match(input$model, c("temporal"), nomatch = 0)) > 0 &
        length(transfer$cohort_table()) > 0) {
      shinyjs::enable("plot_forecasting")
    }
  })

  forecasting <- shiny::eventReactive(input$plot_forecasting, {
    shinyjs::disable("plot_forecasting")
    shinyjs::show("work_forecasting")

    # Calculate forecasting
    model <- input$model
    table <- transfer$cohort_table()
    observation_end_date <- input$fore_obs_end_date
    prediction_end_date <- input$fore_pred_end_date
    variables_type <- input$fore_var_type

    deriv_f <- AegisFunc::calculate_forecasting(
      model = model,
      table = table,
      observation_end_date = observation_end_date,
      prediction_end_date = prediction_end_date,
      variables_type = variables_type
    )


    # Plot forecasting
    data <- deriv_f

    plot_f <- AegisFunc::get_forecasting_plot(
      data = data
    )

    shinyjs::hide("work_forecasting")
    shinyjs::enable("plot_forecasting")

    plot_f
  })

  output$plot_forecasting <- shiny::renderPlot(
    forecasting()
  )
}
