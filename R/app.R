#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
AegisApp <- function(...) {
  ui <- shiny::navbarPage(
    shinyjs::useShinyjs(),
    title = "AegisApp",
    shiny::tabPanel(
      title = "Set database",
      database_ui
    ),
    shiny::tabPanel(
      title = "Get data for analysis",
      cohort_ui
    ),
    shiny::tabPanel(
      title = "Forecasting",
      forecasting_ui
    ),
    shiny::tabPanel(
      title = "Disease Map",
      disease_map_ui
    ),
    shiny::tabPanel(
      title = "Disease Cluster",
      disease_cluster_ui
    )
  )

  server <- function(input, output, session) {
    # # reference: https://github.com/rstudio/shiny/issues/3348#issuecomment-958814151
    # rendered_js_callback_ui <- function(input_id, input_value = "Date.now().toString()") {
    #   tags$script(
    #     glue::glue_safe("Shiny.setInputValue(\"{input_id}\", {input_value})")
    #   )
    # }

    database <- database_server(input, output, session, NULL)
    cohort <- cohort_server(input, output, session, database)
    forecasting_server(input, output, session, cohort)
    disease_map_server(input, output, session, cohort)
    disease_cluster_server(input, output, session, cohort)
  }

  shiny::shinyApp(ui, server, ...)
}
