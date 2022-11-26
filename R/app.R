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
      databaseUI("db")
    ),
    shiny::tabPanel(
      title = "Get cohort table",
      cohortUI("chrt")
    ),
    shiny::tabPanel(
      title = "Get geo data",
      geoUI("geo")
    ),
    shiny::tabPanel(
      title = "Get adjusted table",
      adjustmentUI("adj")
    ),
    shiny::tabPanel(
      title = "Forecasting",
      forecastingUI("fcst")
    ),
    shiny::tabPanel(
      title = "Disease Map",
      diseaseMapUI("dzm")
    ),
    shiny::tabPanel(
      title = "Disease Cluster",
      diseaseClusterUI("dzc")
    )
  )

  server <- function(input, output, session) {
    database <- databaseServer("db")

    cohort <- cohortServer("chrt", list(
      cohort_ids = shiny::reactive(database$cohort_ids())
    ))

    geo <- geoServer("geo", list(
      model = shiny::reactive(cohort$model()),
      cohort_table = shiny::reactive(cohort$cohort_table())
    ))

    adj <- adjustmentServer("adj", list(
      model = shiny::reactive(cohort$model()),
      cohort_table = shiny::reactive(cohort$cohort_table()),
      geo = shiny::reactive(geo$geo())
    ))

    forecastingServer("fcst", list(
      model = shiny::reactive(cohort$model()),
      cohort_table = shiny::reactive(cohort$cohort_table())
    ))

    diseaseMapServer("dzm", list(
      model = shiny::reactive(cohort$model()),
      table = shiny::reactive(adj$table()),
      geo = shiny::reactive(geo$geo())
    ))

    diseaseClusterServer("dzc", list(
      model = shiny::reactive(cohort$model()),
      table = shiny::reactive(adj$table()),
      geo = shiny::reactive(geo$geo())
    ))
  }

  shiny::shinyApp(ui, server, ...)
}
