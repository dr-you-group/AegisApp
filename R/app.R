library(shiny)
library(shinyjs)
library(leaflet)
library(AegisFunc)

AegisApp <- function(demo = FALSE, ...) {
  ui <- navbarPage(
    useShinyjs(),
    title = "AegisApp",
    tabPanel(
      title = "Set database",
      database_ui
    ),
    tabPanel(
      title = "Get data for analysis",
      cohort_ui
    ),
    tabPanel(
      title = "Disease Map",
      disease_map_ui
    ),
    tabPanel(
      title = "Disease Cluster",
      disease_cluster_ui
    )
  )

  server <- function(input, output, session, demo) {
    if (demo) {
      load(file.path(getwd(), "data", "aegis_sample.Rdata"))
    }

    database <- database_server(input, output, session, NULL, demo)
    cohort <- cohort_server(input, output, session, database, demo)
    disease_map_server(input, output, session, cohort)
    disease_cluster_server(input, output, session, cohort)
  }

  shinyApp(ui, server, ...)
}
