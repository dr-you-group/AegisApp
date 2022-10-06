library(shiny)
library(leaflet)
library(AegisFunc)

AegisApp <- function(...) {
  ui <- navbarPage(
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

  server <- function(input, output, session) {
    database <- database_server(input, output, session)
    cohort <- cohort_server(input, output, session, database)
    disease_map_server(input, output, session, cohort)
    disease_cluster_server(input, output, session, cohort)
  }

  shinyApp(ui, server, ...)
}
