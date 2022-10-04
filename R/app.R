library(shiny)
library(AegisFunc)

AegisApp <- function(...) {
  ui <- navbarPage(
    title = "AegisApp",

    tabPanel(
      title = "Set database",
      database_ui
    ),
    tabPanel(
      title = "Get cohort",
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
    database_server(input, output, session)
    cohort_server(input, output, session)
    disease_map_server(input, output, session)
    disease_cluster_server(input, output, session)
  }

  shinyApp(ui, server, ...)
}
