library(shiny)
library(shinyjs)
library(leaflet)
library(AegisFunc)

#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
AegisApp <- function(...) {
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

  server <- function(input, output, session) {
    # observe({
    #   query <- parseQueryString(session$clientData$url_search)
    #   message("query: ", toString(paste(names(query), query, sep = "=", collapse=", ")))
    #
    #   if (!is.null(query$demo) & isTRUE(as.logical(query$demo))) {
    #     data_file <- file.path(getwd(), "data", "aegis_sample.Rdata")
    #     load(data_file)
    #     message("data file: ", toString(data_file))
    #   }
    # })

    database <- database_server(input, output, session, NULL)
    cohort <- cohort_server(input, output, session, database)
    disease_map_server(input, output, session, cohort)
    disease_cluster_server(input, output, session, cohort)
  }

  shinyApp(ui, server, ...)
}
