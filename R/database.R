database_ui <- shiny::fluidPage(
  shiny::titlePanel(
    # app title/description
    "Connect to your database"
  ),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      # inputs
      shiny::selectInput("dbms", "Database dialect", choices = c("sql server")),
      shiny::textInput("path_to_driver", "JDBC path", value = getwd()),
      shiny::textInput("connection_string", "Database connection string", value = "jdbc:sqlserver://IP:PORT;user=ID;password=PW"),
      shiny::textInput("cdm_database_schema", "CDM Database schema", value = "DB.SCHEMA"),
      shiny::textInput("result_database_schema", "Results Database schema", value = "DB.SCHEMA"),
      shiny::verticalLayout(
        shiny::actionButton("print_db", "Print"),
        shiny::actionButton("get_cdm_source", "Get cdm source"),
        shiny::actionButton("get_cohort_list", "Get cohort list")
      )
    ),
    shiny::mainPanel(
      # outputs
      shiny::dataTableOutput("cdm_source"),
      shiny::dataTableOutput("cohort_list"),
      shinyjs::hidden(
        htmltools::p(id = "work_cdm_source", "Processing...")
      ),
      shinyjs::hidden(
        htmltools::p(id = "work_cohort_list", "Processing...")
      )
    )
  )
)

database_server <- function(input, output, session, transfer) {
  shiny::observeEvent(input$print_db, {
    params <- list()
    params$dbms <- input$dbms
    params$path_to_driver <- input$path_to_driver
    params$connection_string <- input$connection_string
    params$cdm_database_schema <- input$cdm_database_schema
    params$result_database_schema <- input$result_database_schema

    message("db_params: ", toString(params))
  })

  cdm_source <- shiny::eventReactive(input$get_cdm_source, {
    shinyjs::disable("get_cdm_source")
    shinyjs::show("work_cdm_source")

    query <- shiny::parseQueryString(session$clientData$url_search)
    message("query: ", toString(paste(names(query), query, sep = "=", collapse=", ")))

    if (!is.null(query$demo) & isTRUE(as.logical(query$demo))) {
      cdm_source <- AegisApp::cdm_source
    } else {
      # Get connection details
      dbms <- input$dbms
      path_to_driver <- input$path_to_driver
      connection_string <- input$connection_string

      conn_info <- AegisFunc::get_connection_details(
        dbms = dbms,
        path_to_driver = path_to_driver,
        connection_string = connection_string
      )

      # Get cdm source
      conn_info <- conn_info
      cdm_database_schema <- input$cdm_database_schema

      cdm_source <- AegisFunc::get_cdm_source(
        conn_info = conn_info,
        cdm_database_schema = cdm_database_schema
      )
    }

    shinyjs::hide("work_cdm_source")
    shinyjs::enable("get_cdm_source")

    cdm_source
  })

  output$cdm_source <- shiny::renderDataTable(
    cdm_source(),
    options = list(pageLength = 5)
  )

  cohort_list <- shiny::eventReactive(input$get_cohort_list, {
    shinyjs::disable("get_cohort_list")
    shinyjs::show("work_cohort_list")

    query <- shiny::parseQueryString(session$clientData$url_search)
    message("query: ", toString(paste(names(query), query, sep = "=", collapse=", ")))

    if (!is.null(query$demo) & isTRUE(as.logical(query$demo))) {
      cohort_list <- AegisApp::cohort_list
    } else {
      # Get connection details
      dbms <- input$dbms
      path_to_driver <- input$path_to_driver
      connection_string <- input$connection_string

      conn_info <- AegisFunc::get_connection_details(
        dbms = dbms,
        path_to_driver = path_to_driver,
        connection_string = connection_string
      )

      # Get cohort list
      conn_info <- conn_info
      result_database_schema <- input$result_database_schema

      cohort_list <- AegisFunc::get_cohort_list_table(
        conn_info = conn_info,
        result_database_schema = result_database_schema
      )
    }

    shinyjs::hide("work_cohort_list")
    shinyjs::enable("get_cohort_list")

    cohort_list
  })

  output$cohort_list <- shiny::renderDataTable(
    cohort_list(),
    options = list(pageLength = 5)
  )

  list(
    cohort_ids = shiny::reactive(cohort_list()$id)
  )
}
