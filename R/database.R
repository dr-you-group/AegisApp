database_ui <- fluidPage(
  titlePanel(
    # app title/description
    "Connect to your database"
  ),
  sidebarLayout(
    sidebarPanel(
      # inputs
      selectInput("dbms", "Database dialect", choices = c("sql server")),
      textInput("path_to_driver", "JDBC path", value = getwd()),
      textInput("connection_string", "Database connection string", value = "jdbc:sqlserver://IP:PORT;user=ID;password=PW"),
      textInput("cdm_database_schema", "CDM Database schema", value = "DB.SCHEMA"),
      textInput("result_database_schema", "Results Database schema", value = "DB.SCHEMA"),
      verticalLayout(
        actionButton("print_db", "Print"),
        actionButton("get_cdm_source", "Get cdm source"),
        actionButton("get_cohort_list", "Get cohort list")
      )
    ),
    mainPanel(
      # outputs
      dataTableOutput("cdm_source"),
      dataTableOutput("cohort_list"),
      hidden(
        p(id = "work_cdm_source", "Processing...")
      ),
      hidden(
        p(id = "work_cohort_list", "Processing...")
      )
    )
  )
)

database_server <- function(input, output, session, transfer, demo) {
  observeEvent(input$print_db, {
    params <- list()
    params$dbms <- input$dbms
    params$path_to_driver <- input$path_to_driver
    params$connection_string <- input$connection_string
    params$cdm_database_schema <- input$cdm_database_schema
    params$result_database_schema <- input$result_database_schema

    message("db_params: ", toString(params))
  })

  cdm_source <- eventReactive(input$get_cdm_source, {
    disable("get_cdm_source")
    show("work_cdm_source")

    if (demo) {
      cdm_source
    } else {
      # Get connection details
      dbms <- input$dbms
      path_to_driver <- input$path_to_driver
      connection_string <- input$connection_string

      conn_info <- get_connection_details(
        dbms = dbms,
        path_to_driver = path_to_driver,
        connection_string = connection_string
      )


      # Get cdm source
      conn_info <- conn_info
      cdm_database_schema <- input$cdm_database_schema

      cdm_source <- get_cdm_source(
        conn_info = conn_info,
        cdm_database_schema = cdm_database_schema
      )
    }

    hide("work_cdm_source")
    enable("get_cdm_source")

    cdm_source
  })

  output$cdm_source <- renderDataTable(
    cdm_source(),
    options = list(pageLength = 5)
  )

  cohort_list <- eventReactive(input$get_cohort_list, {
    disable("get_cohort_list")
    show("work_cohort_list")

    if (demo) {
      cohort_list
    } else {
      # Get connection details
      dbms <- input$dbms
      path_to_driver <- input$path_to_driver
      connection_string <- input$connection_string

      conn_info <- get_connection_details(
        dbms = dbms,
        path_to_driver = path_to_driver,
        connection_string = connection_string
      )


      # Get cohort list
      conn_info <- conn_info
      result_database_schema <- input$result_database_schema

      cohort_list <- get_cohort_list_table(
        conn_info = conn_info,
        result_database_schema = result_database_schema
      )
    }

    hide("work_cohort_list")
    enable("get_cohort_list")

    cohort_list
  })

  output$cohort_list <- renderDataTable(
    cohort_list(),
    options = list(pageLength = 5)
  )

  list(
    cohort_ids = reactive(cohort_list()$id)
  )
}
