database_params_init <- list(
  dbms = c("sql server"),
  path_to_driver = getwd(),
  connection_string = "jdbc:sqlserver://IP:PORT;user=ID;password=PW",
  cdm_database_schema = "DB.SCHEMA",
  result_database_schema = "DB.SCHEMA"
)

database_ui <- fluidPage(
  titlePanel(
    # app title/description
    "Connect to your database"
  ),
  sidebarLayout(
    sidebarPanel(
      # inputs
      selectInput("dbms", "Database dialect", choices = database_params_init$dbms),
      textInput("path_to_driver", "JDBC path", value = database_params_init$path_to_driver),
      textInput("connection_string", "Database connection string", value = database_params_init$connection_string),
      textInput("cdm_database_schema", "CDM Database schema", value = database_params_init$cdm_database_schema),
      textInput("result_database_schema", "Results Database schema", value = database_params_init$result_database_schema),

      verticalLayout(
        actionButton("print_db", "Print"),
        actionButton("get_cdm_source", "Get cdm source"),
        actionButton("get_cohort_list", "Get cohort list")
      )
    ),
    mainPanel(
      # outputs
      dataTableOutput("cdm_source"),
      dataTableOutput("cohort_list")
    )
  )

  # titlePanel(
  #   # app title/description
  # ),
  # sidebarLayout(
  #   sidebarPanel(
  #     # inputs
  #   ),
  #   mainPanel(
  #     # outputs
  #   )
  # )
)

database_server <- function(input, output, session, transfer) {
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
    # Get connection details
    param <- base::list()
    param$conn$dbms <- input$dbms
    param$conn$path_to_driver <- input$path_to_driver
    param$conn$connection_string <- input$connection_string

    conn_info <- get_connection_details(param)


    # Get cdm source
    param <- base::list()
    param$conn_info <- conn_info
    param$query$cdm_database_schema <- input$cdm_database_schema

    cdm_source <- get_cdm_source(param)

    cdm_source
  })

  output$cdm_source <- renderDataTable(
    cdm_source(),
    options = list(pageLength = 5)
  )

  cohort_list <- eventReactive(input$get_cohort_list, {
    # Get connection details
    param <- base::list()
    param$conn$dbms <- input$dbms
    param$conn$path_to_driver <- input$path_to_driver
    param$conn$connection_string <- input$connection_string

    conn_info <- get_connection_details(param)


    # Get cohort list
    param <- base::list()
    param$conn_info <- conn_info
    param$query$result_database_schema <- input$result_database_schema

    # cohort_list <- get_cohort_list_table(param)
    cohort_list <- data.frame(id = c(1:10), n = c(11:20))
    message("cohort_list: ", toString(cohort_list))

    cohort_list
  })

  output$cohort_list <- renderDataTable(
    cohort_list(),
    options = list(pageLength = 5)
  )

  list(
    cohort_ids = reactive(cohort_list()$id)
  )

  # table <- eventReactive(input$get_table, {...})
  # output$table <- renderDataTable(table())
  # plot <- eventReactive(input$get_plot, {...})
  # output$plot <- renderPlot(plot())
  # observeEvent(input$do_any, {})
}
