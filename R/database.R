database_params_init <- list(
  dbms = c("sql server"),
  path_to_driver = getwd(),
  connection_string = "jdbc:sqlserver://IP:PORT;user=ID;password=PW",
  cdm_database_schema = "DB.SCHEMA",
  result_database_schema = "DB.SCHEMA"
)

database_ui <- fluidPage(
  selectInput("dbms", "Database dialect", database_params_init$dbms),
  textInput("path_to_driver", "JDBC path", database_params_init$path_to_driver),
  textInput("connection_string", "Database connection string", database_params_init$connection_string),
  textInput("cdm_database_schema", "CDM Database schema", database_params_init$cdm_database_schema),
  textInput("result_database_schema", "Results Database schema", database_params_init$result_database_schema),

  actionButton("print_db", "Print"),
  actionButton("get_cohort_list", "Get cohort list"),

  dataTableOutput("cohort_list")
)

database_server <- function(input, output, session) {
  # output$conn_info <- renderText({
  #   param <- list()
  #   param$conn$dbms <- input$dbms
  #   param$conn$path_to_driver <- input$path_to_driver
  #   param$conn$connection_string <- input$connection_string
  #
  #   message(
  #     "message param: ",
  #     toString(param)
  #   )
  #
  #   conn_info <- get_connection_details(param)
  #
  #   message(
  #     "message conn_info: ",
  #     toString(conn_info)
  #   )
  #
  #   toString(conn_info)
  # })

  # database_params <- eventReactive(input$save_db_info, {
  #   params <- list()
  #   params$dbms <- input$dbms
  #   params$path_to_driver <- input$path_to_driver
  #   params$connection_string <- input$connection_string
  #   params$cdm_database_schema <- input$cdm_database_schema
  #   params$result_database_schema <- input$result_database_schema
  #
  #   params
  # })

  observeEvent(input$print_db, {
    params <- list()
    params$dbms <- input$dbms
    params$path_to_driver <- input$path_to_driver
    params$connection_string <- input$connection_string
    params$cdm_database_schema <- input$cdm_database_schema
    params$result_database_schema <- input$result_database_schema

    message("db_params: ", toString(params))

  })

  cohort_list <- eventReactive(input$get_cohort_list, {
    mtcars
  })

  output$cohort_list <- renderDataTable(
    cohort_list(),
    options = list(pageLength = 5)
  )

  # render.table <- eventReactive(input$submit_table, {})
  # observeEvent(input$submit_plot, {})
  # observeEvent(input$submit_cluster, {})
}
