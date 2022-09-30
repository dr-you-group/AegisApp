database_ui <- fluidPage(
  selectInput("dbms", "Database dialect", c("sql server")),
  textInput("path_to_driver", "JDBC path", getwd()),
  textInput("connection_string", "Database connection string", "jdbc:sqlserver://IP:PORT;user=ID;password=PW"),
  textInput("cdm_database_schema", "CDM Database schema", "DB.SCHEMA"),
  textInput("result_database_schema", "Results Database schema", "DB.SCHEMA"),

  actionButton("save_db_info", "Save!"),
  actionButton("get_cohort_list", "Get cohort list"),
  actionButton("init_db_info", "Initialize"),

  textOutput("conn_info")
)

database_server <- function(input, output, session) {
  output$conn_info <- renderText({
    param <- list()
    param$conn$dbms <- input$dbms
    param$conn$path_to_driver <- input$path_to_driver
    param$conn$connection_string <- input$connection_string

    message(
      "message param: ",
      toString(param)
    )

    conn_info <- get_connection_details(param)

    message(
      "message conn_info: ",
      toString(conn_info)
    )

    toString(conn_info)
  })
}
