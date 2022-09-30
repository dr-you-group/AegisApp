database_ui <- fluidPage(
  textInput("dbms", "Database dialect", "sql server"),
  textOutput("dbms"),
  textInput("path_to_driver", "JDBC path", getwd()),
  textInput("connection_string", "Database connection string", "jdbc:sqlserver://IP:PORT;user=ID;password=PW"),
  textInput("cdm_database_schema", "CDM Database schema", "DB.SCHEMA"),
  textInput("result_database_schema", "Results Database schema", "DB.SCHEMA"),

  actionButton("save_db_info", "Save!"),
  actionButton("get_cohort_list", "Get cohort list"),
  actionButton("init_db_info", "Initialize")
)

database_server <- function(input, output, session) {
  output$dbms <- renderText({
    input$dbms
  })
}
