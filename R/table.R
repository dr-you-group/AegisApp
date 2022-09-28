table_ui <- function(id) {
  # tableOutput(NS(id, "static"))
  dataTableOutput(NS(id, "dynamic"))
}

table_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # output$static <- renderTable(head(mtcars))
    output$dynamic <- renderDataTable(mtcars, options = list(pageLength = 5))
  })
}
