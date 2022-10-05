cohort_tab <- tabsetPanel(
  id = "cohort_tab",
  type = "tabs",

  tabPanel(
    title = "Get cohort table",

    selectInput("target_cohort_definition_id", "Target cohort definition id", c(1:10)),
    selectInput("outcome_cohort_definition_id", "Outcome cohort definition id", c(1:10)),
    dateInput("cohort_start_date", "Cohort start date", "2020-01-01"),
    dateInput("cohort_end_date", "Cohort end date", "2020-12-31"),
    textInput("time_at_risk_start_date", "Time at risk start date", "0"),
    textInput("time_at_risk_end_date", "Time at risk end date", "0"),
    radioButtons("time_at_risk_end_date_panel", "Time at risk end date panel", c("cohort_start_date", "cohort_end_date")),

    actionButton("print_cohort", "Print"),
    actionButton("get_cohort_table", "Get cohort table"),

    dataTableOutput("cohort_table")
  ),
  tabPanel(
    title = "Get geo data",

    selectInput("name", "Source of the geo data", c("GADM", "KOR")),
    selectInput("country", "Country", c("KOR")),
    radioButtons("level", "Level of the administrative data", c(2:3)),

    actionButton("print_geo", "Print"),
    actionButton("get_geo", "Get geo data"),

    dataTableOutput("geo")
  ),
  tabPanel(
    title = "Get adjustmented table",

    selectInput("mode", "Adjustment mode", c("Std", "Crd")),
    selectInput("fraction", "Fraction", c("100000")),
    selectInput("conf_level", "Confidence level", c("0.95")),

    actionButton("print_adj", "Print"),
    actionButton("get_table_adj", "Get adjustmented table"),

    dataTableOutput("table_adj")
  ),
  tabPanel(
    title = "Plot disease map/cluster",

    # actionButton("get_cohort", "Get cohort table"),
    actionButton("plot_disease_map", "Plot disease map"),
    actionButton("plot_disease_cluster", "Plot disease cluster"),

    # dataTableOutput("cohort")
  )
)

cohort_ui <- fluidPage(
  cohort_tab
)

cohort_server <- function(input, output, session) {
  observeEvent(input$print_cohort, {
    params <- list()
    params$target_cohort_definition_id <- input$target_cohort_definition_id
    params$outcome_cohort_definition_id <- input$outcome_cohort_definition_id
    params$cohort_start_date <- input$cohort_start_date
    params$cohort_end_date <- input$cohort_end_date
    params$time_at_risk_start_date <- input$time_at_risk_start_date
    params$time_at_risk_end_date <- input$time_at_risk_end_date
    params$time_at_risk_end_date_panel <- input$time_at_risk_end_date_panel

    message("cohort_params: ", toString(params))

  })

  observeEvent(input$print_geo, {
    params <- list()
    params$name <- input$name
    params$country <- input$country
    params$level <- input$level

    message("geo_params: ", toString(params))

  })

  observeEvent(input$print_adj, {
    params <- list()
    params$mode <- input$mode
    params$fraction <- input$fraction
    params$conf_level <- input$conf_level

    message("adj_params: ", toString(params))

  })

  cohort_table <- eventReactive(input$get_cohort_table, {
    # Get connection details
    param <- base::list()
    param$conn$dbms <- input$dbms
    param$conn$path_to_driver <- input$path_to_driver
    param$conn$connection_string <- input$connection_string

    conn_info <- get_connection_details(param)


    # Get cohort table
    param <- base::list()
    param$conn_info <- conn_info
    param$query$cdm_database_schema <- input$cdm_database_schema
    param$query$result_database_schema <- input$result_database_schema
    param$query$target_cohort_definition_id <- input$target_cohort_definition_id
    param$query$outcome_cohort_definition_id <- input$outcome_cohort_definition_id
    param$query$cohort_start_date <- input$cohort_start_date
    param$query$cohort_end_date <- input$cohort_end_date
    param$query$time_at_risk_start_date <- input$time_at_risk_start_date
    param$query$time_at_risk_end_date <- input$time_at_risk_end_date
    param$query$time_at_risk_end_date_panel <- input$time_at_risk_end_date_panel

    cohort_table <- get_cohort_analysis_table(param)

    cohort_table
  })

  output$cohort_table <- renderDataTable(
    cohort_table(),
    options = list(pageLength = 5)
  )

  geo <- eventReactive(input$get_geo, {
    # Get geo data
    param <- base::list()
    param$geo$name <- input$name
    param$geo$country <- input$country
    param$geo$level <- input$level

    geo <- get_geo_data(param)

    geo
  })

  output$geo <- renderDataTable(
    geo(),
    options = list(pageLength = 5)
  )

  table_adj <- eventReactive(input$get_table_adj, {
    # Map cohort table with geo data
    param <- base::list()
    param$latlong <- cohort_table()
    param$geo <- geo()

    geo_map <- map_latlong_geo(param)


    # Arrange table
    param <- base::list()
    param$table <- geo_map

    table_arr <- calculate_count_with_geo_oid(param)


    # Adjustment for age and sex
    param <- base::list()
    param$table <- table_arr
    param$adj$mode <- input$mode
    param$adj$fraction <- input$fraction
    param$adj$conf_level <- input$conf_level

    table_adj <- calculate_adjust_age_sex_indirectly(param)

    table_adj
  })

  output$table_adj <- renderDataTable(
    table_adj(),
    options = list(pageLength = 5)
  )

  # cohort <- eventReactive(input$get_cohort, {
  #   # Get connection details
  #   param <- base::list()
  #   param$conn$dbms <- input$dbms
  #   param$conn$path_to_driver <- input$path_to_driver
  #   param$conn$connection_string <- input$connection_string
  #
  #   conn_info <- get_connection_details(param)
  #
  #
  #   # Get cohort table
  #   param <- base::list()
  #   param$conn_info <- conn_info
  #   param$query$cdm_database_schema <- input$cdm_database_schema
  #   param$query$result_database_schema <- input$result_database_schema
  #   param$query$target_cohort_definition_id <- input$target_cohort_definition_id
  #   param$query$outcome_cohort_definition_id <- input$outcome_cohort_definition_id
  #   param$query$cohort_start_date <- input$cohort_start_date
  #   param$query$cohort_end_date <- input$cohort_end_date
  #   param$query$time_at_risk_start_date <- input$time_at_risk_start_date
  #   param$query$time_at_risk_end_date <- input$time_at_risk_end_date
  #   param$query$time_at_risk_end_date_panel <- input$time_at_risk_end_date_panel
  #
  #   cohort_table <- get_cohort_analysis_table(param)
  #
  #
  #   # Get geo data
  #   param <- base::list()
  #   param$geo$name <- input$name
  #   param$geo$country <- input$country
  #   param$geo$level <- input$level
  #
  #   geo <- get_geo_data(param)
  #
  #
  #   # Map cohort table with geo data
  #   param <- base::list()
  #   param$latlong <- cohort_table
  #   param$geo <- geo
  #
  #   geo_map <- map_latlong_geo(param)
  #
  #
  #   # Arrange table
  #   param <- base::list()
  #   param$table <- geo_map
  #
  #   table_arr <- calculate_count_with_geo_oid(param)
  #
  #
  #   # Adjustment for age and sex
  #   param <- base::list()
  #   param$table <- table_arr
  #   param$adj$mode <- input$mode
  #   param$adj$fraction <- input$fraction
  #   param$adj$conf_level <- input$conf_level
  #
  #   table_adj <- calculate_adjust_age_sex_indirectly(param)
  #
  #   table_adj
  # })
  #
  # output$cohort <- renderDataTable(
  #   cohort(),
  #   options = list(pageLength = 5)
  # )

  disease_map <- eventReactive(input$plot_disease_map, {
    # Generate graph file
    param <- base::list()
    param$geo <- geo()

    graph_file_path <- trans_geo_to_graph(param)


    # Calculate disease map
    param <- base::list()
    param$table <- table_adj()
    param$graph_file_path <- graph_file_path

    deriv <- calculate_disease_map(param)


    # Merge geo data with derivatives
    param <- base::list()
    param$geo <- geo()
    param$deriv <- deriv$arranged_table

    data <- merge_geo_with_deriv(param)


    # Plot disease map
    param <- base::list()
    param$data <- data
    param$stats <- deriv$stats

    plot <- get_leaflet_map(param)

    plot
  })

  output$disease_map <- renderPlot(
    disease_map(),
    res = 96
  )

  disease_cluster <- eventReactive(input$plot_disease_cluster, {
    # Calculate disease cluster
    param <- base::list()
    param$table <- table_adj()

    deriv <- calculate_disease_cluster(param)


    # Merge geo data with derivatives
    param <- base::list()
    param$geo <- geo()
    param$deriv <- deriv$arranged_table

    data <- merge_geo_with_deriv(param)


    # Plot disease map
    param <- base::list()
    param$data <- data
    param$stats <- deriv$stats

    plot <- get_leaflet_map(param)

    plot
  })

  output$disease_cluster <- renderPlot(
    disease_cluster(),
    res = 96
  )

  # table <- eventReactive(input$get_table, {...})
  # output$table <- renderDataTable(table())
  # plot <- eventReactive(input$get_plot, {...})
  # output$plot <- renderPlot(plot())
  # observeEvent(input$do_any, {})
}
