cohort_tab <- tabsetPanel(
  id = "cohort_tab",
  type = "tabs",
  tabPanel(
    title = "Get cohort table",
    sidebarLayout(
      sidebarPanel(
        # inputs
        selectInput("target_cohort_definition_id", "Target cohort definition id", choices = c()),
        selectInput("outcome_cohort_definition_id", "Outcome cohort definition id", choices = c()),
        dateInput("cohort_start_date", "Cohort start date", value = "2020-01-01"),
        dateInput("cohort_end_date", "Cohort end date", value = "2020-12-31"),
        textInput("time_at_risk_start_date", "Time at risk start date", value = "0"),
        textInput("time_at_risk_end_date", "Time at risk end date", value = "0"),
        selectInput("time_at_risk_end_date_panel", "Time at risk end date panel", choices = c("cohort_start_date", "cohort_end_date")),
        actionButton("print_cohort_table", "Print"),
        actionButton("get_cohort_table", "Get cohort table")
      ),
      mainPanel(
        # outputs
        dataTableOutput("cohort_table"),
        hidden(
          p(id = "work_cohort_table", "Processing...")
        )
      )
    )
  ),
  tabPanel(
    title = "Get geo data",
    sidebarLayout(
      sidebarPanel(
        # inputs
        selectInput("name", "Source of the geo data", choices = c("KOR", "GADM")),
        selectInput("country", "Country", choices = c("KOR")),
        selectInput("level", "Level of the administrative data", choices = c(2:3)),
        actionButton("print_geo", "Print"),
        actionButton("get_geo", "Get geo data")
      ),
      mainPanel(
        # outputs
        dataTableOutput("geo"),
        hidden(
          p(id = "work_geo", "Processing...")
        )
      )
    )
  ),
  tabPanel(
    title = "Get adjustmented table",
    sidebarLayout(
      sidebarPanel(
        # inputs
        selectInput("mode", "Adjustment mode", choices = c("std", "crd")),
        selectInput("fraction", "Fraction", choices = c("100000")),
        selectInput("conf_level", "Confidence level", choices = c("0.95")),
        actionButton("print_table_adj", "Print"),
        actionButton("get_table_adj", "Get adjustmented table")
      ),
      mainPanel(
        # outputs
        dataTableOutput("table_adj"),
        hidden(
          p(id = "work_table_adj", "Processing...")
        )
      )
    )
  )
)

cohort_ui <- fluidPage(
  titlePanel(
    # app title/description
    "Pre-processing your data"
  ),
  cohort_tab
)

cohort_server <- function(input, output, session, transfer) {
  observeEvent(input$print_cohort_table, {
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

  observeEvent(input$print_table_adj, {
    params <- list()
    params$mode <- input$mode
    params$fraction <- input$fraction
    params$conf_level <- input$conf_level

    message("adj_params: ", toString(params))
  })

  observe({
    disable("get_cohort_table")

    if (length(transfer$cohort_ids()) > 0) {
      enable("get_cohort_table")
    }
  })

  observe({
    disable("get_table_adj")

    if (length(cohort_table()) > 0 & length(geo()@data) > 0) {
      enable("get_table_adj")
    }
  })

  observe({
    updateSelectInput(
      session,
      "target_cohort_definition_id",
      choices = transfer$cohort_ids()
    )
    updateSelectInput(
      session,
      "outcome_cohort_definition_id",
      choices = transfer$cohort_ids()
    )
  })

  cohort_table <- eventReactive(input$get_cohort_table, {
    disable("get_cohort_table")
    show("work_cohort_table")

    query <- parseQueryString(session$clientData$url_search)
    message("query: ", toString(paste(names(query), query, sep = "=", collapse=", ")))

    if (!is.null(query$demo) & isTRUE(as.logical(query$demo))) {
      cohort_table <- AegisApp::cohort_table
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

      # Get cohort table
      conn_info <- conn_info
      cdm_database_schema <- input$cdm_database_schema
      result_database_schema <- input$result_database_schema
      target_cohort_definition_id <- input$target_cohort_definition_id
      outcome_cohort_definition_id <- input$outcome_cohort_definition_id
      cohort_start_date <- input$cohort_start_date
      cohort_end_date <- input$cohort_end_date
      time_at_risk_start_date <- input$time_at_risk_start_date
      time_at_risk_end_date <- input$time_at_risk_end_date
      time_at_risk_end_date_panel <- input$time_at_risk_end_date_panel

      cohort_table <- get_cohort_analysis_table(
        conn_info = conn_info,
        cdm_database_schema = cdm_database_schema,
        result_database_schema = result_database_schema,
        target_cohort_definition_id = target_cohort_definition_id,
        outcome_cohort_definition_id = outcome_cohort_definition_id,
        cohort_start_date = cohort_start_date,
        cohort_end_date = cohort_end_date,
        time_at_risk_start_date = time_at_risk_start_date,
        time_at_risk_end_date = time_at_risk_end_date,
        time_at_risk_end_date_panel = time_at_risk_end_date_panel
      )
    }

    hide("work_cohort_table")
    enable("get_cohort_table")

    cohort_table
  })

  output$cohort_table <- renderDataTable(
    cohort_table(),
    options = list(pageLength = 5)
  )

  geo <- eventReactive(input$get_geo, {
    disable("get_geo")
    show("work_geo")

    # Get geo data
    name <- input$name
    country <- input$country
    level <- input$level

    geo <- get_geo_data(
      name = name,
      country = country,
      level = level
    )

    hide("work_geo")
    enable("get_geo")

    geo
  })

  output$geo <- renderDataTable(
    geo()@data,
    options = list(pageLength = 5)
  )

  table_adj <- eventReactive(input$get_table_adj, {
    disable("get_table_adj")
    show("work_table_adj")

    # Map cohort table with geo data
    latlong <- cohort_table()
    geo <- geo()

    geo_map <- map_latlong_geo(
      latlong = latlong,
      geo = geo
    )


    # Arrange table
    table <- geo_map

    table_arr <- calculate_count_with_geo_oid(
      table = table
    )


    # Adjustment for age and sex
    table <- table_arr
    mode <- input$mode
    fraction <- input$fraction
    conf_level <- input$conf_level

    table_adj <- calculate_adjust_age_sex_indirectly(
      table = table,
      mode = mode,
      fraction = fraction,
      conf_level = conf_level
    )

    hide("work_table_adj")
    enable("get_table_adj")

    table_adj
  })

  output$table_adj <- renderDataTable(
    table_adj(),
    options = list(pageLength = 5)
  )

  list(
    geo = reactive(geo()),
    table_adj = reactive(table_adj())
  )
}
