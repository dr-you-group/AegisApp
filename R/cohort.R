cohort_tab <- shiny::tabsetPanel(
  id = "cohort_tab",
  type = "tabs",
  shiny::tabPanel(
    title = "Get cohort table",
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        # inputs
        # shiny::selectInput("model", "Model to analyse", choices = c("spatial", "temporal", "spatio-temporal")),
        shiny::selectInput("model", "Model to analyse", choices = c("spatial", "temporal")),
        shiny::selectInput("target_cohort_definition_id", "Target cohort definition id", choices = c()),
        shiny::selectInput("outcome_cohort_definition_id", "Outcome cohort definition id", choices = c()),
        shiny::conditionalPanel(
          condition = "['spatial', 'spatio-temporal'].includes(input.model)",
          shiny::dateInput("cohort_start_date", "Cohort start date", value = "2020-01-01"),
          shiny::dateInput("cohort_end_date", "Cohort end date", value = "2020-12-31"),
          shiny::textInput("time_at_risk_start_date", "Time at risk start date", value = "0"),
          shiny::textInput("time_at_risk_end_date", "Time at risk end date", value = "0"),
          shiny::selectInput("time_at_risk_end_date_panel", "Time at risk end date panel", choices = c("cohort_start_date", "cohort_end_date"))
        ),
        shiny::conditionalPanel(
          condition = "['temporal'].includes(input.model)"
          # do something
        ),
        # shiny::conditionalPanel(
        #   condition = "['spatio-temporal'].includes(input.model)",
        #   shiny::selectInput("target_cohort_definition_id", "Target cohort definition id", choices = c()),
        #   shiny::selectInput("outcome_cohort_definition_id", "Outcome cohort definition id", choices = c()),
        #   shiny::dateInput("cohort_start_date", "Cohort start date", value = "2020-01-01"),
        #   shiny::dateInput("cohort_end_date", "Cohort end date", value = "2020-12-31"),
        #   shiny::textInput("time_at_risk_start_date", "Time at risk start date", value = "0"),
        #   shiny::textInput("time_at_risk_end_date", "Time at risk end date", value = "0"),
        #   shiny::selectInput("time_at_risk_end_date_panel", "Time at risk end date panel", choices = c("cohort_start_date", "cohort_end_date"))
        # ),
        shiny::actionButton("print_cohort_table", "Print"),
        shiny::actionButton("get_cohort_table", "Get cohort table")
      ),
      shiny::mainPanel(
        # outputs
        shiny::dataTableOutput("cohort_table"),
        shinyjs::hidden(
          htmltools::p(id = "work_cohort_table", "Processing...")
        )
      )
    )
  ),
  shiny::tabPanel(
    title = "Get geo data",
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        # inputs
        shiny::selectInput("name", "Source of the geo data", choices = c("KOR", "GADM")),
        shiny::selectInput("country", "Country", choices = c("KOR")),
        shiny::selectInput("level", "Level of the administrative data", choices = c(2:3)),
        shiny::actionButton("print_geo", "Print"),
        shiny::actionButton("get_geo", "Get geo data")
      ),
      shiny::mainPanel(
        # outputs
        shiny::dataTableOutput("geo"),
        shinyjs::hidden(
          htmltools::p(id = "work_geo", "Processing...")
        )
      )
    )
  ),
  shiny::tabPanel(
    title = "Get adjusted table",
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        # inputs
        shiny::selectInput("mode", "Adjustment mode", choices = c("std", "crd")),
        shiny::selectInput("fraction", "Fraction", choices = c("100000")),
        shiny::selectInput("conf_level", "Confidence level", choices = c("0.95")),
        shiny::actionButton("print_table_adj", "Print"),
        shiny::actionButton("get_table_adj", "Get adjusted table")
      ),
      shiny::mainPanel(
        # outputs
        shiny::dataTableOutput("table_adj"),
        shinyjs::hidden(
          htmltools::p(id = "work_table_adj", "Processing...")
        )
      )
    )
  )
)

cohort_ui <- shiny::fluidPage(
  shiny::titlePanel(
    # app title/description
    "Pre-processing your data"
  ),
  cohort_tab
)

cohort_server <- function(input, output, session, transfer) {
  shiny::observeEvent(input$print_cohort_table, {
    params <- list()
    params$model <- input$model
    params$dbms <- input$dbms
    params$path_to_driver <- input$path_to_driver
    params$connection_string <- input$connection_string
    params$target_cohort_definition_id <- input$target_cohort_definition_id
    params$outcome_cohort_definition_id <- input$outcome_cohort_definition_id
    params$cohort_start_date <- input$cohort_start_date
    params$cohort_end_date <- input$cohort_end_date
    params$time_at_risk_start_date <- input$time_at_risk_start_date
    params$time_at_risk_end_date <- input$time_at_risk_end_date
    params$time_at_risk_end_date_panel <- input$time_at_risk_end_date_panel

    message("cohort_params: ", toString(params))
    message("sum: ", toString(sum(match(input$model, c("spatial", "spatio-temporal")))))
  })

  shiny::observeEvent(input$print_geo, {
    params <- list()
    params$name <- input$name
    params$country <- input$country
    params$level <- input$level

    message("geo_params: ", toString(params))
  })

  shiny::observeEvent(input$print_table_adj, {
    params <- list()
    params$model <- input$model
    params$mode <- input$mode
    params$fraction <- input$fraction
    params$conf_level <- input$conf_level

    message("adj_params: ", toString(params))
  })

  shiny::observe({
    shinyjs::disable("get_cohort_table")

    if (length(transfer$cohort_ids()) > 0) {
      shinyjs::enable("get_cohort_table")
    }
  })

  shiny::observe({
    shinyjs::disable("get_geo")

    if (sum(match(input$model, c("spatial", "spatio-temporal"), nomatch = 0)) > 0 &
        length(cohort_table()) > 0) {
      shinyjs::enable("get_geo")
    }
  })

  shiny::observe({
    shinyjs::disable("get_table_adj")

    if (sum(match(input$model, c("spatial", "spatio-temporal"), nomatch = 0)) > 0 &
        length(cohort_table()) > 0 &
        length(geo()@data) > 0) {
      shinyjs::enable("get_table_adj")
    }
  })

  shiny::observe({
    shiny::updateSelectInput(
      session,
      "target_cohort_definition_id",
      choices = transfer$cohort_ids()
    )
    shiny::updateSelectInput(
      session,
      "outcome_cohort_definition_id",
      choices = transfer$cohort_ids()
    )
  })

  cohort_table <- shiny::eventReactive(input$get_cohort_table, {
    shinyjs::disable("get_cohort_table")
    shinyjs::show("work_cohort_table")

    query <- shiny::parseQueryString(session$clientData$url_search)
    message("query: ", toString(paste(names(query), query, sep = "=", collapse=", ")))

    if (!is.null(query$demo) & isTRUE(as.logical(query$demo))) {
      if(input$model == "spatial") {
        cohort_table <- AegisApp::cohort_table_for_spatial
      } else if (input$model == "temporal") {
        cohort_table <- AegisApp::cohort_table_for_temporal
      # } else if (input$model == "spatio-temporal") {
      #   cohort_table <- AegisApp::cohort_table_for_spatio_temporal
      } else {
        cohort_table <- AegisApp::cohort_table
      }
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

      # Get cohort table
      model <- input$model
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

      cohort_table <- AegisFunc::get_cohort_analysis_table(
        model = model,
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

    shinyjs::hide("work_cohort_table")
    shinyjs::enable("get_cohort_table")

    cohort_table
  })

  output$cohort_table <- shiny::renderDataTable(
    cohort_table(),
    options = list(pageLength = 5)
  )

  geo <- shiny::eventReactive(input$get_geo, {
    shinyjs::disable("get_geo")
    shinyjs::show("work_geo")

    # Get geo data
    name <- input$name
    country <- input$country
    level <- input$level

    geo <- AegisFunc::get_geo_data(
      name = name,
      country = country,
      level = level
    )

    shinyjs::hide("work_geo")
    shinyjs::enable("get_geo")

    geo
  })

  output$geo <- shiny::renderDataTable(
    geo()@data,
    options = list(pageLength = 5)
  )

  table_adj <- shiny::eventReactive(input$get_table_adj, {
    shinyjs::disable("get_table_adj")
    shinyjs::show("work_table_adj")

    # Map cohort table with geo data
    latlong <- cohort_table()
    geo <- geo()

    geo_map <- AegisFunc::map_latlong_geo(
      latlong = latlong,
      geo = geo
    )


    # Arrange table
    model <- input$model
    table <- geo_map

    table_arr <- AegisFunc::calculate_count_with_geo_oid(
      model = model,
      table = table
    )


    # Adjustment for age and sex
    model <- input$model
    table <- table_arr
    mode <- input$mode
    fraction <- input$fraction
    conf_level <- input$conf_level

    table_adj <- AegisFunc::calculate_adjust_age_sex_indirectly(
      model = model,
      table = table,
      mode = mode,
      fraction = fraction,
      conf_level = conf_level
    )

    shinyjs::hide("work_table_adj")
    shinyjs::enable("get_table_adj")

    table_adj
  })

  output$table_adj <- shiny::renderDataTable(
    table_adj(),
    options = list(pageLength = 5)
  )

  list(
    cohort_table = shiny::reactive(cohort_table()),
    geo = shiny::reactive(geo()),
    table_adj = shiny::reactive(table_adj())
  )
}
