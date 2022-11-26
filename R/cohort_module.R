cohortUI <- function(id) {
  htmltools::tagList(
    # UI here
    shiny::titlePanel(
      # app title/description
      "Get cohort table"
    ),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        # inputs
        shiny::selectInput(shiny::NS(id, "model"), "Model to analyse", choices = c("spatial", "temporal", "spatio-temporal")),
        # shiny::selectInput(shiny::NS(id, "model"), "Model to analyse", choices = c("temporal", "spatio-temporal")),
        shiny::selectInput(shiny::NS(id, "target_cohort_definition_id"), "Target cohort definition id", choices = c()),
        shiny::selectInput(shiny::NS(id, "outcome_cohort_definition_id"), "Outcome cohort definition id", choices = c()),
        shiny::conditionalPanel(
          condition = "['spatial', 'spatio-temporal'].includes(input.model)",
          shiny::dateInput(shiny::NS(id, "cohort_start_date"), "Cohort start date", value = "2020-01-01"),
          shiny::dateInput(shiny::NS(id, "cohort_end_date"), "Cohort end date", value = "2020-12-31"),
          shiny::textInput(shiny::NS(id, "time_at_risk_start_date"), "Time at risk start date", value = "0"),
          shiny::textInput(shiny::NS(id, "time_at_risk_end_date"), "Time at risk end date", value = "0"),
          shiny::selectInput(shiny::NS(id, "time_at_risk_end_date_panel"), "Time at risk end date panel", choices = c("cohort_start_date", "cohort_end_date"))
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
        shiny::actionButton(shiny::NS(id, "print"), "Print"),
        shiny::actionButton(shiny::NS(id, "get_cohort_table"), "Get cohort table")
      ),
      shiny::mainPanel(
        # outputs
        shiny::dataTableOutput(shiny::NS(id, "cohort_table"))
      )
    )
  )
}

cohortServer <- function(id, data) {
# cohortServer <- function(id, data, filter = is.numeric) {
  # stopifnot(is.reactive(data))
  # stopifnot(!is.reactive(filter))

  shiny::moduleServer(id, function(input, output, session) {
    # Logic here
    shiny::observeEvent(input$print, {
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
      message("sum: ", toString(sum(match(input$model, c("spatial", "spatio-temporal"), nomatch = 0))))
    })

    shiny::observe({
      shinyjs::disable("get_cohort_table")

      if (length(data$cohort_ids()) > 0) {
        shinyjs::enable("get_cohort_table")
      }
    })

    shiny::observe({
      shiny::updateSelectInput(
        session,
        "target_cohort_definition_id",
        choices = data$cohort_ids()
      )
      shiny::updateSelectInput(
        session,
        "outcome_cohort_definition_id",
        choices = data$cohort_ids()
      )
    })

    cohort_table <- shiny::eventReactive(input$get_cohort_table, {
      shinyjs::disable("get_cohort_table")

      query <- shiny::parseQueryString(session$clientData$url_search)
      message("query: ", toString(paste(names(query), query, sep = "=", collapse=", ")))

      if (!is.null(query$demo) & isTRUE(as.logical(query$demo))) {
        if(input$model == "spatial") {
          cohort_table <- AegisApp::cohort_table_for_spatial
        } else if (input$model == "temporal") {
          cohort_table <- AegisApp::cohort_table_for_temporal
        } else if (input$model == "spatio-temporal") {
          cohort_table <- AegisApp::cohort_table_for_spatio_temporal
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

      shinyjs::enable("get_cohort_table")

      cohort_table
    })

    output$cohort_table <- shiny::renderDataTable(
      cohort_table(),
      options = list(pageLength = 5)
    )

    list(
      model = shiny::reactive(input$model),
      cohort_table = shiny::reactive(cohort_table())
    )

    # Use data param like so:
    # observeEvent(data(), {
    #   updateSelectInput(session, "var", choices = find_vars(data(), filter))
    # })
    #
    # reactive(data()[[input$var]])
  })
}
