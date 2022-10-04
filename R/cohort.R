cohort_tab <- tabsetPanel(
  id = "cohort_tab",
  type = "tabs",

  tabPanel(
    title = "Define cohort",

    selectInput("target_cohort_definition_id", "Target cohort definition id", c(1:10)),
    selectInput("outcome_cohort_definition_id", "Outcome cohort definition id", c(1:10)),
    dateInput("cohort_start_date", "Cohort start date", "2020-01-01"),
    dateInput("cohort_end_date", "Cohort end date", "2020-12-31"),
    textInput("time_at_risk_start_date", "Time at risk start date", "0"),
    textInput("time_at_risk_end_date", "Time at risk end date", "0"),
    radioButtons("time_at_risk_end_date_panel", "Time at risk end date panel", c("cohort_start_date", "cohort_end_date")),

    actionButton("print_cohort", "Print"),
  ),
  tabPanel(
    title = "Select geo data",

    selectInput("name", "Source of the geo data", c("GADM", "KOR")),
    selectInput("country", "Country", c("KOR")),
    radioButtons("level", "Level of the administrative data", c(2:3)),

    actionButton("print_geo", "Print"),
  ),
  tabPanel(
    title = "Set adjustment",

    selectInput("mode", "Adjustment mode", c("Std", "Crd")),
    selectInput("fraction", "Fraction", c("100000")),
    selectInput("conf_level", "Confidence level", c("0.95")),

    actionButton("print_adj", "Print"),
  ),
  tabPanel(
    title = "Get cohort table",

    actionButton("get_cohort", "Get cohort table"),

    dataTableOutput("cohort")
  )
)

cohort_ui <- fluidPage(
  cohort_tab
)

cohort_server <- function(input, output, session) {
  output$text_cohort <- renderText({
    input$text_cohort
  })

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

  cohort <- eventReactive(input$get_cohort, {
    mtcars
  })

  output$cohort <- renderDataTable(
    cohort(),
    options = list(pageLength = 5)
  )


  # render.table <- eventReactive(input$submit_table, {})
  # observeEvent(input$submit_plot, {})
  # observeEvent(input$submit_cluster, {})
}
