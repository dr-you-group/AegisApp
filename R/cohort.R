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

    actionButton("save_cohort_info", "Save!"),
    actionButton("init_cohort_info", "Initialize")
  ),
  tabPanel(
    title = "Select geo data",

    selectInput("name", "Source of the geo data", c("GADM", "KOR")),
    selectInput("country", "Country", c("KOR")),
    radioButtons("level", "Level of the administrative data", c(2:3)),

    actionButton("save_geo_info", "Save!"),
    actionButton("init_geo_info", "Initialize")
  ),
  tabPanel(
    title = "Set adjustment",

    selectInput("mode", "Adjustment mode", c("Std", "Crd")),
    selectInput("fraction", "Fraction", c("100000")),
    selectInput("conf_level", "Confidence level", c("0.95")),

    actionButton("save_adj_info", "Save!"),
    actionButton("init_adj_info", "Initialize")
  ),
  tabPanel(
    title = "Get cohort table",

    actionButton("get_cohort", "Get cohort table"),
  )
)

cohort_ui <- fluidPage(
  cohort_tab
)

cohort_server <- function(input, output, session) {
  output$text_cohort <- renderText({
    input$text_cohort
  })
}
