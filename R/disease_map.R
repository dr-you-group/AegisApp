disease_map_ui <- fluidPage(
  actionButton("plot_disease_map", "Plot disease map"),

  plotOutput("disease_map", width = "400px")
)

disease_map_server <- function(input, output, session) {

  disease_map <- eventReactive(input$plot_disease_map, {
    plot(1:5)
  })

  output$disease_map <- renderPlot(
    disease_map(),
    res = 96
  )

  # render.table <- eventReactive(input$submit_table, {})
  # observeEvent(input$submit_plot, {})
  # observeEvent(input$submit_cluster, {})
}
