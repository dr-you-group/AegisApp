disease_cluster_ui <- fluidPage(
  actionButton("plot_disease_cluster", "Plot disease cluster"),

  plotOutput("disease_cluster", width = "400px")
)

disease_cluster_server <- function(input, output, session) {

  disease_cluster <- eventReactive(input$plot_disease_cluster, {
    plot(6:10)
  })

  output$disease_cluster <- renderPlot(
    disease_cluster(),
    res = 96
  )

  # render.table <- eventReactive(input$submit_table, {})
  # observeEvent(input$submit_plot, {})
  # observeEvent(input$submit_cluster, {})
}
