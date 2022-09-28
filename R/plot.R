plot_ui <- function(id) {
  plotOutput(NS(id, "plot"), width = "400px")
}

plot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlot(plot(1:5), res = 96)
  })
}
