leafletUI <- function(id) {
  ns <- NS(id)
  # htmltools::tagList(
    # UI here
    # shiny::tabPanel(
      # title = "Plot",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          # inputs
          shiny::selectInput(shiny::NS(id, "color_type"), "Color type", choices = c("colorQuantile", "colorBin", "colorNumeric", "colorFactor")),
          shiny::selectInput(shiny::NS(id, "palette"), "Palette", choices = c("Reds", "Greens")),
          shiny::textInput(shiny::NS(id, "domain"), "Domain", value = ""),
          shiny::textInput(shiny::NS(id, "na_color"), "NA color", value = "#FFFFFF"),
          shiny::selectInput(shiny::NS(id, "alpha"), "Alpha", choices = c(TRUE, FALSE), selected = FALSE),
          shiny::selectInput(shiny::NS(id, "reverse"), "Reverse", choices = c(TRUE, FALSE), selected = FALSE),
          shiny::conditionalPanel(
            condition = paste0("['colorQuantile', 'colorBin'].includes(input[\'", ns("color_type"), "\'])"),
            # condition = "['colorQuantile', 'colorBin'].includes(input.color_type)",
            shiny::selectInput(shiny::NS(id, "right"), "Right", choices = c(TRUE, FALSE), selected = FALSE)
          ),
          shiny::conditionalPanel(
            condition = paste0("['colorQuantile'].includes(input[\'", ns("color_type"), "\'])"),
            # condition = "['colorQuantile'].includes(input.color_type)",
            shiny::numericInput(shiny::NS(id, "n"), "n", value = 9, min = 1, max = 9),
          ),
          shiny::conditionalPanel(
            condition = paste0("['colorBin'].includes(input[\'", ns("color_type"), "\'])"),
            # condition = "['colorBin'].includes(input.color_type)",
            shiny::numericInput(shiny::NS(id, "bins"), "Bins", value = 7, min = 1, max = 9),
            shiny::selectInput(shiny::NS(id, "pretty"), "Pretty", choices = c(TRUE, FALSE), selected = TRUE),
          ),
          shiny::conditionalPanel(
            condition = paste0("['colorFactor'].includes(input[\'", ns("color_type"), "\'])"),
            # condition = "['colorFactor'].includes(input.color_type)",
            shiny::textInput(shiny::NS(id, "levels"), "Levels", value = ""),
            shiny::selectInput(shiny::NS(id, "ordered"), "Ordered", choices = c(TRUE, FALSE), selected = FALSE),
          ),
          shiny::conditionalPanel(
            condition = paste0("['colorNumeric'].includes(input[\'", ns("color_type"), "\'])"),
            # condition = "['colorNumeric'].includes(input.color_type)"
            # do something
          ),
          shiny::actionButton(shiny::NS(id, "print"), "Print"),
          shiny::actionButton(shiny::NS(id, "plot"), "Plot")
        ),
        shiny::mainPanel(
          # outputs
          leaflet::leafletOutput(shiny::NS(id, "output_plot")),
          shiny::uiOutput(shiny::NS(id, "output_plots"))
        )
      )
    # )
  # )
}
