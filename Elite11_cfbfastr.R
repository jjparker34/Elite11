library(tidyverse)
library(shiny)

Sys.setenv(CFBD_API_KEY = "S2JgCsvJ5RPuw2dDwPquZ1xTKefAk5UT/H0qeK9jZAAH46WMsGbKtfRJ4odTswoX")
Elite11Players <- read_csv("Elite11Players.csv")


#Elite 11 MVPs
MVPs <- Elite11Players %>%
  filter(stringr::str_detect(Awards, "Elite"))

MVPs_after_2010 <- MVPs %>%
  filter(Year >= 2010)



MVP_stats_avg <- read_csv("MVP_stats_avg.csv")
MVP_stats_yearly <- read_csv("MVP_stats.csv")

#shiny display
ui <- fluidPage(
  titlePanel("My Shiny App"),
  sidebarLayout(
    sidebarPanel(
      # Input elements go here
    ),
    mainPanel(
      # Output elements go here
    )
  )
)

server <- function(input, output) {
  # Define reactive objects and functions here
}

# Example of a reactive object
reactive_data <- reactive({
  # Compute or process data based on user inputs
  # Return the result
})

output$plot <- renderPlot({
  # Generate a plot based on reactive data
})

shinyApp(ui = ui, server = server)
