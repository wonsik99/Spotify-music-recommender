library(shiny)
library(tidyverse)

load("spotify_mpd_01.RData")

ui <- fluidPage(
  titlePanel("Search Bar with Dropdown Example"),
  
  sidebarLayout(
    sidebarPanel(
      # Use selectizeInput for the search/dropdown functionality
      selectizeInput(
        inputId = "search_select",
        label = "Search and Select an Option:",
        choices = c("", paste(track_attr$track_name, "by", track_attr$artists)), # 'state.name' is a built-in R dataset
        selected = "",
        multiple = FALSE, # Set to TRUE for multi-selection
        options = list(
          placeholder = 'Start typing to search...',
          onInitialize = I('function() { this.setValue(""); }') # Optional: ensures placeholder is shown on load
        )
      )
    ),
    
    mainPanel(
      h4("You selected:"),
      verbatimTextOutput("selected_value")
    )
  )
)

server = function(input, output) {
  output$selected_value <- renderPrint({
    input$search_select
})
}

shinyApp(ui, server)
