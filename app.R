library(shiny)
library(tidyverse)
library(fmsb)
library(ggplot2)

load("spotify_mpd_01.RData")

new_track_attr <- track_attr |> 
  select(track_id, artists, track_name, danceability, energy, loudness,
         acousticness, valence, tempo, track_genre) |>
  drop_na()



ui <- fluidPage(
  titlePanel("Search Bar with Dropdown Example"),
  
  sidebarLayout(
    sidebarPanel(
      # Use selectizeInput for the search/dropdown functionality
      selectizeInput(
        inputId = "search_select",
        label = "Search and Select an Option:",
        choices = c("", paste(track_attr$track_name, "-", track_attr$artists)), # 'state.name' is a built-in R dataset
        selected = "",
        multiple = T, # Set to TRUE for multi-selection
        options = list(
          placeholder = 'Start typing to search...',
          onInitialize = I('function() { this.setValue(""); }') # Optional: ensures placeholder is shown on load
        )
      )
    ),
    
    mainPanel(
      h4("You selected:"),
      verbatimTextOutput("selected_value"),
      h4("Recommendations"),
      tableOutput("resultsTable"), 
      uiOutput("chooseResultUi")
    )
  )
)

server = function(input, output) {
  
  output$selected_value <- renderPrint({
    input$search_select
})
  resultList = reactive({
    req(input$search_select)
    user_input = new_track_attr |> filter(track_name %in% str_split_i(input$search_select, " - ", 1))
    train_data <- new_track_attr |>
      mutate(liked = ifelse(track_id %in% user_input$track_id, 1, 0)) |>
      mutate(is_genre = ifelse(track_genre %in% user_input$track_genre, 1, 0))
    preference_model <- glm(liked ~ danceability + energy + loudness + acousticness + valence + tempo + is_genre,
                            data = train_data, family = binomial)
    prediction_result <- train_data |>
      ungroup() |>
      mutate(predicted_score = predict(preference_model, newdata = train_data, type = "response")) |>
      filter(!track_id %in% user_input$track_id) |> top_n(10, predicted_score) |> # remove input songs
      arrange(desc(predicted_score)) # order
    prediction_result
  })
  
  output$resultsTable = renderTable({
    req(input$search_select)
    resultList()
  })
  
  
  output$chooseResultUi = renderUI({
    options = resultList()
    selectInput("resultSelect", "Select Result", choices = options$track_name)
  })
  
}

shinyApp(ui, server)

