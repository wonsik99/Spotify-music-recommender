library(shiny)
library(tidyverse)
library(dplyr)


load("spotify_mpd_01.RData")

new_track_attr <- track_attr |> 
  select(
    track_id, artists, track_name,
    danceability, energy, loudness,
    acousticness, valence, tempo,
    track_genre
  ) |>
  drop_na()


ui <- fluidPage(
  
  titlePanel("Simple Spotify Recommender (Prototype)"),
  
  sidebarLayout(
    sidebarPanel(
      # User Input for Song Name
      textInput(
        inputId = "song_query",
        label   = "Type a song title:",
        value   = "",
        placeholder = "예: Shape of You"
      ),
      
      # Search Button
      actionButton(
        inputId = "search_btn",
        label   = "Search"
      ),
      
      br(), br(),
      helpText("You can type just part of the song title. (Case-insensitive)")
    ),
    
    mainPanel(
      h4("Matching songs"),
      tableOutput("song_results")
    )
  )
)

## 3. SERVER ----

server <- function(input, output, session) {
  
  # Reacts Everytime the User Presses Search Button
  observeEvent(input$search_btn, {
    
    # If user inputs empty
    if (input$song_query == "") {
      results <- new_track_attr |> head(0)
    } else {
      # Search for tracks whose titles contain the user-entered text (partial match, case-insensitive).
      results <- new_track_attr |>
        filter(str_detect(
          track_name,
          regex(input$song_query, ignore_case = TRUE)
        )) |>
        select(track_name, artists, track_genre, danceability, energy, valence) |>
        head(20)   # Only Top 20
    }
    
    output$song_results <- renderTable(results)
  })
}

## 4. Shiny App ----

shinyApp(ui = ui, server = server)
