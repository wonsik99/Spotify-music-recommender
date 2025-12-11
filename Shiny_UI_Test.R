library(shiny)
library(tidyverse)
library(dplyr)

## 0. 데이터 로드 & 전처리 ----
load("spotify_mpd_01.RData")

new_track_attr <- track_attr |>
  select(
    track_id, artists, track_name,
    danceability, energy, loudness,
    acousticness, valence, tempo,
    track_genre
  ) |>
  drop_na()

# 드롭다운 라벨: "곡 제목 – 아티스트"
song_choices <- new_track_attr |>
  mutate(label = paste0(track_name, " – ", artists)) |>
  select(track_id, label)

choices_vec <- setNames(song_choices$track_id, song_choices$label)


## 1. UI ----
ui <- fluidPage(
  titlePanel("Spotify Song Search + Euclidean Recommendations"),
  
  sidebarLayout(
    sidebarPanel(
      # 여러 곡 선택 가능 (최대 3곡까지)
      selectizeInput(
        inputId  = "song_query",
        label    = "Type a song title (select up to 3):",
        choices  = choices_vec,
        multiple = TRUE,
        options  = list(
          placeholder = "예: HUMBLE",
          maxOptions  = 50,
          maxItems    = 3        # 최대 3곡까지 선택
        )
      )
    ),
    
    mainPanel(
      h4("Selected songs"),
      tableOutput("selected_songs"),
      br(),
      h4("Top 10 similar songs (Euclidean distance)"),
      tableOutput("recommendations")
    )
  )
)


## 2. SERVER ----
server <- function(input, output, session) {
  
  # 1) 유저가 선택한 곡들 ----
  selected_songs_reactive <- reactive({
    req(input$song_query)   # 최소 한 곡은 선택해야 함
    
    new_track_attr |>
      filter(track_id %in% input$song_query)
  })
  
  output$selected_songs <- renderTable({
    selected_songs_reactive() |>
      select(track_id, track_name, artists, track_genre,
             danceability, energy, loudness,
             acousticness, valence, tempo)
  })
  
  # 2) target_profile 계산 (선택된 곡들의 feature 평균) ----
  target_profile_reactive <- reactive({
    songs <- selected_songs_reactive()
    
    songs |>
      summarise(
        target_dance     = mean(danceability),
        target_energy    = mean(energy),
        target_loudness  = mean(loudness),
        target_acoustic  = mean(acousticness),
        target_valence   = mean(valence),
        target_tempo     = mean(tempo)
      )
  })
  
  # 3) Euclidean distance 기반 추천 ----
  recommendations_reactive <- reactive({
    tp <- target_profile_reactive()
    
    new_track_attr |>
      # 유저가 선택한 곡은 추천 리스트에서 제외
      filter(!track_id %in% input$song_query) |>
      mutate(
        similarity_distance = sqrt(
          (danceability - tp$target_dance    )^2 +
            (energy       - tp$target_energy   )^2 +
            (loudness     - tp$target_loudness )^2 +
            (acousticness - tp$target_acoustic )^2 +
            (valence      - tp$target_valence  )^2 +
            (tempo        - tp$target_tempo    )^2
        )
      ) |>
      arrange(similarity_distance) |>
      head(10)
  })
  
  output$recommendations <- renderTable({
    recommendations_reactive() |>
      select(track_name, artists, track_genre,
             danceability, energy, loudness,
             acousticness, valence, tempo,
             similarity_distance)
  })
}

## 3. APP 실행 ----
shinyApp(ui, server)