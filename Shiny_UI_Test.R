library(shiny)
library(tidyverse)
library(dplyr)

## 0. 데이터 로드 & 전처리 (전역 영역) ----
## app.R랑 같은 폴더에 spotify_mpd_01.RData 파일이 있어야 해!
load("spotify_mpd_01.RData")

# track_attr에서 필요한 컬럼만 뽑고 NA 제거
new_track_attr <- track_attr |>
  select(
    track_id, artists, track_name,
    danceability, energy, loudness,
    acousticness, valence, tempo,
    track_genre
  ) |>
  drop_na()

# 드롭다운에 보여줄 라벨: "곡 제목 – 아티스트"
song_choices <- new_track_attr |>
  mutate(label = paste0(track_name, " – ", artists)) |>
  select(track_id, label)

# selectizeInput에 넣을 choices 벡터 (이름=라벨, 값=track_id)
choices_vec <- setNames(song_choices$track_id, song_choices$label)


## 1. UI ----
ui <- fluidPage(
  titlePanel("Spotify Song Search (type + inline suggestions)"),
  
  sidebarLayout(
    sidebarPanel(
      # 여기 한 칸에서 타이핑 + 드롭다운 제안까지 다 처리
      selectizeInput(
        inputId = "song_query",
        label   = "Type a song title:",
        # 🔽 맨 위에 '빈' 항목 하나 추가
        choices = c(" " = "", choices_vec),
        selected = "",   # 앱 시작할 때는 이 빈 값이 선택됨
        options = list(
          placeholder = "예: HUMBLE",
          maxOptions  = 50,
          create      = TRUE,
          openOnFocus = FALSE
        )
      )
    ),
    
    mainPanel(
      h4("Selected / typed value"),
      tableOutput("selected_song")
    )
  )
)


## 2. SERVER ----
server <- function(input, output, session) {
  
  output$selected_song <- renderTable({
    # 아무 것도 입력/선택 안 했으면 실행 X
    req(input$song_query)
    
    # input$song_query 에는
    # - 드롭다운에서 선택하면: track_id 값이 들어오고
    # - 그냥 없는 텍스트를 입력하면: 그 텍스트가 그대로 들어옴
    #
    # 우선 track_id로 매칭되는 곡 있으면 그걸 보여주고,
    # 없으면 그냥 빈 테이블 리턴하도록 할게
    
    output$selected_song <- renderTable({
      req(input$song_query != "")   # ""일 때는 테이블 안 그림
      
      new_track_attr |>
        filter(track_id == input$song_query) |>
        select(track_name, artists, track_genre,
               danceability, energy, loudness,
               acousticness, valence, tempo)
    })
  })
}

## 3. APP 실행 ----
shinyApp(ui, server)