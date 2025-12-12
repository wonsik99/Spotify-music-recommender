library(shiny)
library(tidyverse)
library(fmsb)
library(ggplot2)
library(DT)

load("spotify_mpd_01.RData")

# filter cols, NA 
new_track_attr <- track_attr |> 
  select(track_id, artists, track_name, danceability, energy, loudness,
         acousticness, valence, tempo, track_genre) |>
  drop_na()

# normalize function (min-max scaling)
normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# scaled tracks (0~1)
scaled_tracks <- new_track_attr |>
  ungroup() |>
  mutate(across(c(danceability, energy, loudness, acousticness, valence, tempo), normalize))

# matrix for cosine similarity
track_matrix <- scaled_tracks |>
  select(danceability, energy, loudness, acousticness, valence, tempo) |>
  as.matrix()

# L2 norms for each track vector
norm_tracks <- sqrt(rowSums(track_matrix^2))

# selectize choices (value = track_id, label = "track - artist")
search_choices <- setNames(
  new_track_attr$track_id,
  paste(new_track_attr$track_name, "-", new_track_attr$artists)
)

ui <- fluidPage(
  titlePanel("Music Recommender"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = "search_select",
        label = "Search and Select 3 songs:",
        choices = NULL,  # load in server
        multiple = TRUE,
        options = list(
          placeholder = 'Start typing to search...',
          maxItems = 3
        )
      ),
      actionButton("btn_recommend", "Get Recommendation",
                   class = "btn-primary btn-lg", width = "100%"),
      uiOutput("warning_msg")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("recommendation result", 
                 h3("Top 10 Recommendations"),
                 DTOutput("resultsTable")
        ),
        tabPanel("Radar Chart", 
                 h4("music profile"),
                 plotOutput("radarCharts")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # loading selectize options
  updateSelectizeInput(session, "search_select", choices = search_choices, server = TRUE)
  
  recommendation_data <- eventReactive(input$btn_recommend, {
    
    # warning message (only after button click)
    output$warning_msg <- renderUI({
      req(input$btn_recommend)
      if (length(input$search_select) != 3) {
        div(
          style = "color: red; font-weight: bold; margin-top: 10px; text-align: center;",
          paste("Currently selected", length(input$search_select), ". Please select 3 songs")
        )
      }
    })
    
    # stop if not 3 selected
    req(length(input$search_select) == 3)
    
    selected_ids <- input$search_select
    
    # user profile vector (mean of 3 selected tracks, scaled space)
    target_vector <- scaled_tracks |>
      filter(track_id %in% selected_ids) |>
      select(danceability, energy, loudness, acousticness, valence, tempo) |>
      colMeans(na.rm = TRUE)
    
    # cosine similarity
    dot_product <- track_matrix %*% target_vector
    norm_target <- sqrt(sum(target_vector^2))
    cosine_scores <- as.vector(dot_product / (norm_tracks * norm_target))
    
    # recommendation result (top 10)
    rec_result <- new_track_attr |>
      ungroup() |>
      mutate(similarity_score = cosine_scores) |>
      filter(!track_id %in% selected_ids) |>
      arrange(desc(similarity_score)) |>
      head(10) |>
      mutate(
        spotify_url = paste0(
          "https://open.spotify.com/search/",
          URLencode(paste(track_name, artists), reserved = TRUE)
        ),
        track_name = paste0(
          '<a href="', spotify_url, '" target="_blank">', track_name, '</a>'
        )
      )
    
    list(table = rec_result, target_profile = target_vector)
  })
  
  # table output (clickable track_name)
  output$resultsTable <- renderDT({
    req(recommendation_data())
    
    df <- recommendation_data()$table
    
    # hide spotify_url column in DT (0-based indexing)
    url_col_idx <- which(names(df) == "spotify_url") - 1
    
    datatable(
      df,
      escape = FALSE,   # IMPORTANT: render <a> as link
      rownames = FALSE,
      options = list(
        pageLength = 10,
        columnDefs = list(list(targets = url_col_idx, visible = FALSE))
      )
    ) |>
      formatStyle(
        'similarity_score',
        background = styleColorBar(c(0, 1), 'lightblue'),
        backgroundSize = '98% 88%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # radar chart (optional / placeholder)
  output$radarCharts <- renderPlot({
    req(recommendation_data())
    # TODO: 네가 원하면 여기까지 target_profile vs 선택/추천곡 레이더로 이어서 붙여줄게
    plot.new()
    text(0.5, 0.5, "Radar chart TODO")
  })
}

shinyApp(ui, server)