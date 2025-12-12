library(shiny)
library(dplyr)
library(fmsb)

# Assumes you already created new_track_attr like:
# new_track_attr <- track_attr |>
#   select(track_id, artists, track_name, danceability, energy, loudness,
#          acousticness, valence, tempo, track_genre) |>
#   tidyr::drop_na()

feature_cols <- c("danceability","energy","loudness","acousticness","valence","tempo")

# Build labels once (human-friendly)
track_labels <- paste0(new_track_attr$track_name, " - ", new_track_attr$artists)

# IMPORTANT: selectize will return the *values* (track_id), but show labels
# Name = label, Value = track_id
choices_vec <- setNames(as.character(new_track_attr$track_id), track_labels)

ui <- fluidPage(
  titlePanel("Search Bar with Dropdown Example"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = "search_select",
        label = "Search and Select an Option:",
        choices = choices_vec,
        multiple = TRUE,
        options = list(
          placeholder = "Start typing to search...",
          maxItems = 3,
          plugins = list("remove_button")  # ✅ adds small x on each selected item
        )
      )
    ),
    
    mainPanel(
      h4("User profile (average audio features)"),
      verbatimTextOutput("user_profile"),
      
      h4("Recommendations"),
      tableOutput("resultsTable"),
      uiOutput("chooseResultUi"),
      
      plotOutput("radarCharts")
    )
  )
)

server <- function(input, output, session) {
  
  # Show selected labels (one per line), not raw IDs
  output$selected_value <- renderText({
    if (is.null(input$search_select) || length(input$search_select) == 0) return("")
    ids <- as.character(input$search_select)
    lbls <- track_labels[match(ids, as.character(new_track_attr$track_id))]
    paste(lbls, collapse = "\n")
  })
  
  # Selected songs by track_id (no string split bugs)
  user_input_rv <- reactive({
    req(input$search_select)
    ids <- as.character(input$search_select)
    new_track_attr |> filter(as.character(track_id) %in% ids)
  })
  
  # Stores the actual model
  resultList <- reactive({
    req(input$search_select)
    
    user_input <- user_input_rv()
    validate(need(nrow(user_input) > 0, "No valid songs selected."))
    
    train_data <- new_track_attr |>
      mutate(
        liked = ifelse(track_id %in% user_input$track_id, 1, 0),
        is_genre = ifelse(track_genre %in% user_input$track_genre, 1, 0)
      )
    
    preference_model <- glm(
      liked ~ danceability + energy + loudness + acousticness + valence + tempo + is_genre,
      data = train_data,
      family = binomial
    )
    
    prediction_result <- train_data |>
      ungroup() |>
      mutate(predicted_score = predict(preference_model, newdata = train_data, type = "response")) |>
      filter(!track_id %in% user_input$track_id) |>
      slice_max(predicted_score, n = 10, with_ties = FALSE) |>
      arrange(desc(predicted_score))
    
    prediction_result
  })
  
  output$user_profile <- renderPrint({
    user_input <- user_input_rv()
    req(nrow(user_input) > 0)
    
    profile <- user_input |>
      select(danceability, energy, loudness, acousticness, valence, tempo) |>
      colMeans(na.rm = TRUE)
    
    round(profile, 3)
  })
  
  output$resultsTable <- renderTable({
    req(input$search_select)
    resultList()
  })
  
  output$chooseResultUi <- renderUI({
    options <- resultList()
    
    # Use track_id for selection; show label to user
    rec_labels <- paste0(options$track_name, " - ", options$artists)
    rec_choices <- setNames(as.character(options$track_id), rec_labels)
    
    selectInput("resultSelect", "Select Result", choices = rec_choices)
  })
  
  output$radarCharts <- renderPlot({
    req(input$resultSelect)
    
    options <- resultList()
    user_input <- user_input_rv()
    
    # Pick the chosen recommended song by track_id (NOT track_name)
    rec_id <- as.character(input$resultSelect)
    
    rec_row <- options |>
      filter(as.character(track_id) == rec_id) |>
      select(track_name, artists, all_of(feature_cols))
    
    validate(need(nrow(rec_row) == 1, "Could not find the selected recommendation."))
    
    user_rows <- user_input |>
      select(track_name, artists, all_of(feature_cols))
    
    # Build unique row labels to avoid duplicate row.names error
    rec_name  <- paste0(rec_row$track_name, " - ", rec_row$artists)
    user_names <- paste0(user_rows$track_name, " - ", user_rows$artists)
    
    compared <- bind_rows(rec_row, user_rows) |>
      select(all_of(feature_cols))
    
    # max/min must be computed over numeric feature columns only
    maxrow <- as.data.frame(t(apply(compared, 2, max, na.rm = TRUE)))
    minrow <- as.data.frame(t(apply(compared, 2, min, na.rm = TRUE)))
    
    radar_df <- bind_rows(maxrow, minrow, compared)
    
    # Assign rownames (make unique to prevent "duplicate row.names")
    rn <- c("Max", "Min", rec_name, user_names)
    rownames(radar_df) <- make.unique(rn)
    
    fmsb::radarchart(
      radar_df,
      axistype = 1,
      pcol = c(NA, NA, "red", "green", "blue", "black")[seq_len(nrow(radar_df))],
      plwd = 2
    )
    
    legend(
      x = "left",
      legend = rownames(radar_df)[-(1:2)],
      bty = "n",
      pch = 20,
      col = c("red", "green", "blue", "black")[seq_len(nrow(radar_df) - 2)]
    )
  })
}

shinyApp(ui, server)