# Spotify Music Recommender

A Shiny web application that recommends music based on audio features using cosine similarity. Select 3 songs you like, and the app will recommend 10 similar tracks based on their audio characteristics.

## Features

- **Music Recommendation**: Get personalized song recommendations based on your music taste
- **Interactive Search**: Search and select up to 3 songs from the dataset
- **Visualizations**:
  - Radar charts comparing recommended songs with your selections
  - Parallel coordinates plot showing feature vectors
- **Detailed Song Information**: View artist, genre, key, time signature, duration, and more

## Prerequisites

Before running this application, make sure you have:

- **R** (version 4.0 or higher recommended)
- **RStudio** (optional, but recommended for easier development)

## Installation

### 1. Install Required R Packages

Open R or RStudio and run the following commands to install all required packages:

```r
# Install packages if not already installed
install.packages(c("shiny", "tidyverse", "fmsb", "ggplot2", "DT"))
```

### 2. Verify Data File

Ensure that the data file `spotify_mpd_01.RData` is present in the project directory. This file contains the Spotify track data needed for the application.

## Running the Application Locally

### Method 1: Using RStudio

1. Open the project in RStudio by double-clicking `Spotify-popularity-analysis.Rproj` or opening it from RStudio's File menu
2. Open `app.R` in the editor
3. Click the **"Run App"** button in the top-right corner of the editor pane, or press `Ctrl+Shift+Enter` (Windows/Linux) or `Cmd+Shift+Enter` (Mac)

### Method 2: Using R Console

1. Open R or RStudio
2. Navigate to the project directory:
   ```r
   setwd("/path/to/Spotify-music-recommender")
   ```
   Or use the full path:
   ```r
   setwd("~/Desktop/Spotify-music-recommender")
   ```
3. Run the application:
   ```r
   shiny::runApp("app.R")
   ```

### Method 3: Using Command Line

From the terminal, navigate to the project directory and run:

```bash
Rscript -e "shiny::runApp('app.R')"
```

## How to Use

1. **Start the App**: Once the app launches, it will open in your default web browser (usually at `http://127.0.0.1:XXXX`)

2. **Select Songs**:

   - In the sidebar, use the search box to find and select 3 songs
   - Type to search by song name or artist
   - You must select exactly 3 songs

3. **Get Recommendations**:

   - Click the "Get Recommendation" button
   - View the top 10 recommended songs in the "recommendation result" tab

4. **Explore Results**:
   - **recommendation result**: View the top 10 recommendations in a sortable table with similarity scores
   - **Music Profile**: Select a recommended song to see a radar chart comparing it with your selected songs, plus detailed information
   - **Vector DNA**: View a parallel coordinates plot showing how the recommended songs' feature vectors compare to your average profile

## Project Structure

```
Spotify-music-recommender/
├── app.R                    # Main Shiny application
├── spotify.qmd             # Quarto document with analysis code
├── spotify_mpd_01.RData    # Spotify track data (required)
├── spotify_mpd_001.RData   # Alternative smaller dataset
├── README.md               # This file
└── Spotify-popularity-analysis.Rproj  # RStudio project file
```

## How It Works

The recommendation system uses **cosine similarity** to find songs similar to your selections:

1. **Feature Extraction**: Each song is represented by 6 audio features:

   - Danceability
   - Energy
   - Loudness
   - Acousticness
   - Valence
   - Tempo

2. **Normalization**: All features are normalized to a 0-1 scale

3. **Target Vector**: The average of your 3 selected songs' feature vectors is calculated

4. **Similarity Calculation**: Cosine similarity is computed between the target vector and all other songs in the dataset

5. **Recommendation**: The top 10 songs with highest similarity scores (excluding your selected songs) are recommended

## Troubleshooting

### App won't start

- Make sure all required packages are installed
- Verify that `spotify_mpd_01.RData` exists in the project directory
- Check R console for error messages

### No recommendations appear

- Ensure you've selected exactly 3 songs before clicking "Get Recommendation"
- Check that the data file loaded correctly (you should see "button ok", "vector ok", "cosine ok" messages in the console)

### Port already in use

- If you see a port error, close other Shiny apps or specify a different port:
  ```r
  shiny::runApp("app.R", port = 3838)
  ```

## Dependencies

- `shiny`: Web application framework
- `tidyverse`: Data manipulation and visualization
- `fmsb`: Radar chart functionality
- `ggplot2`: Advanced plotting
- `DT`: Interactive data tables

## License

This project is for educational purposes.
