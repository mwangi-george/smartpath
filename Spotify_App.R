pacman::p_load(
  tidyverse, shiny, janitor, shinycssloaders, plotly, shinydashboard, shinyWidgets
)

spotify <- read_csv("data/spotify-2023.csv", show_col_types = F) %>% janitor::clean_names()

header <- dashboardHeader(title = "Analysis of Most Streamed Spotify Songs 2023",titleWidth = 1500)

sidebar <- dashboardSidebar(collapsed = T)

body <- dashboardBody(
  tags$head(tags$style(HTML('
.box {margin-top: 2px;margin-left: 0px; margin-right: 0px; margin-bottom:2px;padding:-10px}
div {padding: 0 !important;}'
  ))),
  fluidRow(
    box(width = 6, plotlyOutput("top_10_artists", height = 300) %>% withSpinner()),
    box(width = 6, plotlyOutput("contributions", height = 300) %>% withSpinner()),
    box(width = 6, plotlyOutput("pop_over_time", height = 300) %>% withSpinner()),
    box(width = 6, plotlyOutput("avg_happiness", height = 300) %>% withSpinner()),
    box(width = 6, plotlyOutput("dist", height = 300) %>% withSpinner()),
    box(width = 6, plotlyOutput("key_signatures", height = 300) %>% withSpinner()),
    box(width = 12, plotlyOutput("bpm_vs_dance", height = 500) %>% withSpinner())
  )
)

ui <- dashboardPage(
  header, sidebar, body,
  skin = "green")

server <- function(input, output, session){
  
  output$top_10_artists <- renderPlotly({
    spotify %>% count(artist_s_name, sort = T) %>% 
      top_n(n = 10, wt = n) %>% 
      mutate(artist_s_name = fct_reorder(artist_s_name, n, .desc = T)) %>% 
      plotly::plot_ly(x = ~artist_s_name, y = ~n) %>% 
      add_bars(color = I("#1DB954")) %>% 
      layout(
        title = "Top 10 Artists with most songs",
        xaxis = list(title = "Artist Name"),
        yaxis = list(title = "Number of Songs")
      )%>%
      config(displayModeBar = FALSE)
  })
  
  output$contributions <- renderPlotly({
    spotify %>% 
      mutate(artist_count = as_factor(artist_count)) %>% 
      count(artist_count, sort = T) %>% 
      plot_ly(x = ~artist_count, y = ~ n) %>% 
      add_bars(color = I("#1DB954")) %>% 
      layout(
        title = "How Many Artists are Contributing to Songs",
        xaxis = list(title = "Number of Artists"),
        yaxis = list(title = "Number of Songs")
      )%>%
      config(displayModeBar = FALSE)
  })
  
  output$pop_over_time <- renderPlotly({
    spotify %>% 
      group_by(released_year) %>% 
      summarise(pop = sum(in_spotify_playlists)) %>% 
      plot_ly(x = ~released_year, y = ~pop, hoverinfo = "text",
              text = ~paste("Year:", released_year, "<br>", 
                            "Playlists:", pop)
      ) %>% 
      add_lines(color = I("#1DB954")) %>% 
      add_markers(color = I("#1DB954"), showlegend = F) %>% 
      layout(
        title = "Popularity of songs on Spotify over the years",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Number of Playlists")
      )%>%
      config(displayModeBar = FALSE)
  })
  
  output$avg_happiness <- renderPlotly({
    spotify %>% 
      dplyr::summarise(
        avg_valence = mean(valence_percent), .by = released_year
      ) %>% 
      plot_ly(
        x = ~released_year, y =~ avg_valence,
        hoverinfo = "text",
        text =  ~paste(
          "Year:", released_year, "<br>", "Average Happiness Score:", paste0(round(avg_valence), "%")
        )
      ) %>% 
      add_lines(color = I("#1DB954")) %>% 
      add_markers(color = I("#1DB954")) %>% 
      layout(
        showlegend = F,
        title = "Average valence (happiness) score for songs over time",
        xaxis = list(title = "Year", zeroline = T),
        yaxis = list(title = "Happiness Score")
      )%>%
      config(displayModeBar = FALSE)
  })
  output$dist <- renderPlotly({
    spotify %>% 
      mutate(streams = as.numeric(streams)) %>% 
      plot_ly(x = ~streams, hoverinfo = "x") %>% 
      add_histogram(color = I("#1DB954")) %>% 
      layout(
        title = "Distribution of song streams across the dataset",
        xaxis = list(title = "Streams"),
        yaxis = list(title = "Frequency")
      )%>%
      config(displayModeBar = FALSE)
  })
  
  output$bpm_vs_dance <- renderPlotly({
    spotify %>% 
      group_by(key) %>% 
      nest() %>% 
      mutate(
        plot = map2(
          data, key,
          \(data, key)
          plot_ly( 
            data = data,
            x = ~ bpm, 
            y = ~ danceability_percent,
            color = ~mode,
            hoverinfo = "text",
            text =  ~paste(
              "BPM:", bpm, "<br>", "Danceability:", paste0(danceability_percent, "%"), "<br>", 
              "Key:", key,  "<br>", "Mode:", mode
            )
          ) %>% 
            add_markers(name = ~key, colors = c("#1DB954", "#191414")) %>% 
            layout(
              showlegend = F,
              title = "Relationship between song tempo (BPM) and Danceability (Faceted by Key Signatures & Colored by Mode)",
              xaxis = list(title = "BPM (Tempo)"),
              yaxis = list(title = "% Danceability")
            ) %>% 
            config(displayModeBar = F)
        )
      ) %>% 
      subplot(nrows = 3, shareX = T, shareY = T)
  })
  
  output$key_signatures <- renderPlotly({
    spotify %>% 
      filter(!is.na(key)) %>% 
      count(key) %>% 
      mutate(n = round(n/sum(n)*100, 2)) %>% 
      mutate(key = fct_reorder(key, n, .desc = F)) %>% 
      plot_ly(
        x = ~n, y = ~ key,
        hoverinfo = "text",
        text =  ~paste(
          "Key Signature:", key, "<br>", "% Popularity:", paste0(n, "%")
        )
      ) %>% 
      add_bars(orientation = 'h', color = I("#1DB954")) %>% 
      layout(
        title = "Most common key signatures in popular songs",
        xaxis = list(zeroline = F, title = "Popularity"), yaxis = list(title = "Key"))%>%
      config(displayModeBar = FALSE)
  })
}

shinyApp(ui, server)
