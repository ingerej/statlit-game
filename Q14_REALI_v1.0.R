

set.seed(123)
pop_data <- data.frame(x = rsnorm(1000, mean = 8.5, sd = 1.5, xi = 0.6))

interval_labels <- c(
  "2 til 3",
  "3 til 4",
  "4 til 5",
  "5 til 6",
  "6 til 7",
  "7 til 8",
  "8 til 9",
  "9 til 10",
  "10 til 11",
  "11 til 12"
)

interval_bounds <- list(
  c(2, 3),
  c(3, 4),
  c(4, 5),
  c(5, 6),
  c(6, 7),
  c(7, 8),
  c(8, 9),
  c(9, 10),
  c(10, 11),
  c(11, 12)
)

ui <- fluidPage(
  useShinyjs(),
  
  # CSS
  tags$head(
    tags$style(HTML("
      body, label, input, button, select {
        font-family: 'Times New Roman', serif;
      }

      h1, h2, h3, h4 {
        font-family: 'Times New Roman', serif;
      }

      .shiny-input-radiogroup label {
        font-size: 14px;
      }
    "))
  ),
  
  titlePanel("Spørsmål 14: Gjennomsnitt og intervall"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Spørsmål:"),
      p("Her er fordelingen av flere målinger gjort av en populasjon. 
         Et utvalg på 10 tilfeldig trukkede verdier tas fra populasjonen 
         og utvalgsgjennomsnittet blir regnet ut. 
         Hvilket intervall er mest sannsynlig å inkludere utvalgsgjennomsnittet?")
    ),
    
    mainPanel(
      plotlyOutput("distPlot", height = "400px"),
      br(),
      actionButton("submit", "Sjekk svar"), 
      br(), 
      textOutput("feedback"),
      br(),
      uiOutput("etter_svar")
    )
  )
)

server <- function(input, output, session) {
  
  # start på 8–9 → det er det 7. intervallet
  current_interval <- reactiveVal(4)
  
  output$distPlot <- renderPlotly({
    idx <- current_interval()
    chosen <- interval_bounds[[idx]]
    
    breaks_seq <- seq(
      floor(min(pop_data$x)),
      ceiling(max(pop_data$x)),
      by = 0.2
    )
    
    hist_data <- hist(
      pop_data$x,
      breaks = breaks_seq,
      plot = FALSE
    )
    
    df_hist <- data.frame(
      x = hist_data$mids,
      y = hist_data$counts,
      color = ifelse(hist_data$mids >= chosen[1] & hist_data$mids < chosen[2],
                     "#4CAF50",
                     "black")
    )
    
    g <- ggplot(df_hist, aes(x = x, y = y, fill = color)) +
      geom_col(color = "white") +
      scale_fill_identity() +
      scale_x_continuous(
        breaks = seq(floor(min(pop_data$x)), ceiling(max(pop_data$x)), by = 1)
      ) +
      labs(x = "Måling", y = "Frekvens") +
      theme_minimal()
    
    ggplotly(g, source = "dist", tooltip = NULL) |>
      config(displayModeBar = FALSE)
  })
  
  # svar ved klikk på histogrammet
  observeEvent(event_data("plotly_click", source = "dist"), {
    click <- event_data("plotly_click", source = "dist")
    if (is.null(click)) return()
    
    x_clicked <- click$x
    
    idx <- which(sapply(interval_bounds, function(b) x_clicked >= b[1] & x_clicked < b[2]))
    if (length(idx) == 0) return()
    
    current_interval(idx)
  })
  
  observeEvent(input$submit, {
    idx <- current_interval()
    chosen_label <- interval_labels[idx]
    
    if (chosen_label == "8 til 9") {
      output$feedback <- renderText("Riktig! Utvalgets gjennomsnitt ligger mest sannsynlig mellom 8 og 9.")
    } else {
      output$feedback <- renderText("Feil. Riktig svar er mellom 8 og 9.")
    }
    
    output$etter_svar <- renderUI({
      div(
        style = "display: flex; gap: 20px;",
        actionButton("forklaring", "Få forklaring"),
        actionButton("next", "Neste spørsmål")
      )
    })
  })
  
  observeEvent(input$forklaring, {
    showModal(modalDialog(
      title = "Forklaring",
      "Det nye utvalgsgjennomsnittet blir trolig liggende rundt gjennomsnittet for dette utvalget. Det vil derfor mest sannsynlig ligge mellom 8 og 9.",
      easyClose = TRUE,
      footer = modalButton("Lukk")
    ))
  })
}

shinyApp(ui = ui, server = server)
