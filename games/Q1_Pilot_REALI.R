#question 1 in the Pilot study of REALI

library(shiny)


Q1_Pilot_ui <- function() {
  tagList(
    
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
    
    sidebarLayout(
      sidebarPanel(
        h5("Deskriptiv statistikk"),
        h3("Tolk sannsynlighet i kontekst"),
        p("En videregåendelærer var bekymret for om elevene fikk nok søvn..."),
        br(),
        p("Hun ønsker å rapportere funnene sine...")
      ),
      
      mainPanel(
        h3("Fordeling av søvntimer"),
        plotOutput("sleepPlot", height = "300px", width = "60%"),
        
        tags$div(
          style = "font-size: 50px;",
          radioButtons(
            inputId = "svar",
            label = tags$h3("Velg et svaralternativ:"),
            choices = list(
              "tekst1" = "a",
              "tekst2" = "b",
              "tekst3" = "c"
            )
          )
        ),
        
        actionButton("submit", "Sjekk svar"),
        br(),
        textOutput("resultat"),
        br(),
        uiOutput("etter_svar"),
        
        br(),
        actionButton("next", "Neste spill")  # ✅ viktig
      )
    )
  )
}



Q1_Pilot_server <- function(input, output, session) {
  
  visForklaring <- reactiveVal(FALSE)
  
  observeEvent(input$submit, {
    riktig <- "c"
    
    if (input$svar == riktig) {
      output$resultat <- renderText("Riktig!")
    } else {
      output$resultat <- renderText("Feil. Det rette svaret er c")
    }
    
    visForklaring(TRUE)
  })
  
  output$etter_svar <- renderUI({
    if (visForklaring()) {
      div(
        style = "display: flex; gap: 20px;",
        actionButton("forklaring", "Få forklaring"),
        actionButton("next", "Neste spill")  
      )
    }
  })
  
  observeEvent(input$forklaring, {
    showModal(modalDialog(
      title = "Forklaring",
      "Her er c det mest riktige svaret...",
      easyClose = TRUE,
      footer = actionButton("lukk", "Lukk forklaring")
    ))
  })
  
  observeEvent(input$lukk, {
    removeModal()
  })
  
  output$sleepPlot <- renderPlot({
    
    hours <- seq(3, 10, by = 0.5)
    
    counts <- c(1,0,0,0,2,4,8,11,17,11,8,6,4,3,1)
    
    x <- rep(hours, counts)
    y <- unlist(lapply(counts, function(n) if (n > 0) seq_len(n) else numeric(0)))
    
    plot(
      x, y,
      pch = 19,
      xlab = "Hours of Sleep",
      axes = FALSE
    )
    
    axis(1, at = hours)
    box()
  })
}
``
