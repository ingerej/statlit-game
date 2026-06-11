#Q4 Pilot REALI

#######CHANGE TXT for forklaring
library(shiny)

# ---------- UI ----------
Q4_Pilot_ui <- function() {
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
        p("Grafen til høyre viser timesdistribusjon av søvn forrige natt for en gruppe univeristetsstudenter."),
        br(),
        p("Hvilket av de underliggende grafene fremstiller den samme dataen som histogrammet?")
      ),   
      
      mainPanel(
        h3("Fordeling av søvntimer"),
        plotOutput("sleepPlot", height = "300px", width = "60%"), 
        
        plotOutput("alternativPlot", height = "250px", width = "50%", click = "alternativ_click"),
        textOutput("valgtPlot"),
        
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



# ---------- SERVER ----------
Q4_Pilot_server <- function(input, output, session) {
  
  visForklaring <- reactiveVal(FALSE)
  valgtAlternativ <- reactiveVal(NULL)
  
  observeEvent(input$submit, {
    
    if (is.null(valgtAlternativ())) {
      output$resultat <- renderText("Du må velge et alternativ først.")
      return()
    }
    
    riktig <- "A"
    
    if (valgtAlternativ() == riktig) {
      output$resultat <- renderText("Riktig!")
    } else {
      output$resultat <- renderText("Feil. Det rette svaret er A.")
    }
    
    visForklaring(TRUE)
  })
  
  
  output$etter_svar <- renderUI({
    if (visForklaring()) {
      div(
        style = "display: flex; gap: 20px;",
        actionButton("forklaring", "Få forklaring"),
        actionButton("next", "Neste spill")  # ✅ SAMME ID!
      )
    }
  })
  
  
  observeEvent(input$forklaring, {
    showModal(modalDialog(
      title = "Forklaring",
      "Her er A det mest riktige svaret...",
      easyClose = TRUE,
      footer = actionButton("lukk", "Lukk forklaring")
    ))
  })
  
  
  observeEvent(input$lukk, {
    removeModal()
  })
  
  
  output$sleepPlot <- renderPlot({
    library(ggplot2)
    
    hours  <- seq(3, 10, by = 0.5)
    counts <- c(1,0,0,0,2,4,8,11,17,11,8,6,4,3,1)
    
    df <- data.frame(hour = hours, count = counts)
    
    ggplot(df, aes(x = hour, y = count)) +
      geom_col(width = 0.5, fill = "lightblue", color = "black") +
      scale_x_continuous(breaks = 3:10, limits = c(2.75, 10.25)) +
      labs(
        x = "Timer med søvn",
        y = "Antall",
        title = "Histogram over søvntimer"
      ) +
      theme_minimal(base_size = 14)
  })
  
  
  output$alternativPlot <- renderPlot({
    library(ggplot2)
    
    df <- data.frame(
      gruppe = factor(c("A", "B", "C"), levels = c("C", "B", "A")),
      ymin   = c(4, 5, 3),
      lower  = c(6, 6.5, 5),
      middle = c(7, 7, 7),
      upper  = c(8, 8, 9),
      ymax   = c(10, 10, 10)
    )
    
    outliers <- data.frame(
      gruppe = factor(c("A", "B"), levels = c("C", "B", "A")),
      verdi  = c(3, 3)
    )
    
    ggplot(df, aes(x = gruppe)) +
      geom_boxplot(
        aes(ymin = ymin, lower = lower, middle = middle, upper = upper, ymax = ymax),
        stat  = "identity",
        fill  = c("#FFDAB9", "#B0E0E6", "#E6E6FA"),
        color = "gray40",
        width = 0.4
      ) +
      geom_point(
        data = outliers,
        aes(x = gruppe, y = verdi),
        color = "black",
        size  = 2
      ) +
      coord_flip() +
      scale_y_continuous(breaks = 3:10, limits = c(3, 10)) +
      labs(
        title = "Svaralternativer A, B og C",
        y = "Søvntimer",
        x = ""
      ) +
      theme_minimal(base_size = 14)
  })
  
  
  observeEvent(input$alternativ_click, {
    
    y <- input$alternativ_click$y
    
    valgt <- dplyr::case_when(
      y < 1 ~ "C",
      y < 2 ~ "B",
      TRUE  ~ "A"
    )
    
    valgtAlternativ(valgt)
    output$valgtPlot <- renderText(paste("Du valgte alternativ", valgt))
  })
}
``
