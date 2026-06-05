#question 1 in the Pilot study of REALI

ui <- fluidPage(
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
  theme = shinytheme("yeti"),
  
  navbarPage("Bli en ekspert på statistikkyndighet <3"),
             
             tabPanel("Spørsmål x",
                      sidebarLayout(
                        
                        sidebarPanel(
                          h5("Deskriptiv statistikk"),
                          h3("Tolk sannsynlighet i kontekst"),
                          p("En videregåendelærer var bekymret for om elevene fikk nok søvn. Hun spurte hver elev hvor mye de hadde sovet natten i forveien. Til høyre ser dere oversikten hun laget fra svarene."),
                          br(), 
                          p("Hun ønsker å rapportere funnene sine til skolens rektor ved å bruke passente statistisk språk for å beskrive og tolke fordelingen i kontekst. Velg det ", 
                            tags$b("mest passende"), " utsagnet hun kan bruke for å oppsummere resultatet.")
                        ),
                        
                        mainPanel(
                          h3("Fordeling av søvntimer"),
                          plotOutput("sleepPlot", height = "300px",  width = "60%"),
                          
                          tags$div(
                            style = "font-size: 50px;",
                            radioButtons(
                              inputId = "svar",
                              label = tags$h3("Velg et svaralternativ:"),
                              choices = list(
                                "Verdiene går fra 3 til 10, hvor de er økende i høyde frem til 7, før dem minker til 10. De fleste verdiene er på 7. Det er et gap mellom 3 og 5. Standardavviket er rundt 2 timer." = "a",
                                "Timesdistribusjonen av søvn følger en normalkurve og er sentrert ved 7. Det er et gap mellom tre og fem. Studenten som sov 3 timer kan være en uteligger." = "b",
                                "Timesdistribusjonen av søvn er tilnærmet bell-formet. Typetallet for søvn er 7 timer og standardavviket er rundt 1 time." = "c"
                              )
                            )
                          ),
                          
                          actionButton("submit", "Sjekk svar"),
                          br(),
                          textOutput("resultat"),
                          br(),
                          uiOutput("etter_svar")
                        )
                      )
             )
  )
             
server <- function(input, output, session) {
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
        style = "display: flex; justify-content: left; gap: 20px; margin-top: 10px;",
        actionButton("forklaring", "Få forklaring"),
        actionButton("neste", "Neste spørsmål")
      )
    }
  })
  
  
  observeEvent(input$forklaring, {
    showModal(modalDialog(
      title = "Forklaring",
      "Her er c det mest riktige svaret. A har både noe svak fagterminologi, og påstår samtidig at standardavviket er 2. Et standardavvik skal romme ca. 68% av svarene. For denne fordelingen er dermed standardavviket nærmere 1 times søvn enn det er 2 timer søvn. Svaralternativ b blir feil fordi fordelingen er klokkefomet (bellskurve), men den er ikke symmetrisk, og er derfor ikke normalfordelt.",
      easyClose = TRUE,
      footer = tagList(
        actionButton("lukk", "Lukk forklaring")
      )
    ))
  })
  
  observeEvent(input$lukk, {
    removeModal()
  })
  
  # plot for sleep
  output$sleepPlot <- renderPlot({
    
    # X-verdier i halvtall fra 3 til 10
    hours <- seq(3, 10, by = 0.5)
    
    # Antall prikker for hver søvntime (i samme rekkefølge)
    counts <- c(
      1,  # 3.0
      0,  # 3.5
      0,  # 4.0
      0,  # 4.5
      2,  # 5.0
      4,  # 5.5
      8,  # 6.0
      11, # 6.5
      17, # 7.0
      11, # 7.5
      8,  # 8.0
      6,  # 8.5
      4,  # 9.0
      3,  # 9.5
      1   # 10.0
    )
    
    # dots
    x <- rep(hours, counts)
    y <- unlist(lapply(counts, function(n) if (n > 0) seq_len(n) else numeric(0)))
    
    # Plot
    plot(
      x, y,
      pch = 19, col = "black",
      xlab = "Hours of Sleep",
      ylab = "",
      main = "",
      axes = FALSE,
      xlim = c(2.8, 10.2),
      ylim = c(0, max(counts) + 2)
    )
    
    axis(1, at = hours)
    box()
  })
}

shinyApp(ui = ui, server = server)
