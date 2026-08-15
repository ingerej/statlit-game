#REALI_quest_12_ver_2.0

library(shiny)
library(shinythemes)

# ---------- UI ----------
ui <- function() {
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
        p("Ifølge det nasjonale kreftinstituttet i USA er sannsynligheten for å utvikle prostatakreft i løpet av livet .15 for amerikanske menn. Hva betyr sannsynligheten .15 i dette tilfellet?")
      ),
      
      mainPanel(
        h2("Svaralternativer"), 
        
        tags$div(style = "font-size: 50px;",
                 radioButtons(
                   inputId = "svar",
                   label = tags$h3("Velg et alternativ:"),
                   choices = list(
                     "For alle menn som lever i USA vil tilnærmet 15% av dem utvikle prostatakreft i løpet av livet" = "a",
                     "Hvis man tilfeldig velger ut en mann i USA vil det være en 15% sannsynlighet for at han utvikler prostatakreft i løpet av livet" = "b",
                     "Ved tilfeldig utvalg av 100 amerikanske menn vil 15 av dem utvikle prostatakreft" = "c",
                     "Både svaralternativ a og b er riktig" = "d"
                   )
                 )
        ),
        
        actionButton("submit", "Sjekk svar"),
        br(), br(),
        textOutput("resultat"),
        br(),
        uiOutput("forklaring_knapp"),
        br(),
        actionButton("next", "Neste spill")  # ✅ endret fra "neste"
      )
    )
  )
}



# ---------- SERVER ----------
server <- function(input, output, session) {
  
  visForklaring <- reactiveVal(FALSE)
  
  observeEvent(input$submit, {
    riktig <- "d"
    
    if (input$svar == riktig) {
      output$resultat <- renderText("Riktig, i dette tilfellet er både a og b riktig")
    } else {
      output$resultat <- renderText("Feil, i dette tilfellet var både a og b riktig")
    }
    
    visForklaring(TRUE)
  })
  
  
  output$forklaring_knapp <- renderUI({
    if (visForklaring()) {
      actionButton("forklaring", "Få forklaring")
    }
  })
  
  
  observeEvent(input$forklaring, {
    showModal(modalDialog(
      title = "Forklaring",
      "Sannsynligheten .15 betyr både at omtrent 15 % av alle amerikanske menn vil utvikle prostatakreft i løpet av livet, og at en tilfeldig valgt mann har 15 % sjanse for å utvikle det...",
      easyClose = TRUE,
      footer = actionButton("lukk", "Lukk forklaring")
    ))
  })
  
  
  observeEvent(input$lukk, {
    removeModal()
  })
}

shinyApp(ui = ui, server = server)