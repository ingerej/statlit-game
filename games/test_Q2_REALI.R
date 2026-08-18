question <- list(
  
  id = "reali_quest_12_ver_2_0",
  
  title = "Tolk sannsynlighet i kontekst",
  
  ui = function(id) {
    
    ns <- shiny::NS(id)
    
    shiny::tagList(
      
      shiny::tags$head(
        shiny::tags$style(shiny::HTML("
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
      
      shiny::h5("Deskriptiv statistikk"),
      
      shiny::h3("Tolk sannsynlighet i kontekst"),
      
      shiny::p(
        "Ifølge det nasjonale kreftinstituttet i USA er sannsynligheten for å utvikle prostatakreft i løpet av livet .15 for amerikanske menn. Hva betyr sannsynligheten .15 i dette tilfellet?"
      ),
      
      shiny::h2("Svaralternativer"),
      
      shiny::radioButtons(
        ns("svar"),
        label = shiny::tags$h3("Velg et alternativ:"),
        choices = c(
          "For alle menn som lever i USA vil tilnærmet 15% av dem utvikle prostatakreft i løpet av livet" = "a",
          "Hvis man tilfeldig velger ut en mann i USA vil det være en 15% sannsynlighet for at han utvikler prostatakreft i løpet av livet" = "b",
          "Ved tilfeldig utvalg av 100 amerikanske menn vil 15 av dem utvikle prostatakreft" = "c",
          "Både svaralternativ a og b er riktig" = "d"
        )
      ),
      
      shiny::actionButton(
        ns("submit"),
        "Sjekk svar"
      ),
      
      shiny::br(),
      shiny::br(),
      
      shiny::uiOutput(
        ns("feedback")
      ),
      
      shiny::br(),
      
      shiny::uiOutput(
        ns("forklaring_knapp")
      )
    )
  },
  
  server = function(id, context) {
    
    shiny::moduleServer(id, function(input, output, session) {
      
      status <- shiny::reactiveVal("not_answered")
      message <- shiny::reactiveVal(NULL)
      
      visForklaring <- shiny::reactiveVal(FALSE)
      
      observeEvent(input$submit, {
        
        shiny::req(input$svar)
        
        riktig <- "d"
        
        if (input$svar == riktig) {
          
          status("correct")
          message("Riktig, i dette tilfellet er både a og b riktig")
          
        } else {
          
          status("incorrect")
          message("Feil, i dette tilfellet var både a og b riktig")
          
        }
        
        visForklaring(TRUE)
      })
      
      output$feedback <- shiny::renderUI({
        
        shiny::req(message())
        
        if (status() == "correct") {
          
          shiny::div(
            class = "alert alert-success",
            message()
          )
          
        } else {
          
          shiny::div(
            class = "alert alert-warning",
            message()
          )
        }
      })
      
      output$forklaring_knapp <- shiny::renderUI({
        
        if (visForklaring()) {
          
          shiny::actionButton(
            session$ns("forklaring"),
            "Få forklaring"
          )
        }
      })
      
      observeEvent(input$forklaring, {
        
        shiny::showModal(
          shiny::modalDialog(
            title = "Forklaring",
            "Sannsynligheten .15 betyr både at omtrent 15 % av alle amerikanske menn vil utvikle prostatakreft i løpet av livet, og at en tilfeldig valgt mann har 15 % sjanse for å utvikle det.",
            easyClose = TRUE,
            footer = shiny::tagList(
              shiny::modalButton("Lukk")
            )
          )
        )
      })
      
      return(
        shiny::reactive({
          list(
            status = status(),
            solved = identical(status(), "correct"),
            message = message()
          )
        })
      )
    })
  }
)