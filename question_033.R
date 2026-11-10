

question <- list(
  id = "question_033",
  title = "Myntkast",
  
  ui = function(id) {
    ns <- shiny::NS(id)
    
    shiny::tagList(
      shiny::h2("Myntkast"),
      shiny::p(
        "To studenter gjennomfører myntkast og markerer om mynten landet med mynt opp. En student gjennomføre 25 myntkast mens den andre gjennomførte 125 myntkast. Hvilken student er mest sannsynlig å få 48%-52% av dkastebe deres med mynt opp?"
      ),
      shiny::radioButtons(
        ns("svar"),
        label = "Velg et alternativ:",
        choices = c(
          "Studenten som gjennomfører 25 myntkast frde færre kast vil øke sannsynligheten for at 50% av resultatene har mynt opp" = "a",
          "Studenten som gjennomfører 125 myntkast fordi fere myntkast vil øke sannsynligheten for at resultatet gir 50% mynt" = "b",
          "Ingen studenter er mer sannsynlig for det, fordi myntkast er tilfeldig og derfor kan du ikke predikere utfall av myntkast" = "c"
        )
      ),
      shiny::br(),
      shiny::actionButton(
        ns("submit"),
        "Sjekk svar"
      ),
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
      
      shiny::observeEvent(input$submit, {
        shiny::req(input$svar)
        
        riktig <- "b"  # Riktig svar
        
        if (input$svar == riktig) {
          status("correct")
          message("Riktig!")
        } else {
          status("incorrect")
          message("Det var feil")
        }
        
        visForklaring(TRUE)
      })
      
      output$feedback <- shiny::renderUI({
        shiny::req(message())
        if (status() == "correct") {
          shiny::div(class = "alert alert-success", message())
        } else {
          shiny::div(class = "alert alert-warning", message())
        }
      })
      
      output$forklaring_knapp <- shiny::renderUI({
        if (visForklaring()) {
          shiny::actionButton(
            session$ns("forklaring"),
            "Vis forklaring"
          )
        }
      })
      
      shiny::observeEvent(input$forklaring, {
        shiny::showModal(
          shiny::modalDialog(
            title = "Forklaring",
            "Skriv inn forklaringen her.",
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