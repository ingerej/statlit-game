

question <- list(
  id = "question_022",
  title = "Hunders luktesans",
  
  ui = function(id) {
    ns <- shiny::NS(id)
    
    shiny::tagList(
      shiny::h2("Hunders luktesans"),
      shiny::p(
        "Hunder har veldig god luktesans, og er trent til å gjenkjenne forskjellige objekter ved å plukke opp forskjellige luktbilder. Et eksperiment ble utførst med en hund i Japan som var trent til å tarmkraft i avføringsprøver. En test hadde 5 prøver, hvor en var kreftpasient og fire var friske, og hunden skulle markere kreftpasienten. Dette ble gjort 38 ganger. Av disse 38 hadde hunden rett 37 ganger. En hypotese test ble gjennomført for å se om dette kan ha skjedd ved tilfeldigheter. Alternativhypotesener at hunden gjenkjenner kreft i mer enn en femtedel av tiden. P-verdien er under .001. Anta at studien har et godt design og bruk et signifikansnivå på .05 til å ta en avgjørelse:"
      ),
      shiny::radioButtons(
        ns("svar"),
        label = "Velg et alternativ:",
        choices = c(
          "Avvis nullhypotesen og konkluder med at hunden korret identifiserer kreft i mer enn en femtedel av gangene." = "a",
          "Det er nok statistisk bevis til å bevise at hunden korrekt identifiserer kreft mer enn en femtedel av tiden." = "b",
          "Ikke avvis nullhypotesen og konkluder med at det ikke er bevis for at hunden korret identifiserer kreft mer enn en femtedel av tiden." = "c"
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
        
        riktig <- "a"  # Riktig svar
        
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