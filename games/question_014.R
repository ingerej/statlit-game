

question <- list(
  id = "question_014",
  title = "Nasjonale undersøkelser",
  
  ui = function(id) {
    ns <- shiny::NS(id)
    
    shiny::tagList(
      shiny::h2("Nasjonale undersøkelser"),
      shiny::p(
        "Statistisk sentralbyrå sendte ut en spørreundersøkelse til en nasjonalt representatic grupper på 1002 norske voksne i 2013. Av disse hadde 21% en epostadresse eller en konto på sosiale medier som var blitt hacket. Identifiser populasjonen for hvem Statistisk sentralbyrå kan trekke en slutning til på bakgrunn av resultatene på spørreundersøkelsen. Identifiser også de interessante statistiske parametrene."
      ),
      shiny::radioButtons(
        ns("svar"),
        label = "Velg et alternativ:",
        choices = c(
          "Populasjonen er alle norske voksne i 2013, Utvalget er de 21% av norske voksne som haddenen epost eller sosial konto som ble hacket." = "a",
          "Populasjonen er de 1002 norske voksne som svarte på spørreundersøkelsen. Utvalget er alle de norske voksne i 2013" = "b",
          "Populasjonen er alle norske voksne i 2013. Utvalget er de 1002 norske voksne som svarte på undersøkelsen" = "c"
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
        
        riktig <- "c"  # Riktig svar
        
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