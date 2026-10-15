

question <- list(
  id = "question_017",
  title = "Resirkulering",
  
  ui = function(id) {
    ns <- shiny::NS(id)
    
    shiny::tagList(
      shiny::h2("Resirkulering"),
      shiny::p(
        "Forskere utførte nylig en spørreundersøkelse av 1 000 tilfeldig utvalgtee voksne i USA og fant en sterk, positiv, statistisk signifikant korrelasjon mellom inntekt og antall komteinere de voksne rapporterte at de resirkulerte gjennom en typisk uke. 
        /n Kan forskerne konkludere med at høyere inntekt påvirker hvor mye resirkulering voksne amerikanske mennesker gjør? Velg svaret som passer best av det følgende:"
      ),
      shiny::radioButtons(
        ns("svar"),
        label = "Velg et alternativ:",
        choices = c(
          "Nei, fordi utvalget er for lite til å tillate å trekke kausale sluttninger" = "a",
          "Nei, fordi det mangler randommisert utvelgelse, og da kan man ikke trekke kausale slutninger" = "b",
          "Ja, fordi det er statistisk signifikant resultat og da kan man trekke kausale slutninger" = "c", 
          "Ja, fordi utvalget var ransomisert valgt ut så man kan trekke kausale slutninger" = "d"
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