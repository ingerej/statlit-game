

question <- list(
  id = "question_026",
  title = "Tabletter som hindrer synstap?",
  
  ui = function(id) {
    ns <- shiny::NS(id)
    
    shiny::tagList(
      shiny::h2("Tabletter som hindrer synstap?"),
      shiny::p(
        "En forskningartikkel rapporterer resultatet av et nytt legemiddel. Legemiddelet er tenkt å minke synstapet for personer med forklakning på øynet mer effektivt enn nåværende behandling. Artikkelen rapporterer en p-verdi på .04 i resultatseksjonen. Hvilket valg presenterer den rette tolkningen av denne p-verdien?"
      ),
      shiny::radioButtons(
        ns("svar"),
        label = "Velg et alternativ:",
        choices = c(
          "Vi konkluderer at de nye tablettene ikke er effektive fordi det bare er en .04 sannsynlighet for at legemiddelet er mer effektivt enn den nåværende behandlingen." = "a",
          "Vi konkluderer at de nye tablettene er effektive fordi resultatet de fant eller et mer ekstremt resultat bare ville skjedd 4% a tiden dersom legemiddelet ikke var effektivt." = "b",
          "Vi konkluderer at de nye tablettene er effektive fordi det bare er 4% sjanse for at de ikke er det." = "c", 
          "Vi kinkluderer at de nye tablettene ikke er effektive fordi forskjellene for personene med forkalkninger på øynene med synstap mellom behandlingene bare er .04" = "d"
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