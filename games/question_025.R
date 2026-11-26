

question <- list(
  id = "question_025",
  title = "Er det kjønnsforskjeller i tv-titting?",
  
  ui = function(id) {
    ns <- shiny::NS(id)
    
    shiny::tagList(
      shiny::h2("Er det kjønnsforskjeller i tv-titting?"),
      shiny::p(
        "En forskningsgruppe er interessert i om det er en signifikant forskjell i gjennomsnittlig antall timer man ser på tv for jenter og gutter i åttende klasse. Etter å ha samlet og analysert data fant forskningsgruppen at det var en forskjell på 4.37 timer per uke mellom gruppene. Forskningen gjennomførte en test for å verifisere om gjennomsnittsforskjellene var statistisk signifikant og fant en p-verdi på .001. Hva vil være den rette konklusjonen å trekke?"
      ),
      shiny::radioButtons(
        ns("svar"),
        label = "Velg et alternativ:",
        choices = c(
          "Forskjellene mellom gruppene skjedde ved en tilfeldighet fordi p-verdien er så lav" = "a",
          "Forskjellene mellom gruppene er ikke statistisk signifikant fordi p-verdien er så lav" = "b",
          "Det er bevis for en forskjell i gjennomsnitt mellom menn og kvinner i populasjonen, men det kan være det ikke er en forskjell i virkeligheten" = "c", 
          "Det er ikke sterke beviser for forskjell mellom gruppene i populasjonen, men det er mulig at det er det i virkeligheten." = "d"
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