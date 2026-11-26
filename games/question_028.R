

question <- list(
  id = "question_028",
  title = "Ny lyspære",
  
  ui = function(id) {
    ns <- shiny::NS(id)
    
    shiny::tagList(
      shiny::h2("Ny lyspære"),
      shiny::p(
        "Det skal produseres en ny type lyspære. Det forrige designet hadde en livstid på 1,200 timer. Den nye har en estimert livstid på 1,200.2 timer basert på et utvalg på 40 000 lyspærer. Selv om forskjellen var ganske liten er gjennomsnittsforskjellen statistisk signifikant. Et signifikant resultat for en så liten forskjell skjer fordi:"
      ),
      shiny::radioButtons(
        ns("svar"),
        label = "Velg et alternativ:",
        choices = c(
          "Det nye designet hadde mer variasjon enn det forrige designet" = "a",
          "Utvalgsstørrelsen for det nye designet er veldig stort" = "b",
          "Gjennomsnittet på 1,200 timer for det forrige designet er stort." = "c"
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