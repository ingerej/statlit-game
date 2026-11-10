

question <- list(
  id = "question_029",
  title = "konfidensintervallet for mobiltelefoner",
  
  ui = function(id) {
    ns <- shiny::NS(id)
    
    shiny::tagList(
      shiny::h2("Konfidensintervallet for mobiltelefoner"),
      shiny::p(
        "SSB undersøkte 2 076 tilfeldig utvalgte voksne som bruker mobiltelefoner i 2013. 60% av disse hadde tilgang på internett. 95% konfidensintervall 58% til 62%. Hva prøver dette intervallet å estimere?"
      ),
      shiny::radioButtons(
        ns("svar"),
        label = "Velg et alternativ:",
        choices = c(
          "Gjennomsnittlig vokne med mobiltelefoner som har til internett i 2013" = "a",
          "Prosenten av de 2 076 voksne som bruker mobiltellefor som har tilgang på internett i 2013." = "b",
          "Prosenten av alle voksne med mobiltelefoner som har tilgang til internett i 2013" = "c", 
          "For voksne som hat tilgang til internett på deres mobiltelefon, bare 58%-62% var konfidente/selvsikre på å bruke internettet på mobilen" = "d"
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