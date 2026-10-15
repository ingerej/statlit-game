

question <- list(
  id = "question_020",
  title = "Sommerleir",
  
  ui = function(id) {
    ns <- shiny::NS(id)
    
    shiny::tagList(
      shiny::h2("Sommerleir"),
      shiny::p(
        "100 student-atleter var med på sommerleir for å trene mot et spesifikt avslutningsløp. Alle 100 atletene fulgte det samme treningsprogrammet som forberedelse til avsluttningsløpet. 50 av atletene var tilfeldig plassert i en gruppe som også trente styrketrening utenfor de andre treningene. De resterende 50 trente ikke styrketrening i tillegg. På slutten av sommeren løp alle 100 individene det samme løpet og deres individuelle tid (i sekunder) ble tatt opp. 
        \n Gjennomsnittlig fart for treningsgruppen var 44 sekunder, mens gjennomsnittlig fart for ikke-treningsgruppen av 66 sekunder. Standardavviket til ikketreningsgruppen var 20 sekunder. Basert på de foreslåtte mulige standardavvikene for treningsgruppen, hvilken av disse verdiene ville gir best evidens for at det utgjorde en forskjell hvilken gruppe du var i?"
      ),
      shiny::radioButtons(
        ns("svar"),
        label = "Velg et alternativ:",
        choices = c(
          "10 sekunder" = "a",
          "20 sekunder" = "b",
          "30 sekunder" = "c"
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