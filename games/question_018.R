

question <- list(
  id = "question_018",
  title = "Ny fotballstadion?",
  
  ui = function(id) {
    ns <- shiny::NS(id)
    
    shiny::tagList(
      shiny::h2("Ny fotballstadion?"),
      shiny::p(
        "En sportsjournalist ønsker å vite hvor mange fotballfans i en stor by som støtter byggingen av en ny stadion. Hun står utenfor den nåværende fotballstadion før en kampt og spør de første 250 personene som kommer dit. I nyhetsartikkelen rapporterer hun at resultatene fra utvalget er et estimat på prosentandelen av fotballfans i byen som støtter byggingen av en ny stadion. Hvilken påstant er korrekt med tanke på utvalgsmetoden?"
      ),
      shiny::radioButtons(
        ns("svar"),
        label = "Velg et alternativ:",
        choices = c(
          "Utvalget er random, og gir et godt estimat" = "a",
          "Fordi utvalget er så lite vil det ikke gi et nøyaktig estimat" = "b",
          "Fordi alle fansene hadde en mulighet til å bli spurt vil det gi et godt estimat" = "c", 
          "Utvalgsmetoden er biased og vil ikkke gi et nøyaktig estimat" = "d"
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
        
        riktig <- "d"  # Riktig svar
        
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