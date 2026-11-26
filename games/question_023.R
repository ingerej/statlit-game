

question <- list(
  id = "question_023",
  title = "Deet er etablert at under normale omgivelsesbetingelser akn en voksen stormunnsabbor i Silver Lake ha en gjennomsnittlig lengde på 36.9 cm med standardavvik på 9 cm. folk som fisker i Silver Lake har en stund påstått at de dette året får mindre stormunnabborer. Hvilket av de følgende gir størt bevis for påstanden for at de fanger fisk som er mindre enn den gjennomsnittlige lengden på 36.9 cm?",
  
  ui = function(id) {
    ns <- shiny::NS(id)
    
    shiny::tagList(
      shiny::h2("Introduksjonskurs i statistikk"),
      shiny::p(
        "30 studenter i Introduksjon til statistikk tok en quiz verdt 30 poeng. Standardavviket til quiz-skårene var ett poeng. Hvilken av de følgende alternativene gir den beste tolkningen av dette standardavviket?"
      ),
      shiny::radioButtons(
        ns("svar"),
        label = "Velg et alternativ:",
        choices = c(
          "Et tilfeldig utvalg på 100 med et utvalgsgjennomsnitt på 36.9 cm" = "a",
          "Et tilfeldig utvalg på 36 med et utvalgsgjennomsnitt på 34.5 cm" = "b",
          "Et tilfeldig utvalg på 100 med et utvalgsgjennomsnitt på 34.5 cm" = "c", 
          "Et tilfeldig utvalg på 36 med et utvalgsgjennomsnitt på 36.9 cm" = "d"
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