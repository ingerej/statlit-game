

question <- list(
  id = "question_021",
  title = "Hjemløse i Minnesota",
  
  ui = function(id) {
    ns <- shiny::NS(id)
    
    shiny::tagList(
      shiny::h2("Hjemløse i Minnesota"),
      shiny::p(
        "Det ble gjort en spørreundersøkelse på hjemløse i Minnesota. I 2012 ble det spurt 4465 hjløse voksne hvor mange netter de hadde hatt ute, i en bil, i en forlatt byggning, eller andre steder man ikek er ment å sove. Ett av forskningsspørsmålene var om det er forskjeller i kvinner og menn når det kommer til antall netter utendørs eller i plasser man ikke er ment å sove. Hvilket av de følgende utsagne er nullhypotesen for en statistisk test designet for å svare på spørreundersøkelsen?"
      ),
      shiny::radioButtons(
        ns("svar"),
        label = "Velg et alternativ:",
        choices = c(
          "Det er ingen forskjell mellom menn og kvinner når det kommer til antall netter tilbragt utendørs eller i plasser man ikke er ment å bo" = "a",
          "Det er en forskjell mellom menn og kvinner når det kommer til antall netter tilbragt utendørs eller i plasser man ikke er ment å bo" = "b",
          "Det er ingen forskjell mellom menn og kvinner når det kommer til gjennomsnittlig antall netter tilbragt utendørs eller i plasser man ikke er ment å bo" = "c", 
          "Det er en forskjell mellom menn og kvinner når det kommer til gjennomsnittlig antall netter tilbragt utendørs eller i plasser man ikke er ment å bo" = "d"
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