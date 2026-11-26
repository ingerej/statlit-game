

question <- list(
  id = "question_024",
  title = "Vil privatlærer forbedre karakterene dine?",
  
  ui = function(id) {
    ns <- shiny::NS(id)
    
    shiny::tagList(
      shiny::h2("Vil privatlærer forbedre karakterene dine?"),
      shiny::p(
        "Mane studenter gjør det bedre på andre forsøk selv om de ikke coaches. Derfor ble 4200 studenter tilfeldig utvalgt til å ta en universitetstest to ganger. Av disse ble 500 kurset av en privatlærer mellom de to forsøkene. Studien sammenlignet gjennomsnittlig økning i testskåren for de to gruppene. Resultatet viste at de som ble coachet av en privatlærer hadde en større økning, men at den gjennomsnittlige økningen ikke var statistisk signifikant. Hva betyr dette?"
      ),
      shiny::radioButtons(
        ns("svar"),
        label = "Velg et alternativ:",
        choices = c(
          "Økningen i testskårene kan ha skjedd ved en tilfeldighet selv om caoching egentlig ikke har noen effekt" = "a",
          "Økningen i testskårene gir ingen forskjell for å komme seg inn på ett universitet siden det ikke er statistisk signifikant." = "b",
          "Studien hadde et dårlig design fordi de ikke hadde et likt antall nummer i den coachet og ikke coachede gruppen med studenter" = "c" 
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