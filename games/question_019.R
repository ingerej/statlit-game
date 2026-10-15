

question <- list(
  id = "question_019",
  title = "Søvnappne og attraktivitet",
  
  ui = function(id) {
    ns <- shiny::NS(id)
    
    shiny::tagList(
      shiny::h2("Søvnappne og attraktivitet"),
      shiny::p(
        "En forsker vil finne svaret på følgende forskningsspørsmål: Er søvnappne-pasienter mer attraktive etter å ha fått den nye behandlingen (kontinuerlig positivt lufttrykksterapi)? 20 pasienter med søvnappne ble gitt den nye behandlingen, og bilder ble tatt før behandlingen samt to måneder etter avsluttet behandling. En uavhengig rater ble vist før- og etterbildene for hver pasient i random rekkefølge og spurt om å avgjøre i hvilket av bildene pasienten var mest attraktiv. 70% av pasientene var vurdert som mer attraktiv i etterbildet. Er det behov for å lage en hypotesetest for å avgjøre om behandlingen er effetktiv, eller kan forskeren bare bruke statistikken på 70% som bevis for effekten?"
      ),
      shiny::radioButtons(
        ns("svar"),
        label = "Velg et alternativ:",
        choices = c(
          "Forskeren trenger ikke å gjennomføre en hypotesetest fordi 70% er mye større enn 50% og det er derfor bevis for at resultatet ikke var en tilfeldighet" = "a",
          "Forskeren burde gjennomføre en hypotesetest fordi typotesetester alltid er passende" = "b",
          "Forskeren burde gjennomføre en hypotesetest for å avgjøre om utvalgsstatistikken trolig ikke skjedde ved en tilfeldighet" = "c"
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