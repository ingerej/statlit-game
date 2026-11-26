

question <- list(
  id = "question_034",
  title = "Lottorekker",
  
  ui = function(id) {
    ns <- shiny::NS(id)
    
    shiny::tagList(
      shiny::h2("Lottorekker"),
      shiny::p(
        "Gopher 5 er et pengespill i Minnesota. For å spille velger man ut fem nummer mellom 1 og 47, men du kan bare bruke nummeret en gang. Nummerene er listet opp nummerigsk og ikke nødvendig vis i den rekkefølgen de ble valgt. En spiller vinner Gopher 5 Jackpot hvis alle fem nummerene som er valgt matcher med de fem nummerene som blir trukket av en datmaskin. Her er fire sett av fem nummer som spillere har valgt: Sett 1; 5-10-15-20-25, sett 2; 1-13-25-31-42, sett 3; 10-16-24-25-40, sett 4; 1-2-3-4-5." 
      ),
      shiny::radioButtons(
        ns("svar"),
        label = "Hvilket av disse settene er mindre sannsynlig til å vinne Gopher 5?",
        choices = c(
          "Sett 1" = "a",
          "Sett 2" = "b",
          "Sett 3" = "c", 
          "Sett 4" = "d",
          "Ingen av de overstående" = "e"
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
        
        riktig <- "e"  # Riktig svar
        
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