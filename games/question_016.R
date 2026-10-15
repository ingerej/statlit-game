

question <- list(
  id = "question_016",
  title = "Meningsmåling på TV",
  
  ui = function(id) {
    ns <- shiny::NS(id)
    
    shiny::tagList(
      shiny::h2("Meningsmåling på TV"),
      shiny::p(
        "En lokal TV-stasjoni en by med 500 000 innbyggere gjennomførte nylig en meningsmåling hvor de ba seerne om å ringe inn og gi deres støtte eller si imot en kontroversiell folkeavstemning rundt det kommende valget. Over 10 000 mennesker responderte der 67% var imot folkeavstemningen. TV stasjonen annonserte at de bar overbevist om at folkeavstemningen ville tape i valget. Velg det svaret under som indikerer om TV-stasjonens annonsering var gyldig eller ikke og hvorfor."
      ),
      shiny::radioButtons(
        ns("svar"),
        label = "Velg et alternativ:",
        choices = c(
          "Ugyldig fordi utvalget er over 1/50 av størrelsen til populasjonen" = "a",
          "Ugyldig fordi ikke alle seerne hadde muligheten til å svare på undersøkelsen" = "b",
          "Ugyldig fordi utvalget kanskje ikke er representativt for populasjonen" = "c" 
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