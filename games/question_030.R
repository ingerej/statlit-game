

question <- list(
  id = "question_030",
  title = "Skilsmisse blant veteraner",
  
  ui = function(id) {
    ns <- shiny::NS(id)
    
    shiny::tagList(
      shiny::h2("Skilsmisse blant veteraner"),
      shiny::p(
        "I en nylig studie veteraner fra Vietnamkrigen ble det valgt ut et tilfeldig utvalg av veteraner og spurt on de har vært skildt minst en gang. Forskerne kalkulerte 95*% konfidensintervall for prosenten av veteraner som har blitt skildt minst en gang (35%-39%). Hvilken av de følgende utsagnene er sanne om midten av dette intervallet (37%)?"
      ),
      shiny::radioButtons(
        ns("svar"),
        label = "Velg et alternativ:",
        choices = c(
          "Vi kan si at 37% av veteraner i utvalget har vært skildt minst en gang" = "a",
          "Vi kan si at 37% av veteraner i populasjonen har vært skildt minst en gang" = "b",
          "Vi kan si at 95% av veteraner i utvalget har vært skildt mindt en gang" = "c", 
          "Vi kan si at 95% av veteraner i populasjonen har vært skildt minst en gang" = "d"
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