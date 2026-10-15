

question <- list(
  id = "question_015",
  title = "Miss amerika i dag",
  
  ui = function(id) {
    ns <- shiny::NS(id)
    
    shiny::tagList(
      shiny::h2("Miss amerika i dag"),
      shiny::p(
        "CNN gjennomførte en rask meningsmåling med et ramdomisert utvalg på 5581 Amerikanere 19.september 1999 for å undersøke hvor mange amerikanere som fortsatt tenker at Miss amerika konkurransen er relavant i dag. For utvalget svarte 1192 personer ja og 4389 personer nei. Indentifiser statistikken og parametrene som er av interesse."
      ),
      shiny::radioButtons(
        ns("svar"),
        label = "Velg et alternativ:",
        choices = c(
          "Statistikken er den delen av utvalget som svarte ja (1192/5581 = .214) og parameteret er de 5581 amerikanerne som tok del i spørreundersøkelsen" = "a",
          "Statistikken er de 5581 amerikanerne som tok del i spørreundersøkelsen og parameteret er alle amerikanere" = "b",
          "Statistikken er den delen av alle amerikanere som tenker at konkurransen fortsatt er relevant og parameteret er den utvalgsfordelingen av personer som svarte ja (1192/5581 = .214)" = "c", 
          "Statistikken er den utvalgsfordelingen av personer som svarte ja (1192/5581 = .214) og parameteret er den andelen av alle amerikanere som tenker konkurransen fortsatt er relevant" = "d"
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