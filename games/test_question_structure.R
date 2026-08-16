#this is an example of a question.  Not relevant, but used to test the system
question <- list(
  
  id = "question_001",
  
  title = "Hovedstaden i Norge",
  
  ui = function(id) {
    ns <- shiny::NS(id)
    
    shiny::tagList(
      shiny::h2("Hvilken by er hovedstaden i Norge?"),
      
      shiny::radioButtons(
        ns("answer"),
        label = NULL,
        choices = c(
          "Bergen",
          "Oslo",
          "Trondheim"
        )
      ),
      
      shiny::actionButton(
        ns("check_answer"),
        "Kontroller svaret"
      ),
      
      shiny::uiOutput(
        ns("feedback")
      )
    )
  },
  
  server = function(id, context) {
    shiny::moduleServer(id, function(input, output, session) {
      
      status <- shiny::reactiveVal("not_answered")
      message <- shiny::reactiveVal(NULL)
      
      shiny::observeEvent(input$check_answer, {
        shiny::req(input$answer)
        
        if (input$answer == "Oslo") {
          status("correct")
          message("Riktig!")
        } else {
          status("incorrect")
          message("Det var ikke riktig. Prøv igjen.")
        }
      })
      
      output$feedback <- shiny::renderUI({
        shiny::req(message())
        
        if (status() == "correct") {
          shiny::div(
            class = "alert alert-success",
            message()
          )
        } else {
          shiny::div(
            class = "alert alert-warning",
            message()
          )
        }
      })
      
      # Kontrakten med hovedappen
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