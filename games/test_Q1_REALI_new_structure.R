question <- list(
  
  id = "q1_pilot_reali",
  
  title = "Deskriptiv statistikk",
  
  ui = function(id) {
    
    ns <- shiny::NS(id)
    
    shiny::tagList(
      
      shiny::h2("Test"),
      
      shiny::plotOutput(
        ns("sleepPlot")
      ),
      
      shiny::radioButtons(
        ns("svar"),
        label = "Velg riktig svar",
        choices = c(
          "a" = "a",
          "b" = "b",
          "c" = "c",
          "d" = "d"
        )
      ),
      
      shiny::actionButton(
        ns("submit"),
        "Kontroller svaret"
      ),
      
      shiny::uiOutput(
        ns("feedback")
      ),
      
      shiny::uiOutput(
        ns("etter_svar")
      )
    )
  },
  
  server = function(id, context) {
    
    shiny::moduleServer(id, function(input, output, session) {
      
      status <- shiny::reactiveVal("not_answered")
      message <- shiny::reactiveVal(NULL)
      
      visForklaring <- shiny::reactiveVal(FALSE)
      
      observeEvent(input$submit, {
        
        shiny::req(input$svar)
        
        riktig <- "c"
        
        if (input$svar == riktig) {
          
          status("correct")
          message("Riktig!")
          
        } else {
          
          status("incorrect")
          message("Feil. Det rette svaret er c")
          
        }
        
        visForklaring(TRUE)
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
      
      output$etter_svar <- shiny::renderUI({
        
        if (visForklaring()) {
          
          shiny::div(
            style = "display: flex; gap: 20px;",
            
            shiny::actionButton(
              session$ns("forklaring"),
              "Få forklaring"
            )
          )
        }
      })
      
      observeEvent(input$forklaring, {
        
        shiny::showModal(
          shiny::modalDialog(
            title = "Forklaring",
            "Her er c det mest riktige svaret...",
            easyClose = TRUE,
            footer = shiny::tagList(
              shiny::modalButton("Lukk")
            )
          )
        )
      })
      
      output$sleepPlot <- shiny::renderPlot({
        
        hours <- seq(3, 10, by = 0.5)
        
        counts <- c(
          1, 0, 0, 0, 2, 4, 8, 11,
          17, 11, 8, 6, 4, 3, 1
        )
        
        x <- rep(hours, counts)
        
        y <- unlist(
          lapply(
            counts,
            function(n) {
              if (n > 0) seq_len(n) else numeric(0)
            }
          )
        )
        
        plot(
          x,
          y,
          pch = 19,
          xlab = "Hours of Sleep",
          axes = FALSE
        )
        
        axis(1, at = hours)
        
        box()
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