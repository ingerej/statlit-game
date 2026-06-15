library(shiny)


# all questions withholds the variables: id, type, tittle, description, has_plot, answear alternatives, right answaer, explanation
# Moreover, every answaer withholds the static: answear button, show explanation, next answear. 




#ui function
render_question_ui <- function(q) {
  
  tagList(
    
    h2(q$title),
    p(q$description),
    
    # Plot if has_plot == TRUE
    if (q$has_plot) {
      plotOutput("plot")
    },
    
    # answear alternatives
    radioButtons(
      "answer",
      "Velg svar:",
      choices = setNames(seq_along(q$choices), q$choices)
    ),
    
 
    actionButton("submit", "Sjekk svar"),
    br(), br(),
    
    textOutput("feedback"),
    uiOutput("explanation_ui"),
    
    br(),
    actionButton("next", "Neste spørsmål")
  )
}

# server function

question_server <- function(input, output, session, q) {
  
  show_expl <- reactiveVal(FALSE)
  
  observeEvent(input$submit, {
    
    if (is.null(input$answer)) {
      output$feedback <- renderText("Velg et svar først")
      return()
    }
    
    if (as.numeric(input$answer) == q$correct) {
      output$feedback <- renderText("Riktig!")
    } else {
      output$feedback <- renderText("Feil")
    }
    
    show_expl(TRUE)
  })
  
  output$explanation_ui <- renderUI({
    if (show_expl()) {
      tagList(
        p(q$explanation)
      )
    }
  })
  
  #plot
  if (q$has_plot) {
    output$plot <- renderPlot({
      q$plot_fun()
    })
  }
}


ui <- fluidPage(
  uiOutput("question_ui")
)


server <- function(input, output, session) {
  
  current_q <- reactiveVal(1)
  
  # show question
  output$question_ui <- renderUI({
    render_question_ui(questions[[current_q()]])
  })
  
  # right logic
  observe({
    question_server(input, output, session, questions[[current_q()]])
  })
  
  # next
  observeEvent(input$next, {
    next_q <- current_q() + 1
    
    if (next_q > length(questions)) {
      next_q <- 1
    }
    
    current_q(next_q)
  })
}



shinyApp(ui, server)