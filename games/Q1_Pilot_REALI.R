# Q1_Pilot_REALI.R
#q1 reali pilot i dont know and i hate ai
Q1_Pilot_ui <- function() {
 tagList(
  sidebarLayout(
   sidebarPanel(
    h5("Deskriptiv statistikk")
 ),
mainPanel(
   h3("Test")
  )
 )
)
}


hei <- function() {
  return("heisann")
}




Q1_Pilot_server <- function(input, output, session) {

  visForklaring <- reactiveVal(FALSE)

  observeEvent(input$submit, {
    riktig <- "c"

    if (input$svar == riktig) {
      output$resultat <- renderText("Riktig!")
    } else {
      output$resultat <- renderText("Feil. Det rette svaret er c")
    }

    visForklaring(TRUE)
  })

  output$etter_svar <- renderUI({
    if (visForklaring()) {
      div(
        style = "display: flex; gap: 20px;",
        actionButton("forklaring", "Få forklaring"),
        actionButton("next", "Neste spill")
      )
    }
  })

  observeEvent(input$forklaring, {
    showModal(modalDialog(
      title = "Forklaring",
      "Her er c det mest riktige svaret...",
      easyClose = TRUE,
      footer = actionButton("lukk", "Lukk forklaring")
    ))
  })

  observeEvent(input$lukk, {
    removeModal()
  })

  output$sleepPlot <- renderPlot({

    hours <- seq(3, 10, by = 0.5)

    counts <- c(1,0,0,0,2,4,8,11,17,11,8,6,4,3,1)

    x <- rep(hours, counts)
    y <- unlist(lapply(counts, function(n) if (n > 0) seq_len(n) else numeric(0)))

    plot(
      x, y,
      pch = 19,
      xlab = "Hours of Sleep",
      axes = FALSE
    )

    axis(1, at = hours)
    box()
  })
}
#shinyApp(ui = Q1_Pilot_ui, server = Q1_Pilot_server)