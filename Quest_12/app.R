#here is question 12 i REALI
library(shiny)



ui <- fluidPage(
  titlePanel("Tolking av sannsynlighet"),
  radioButtons("svar", "Ifølge det nasjonale  kreftinstituttet i USA er sannsynligheten for å utvikle prostatakreft i løpet av livet .15 for amerikanske menn. Hva betyr sannsynligheten .15 i dette tilfellet?",
               choices = list(
                 "For alle menn som bor i USA, vil omtrent 15% utvikle prostatakreft i løpet av livet." = "a",
                 "Hvis du tilfeldig velger en mann i USA, er det 15% sjanse for at han vil utvikle prostatakreft i løpet av livet." = "b",
                 "I et tilfeldig utvalg på 100 menn i USA vil 15 menn utvikle prostatakreft." = "c",
                 "Både alternativ 1 og 2 er riktige." = "d"
               )),
  actionButton("submit", "Sjekk svar"),
  textOutput("resultat")
)

server <- function(input, output) {
  observeEvent(input$submit, {
    riktig <- "d"  # Riktig svar
    if (input$svar == riktig) {
      output$resultat <- renderText("✅ Riktig! Både alternativ 1.og 2 er korrekte.")
    } else {
      output$resultat <- renderText("❌ Feil. Prøv igjen.")
    }
  })
}

shinyApp(ui, server)
