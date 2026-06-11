
library(shiny)

source("games/game1.R")
source("games/game2.R")


ui <- fluidPage(
  uiOutput("game_ui")
)


server <- function(input, output, session) {
  
  # Liste med spill
  games <- list(game1_ui, game2_ui)
  
  # Hvilket spill vi er på
  current_game_index <- reactiveVal(1)
  
  
  # Vis spill
  output$game_ui <- renderUI({
    games[[current_game_index()]]()
  })
  
  
  # NEXT-KNAPP (DET DU MANGLER!)
  observeEvent(input$next, {
    
    next_index <- current_game_index() + 1
    
    # loop tilbake til starten hvis vi er på siste
    if (next_index > length(games)) {
      next_index <- 1
    }
    
    current_game_index(next_index)
  })
}


shinyApp(ui, server)