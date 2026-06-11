library(shiny)

source("games/Q1_Pilot_REALI.R")
source("games/Q4_Pilot_REALI.R") 
source("games/Q12_REALI_v2.0.R")

ui <- fluidPage(
  uiOutput("game_ui")
)

server <- function(input, output, session) {
  
  games_ui <- list(
    Q1_Pilot_ui, 
    Q4_Pilot_ui, 
    Q12_ui
  )
  
  games_server <- list(
    Q1_Pilot_server, 
    Q4_Pilot_server, 
    Q12_ui
  )
  
  current_game_index <- reactiveVal(1)
  
  output$game_ui <- renderUI({
    games_ui[[current_game_index()]]()
  })
  
  # start riktig server
  observe({
    games_server[[current_game_index()]](input, output, session)
  })
  
  observeEvent(input$next, {
    
    next_index <- current_game_index() + 1
    
    if (next_index > length(games_ui)) {
      next_index <- 1
    }
    
    current_game_index(next_index)
  })
}

shinyApp(ui, server)

