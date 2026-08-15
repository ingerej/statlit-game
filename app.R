#install.packages("shiny")
library(shiny)

library(bslib) #gives card as a setup

#install.packages("")
#library() #gives namespace in R so the games can be set with the same function, but as a collective thread


#source("games/Q1_Pilot_REALI.R")

library(tidyverse)
users <- read.csv("users.csv")


current_user <- users[users$username == "user2", ]
current_level <- current_user$level

games <- read.csv("games.csv") 
matching_games <- games[
  games$from_level <= current_level & games$to_level >= current_level,
]

chosen_game <- matching_games [
  sample.int(nrow(matching_games), 1), 
  ,
  drop = FALSE
]

chosen_game

questions <- read.csv("questions.csv") 
head(questions)

chosen_questions <- questions[questions$game_id == chosen_game$id, ]

current_question <-  chosen_questions [1, ]
filename <- current_question$filename[1]
file_path <- file.path("games", filename)
source(file_path)
shinyApp(ui, server)




  
#git addhei()
ui <- page_sidebar(
  title = "Hvordan bli en statistikkekspert",
  sidebar = sidebar(
     h3("sett inn brukernavn"), #make log in
        tags$div(
          style = "font-size: 15px;",
          radioButtons(
            inputId = "spill",
            label = tags$h3("Velg et spill"),
            choices = list(
              "Level 1" = "1",
              "Level 2" = "2",
              "Level 3" = "3"
            )
          )
        ),
        actionButton("submit", "Start")
      
  ), #this is a sidebar with choices which can be sent away
  card(
    #this is the main page, card gives a frame
    card_header(
      htmlOutput("card_header_t") 
    ), #this is a function for headliner in the card collected from a game
    htmlOutput("card_content")#this gives a body to the card collected from a game
)
)
  
  
#   
#   navbarPage(title = "Kjempefin app", tabPanel(sidebarLayout(
#   sidebarPanel(
#     tags$div(
#       style = "font-size: 50px;",
#       radioButtons(
#         inputId = "spill",
#         label = tags$h3("Velg et spill"),
#         choices = list(
#           "Spill 1" = "a",
#           "Spill 2" = "b",
#           "Spill 3" = "c"
#         )
#       )
#     ),
#     actionButton("submit", "Start") 
#   ),
#   mainPanel = h1("test")
# )))



server <- function(input, output, session) {
  observeEvent(input$submit, {
    if (input$spill == "1") {
      source("game_template/level_1.R")
      output$card_header_t <- renderUI(
        card_header_test()
        
      )
      output$card_content <- renderUI(
        card_body_test()
        
      )
      # message(test())
    }
    else if (input$spill == "2") {
      source("game_template/level_2.R")
      output$card_header_t <- renderUI(
        card_header_test()
        
      )
      output$card_content <- renderUI(
        card_body_test()
        
      )
     # message(test())
    }
    else if (input$spill == "3") {
      source("game_template/level_3.R")
      output$card_header_t <- renderUI(
        card_header_test()
        
      )
      output$card_content <- renderUI(
        card_body_test()
        
      )
    }
      else 
        (output$card_content <- renderUI(
        function(){
          return("Ups, dette gikk visst dårlig")
        }))
      # message(test())
    
  })
  
  
}
#
# current_ui <- Q1_Pilot_ui
#
# current_server <- Q1_Pilot_server

shinyApp(ui = ui, server = server)