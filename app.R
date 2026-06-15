library(shiny)

library(bslib) #gives card as a setup

install.packages("")
library() #gives namespace in R so the games can be set with the same function, but as a collective thread


#source("./games/Q1_Pilot_REALI.R")
source("./game_template/game_example.R")

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
              "Spill 1" = "a",
              "Spill 2" = "b",
              "spill eksempel" = "c"
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
    if (input$spill == "a") {
      output$spillet
    }
    else if (input$spill == "c") {
      output$card_header_t <- renderUI(
        card_header_test()
        
      )
      output$card_content <- renderUI(
        card_body_test()
        
      )
      message(test())
    }
  })
  
  
}
#
# current_ui <- Q1_Pilot_ui
#
# current_server <- Q1_Pilot_server

shinyApp(ui = ui, server = server)