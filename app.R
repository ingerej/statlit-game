library(shiny)

library(bslib)
#source("./games/Q1_Pilot_REALI.R")
source("./game_template/game_example.R")

#git addhei()
ui <- page_sidebar(
  title = "Hvordan bli en statistikkekspert",
  sidebar = sidebar(
     
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
  card( #this is the main page, card gives a frame
    card_header("overskrift"), #this is headliner in the card
    "her er hovedinnholdet" #this gives a body to the card
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
      test()
    }
  })
}
#
# current_ui <- Q1_Pilot_ui
#
# current_server <- Q1_Pilot_server

shinyApp(ui = ui, server = server)