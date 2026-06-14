library(shiny)

library(bslib)
source("./games/Q1_Pilot_REALI.R")

hei()
ui <- page_sidebar()
  
  
  
  navbarPage(title = "Kjempefin app", tabPanel(sidebarLayout(
  sidebarPanel(
    tags$div(
      style = "font-size: 50px;",
      radioButtons(
        inputId = "spill",
        label = tags$h3("Velg et spill"),
        choices = list(
          "Spill 1" = "a",
          "Spill 2" = "b",
          "Spill 3" = "c"
        )
      )
    ),
    actionButton("submit", "Start") 
  ),
  mainPanel = h1("test")
)))



server <- function(input, output, session) {
  observeEvent(input$submit, {
    if (input$spill == "a") {
      output$spillet
    } 
  })
}
#
# current_ui <- Q1_Pilot_ui
#
# current_server <- Q1_Pilot_server

shinyApp(ui = ui, server = server)