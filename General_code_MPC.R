#a general MPC layout

library(shiny)
install.packages("shinythemes")
library(shinythemes)

#use navigation barns to set the 3-5 questions together

ui <- fluidPage(tags$head(
  tags$style(HTML("
  body, label, input, button, select {
      font-family: 'Times New Roman', serif;
  }
  
  h1, h2, h3, h4 {
      font-family: 'Times New Roman', serif;
    }
    
    .shiny-input-radiogroup label {
      font-size: 14px;
    }
  "))
), theme = shinytheme("yeti"),
                navbarPage("Bli en ekspert på statistikkyndighet <3",
                
                           tabPanel("Spørsmål x",
                
                                    sidebarLayout(
                 
                                       sidebarPanel(
                                          h3("her skriver du overordnet spørsmål/tema"),
                                           p("her kommer selve oppgaven til sudentene")
                                           ), #sidebarlayout
                 
                  
                                        mainPanel(
                                           h3("svaralternativer"), 
                 
                                            radioButtons(
                                              inputId = "svar",
                                               label = "Velg et alternativ:",
                                              choices = list(
                                                 "Svaralt. 1." = "a",
                                                   "svaralt. 2." = "b",
                                                  "svaralt. 3." = "c",
                                                   "svaralt. 4." = "d"
                                                  )#list
                                               ), #radiobutton
                  
                                            actionButton("submit", "Sjekk svar"),
                                           br(), br(),
                                           textOutput("resultat"),
                                           br(),
                                           uiOutput("forklaring_knapp"),
                                           br(),
                                           actionButton("neste", "Neste spørsmål")
                                           )#mainPanel
                                       )#sidebarlayout
                                )#tabpanel
                           )#navbarpage
                )#fluidpage

                  

server <- function(input, output, session) {
  visForklaring <- reactiveVal(FALSE)
  
  observeEvent(input$submit, {
    riktig <- "d"  # Riktig svar
    
    if (input$svar == riktig) {
      output$resultat <- renderText("Text hvis riktig.")
    } else {
      output$resultat <- renderText("tekst hvis feil")
    }
    visForklaring(TRUE)
  }) #observeevent with ifelse
  output$forklaring_knapp <- renderUI({
    if (visForklaring()) {
      actionButton("forklaring", "Få forklaring")
    }
  }) #outout
  observeEvent(input$forklaring, {
    showModal(modalDialog(
      title = "Forklaring",
      "Sannsynligheten .15 betyr både at omtrent 15 % av alle amerikanske menn vil utvikle prostatakreft i løpet av livet, og at en tilfeldig valgt mann har 15 % sjanse for å utvikle det.",
      easyClose = TRUE, 
      footer = tagList(
        actionButton("lukk", "Lukk forklaring")
      )#footer close
    )) #modal dialog and showmodal
  })#observeevent
  observeEvent(input$lukk, {
  removeModal()
    })
}

shinyApp(ui = ui, server = server)