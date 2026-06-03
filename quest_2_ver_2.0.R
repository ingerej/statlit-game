#REALI_quest_12_ver_2.0
library(shiny)
install.packages("shinythemes")
library(shinythemes)

#use navigation barns to set the 3-5 questions together
#here I need both a grading of the right answear, with a and b getting somewhat right, but not completely

ui <- fluidPage(theme = shinytheme("yeti"),
                navbarPage("Bli en ekspert på statistikkyndighet <3",
                           
                           tabPanel("Spørsmål x", #change when setting the question-packages
                                    
                                    sidebarLayout(
                                      
                                      sidebarPanel(
                                        h3("Deskriptiv statistikk"),
                                        h1("tolk sannsynlighet i kontekst"),
                                        p("Ifølge det nasjonale  kreftinstituttet i USA er sannsynligheten for å utvikle prostatakreft i løpet av livet .15 for amerikanske menn. Hva betyr sannsynligheten .15 i dette tilfellet?")
                                      ), #sidebarlayout
                                      
                                      
                                      mainPanel(
                                        h3("svaralternativer"), 
                                        
                                        radioButtons(
                                          inputId = "svar",
                                          label = "Velg et alternativ:",
                                          choices = list(
                                            "For alle menn som lever i USA vil tilnærmet 15% av dem utvikle prostatakreft i løpet av livet" = "a",
                                            "Hvis man tilfeldig velger ut en mann i USA vil det være en 15% sannsynlighet for at han utvikler prostatakreft i løpet av livet" = "b",
                                            "Ved tilfeldig utvalg av 100 amerikanske menn vil 15 av dem utvikle prostatakreft" = "c",
                                            "Både svaralternativ a og b er riktig" = "d"
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
      output$resultat <- renderText("Riktig, i dette tilfellet er både a og b riktig")
    } else {
      output$resultat <- renderText("Feil, i dette tilfellet var både a og b riktig")
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
      "Sannsynligheten .15 betyr både at omtrent 15 % av alle amerikanske menn vil utvikle prostatakreft i løpet av livet, og at en tilfeldig valgt mann har 15 % sjanse for å utvikle det. Dermed er både svaralternativ a og b rett. c er derimot ikke rett fordi den påstår at 15 av 100 vil utvikle prostatakreft. Sannsynlighet kan aldri si med sikkerhet hva som vil skje, men kan prøve å forutsi hva som trolig vil skje",
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