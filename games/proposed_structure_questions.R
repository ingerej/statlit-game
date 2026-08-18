# Proposed structure of the questions
question <- list(
  id = "question_001",
  title = "Hovedstaden i Norge",
  
  ui = function(id) {
    # Returnerer UI som skal vises i body
  },
  
  server = function(id, context) {
    # Returnerer en reaktiv resultatstatus
  }
)



#proposed contract for each question
list(
  status = "not_answered",
  solved = FALSE,
  message = NULL
)



#message if there is a false in something
#Innlesing med feilhåndtering:
load_question <- function(filepath) {
  if (!file.exists(filepath)) {
    stop("Finner ikke spørsmålsfilen: ", filepath)
  }
  
  question_environment <- new.env(
    parent = globalenv()
  )
  
  sys.source(
    filepath,
    envir = question_environment
  )
  
  if (!exists(
    "question",
    envir = question_environment,
    inherits = FALSE
  )) {
    stop(
      "Spørsmålsfilen må opprette et objekt kalt 'question': ",
      filepath
    )
  }
  
  question <- get(
    "question",
    envir = question_environment,
    inherits = FALSE
  )
  
  required_fields <- c(
    "id",
    "title",
    "ui",
    "server"
  )
  
  missing_fields <- setdiff(
    required_fields,
    names(question)
  )
  
  if (length(missing_fields) > 0) {
    stop(
      "Spørsmålet mangler feltene: ",
      paste(missing_fields, collapse = ", ")
    )
  }
  
  if (!is.function(question$ui)) {
    stop("Feltet 'ui' må være en funksjon")
  }
  
  if (!is.function(question$server)) {
    stop("Feltet 'server' må være en funksjon")
  }
  
  question
}



----------------
  #a new structure that works
  
  
  #Her er en enkel mal som følger nøyaktig samme struktur som de fungerende spørsmålene dine. Du kan kopiere denne filen og bare bytte ut tekst, svaralternativer, fasit og forklaring.

question <- list(
  
  id = "unik_sporsmal_id",
  
  title = "Tittel på spørsmålet",
  
  ui = function(id) {
    
    ns <- shiny::NS(id)
    
    shiny::tagList(
      
      shiny::h2("Spørsmålstittel"),
      
      shiny::p(
        "Skriv spørsmålet her."
      ),
      
      shiny::radioButtons(
        ns("svar"),
        label = "Velg et alternativ:",
        choices = c(
          "Svaralternativ A" = "a",
          "Svaralternativ B" = "b",
          "Svaralternativ C" = "c",
          "Svaralternativ D" = "d"
        )
      ),
      
      shiny::actionButton(
        ns("submit"),
        "Sjekk svar"
      ),
      
      shiny::br(),
      shiny::br(),
      
      shiny::uiOutput(
        ns("feedback")
      ),
      
      shiny::br(),
      
      shiny::uiOutput(
        ns("forklaring_knapp")
      )
    )
  },
  
  server = function(id, context) {
    
    shiny::moduleServer(id, function(input, output, session) {
      
      status <- shiny::reactiveVal("not_answered")
      message <- shiny::reactiveVal(NULL)
      
      visForklaring <- shiny::reactiveVal(FALSE)
      
      observeEvent(input$submit, {
        
        shiny::req(input$svar)
        
        riktig <- "a"   # Endre til riktig alternativ
        
        if (input$svar == riktig) {
          
          status("correct")
          message("Riktig!")
          
        } else {
          
          status("incorrect")
          message("Feil. Prøv igjen.")
          
        }
        
        visForklaring(TRUE)
      })
      
      output$feedback <- shiny::renderUI({
        
        shiny::req(message())
        
        if (status() == "correct") {
          
          shiny::div(
            class = "alert alert-success",
            message()
          )
          
        } else {
          
          shiny::div(
            class = "alert alert-warning",
            message()
          )
        }
      })
      
      output$forklaring_knapp <- shiny::renderUI({
        
        if (visForklaring()) {
          
          shiny::actionButton(
            session$ns("forklaring"),
            "Få forklaring"
          )
        }
      })
      
      observeEvent(input$forklaring, {
        
        shiny::showModal(
          shiny::modalDialog(
            title = "Forklaring",
            "Skriv inn forklaringen her.",
            easyClose = TRUE,
            footer = shiny::tagList(
              shiny::modalButton("Lukk")
            )
          )
        )
      })
      
      return(
        shiny::reactive({
          list(
            status = status(),
            solved = identical(status(), "correct"),
            message = message()
          )
        })
      )
    })
  }
)

#et du vanligvis endrer

#ID

#id = "q13_deskriptiv_statistikk"


#Tittel

# title = "Tolk sannsynlighet"
# 
# 
# Spørsmålstekst
# 
# shiny::p("Hva betyr en sannsynlighet på 0,20?")
# 
# 
# Svaralternativer
# 
# choices = c(
#   "Alternativ 1" = "a",
#   "Alternativ 2" = "b",
#   "Alternativ 3" = "c",
#   "Alternativ 4" = "d"
# )
# 
# 
# Fasit
# 
# riktig <- "c"
# 
# 
# Tilbakemelding
# 
# message("Riktig!")
# 
# 
# og
# 
# message("Feil. Riktig svar er C.")
# 
# 
# Forklaring
# 
# "Skriv den faglige forklaringen her."
# 
# 
# Dette er en god "master-template" for alle nye REALI-spørsmål.