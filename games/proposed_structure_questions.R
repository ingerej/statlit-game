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