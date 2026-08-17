#install.packages("shiny")
library(shiny)

library(bslib) #gives card as a setup

#install.packages("")
#library() #gives namespace in R so the games can be set with the same function, but as a collective thread



#source("games/Q1_Pilot_REALI.R")

library(tidyverse)
# users <- read.csv("users.csv")
# 
# 
# current_user <- users[users$username == "user2", ]
# current_level <- current_user$level
# 
# games <- read.csv("games.csv")
# matching_games <- games[
#   games$from_level <= current_level & games$to_level >= current_level,
# ]
# 
# chosen_game <- matching_games [
#   sample.int(nrow(matching_games), 1),
#   ,
#   drop = FALSE
# ]
# 
# chosen_game
# 
# questions <- read.csv("questions.csv")
# head(questions)
# 
# chosen_questions <- questions[questions$game_id == chosen_game$id, ]
# 
# current_question <-  chosen_questions [1, ]
# filename <- current_question$filename[1]
# file_path <- file.path("./games", filename)
# file_pathusers <- read.csv("users.csv")
# 
# 
# current_user <- users[users$username == "user2", ]
# current_level <- current_user$level
# 
# games <- read.csv("games.csv")
# matching_games <- games[
#   games$from_level <= current_level & games$to_level >= current_level,
# ]
# 
# chosen_game <- matching_games [
#   sample.int(nrow(matching_games), 1),
#   ,
#   drop = FALSE
# ]
# 
# chosen_game
# 
# questions <- read.csv("questions.csv")
# head(questions)
# 
# chosen_questions <- questions[questions$game_id == chosen_game$id, ]
# 
# current_question <-  chosen_questions [1, ]
# filename <- current_question$filename[1]
# file_path <- file.path("./games", filename)
# file_path
# source(file_path)



source("R/load_questions.R")
question_metadata <- read.csv(
  "questions.csv", 
  stringsAsFactors = FALSE
  )

# question_modules <- setNames(
#   
#   lapply(
#     
#     question_metadata$filename,
#     
#     function(filename) {
#       
#       
#       
#       cat("filename = [", filename, "]\n", sep="")
#       
#       
#       
#       file_path <- file.path("./games", filename)
#       
#       
#       
#       cat("filepath = [", file_path, "]\n", sep="")
#       
#       
#       
#       load_question(file_path)
#       
#     }
#   ),
#   question_metadata$id
# )

question_modules <- setNames(
  lapply(
    question_metadata$filename,
    function(filename) {
      file_path <- file.path("./games",filename)
      load_question(file_path)
    }
  ),
  question_metadata$id
)
# shinyApp(ui, server)

  
#git addhei()
# ui <- page_sidebar(
#   title = "Hvordan bli en statistikkekspert",
#   sidebar = sidebar(
#      h3("sett inn brukernavn"), #make log in
#         tags$div(
#           style = "font-size: 15px;",
#           radioButtons(
#             inputId = "spill",
#             label = tags$h3("Velg et spill"),
#             choices = list(
#               "Level 1" = "1",
#               "Level 2" = "2",
#               "Level 3" = "3"
#             )
#           )
#         ),
#         actionButton("submit", "Start")
#       
#   ), #this is a sidebar with choices which can be sent away
#   card(
#     #this is the main page, card gives a frame
#     card_header(
#       htmlOutput("card_header_t") 
#     ), #this is a function for headliner in the card collected from a game
#    # htmlOutput("card_content")#this gives a body to the card collected from a game #removes ("card_content") from this place
#     htmlOutput(uiOutput("question_body"))
#    )
# )
  
  
ui <- fluidPage(
  titlePanel("Læringsspill"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Kontrollpanel"),
      
      textInput(label = "Username:", 
                inputId = "Username",
                placeholder = "Username"), 
      
      actionButton("submit", "Start"),
      
      # selectInput(
      #   "selected_question",
      #   "Velg spørsmål",
      #   choices = question_metadata$id
      # ),
      # 
      # hr(),
      
      textOutput("question_status"),
      
      actionButton(
        "next_question",
        "Neste spørsmål"
      )
    ),
    
    mainPanel(
      uiOutput("question_body")
    )
  )
)


#
#    navbarPage(title = "Kjempefin app", tabPanel(sidebarLayout(
#    sidebarPanel(
#      tags$div(
#        style = "font-size: 50px;",
#        textInput()
#      ),
#      actionButton("submit", "Start")
#    ),
#    mainPanel = h1("test")
# )))



#server <- function(input, output, session) {
 
  # observeEvent(input$submit, {
   # if (input$spill == "1") {
    #  source("game_template/level_1.R")
     # output$card_header_t <- renderUI(
      #  card_header_test()
        
      #)
      #output$card_content <- renderUI(
       # card_body_test()
        
      #)
      # message(test())
    #}
    #else if (input$spill == "2") {
     # source("game_template/level_2.R")
      #output$card_header_t <- renderUI(
       # card_header_test()
        
      #)
      #output$card_content <- renderUI(
       # card_body_test()
        
      #)
     # message(test())
    #}
    #else if (input$spill == "3") {
     # source("game_template/level_3.R")
      #output$card_header_t <- renderUI(
       # card_header_test()
        
      #)
      #output$card_content <- renderUI(
       # card_body_test()
        
    #  )
    #}
     # else 
      #  (output$card_content <- renderUI(
       # function(){
        #  return("Ups, dette gikk visst dårlig")
        #}))
      # message(test())
    
#  })
  
  
#}
#
# current_ui <- Q1_Pilot_ui
#
# current_server <- Q1_Pilot_server

#shinyApp(ui = ui, server = server)


server <- function(input, output, session) {
  
  current_question_id <- reactiveVal(NULL)
  current_game_id <- reactiveVal(NULL)
  current_question_index <- reactiveVal(NULL)
  game_results <- reactiveVal(c())
  
  finish_game <- function() {
    results <- game_results()
    score <- mean(results)
    showModal(
      modalDialog(
        title = "Spillet er ferdig!",
        paste0(
          "Du fikk ",
          round(score * 100),
          "% riktige svar."
        ),
        easyClose = TRUE
      )
    )
  
  observeEvent(input$submit, {
 message("Username: ", input$Username)
    
    users <- read.csv("users.csv")
    current_user <- users[users$username == input$Username, ]
   
     if (nrow(current_user) == 0) {
      new_user <- data.frame(
     username = input$Username,
        level = 50
      )

      users <- rbind(users, new_user)
      write.csv(
        users,
        "users.csv",
        row.names = FALSE
        )

      current_level <- 50
    } 
    else {

      current_level <- current_user$level
    }
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
    
    message(chosen_game$id)
    
    questions <- read.csv("questions.csv") 
    head(questions)
    
    chosen_questions <- questions[questions$game_id == chosen_game$id, ]
    
    current_question <-  chosen_questions [1, ]
    current_game_id(chosen_game$id)
    current_question_index(1)
    current_question_id(current_question$id)
    
    message(current_question$filename)
    
    filename <- current_question$filename[1]
    file_path <- file.path("./games", filename)
    
    
    
    work_in_progress <- load_question(file_path)
    message(work_in_progress)
    

    
    file_path
  #  source(file_path)
    
  
    
    }
  )
  
  
  
  context <- list(
    user_id = reactive("user2"),
    level = reactive(50)
  )
  
  question_results <- lapply(
    question_metadata$id,
    function(question_id) {
      question_modules[[question_id]]$server(
        id = question_id,
        context = context
      )
    }
  )
  
  names(question_results) <- question_metadata$id
  
  output$question_body <- renderUI({
    
    
    
    question_id <- req(current_question_id())
    
    
    
    question <- question_modules[[question_id]]
    
    
    
    tagList(
      
      h1(question$title),
      
      question$ui(question_id)
      
    )
    
  })
  
  selected_result <- reactive({
    question_id <- req(current_question_id())
    question_results[[question_id]]()
  })
  
  output$question_status <- renderText({
    result <- selected_result()
    
    switch(
      result$status,
      not_answered = "Ikke besvart",
      incorrect = "Ikke riktig",
      correct = "Oppgaven er løst",
      "Ukjent status"
    )
  })
  
  observeEvent(input$next_question, {
    result <- selected_result()
    old_results <- game_results()
    game_results(
      c(
        old_results,
        identical(result$status, "correct")
      )
    )
    questions <- read.csv(
      "questions.csv",
      stringsAsFactors = FALSE
    )
    chosen_questions <- questions[
      questions$game_id == current_game_id(),
    ]
    next_index <- current_question_index() + 1
    
    if(next_index <= nrow(chosen_questions)) {
      current_question_index(next_index)
      current_question_id(
        chosen_questions$id[next_index]
      )
    } else {
     
      finish_game()
    }
      return()
  }
  )
  }
}
    
    # Hovedappen oppdaterer brukerens nivå,
    # lagrer resultatet og velger neste spørsmål.
  

shinyApp(ui, server)