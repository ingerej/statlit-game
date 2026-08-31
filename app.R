#install.packages("shiny")
library(shiny)

library(bslib) #gives card as a setup

library(tidyverse) #i dont think i use this now-actually. 

#getting/collecting the questions
source("R/load_questions.R")
question_metadata <- read.csv(
  "questions.csv", 
  stringsAsFactors = FALSE
  )

#installing all questions
question_modules <- setNames(
  lapply( #lapply means: do the same for everything
    question_metadata$filename,
    function(filename) {
      file_path <- file.path("./games",filename)
      load_question(file_path)
    }
  ),
  question_metadata$id
)

  
ui <- fluidPage(
  titlePanel("Læringsspill"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Kontrollpanel"),
      
      textInput(label = "Username:", 
                inputId = "Username",
                placeholder = "Username"), 
      
      actionButton("submit", "Start"),
      
      
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



server <- function(input, output, session) {
  #those under is controlling the game, and making them start on zero when no games has started.
  current_question_id <- reactiveVal(NULL)
  current_game_id <- reactiveVal(NULL)
  current_question_index <- reactiveVal(NULL)
  game_results <- reactiveVal(c())
  
  #next: popup window for knowing when the game is over, which is- when all the questions sat to that game is played.
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
  }
  
  #log in and starting the right game
  observeEvent(input$submit, {
 message("Username: ", input$Username)
    
    users <- read.csv("users.csv") #check the username with already stored usernames
    current_user <- users[users$username == input$Username, ]
   
     if (nrow(current_user) == 0) { #if the username does not exists. Start at 50%
      new_user <- data.frame(
     username = input$Username,
        level = 50
      )

      users <- rbind(users, new_user)
      write.csv( #write the new name in the csv file. 
        users,
        "users.csv",
        row.names = FALSE
        )

      current_level <- 50
    } 
    else {

      current_level <- current_user$level #if it does exist- continue
    }
    
    games <- read.csv("games.csv") #starting a games based on the usernames level
    matching_games <- games[
      games$from_level <= current_level & games$to_level >= current_level, #chose the right difficulty for the users level
    ]
    
    chosen_game <- matching_games [
      sample.int(nrow(matching_games), 1), #pick one of the matching games
      ,
      drop = FALSE
    ]
    
    message(chosen_game$id) #give the server the game- it is a check for us to see if it stops before this or not
    
    questions <- read.csv("questions.csv")  #read the questions
    head(questions)
    
    chosen_questions <- questions[questions$game_id == chosen_game$id, ]
    
    current_question <-  chosen_questions [1, ] #choose the first question of chosen game
    current_game_id(chosen_game$id)
    current_question_index(1)
    current_question_id(current_question$id)
    
    message(current_question$filename) #give us the question
    
    #getting the question to the show-side
    filename <- current_question$filename[1]
    file_path <- file.path(".//games", filename)
    
    
    
    work_in_progress <- load_question(file_path)
    message(work_in_progress)
    
    file_path
    }
  )
  
  #under is the start of sending a feedback changing the level over time. But not done yet
  context <- list(
    user_id = reactive("user2"),
    level = reactive(50)
  )
  
  #results of the question
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
  
  #this makes the game show in app.R and not in its own space
  output$question_body <- renderUI({
    question_id <- req(current_question_id()) #req- dont start befor you have it. 
    #what to show
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
  #text for the questions given in the sidebar
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
  
  #buttin: next question, and used later for precentage right
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
    next_index <- current_question_index() + 1 #find the next row
    
    #is there anymore questions?
    if(next_index <= nrow(chosen_questions)) {
      current_question_index(next_index)
      current_question_id(
        chosen_questions$id[next_index]
      ) 
    } else { #if not: they are done, so the show the window given earlier
     
      finish_game()
    }
      return()
  }
  )
}

#Later, make the code so the results is getting sent to questions.csv

#Samle statistikk for å se om den virker. Se på resultatene fra de som bruker over tid. Hente klokka når ting starter og slutter, lagre data om hvordan de bruker det. 
#R kan lage csv over bruken. Ledd det inn i app.R. Tid når bruker starter og stopper, hvor mange ganger bruker er inne. Måle svar, hvor bra bruker skårer, og hvordan dette utvikler seg over tid. 
#Lage fancy resultatplot over hvordan appen virker for ulike type brukere. ->utifra hvor ofte noen er inne ofte. 
#Pappas umiddelbare tanker spørsmålene kan kategoriseres i flere typer- prøve å skille ut om noen spørsmål lærer man av fort og noen ikke. Om spørsmål om histogrammer læres raskere enn andre. 
#Mens spørsmål som er dårligere formulert, vanskeligere å lære av dem. 
#skal det være passord? Det er ikke så vanskelig å få til.

#fint å ha: hvor ofte inne, hvor lang tid de bruker per gang, endre .csv til google for at det skal fungere med shiny-appen
#få den opp på shiny app, og gjør det om til google sheets.Feedback loop venter. Endre i tråd med Adrian sine tanker.  

#gjøremål: skrives oppgave, vurdere vanskelighetsgrader og sette sammen spørsmålspakker - som har nivå,  



#Adrian sine tanker
#sette spørsmål om subjektiv vurdering av spørsmålene. "hvor vanskelig synes du spørsmålene var?" Det kan gi bra vurdering til videre. 
#ved tre spørsmål per spill er en feil 33%. per nå er det jo prosenter -  hard straff ved en feil... annen utregning? 
#rett mer belønning, men feil svar straffer mindre? 

#åpne nivåer? gi dem valget selv? Vil du høyere dersom du får alle rette eller maks en feil? Gir mer opplevd autonomi over egen utvikling. mer konkurranse? "ansvar for egen læring". 


shinyApp(ui, server)