#install.packages("shiny")
library(shiny)

library(bslib) #gives card as a setup

#install.packages("googlesheet4")
library(googlesheets4)

source("user_state.R")

source("content_structure.R")


#getting/collecting the questions
source("R/load_questions.R")
question_metadata <- read.csv(
  "questions.csv",
  stringsAsFactors = FALSE
)

#installing all questions
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

#### ui ####

ui <- page_fluid(
  # NYTT: tema fra bslib
  # theme = bslib::bs_theme(
  #   version = 5,
  #   bootswatch = "flatly",
  #   base_font = "Helvetica, Arial, sans-serif"
  # ),
  # 
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = "Helvetica, Arial, sans-serif",
    
    # Lys, pastell-palette
    bg        = "#FFFCF7",  # Lys, varm bakgrunn
    fg        = "#29324B",  # Mørk blå tekst
    primary   = "#7460B3",  # Lavendel (mørkere for lesbarhet)
    secondary = "#E5D9F7",  # Lys pastell-lavendel
    success   = "#BFE5CF",  # Pastellgrønn / mint
    info      = "#C8E4F6",  # Lys himmelblå
    warning   = "#FFE8AC",  # Myk vaniljegul
    danger    = "#F5C7D2"   # Lys rosafarge
  ),
  # NYTT: finner tittel
  titlePanel("Læringsspill", windowTitle = "Læringsspill"),
  
  # NYTT: luftigere layout med CSS
  tags$style(HTML("
    body { padding: 20px; }
    .radio { margin-bottom: 5px; }
    .btn { border-radius: 6px; }
  ")),
  
  tags$h1("Læringsspill", align = "center"),
  hr(),
  
  layout_sidebar(
    sidebar = sidebar(
      h4("Kontrollpanel"),
      
      textInput(label = "Username:", 
                inputId = "Username",
                placeholder = "Username"), 
      
      actionButton("submit", "Start", icon = icon("play")),
      
      textOutput("question_status"),
      
      actionButton(
        "next_question",
        "Neste spørsmål",
        icon = icon("arrow-right")
      ),
      
      # NYTT: avslutningsknapp som vises når spillet er ferdig
      uiOutput("finish_button")
    ),
    
    mainPanel(
      uiOutput("question_body"),
      hr()
    )
  )
)


#### end ui ####

server <- function(input, output, session) {
  #those under is controlling the game, and making them start on zero when no games has started.
  current_question_id <- reactiveVal(NULL)
  current_game_id <- reactiveVal(NULL)
  current_question_index <- reactiveVal(NULL)
  game_results <- reactiveVal(c())
  
  current_lesson_id <- reactiveVal(NULL)
  
  current_user <- reactiveVal(NULL)
  
  # NYTT: holder styr på om spillet er ferdig
  game_finished <- reactiveVal(FALSE)
  
  # NYTT: holder resultatet som skal sendes tilbake til app.R
  final_score <- reactiveVal(NULL)
  #add some more global 
  question <- NULL
  levels <- NULL
  lessons <- NULL
  
  
  
  #log in and starting the right game
  observeEvent(input$submit, {
    message("Username: ", input$Username)
    
    
    username <- trimws(input$Username)
    req(nzchar(username))
    
    # Hent eksisterende bruker, eller opprett et nytt objekt
    user <- load_user(username)
    
    if (is.null(user)) {
      user <- new_user(username)
    }
    
    # Lagre innlogget bruker i denne økten
    current_user(user)
    
    # Ny bruker må velge nivå før vi kan finne et spørsmål
    if (is.na(user$level)) {
      showNotification("Velg et nivå for å starte.", type = "message")
      return()
    }
    
    # Kontroller at nivået finnes i innholdsstrukturen
    if (!(user$level %in% names(app_content$levels))) {
      showNotification("Det lagrede nivået finnes ikke.", type = "error")
      return()
    }
    
    # Finn leksjonen og spørsmålsposisjonen brukeren skal fortsette på
    rp <- resume_point(user, app_content)
    
    if (rp$level_done) {
      current_lesson_id(NULL)
      current_question_index(NULL)
      current_question_id(NULL)
      
      showNotification(
        "Alle leksjonene på nivået er fullført.",
        type = "message"
      )
      
      # Her kan dere senere vise nivåprøven
      return()
    }
    
    # Hent leksjonen
    lesson <- get_lesson(
      app_content,
      level_id = user$level,
      lesson_id = rp$lesson_id
    )
    
    # Finn neste spørsmåls-id
    question_id <- lesson$question_ids[rp$question_index]
    
    # Hent informasjonen om spørsmålet, inkludert filnavnet
    question <- get_question(app_content, question_id)
    
    # Oppdater hvor brukeren er i appen
    current_lesson_id(rp$lesson_id)
    current_question_index(rp$question_index)
    current_question_id(question_id)
    
    # Filen som tilhører spørsmålet
    file_path <- file.path("games", question$filename)
    
    message("Neste spørsmål: ", question_id)
    message("Spørsmålsfil: ", file_path)
    work_in_progress <- load_question(file_path)
    
    message(work_in_progress)
    
    file_path
    
  }
  )
  
  
  
  observeEvent(input$next_question, {
    user <- req(current_user())
    question_id <- req(current_question_id())
    previous_lesson_id <- req(current_lesson_id())
    
    # Spørsmålet må være besvart
    result <- req(selected_result())
    
    if (!(result$status %in% c("correct", "incorrect"))) {
      showNotification(
        "Svar på spørsmålet før du går videre.",
        type = "warning"
      )
      return()
    }
    
    tryCatch({
      # Lagre siste besvarte spørsmål
      user <- complete_question(user, question_id)
      current_user(user)
      
      # Finn neste posisjon med samme funksjon som ved innlogging
      rp <- resume_point(user, app_content)
      message("rp: ", rp)
      # Har brukeren fullført leksjonen?
      lesson_finished <- rp$level_done ||
        !identical(rp$lesson_id, previous_lesson_id)
      
      if (lesson_finished) {
        user <- complete_lesson(user, previous_lesson_id)
        current_user(user)
        
        current_question_id(NULL)
        current_question_index(NULL)
        current_lesson_id(NULL)
        
        if (rp$level_done) {
          showNotification("Klar for nivåprøven!")
          # Vis nivåprøven her
        } else {
          current_lesson_id(rp$lesson_id)
          showNotification("Klar for neste leksjon!")
          # Vis leksjonsforklaringen her, før spørsmålene starter
        }
        
      } else {
        # Neste spørsmål i samme leksjon
        lesson <- get_lesson(
          app_content,
          user$level,
          rp$lesson_id
        )
        
        current_lesson_id(rp$lesson_id)
        current_question_index(rp$question_index)
        current_question_id(
          lesson$question_ids[rp$question_index]
        )
        
        # Filen som tilhører spørsmålet
        file_path <- file.path("games", question$filename)
        
        message("Neste spørsmål: ", question_id)
        message("Spørsmålsfil: ", file_path)
        work_in_progress <- load_question(file_path)
        
        message(work_in_progress)
        
        file_path
      }
      
    }, error = function(e) {
      message("Feil ved neste spørsmål: ", conditionMessage(e))
      showNotification(
        "Kunne ikke gå videre. Prøv igjen.",
        type = "error"
      )
    })
  }, ignoreInit = TRUE)
  
  #  #shall read level, lessons and question, and make them ready. Make them global so everyone reach them
  # start_up <- function() {
  #   questions <- read.csv("questions.csv")  #read the questions
  #   levels <- read.csv("levels.csv")   #read the levels
  #   lessons <- read.csv("lessons.csv")  #read the lessons
  #   
  #   
  # } 
  
  
  #next: popup window for knowing when the game is over, which is- when all the questions sat to that game is played.
  finish_game <- function() {
    results <- game_results()
    score <- mean(results)
    # NYTT: lagre scoren slik at den kan brukes videre
    final_score(round(score * 100))
    game_finished(TRUE)
    
    showModal(
      modalDialog(
        title = "Spillet er ferdig!",
        paste0(
          "Du fikk ",
          round(score * 100),
          "% riktige svar."
        ),
        # NYTT: knapp for å avslutte og sende resultatet
        footer = tagList(
          actionButton("close_game", "Avslutt og oppdater nivå"),
          actionButton("game_more", "Jeg vil spille mer")
        ),
        easyClose = TRUE
      )
    )
  }
  
  chosen_questions <- reactiveVal(NULL)
  
  
  
  run_question <- function(user_level, user_lesson) {
    level_filename <- levels$filename[
      levels$level_id == user_level
    ]
    current_lessons <- read.csv(level_filename)
    
    head(questions)
    
    chosen_questions <<- questions[questions$lesson_id == user_lesson, ] #foreløpig tar vi alle spørsmål i lesson
    message(chosen_questions)
    chosen_game <- user_lesson
    current_question <- chosen_questions [1, ] #choose the first question of chosen game
    current_game_id(chosen_game)
    current_question_index(1)
    current_question_id(current_question$id)
    chosen_questions
    
  }
  
  
  
  
  
  
  
  #under is the start of sending a feedback changing the level over time. But not done yet
  # context <- list(
  #   user_id = reactive("user2"),
  #   level = reactive(50)
  # )
  
  #results of the question
  question_results <- lapply(
    question_metadata$id,
    function(question_id) {
      question_modules[[question_id]]$server(
        id = question_id #,
        # context = context
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
  
  
  
  # #button: next question, and used later for percentage right
  # observeEvent(input$next_question, {
  #   result <- selected_result()
  #   old_results <- game_results()
  #   game_results(
  #     c(
  #       old_results,
  #       identical(result$status, "correct")
  #     )
  #   )
  #   # questions <- read.csv(
  #   #   "questions.csv",
  #   #   stringsAsFactors = FALSE
  #   # )
  #   # chosen_questions <- questions[
  #   #   questions$game_id == current_game_id(),
  #   # ]
  #   next_index <- current_question_index() + 1 #find the next row
  #   #message(chosen_questions)
  #   #is there anymore questions?
  #   if(next_index <= nrow(chosen_questions)) {
  #     current_question_index(next_index)
  #     current_question_id(
  #       chosen_questions[next_index, 1]
  #     ) 
  #    # message(chosen_questions)
  #   } else { #if not: they are done, so they show the window given earlier
  #     finish_game()
  #   }
  #   return()
  # }
  # )
  # 
  # 
  
  
  # NYTT: knapp som vises i sidebar når spillet er ferdig
  output$finish_button <- renderUI({
    if (game_finished()) {
      actionButton(
        "close_game",
        "Avslutt og oppdater nivå"
      )
    }
  })
  
  
  
  
  # NYTT: håndter klikk på avslutningsknappen
  observeEvent(input$close_game, {
    # # Oppdater brukerens nivå basert på scoren
    # new_level <- update_user_level(
    #   username = input$Username,
    #   score = final_score()
    # )
    # 
    # # Gi beskjed til brukeren
    # showNotification(
    #   paste0(
    #     "Nivået ditt er oppdatert til ",
    #     new_level
    #   ),
    #   type = "message"
    # )
    
    # Nullstill tilstanden for et nytt spill
    current_question_id(NULL)
    current_game_id(NULL)
    current_question_index(NULL)
    game_results(c())
    game_finished(FALSE)
    final_score(NULL)
  })
  
  
  
  
  observeEvent(input$game_more, {
    #her skal den starte neste lession
    message("game more")
    # oppdatere bruker, kjøre neste leksjon
    # lag funksjon som er run lesson med parameter
    # Nullstill tilstanden for et nytt spill
    # current_question_id(NULL)
    # current_game_id(NULL)
    # current_question_index(NULL)
    # game_results(c())
    # game_finished(FALSE)
    # final_score(NULL)
  })
  
  
  
  
  
  
  # ----------------------------------------------------------------------
  # Shiny-integrasjon (eksempel — kjøres ikke her)
  # ----------------------------------------------------------------------
  if (FALSE) {
    
    # i server():
    #
    # current_user <- reactiveVal(NULL)
    #
    # # innlogging
    # observeEvent(input$submit, {
    #   user <- load_user(input$Username)
    #   if (is.null(user)) user <- new_user(input$Username)
    #   current_user(user)
    # })
    #
    # # praktiske reactive-utledninger
    # user_level <- reactive({
    #   user <- current_user()
    #   if (is.null(user)) NULL else user$level
    # })
    #
    # # hvert spørsmål som besvares lagres — brukeren kan stoppe når hen vil
    # observeEvent(input$question_answered, {
    #   user <- current_user()
    #   user <- complete_question(user, current_question_id())
    #   current_user(user)
    # })
    #
    # # GJENOPPTA ved innlogging: fortsett der brukeren slapp
    # observeEvent(input$submit, {
    #   user <- current_user()
    #   if (!is.null(user) && !is.na(user$level)) {
    #     rp <- resume_point(user, app_content)
    #     if (!rp$level_done) {
    #       current_lesson_id(rp$lesson_id)
    #       current_question_index(rp$question_index)
    #       # vis leksjonen / spørsmålet rp
    #     } else {
    #       # alle leksjonene tatt → vis nivåprøven
    #     }
    #   }
    # })
    #
    # # leksjon fullført: oppdater objektet OG reactiveVal-en
    # observeEvent(input$lesson_finished, {
    #   user <- current_user()
    #   user <- complete_lesson(user, current_lesson_id())
    #   current_user(user)
    # })
    #
    # # nivåprøve bestått:
    # user <- current_user()
    # user <- advance_level(user)
    # current_user(user)
  }
  
}












# NYTT: funksjon som oppdaterer brukerens nivå i Google Sheets
# update_user_level <- function(username, score) {
#   url <- "https://docs.google.com/spreadsheets/d/1W4sAJIYOhlGTOQnFLhTKImXWy4OV8USSX57P4dm-t_A/edit?gid=0#gid=0"
#   
#   users <- read_sheet(url, sheet = "Ark 1")
#   current_user <- users[users$username == username, ]
#   
#   if (nrow(current_user) > 0) {
#     # Nytt nivå = gammelt nivå justert mot scoren
#     # Her kan du velge egen formel
#     old_level <- current_user$level
#     new_level <- round((old_level + score) / 2)
#     
#     # Oppdater i Google Sheets
#     users$level[users$username == username] <- new_level
#     sheet_write(users, ss = url, sheet = "Ark 1")
#     
#     return(new_level)
#   } else {
#     return(50)  # Fallback
#   }
# }



#Samle statistikk for å se om den virker. Se på resultatene fra de som bruker over tid. Hente klokka når ting starter og slutter, lagre data om hvordan de bruker det.
#R kan lage csv over bruken. Ledd det inn i app.R. Tid når bruker starter og stopper, hvor mange ganger bruker er inne. Måle svar, hvor bra bruker skårer, og hvordan dette utvikler seg over tid.
#Lage fancy resultatplot over hvordan appen virker for ulike type brukere. ->utifra hvor ofte noen er inne ofte.
#Pappas umiddelbare tanker spørsmålene kan kategoriseres i flere typer- prøve å skille ut om noen spørsmål lærer man av fort og noen ikke. Om spørsmål om histogrammer læres raskere enn andre.
#Mens spørsmål som er dårligere formulert, vanskeligere å lære av dem.
#skal det være passord? Det er ikke så vanskelig å få til.

#fint å ha: hvor ofte inne, hvor lang tid de bruker per gang, endre .csv til google for at det skal fungere med shiny-appen

#gjøremål: skrives oppgave, vurdere vanskelighetsgrader og sette sammen spørsmålspakker - som har nivå,

#Adrian sine tanker
#sette spørsmål om subjektiv vurdering av spørsmålene. "hvor vanskelig synes du spørsmålene var?" Det kan gi bra vurdering til videre.
#ved tre spørsmål per spill er en feil 33%. per nå er det jo prosenter -  hard straff ved en feil... annen utregning?
#rett mer belønning, men feil svar straffer mindre?

#åpne nivåer? gi dem valget selv? Vil du høyere dersom du får alle rette eller maks en feil? Gir mer opplevd autonomi over egen utvikling. mer konkurranse? "ansvar for egen læring".

#estimator er testobservator


shinyApp(ui, server)