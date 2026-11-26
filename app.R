#install.packages("shiny")
library(shiny)

library(bslib) #gives card as a setup

#install.packages("googlesheet4")
library(googlesheets4)

# NYTT: automatisk innlogging mot Google Sheets med servicekonto
# (ingen Gmail-godkjenning). Se google_auth.R og docs/google-oppsett.md.
source("google_auth.R")
init_google_auth()

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
    /* NYTT: grå «ikke aktiv»-knapp (Start mens man spiller, Avslutt) */
    .btn-idle, .btn-idle:disabled {
      background-color: #E3E3E3; border-color: #E3E3E3; color: #666;
    }
    .btn-idle:hover:not(:disabled) { background-color: #D4D4D4; color: #333; }
  ")),
  
  # NYTT: JavaScript som låser brukernavnfeltet og bytter farge på
  # Start-knappen etter hva som skjer i appen (sendes fra server).
  tags$script(HTML("
    Shiny.addCustomMessageHandler('ui_state', function(m) {
      var start_active = (m.state === 'out' || m.state === 'waiting');
      $('#Username').prop('disabled', m.state !== 'out');
      $('#submit').prop('disabled', !start_active)
                  .toggleClass('btn-primary', start_active)
                  .toggleClass('btn-idle', !start_active);
      $('#end_session').prop('disabled', m.state === 'out');
    });
  ")),
  
  tags$h1("Læringsspill", align = "center"),
  hr(),
  
  layout_sidebar(
    sidebar = sidebar(
      h4("Kontrollpanel"),
      
      textInput(label = "Username:", 
                inputId = "Username",
                placeholder = "Username"), 
      
      actionButton("submit", "Start", icon = icon("play"),
                   class = "btn-primary"),
      
      # NYTT: viser hvilket nivå brukeren er på
      uiOutput("level_info"),
      
      textOutput("question_status"),
      
      # NYTT: hovedknappen. Viser «Neste spørsmål», og bytter til
      # «Gå til nivå: ...» når alt på nivået er fullført.
      uiOutput("main_action_ui"),
      
      # NYTT: avslutningsknapp som vises når spillet er ferdig
      uiOutput("finish_button"),
      
      # NYTT: grå knapp for å avslutte når som helst
      actionButton("end_session", "Avslutt", icon = icon("stop"),
                   class = "btn-idle w-100"),
      
      # NYTT: lite synlig lenke nederst — alltid tilgjengelig, men ikke
      # fremtredende
      hr(),
      div(
        class = "small",
        actionLink("change_level", "Endre nivå", icon = icon("sliders"),
                   class = "text-muted")
      )
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
  
  
  #### Nivåvalg: hjelpere ----
  
  # Viser der brukeren skal fortsette (samme regel som ved innlogging).
  # Ferdig nivå: spørsmålet tømmes, og main_action_ui viser «Gå til nivå»-knappen.
  start_from_resume_point <- function(user) {
    rp <- resume_point(user, app_content)
    if (rp$level_done) {
      current_lesson_id(NULL)
      current_question_index(NULL)
      current_question_id(NULL)
      return(invisible(rp))
    }
    lesson <- get_lesson(app_content, user$level, rp$lesson_id)
    current_lesson_id(rp$lesson_id)
    current_question_index(rp$question_index)
    current_question_id(lesson$question_ids[rp$question_index])
    invisible(rp)
  }
  
  # Dialogen for nivåvalg. Valgene bygges fra app_content, så nye
  # level_*.csv-filer dukker opp automatisk. first = TRUE: ny bruker,
  # dialogen kan ikke lukkes uten at et nivå er valgt.
  level_modal <- function(current_level = NA_character_, first = FALSE) {
    choices  <- level_choices(app_content)
    selected <- if (!is.na(current_level) && current_level %in% choices) {
      current_level
    } else {
      choices[[1]]
    }
    modalDialog(
      title = if (first) "Velg nivå" else "Endre nivå",
      if (first) {
        p("Velg nivået du vil starte på. Du kan endre det senere hvis det blir for lett eller for vanskelig.")
      } else {
        div(
          class = "alert alert-warning",
          strong("Obs: "),
          "Hvis du bytter nivå, nullstilles fremgangen din på nåværende nivå ",
          "(fullførte leksjoner og hvor langt du har kommet). ",
          "Du starter på begynnelsen av det nye nivået."
        )
      },
      radioButtons("level_choice", NULL, choices = choices, selected = selected),
      footer = tagList(
        if (!first) modalButton("Avbryt"),
        actionButton(
          "level_confirm",
          if (first) "Start på dette nivået" else "Bytt nivå",
          class = "btn-primary fw-bold"
        )
      ),
      easyClose = !first
    )
  }
  
  
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
    
    # Ny bruker (eller ugyldig lagret nivå) må velge nivå først
    if (is.na(user$level)) {
      showModal(level_modal(first = TRUE))
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
  
  
  #### Endre nivå (alltid tilgjengelig) ----
  observeEvent(input$change_level, {
    user <- current_user()
    if (is.null(user)) {
      showNotification("Skriv inn brukernavn og trykk Start først.",
                       type = "warning")
      return()
    }
    showModal(level_modal(user$level, first = is.na(user$level)))
  })
  
  observeEvent(input$level_confirm, {
    user   <- req(current_user())
    chosen <- req(input$level_choice)
    
    if (!(chosen %in% names(app_content$levels))) return()
    if (identical(user$level, chosen)) {   # samme nivå: ingenting å endre
      removeModal()
      return()
    }
    
    saved <- tryCatch(
      set_level(user, chosen),
      error = function(e) {
        message("Kunne ikke lagre nivå: ", conditionMessage(e))
        showNotification("Nivået ble ikke lagret. Prøv igjen.", type = "error")
        NULL
      }
    )
    if (is.null(saved)) return()
    
    current_user(saved)
    removeModal()
    start_from_resume_point(saved)
  })
  
  #### Gå opp et nivå når alt er fullført ----
  # level_status er en reactiveVal som bare endres når innholdet faktisk
  # endres, så hovedknappen ikke tegnes på nytt etter hvert spørsmål.
  #   done  = alle leksjoner med spørsmål er fullført
  #   empty = nivået har ingen spørsmål i det hele tatt (ikke «fullført»)
  level_status <- reactiveVal(NULL)
  
  observe({
    user <- current_user()
    if (is.null(user) || is.na(user$level)) {
      level_status(NULL)
      return()
    }
    # Mens et spørsmål vises er nivået aldri «ferdig» — fremgangen
    # lagres nå rett etter at svaret er kontrollert, og da kan
    # resume_point() allerede peke forbi siste spørsmål.
    if (!is.null(current_question_id())) {
      level_status(list(
        empty   = FALSE,
        done    = FALSE,
        next_id = next_level_id(app_content, user$level)
      ))
      return()
    }
    empty <- length(get_level_questions(app_content, user$level)) == 0
    rp    <- resume_point(user, app_content)
    level_status(list(
      empty   = empty,
      done    = !empty && isTRUE(rp$level_done),
      next_id = next_level_id(app_content, user$level)
    ))
  })
  
  # NYTT: Start-knappen skifter tekst. Når en leksjon er ferdig og
  # brukeren venter på neste (ingen spørsmål vist, nivået ikke ferdig
  # eller tomt), står det «Start neste leksjon». Ellers «Start».
  observe({
    user <- current_user()
    st   <- level_status()
    waiting_for_next_lesson <-
      !is.null(user) && !is.na(user$level) &&
      is.null(current_question_id()) &&
      !is.null(st) && !isTRUE(st$done) && !isTRUE(st$empty)
    
    updateActionButton(
      session, "submit",
      label = if (waiting_for_next_lesson) "Start neste leksjon" else "Start",
      icon  = icon("play")
    )
  })
  
  #### Lagre fremgang når svaret er kontrollert ----
  # Før ble fremgangen først lagret ved «Neste spørsmål». Nå lagres den
  # så snart svaret er kontrollert (riktig eller galt), så den ikke går
  # tapt hvis brukeren avslutter før de går videre.
  saved_question <- reactiveVal(NULL)   # spørsmålet som allerede er lagret
  
  observeEvent(current_question_id(), {
    saved_question(NULL)                # nytt spørsmål: ikke lagret ennå
  }, ignoreNULL = FALSE)
  
  observeEvent(selected_result(), {
    user      <- req(current_user())
    qid       <- req(current_question_id())
    lesson_id <- req(current_lesson_id())
    result    <- selected_result()
    req(result$status %in% c("correct", "incorrect"))
    if (identical(saved_question(), qid)) return()   # allerede lagret
    
    qids    <- get_questions(app_content, lesson_id)
    is_last <- length(qids) > 0 && identical(qid, qids[length(qids)])
    
    saved <- tryCatch(
      {
        # Siste spørsmål i leksjonen: marker hele leksjonen som ferdig
        # (complete_lesson nullstiller også last_question_done).
        if (is_last) complete_lesson(user, lesson_id)
        else complete_question(user, qid)
      },
      error = function(e) {
        message("Kunne ikke lagre fremgang: ", conditionMessage(e))
        showNotification(
          "Fremgangen ble ikke lagret. Prøv å trykke «Neste spørsmål» igjen.",
          type = "error"
        )
        NULL
      }
    )
    if (!is.null(saved)) {
      current_user(saved)
      saved_question(qid)
    }
  }, ignoreInit = TRUE)
  
  # NYTT: viser hvilket nivå brukeren er på
  output$level_info <- renderUI({
    user <- current_user()
    if (is.null(user)) return(NULL)
    if (is.na(user$level)) return(div(class = "mb-2", "Nivå: ikke valgt"))
    div(class = "mb-2",
        "Nivå: ", strong(get_level(app_content, user$level)$label))
  })
  
  # NYTT: styrer hva som kan trykkes på, og låser brukernavnfeltet
  # når noen er logget inn (så man ikke bytter navn midt i en økt).
  #   out     = ingen innlogget: Start er aktiv, brukernavn kan skrives
  #   playing = spørsmål vises: Start er grå
  #   waiting = venter på neste leksjon: Start er aktiv igjen
  #   locked  = innlogget, men ingenting å starte (nivå ferdig/tomt/uten valg)
  observe({
    user <- current_user()
    st   <- level_status()
    state <- if (is.null(user)) {
      "out"
    } else if (!is.null(current_question_id())) {
      "playing"
    } else if (is.na(user$level) || is.null(st) ||
               isTRUE(st$done) || isTRUE(st$empty)) {
      "locked"
    } else {
      "waiting"
    }
    session$sendCustomMessage("ui_state", list(state = state))
  })
  
  # NYTT: avslutt når som helst. Fremgangen er allerede lagret etter
  # hvert besvarte spørsmål. Siden lastes på nytt, så alt nullstilles
  # (også svarene i spørsmålsmodulene) og neste bruker starter rent.
  observeEvent(input$end_session, {
    session$reload()
  })
  
  output$main_action_ui <- renderUI({
    st <- level_status()
    
    if (!is.null(st) && isTRUE(st$empty)) {
      return(p("Dette nivået har ingen spørsmål ennå. Bruk «Endre nivå» nederst for å velge et annet."))
    }
    if (!is.null(st) && isTRUE(st$done)) {
      if (is.null(st$next_id)) {
        return(p(strong("Du har fullført alle nivåene.")))
      }
      return(tagList(
        p("Du har fullført dette nivået."),
        actionButton(
          "level_up",
          paste0("Gå til nivå: ", get_level(app_content, st$next_id)$label),
          icon = icon("arrow-up"),
          class = "btn-primary fw-bold w-100"
        )
      ))
    }
    actionButton(
      "next_question",
      "Neste spørsmål",
      icon = icon("arrow-right"),
      class = "btn-primary fw-bold w-100"
    )
  })
  
  observeEvent(input$level_up, {
    user <- req(current_user())
    st   <- level_status()
    req(isTRUE(st$done), !is.null(st$next_id))   # bare når alt er fullført
    
    saved <- tryCatch(
      advance_level(user, app_content),
      error = function(e) {
        message("Kunne ikke gå til neste nivå: ", conditionMessage(e))
        showNotification("Nytt nivå ble ikke lagret. Prøv igjen.", type = "error")
        NULL
      }
    )
    if (is.null(saved)) return()
    
    current_user(saved)
    start_from_resume_point(saved)
  })
  
  
  
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
      # Lagre siste besvarte spørsmål — hvis det ikke allerede er lagret
      # rett etter at svaret ble kontrollert (se saved_question).
      if (!identical(saved_question(), question_id)) {
        user <- complete_question(user, question_id)
        current_user(user)
        saved_question(question_id)
      }
      
      # Finn neste posisjon med samme funksjon som ved innlogging
      rp <- resume_point(user, app_content)
      message("rp: ", rp)
      # Har brukeren fullført leksjonen?
      lesson_finished <- rp$level_done ||
        !identical(rp$lesson_id, previous_lesson_id)
      
      if (lesson_finished) {
        if (!(as.character(previous_lesson_id) %in% user$lessons_done)) {
          user <- complete_lesson(user, previous_lesson_id)
          current_user(user)
        }
        
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
        
        # RETTET: her stod det question$filename, men `question` er NULL
        # i server(), så file.path() ga tom sti og load_question() feilet.
        # Spørsmålsmodulene er allerede lastet ved oppstart
        # (question_modules), så fila trenger ikke lastes på nytt.
        message("Neste spørsmål: ", current_question_id())
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
  # (gammel kode, kommentert ut — beholdt uendret fra forrige versjon)
  
  
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
  })
  
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