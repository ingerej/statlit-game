# Brukerobjekt + lagring i Google Sheet
#
# source("user_state.R") sammen med content_structure.R (globalt).
#
# To lag, to roller:
#   - app_content (content_structure.R): statisk innhold, likt for alle
#   - current_user (denne fila): innlogget brukers tilstand, lagret i
#     Google Sheet slik at fremgangen overlever at man lukker appen
#
# Brukernavnet er brukerens eget valg (fritt navn). Brukerarket er flat
# (én rad per bruker, én kolonne per felt) så det er lett å lese og
# redigere for hånd. Lister (fullførte leksjoner) lagres som
# kommaseparert tekst i én kolonne og gjøres om til vektor vedlasting.
#
# Kolonner denne fila styrer: username, level, lessons_done,
# levels_done, last_question_done, last_seen. ANDRE kolonner i arket —
# som levels_old — røres ikke av koden. De kan slettes manuelt i arket
# når strukturen er ferdig; koden klarer seg uansett.
#
# Brukerobjektet ser slik ut:
#   list(
#     username           = "testbruker",
#     level              = "1",             # NA til brukeren har valgt
#     lessons_done       = c("3"),          # fullførte leksjoner på NÅVÆRENDE nivå
#     levels_done        = character(0),    # fullførte nivåer
#     last_question_done = "question_002",  # NA = ikke startet på leksjonen
#     last_seen          = "2026-11-12 09:41"
#   )
#
# last_question_done brukes til at brukeren kan stoppe når hen vil og
# fortsette der hen slapp — se resume_point() lenger ned.
# ----------------------------------------------------------------------

# Adresse til brukerarket (samme ark som app.R brukte)
user_sheet_url <- Sys.getenv(
  "USER_SHEET_URL",
  unset = "https://docs.google.com/spreadsheets/d/1W4sAJIYOhlGTOQnFLhTKImXWy4OV8USSX57P4dm-t_A/edit?gid=0#gid=0"
)
user_sheet_tab <- "Ark 1"

# kolonnenavn i arket — endres her hvis arket endres
USER_COL_USERNAME <- "username"
USER_COL_LEVEL    <- "level"
USER_COL_LESSONS  <- "lessons_done"
USER_COL_LEVELS   <- "levels_done"
USER_COL_LASTQ    <- "last_question_done"
USER_COL_SEEN     <- "last_seen"

now_stamp <- function() format(Sys.time(), "%Y-%m-%d %H:%M")

# kommaseparert tekst <-> vektor av id-er
parse_ids <- function(x) {
  if (is.na(x)) return(character(0))
  x <- trimws(strsplit(as.character(x), ",")[[1]])
  x <- x[x != ""]
  x
}
paste_ids <- function(ids) paste(as.character(ids), collapse = ",")

# ----------------------------------------------------------------------
# Opprette, lese og lagre brukere
# ----------------------------------------------------------------------

# ny, tom bruker (level NA til brukeren velger)
new_user <- function(username) {
  list(
    username           = username,
    level              = NA_character_,
    lessons_done       = character(0),
    levels_done        = character(0),
    last_question_done = NA_character_,
    last_seen          = now_stamp()
  )
}

# les én bruker fra arket — NULL hvis navnet ikke finnes
load_user <- function(username) {
  users_df <- googlesheets4::read_sheet(user_sheet_url, sheet = user_sheet_tab)
  
  row <- users_df[users_df[[USER_COL_USERNAME]] == username, , drop = FALSE]
  if (nrow(row) == 0) return(NULL)
  
  # sørg for at kolonnene finnes (eldste versjoner av arket mangler dem)
  get_col <- function(col) {
    if (col %in% names(row)) row[[col]][1] else NA
  }
  
  user <- list(
    username           = username,
    level              = as.character(get_col(USER_COL_LEVEL)),
    lessons_done       = parse_ids(get_col(USER_COL_LESSONS)),
    levels_done        = parse_ids(get_col(USER_COL_LEVELS)),
    last_question_done = as.character(get_col(USER_COL_LASTQ)),
    last_seen          = as.character(get_col(USER_COL_SEEN))
  )
  
  # tomme verdier skal være NA, ikke "NA"-streng eller NULL
  if (length(user$level) == 0) user$level <- NA_character_
  if (length(user$last_question_done) == 0) {
    user$last_question_done <- NA_character_
  }
  
  # gamle nivå-verdier (f.eks. 50 fra 0-100-skalaen) gjelder ikke lenger:
  # behold levels_old i arket, men behandla nivået som uvalgt
  if (is.na(user$level) || !(user$level %in% names(app_content$levels))) {
    user$level <- NA_character_
  }
  
  user
}

# lagre brukerobjektet tilbake til arket.
# Hele arket skrives, så ukjente kolonner (levels_old) bevares som de er.
save_user <- function(user) {
  users_df <- googlesheets4::read_sheet(user_sheet_url, sheet = user_sheet_tab)
  if (!(USER_COL_USERNAME %in% names(users_df))) {
    stop("Brukerarket mangler kolonnen '", USER_COL_USERNAME, "'.")
  }
  
  # sørg for at alle kolonnene vi bruker finnes i arket
  for (col in c(USER_COL_LEVEL, USER_COL_LESSONS, USER_COL_LEVELS,
                USER_COL_LASTQ, USER_COL_SEEN)) {
    if (!(col %in% names(users_df))) users_df[[col]] <- NA
  }
  
  row_i <- which(users_df[[USER_COL_USERNAME]] == user$username)
  if (length(row_i) > 1) {
    stop("Flere rader har samme brukernavn. Ingen endringer ble lagret.")
  }
  
  if (length(row_i) == 0) {
    # Lag først en tom rad med ALLE arkets kolonner, også levels_old.
    empty_row <- users_df[NA_integer_, , drop = FALSE]
    users_df <- rbind(users_df, empty_row)
    row_i <- nrow(users_df)
  }
  users_df[[USER_COL_USERNAME]][row_i] <- user$username
  users_df[[USER_COL_LEVEL]][row_i]    <- user$level
  users_df[[USER_COL_LESSONS]][row_i]  <- paste_ids(user$lessons_done)
  users_df[[USER_COL_LEVELS]][row_i]   <- paste_ids(user$levels_done)
  users_df[[USER_COL_LASTQ]][row_i]    <- user$last_question_done
  users_df[[USER_COL_SEEN]][row_i]     <- user$last_seen
  
  googlesheets4::sheet_write(users_df, ss = user_sheet_url,
                             sheet = user_sheet_tab) #her må koden endres til å bare være individet- range_write
  invisible(user)
}

# ----------------------------------------------------------------------
# Endringer på brukeren — hver funksjon oppdaterer, lagrer og returnerer
# det oppdaterte objektet (lagre det med current_user(objekt) i server)
# ----------------------------------------------------------------------

# velge nivå (etter nivåvalg / plasseringssjekk)
set_level <- function(user, level) {
  user$level              <- as.character(level)
  user$lessons_done       <- character(0)   # nytt nivå = nullstilling
  user$last_question_done <- NA_character_
  user$last_seen          <- now_stamp()
  save_user(user)
  user
}

# merke ett spørsmål som besvart — lagres for hvert spørsmål, slik at
# brukeren kan stoppe når hen vil og fortsette på samme sted
complete_question <- function(user, question_id) {
  user$last_question_done <- as.character(question_id)
  user$last_seen          <- now_stamp()
  save_user(user)
  user
}

# merke en leksjon som fullført (pekeren nullstilles — leksjonen er ferdig)
complete_lesson <- function(user, lesson_id) {
  if (!(as.character(lesson_id) %in% user$lessons_done)) {
    user$lessons_done <- c(user$lessons_done, as.character(lesson_id))
  }
  user$last_question_done <- NA_character_
  user$last_seen          <- now_stamp()
  save_user(user)
  user
}

# fullføre et nivå og gå videre til neste
advance_level <- function(user, content = app_content) {
  nxt <- next_level_id(content, user$level)
  if (is.null(nxt)) stop("Det finnes ikke flere nivåer.")
  user$levels_done        <- unique(c(user$levels_done, user$level))
  user$level              <- nxt
  user$lessons_done       <- character(0)
  user$last_question_done <- NA_character_
  user$last_seen          <- now_stamp()
  save_user(user)
  user
}

# ----------------------------------------------------------------------
# Gjenopptakelse: hvor skal denne brukeren fortsette?
# ----------------------------------------------------------------------
# Bruker last_question_done til å finne nøyaktig posisjon. Logikken:
#   1. Finn første leksjon på brukerens nivå som ikke er fullført
#   2. Hvis last_question_done ligger i den leksjonens spørsmål:
#      fortsett på spørsmålet ETTER det (var det siste, er leksjonen i
#      praksis ferdig)
#   3. Ellers: start på leksjonens første spørsmål
#
# Returnerer en liste:
#   list(lesson_id = "3", question_index = 2, level_done = FALSE)
# level_done = TRUE betyr at alle leksjonene på nivået er tatt —
# neste steg er nivåprøven (lesson_id og question_index er da NA/NULL).
resume_point <- function(user, content) {
  lessons <- get_lessons(content, user$level)
  
  for (lesson_id in names(lessons)) {
    
    # Hopp over fullførte leksjoner
    if (lesson_id %in% user$lessons_done) next
    
    qids <- lessons[[lesson_id]]$question_ids
    
    # Hopp over leksjoner uten spørsmål
    if (length(qids) == 0) next
    
    if (!is.na(user$last_question_done) &&
        user$last_question_done %in% qids) {
      
      idx <- match(user$last_question_done, qids)
      
      # Siste spørsmål er besvart:
      # fortsett løkken til neste leksjon
      if (idx == length(qids)) next
      
      # Flere spørsmål i samme leksjon
      return(list(
        lesson_id      = lesson_id,
        question_index = idx + 1L,
        level_done     = FALSE
      ))
    }
    
    # Ingen spørsmål registrert som fullført i denne leksjonen:
    # start på første spørsmål
    return(list(
      lesson_id      = lesson_id,
      question_index = 1L,
      level_done     = FALSE
    ))
  }
  
  # Ingen flere leksjoner på nivået
  list(
    lesson_id      = NA_character_,
    question_index = NULL,
    level_done     = TRUE
  )
}
