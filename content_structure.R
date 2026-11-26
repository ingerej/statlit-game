# Global innholdsstruktur: levels -> lessons -> questions
#
# Bruk: source("content_structure.R") ETT sted — i global.R eller helt
# i toppen av app.R, ALDRI inne i server(). Innholdet lastes da én
# gang når appen starter, og alle økter deler samme (read-only) struktur.
# All per-bruker-tilstand (hvor langt noen har kommet) skal ligge i
# reactiveVal inne i server — ikke her.
#
# Fire filer styrer innholdet:
#
#   levels.csv:    level_id, name, filename
#                  filename peker på nivåets egen fil (f.eks. level_lett.csv)
#
#   level_lett.csv level_middels.csv, level_vanskelig.csv (osv.):
#                  lessons — én rad per leksjon, i rekkefølgen
#                  leksjonene skal kjøres på dette nivået.
#                  Ekstra kolonner blir automatisk felt på nivået
#                  ("ulik informasjon per nivå").
#                  Nye level_*.csv-filer i mappa legges automatisk til
#                  som nye nivåer (se add_discovered_levels).
#
#   lessons.csv:   lesson_id, lesson_name
#
#   questions.csv: id, lesson_id, filename
#                  global spørsmålsbank — alle ekstra kolonner blir
#                  felt på spørsmålet
#
# Ved oppstart valideres alle filene: manglende kolonner, tomme eller
# duplikate id-er, ødelagte referanser og semikolon-separerte filer
# gir tydelige norske feilmeldinger i stedet for kryptiske krasjer.
# ----------------------------------------------------------------------

# Leser en innholds-CSV. Norsk Excel lagrer ofte semikolon-separerte
# filer — hvis komma-lesing gir bare én kolonne, prøves semikolon.
read_content_csv <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (ncol(df) == 1 && any(grepl(";", names(df)))) {
    df <- utils::read.csv2(path, stringsAsFactors = FALSE, check.names = FALSE)
  }
  df
}

# Sjekk at en dataramme har de forventede kolonnene — si tydelig fra hvis ikke
stop_if_missing_cols <- function(df, cols, file_label) {
  mangler <- setdiff(cols, names(df))
  if (length(mangler) > 0) {
    stop(
      file_label, ": mangler kolonne(ne) '",
      paste(mangler, collapse = "', '"), "'.\n",
      "Kolonner funnet: '", paste(names(df), collapse = "', '"), "'.\n",
      "Sjekk kolonnenavnene — og at filen er lagret med komma som separator ",
      "(norsk Excel bruker ofte semikolon)."
    )
  }
}

# Sjekk at id-kolonnen er gyldig: ingen tomme eller duplikate id-er
stop_if_bad_ids <- function(df, col, file_label) {
  ids   <- as.character(df[[col]])
  tomme <- which(is.na(ids) | trimws(ids) == "")
  if (length(tomme) > 0) {
    stop(file_label, ": tom eller manglende id i kolonnen '", col,
         "' på rad ", paste(tomme, collapse = ", "), ".")
  }
  dup <- unique(ids[duplicated(ids)])
  if (length(dup) > 0) {
    stop(file_label, ": duplikate id-er i kolonnen '", col, "': '",
         paste(dup, collapse = "', '"), "'.")
  }
}

# ----------------------------------------------------------------------
# Automatisk oppdagelse av nivåer
# ----------------------------------------------------------------------
# Visningsnavn for et nivå: kolonnen 'name' i levels.csv hvis den er
# fylt ut, ellers hentes navnet fra filnavnet:
#   level_lett.csv -> "Lett",  level_ekspert.csv -> "Ekspert"
level_label <- function(name, filename) {
  lab <- if (length(name) == 1 && !is.na(name) && nzchar(trimws(name))) {
    trimws(name)
  } else {
    sub("^level_", "", tools::file_path_sans_ext(basename(filename)))
  }
  paste0(toupper(substring(lab, 1, 1)), substring(lab, 2))
}

# Finner level_*.csv-filer i mappa som IKKE står i levels.csv, og legger
# dem til som nye nivåer (etter de eksisterende, alfabetisk). Da holder
# det å legge en ny fil, f.eks. level_ekspert.csv, ved siden av de andre
# — uten å røre levels.csv. Skru av med auto_discover = FALSE.
add_discovered_levels <- function(levels_df, dir = ".",
                                  pattern = "^level_.*\\.csv$") {
  files <- sort(list.files(dir, pattern = pattern))
  new_files <- setdiff(files, as.character(levels_df$filename))
  if (length(new_files) == 0) return(levels_df)
  
  levels_df$level_id <- as.character(levels_df$level_id)
  ids_num <- suppressWarnings(as.numeric(levels_df$level_id))
  start <- if (all(is.na(ids_num))) 0 else max(ids_num, na.rm = TRUE)
  
  new_df <- levels_df[rep(NA_integer_, length(new_files)), , drop = FALSE]
  new_df$level_id <- as.character(start + seq_along(new_files))
  new_df$filename <- new_files
  new_df$name <- vapply(new_files, function(f) level_label(NA, f), character(1))
  
  out <- rbind(levels_df, new_df)
  rownames(out) <- NULL
  message("Nye nivåfiler funnet og lagt til: ",
          paste(new_files, collapse = ", "))
  out
}

load_app_content <- function(levels_file   = "levels.csv",
                             lessons_file  = "lessons.csv",
                             questions_file = "questions.csv",
                             auto_discover  = TRUE) {
  
  levels_df    <- read_content_csv(levels_file)
  lessons_df   <- read_content_csv(lessons_file)
  questions_df <- read_content_csv(questions_file)
  
  # --- validering: gi tydelig feil FØR koden krasjer kryptisk ---------
  stop_if_missing_cols(levels_df,    c("level_id", "name", "filename"), levels_file)
  if (auto_discover) levels_df <- add_discovered_levels(levels_df)
  stop_if_missing_cols(lessons_df,   c("lesson_id", "lesson_name"),     lessons_file)
  stop_if_missing_cols(questions_df, c("id", "lesson_id", "filename"), questions_file)
  
  stop_if_bad_ids(levels_df,    "level_id",  levels_file)
  stop_if_bad_ids(lessons_df,   "lesson_id", lessons_file)
  stop_if_bad_ids(questions_df, "id",        questions_file)
  
  # referanser: spørsmål må peke på leksjoner som finnes
  dode_lessons <- setdiff(as.character(questions_df$lesson_id),
                          as.character(lessons_df$lesson_id))
  if (length(dode_lessons) > 0) {
    stop(questions_file, ": spørsmål peker på leksjon(er) som ikke finnes i ",
         lessons_file, ": '", paste(dode_lessons, collapse = "', '"), "'.")
  }
  
  content <- list(
    levels              = list(),  # "1" -> level-objekt med lessons
    questions           = list(),  # question_id -> spørsmålsdata (flat)
    questions_by_lesson = list(),  # lesson_id -> vektor av question_id-er
    levels_df           = levels_df,    # rådata beholdes for filtering
    lessons_df          = lessons_df,
    questions_df        = questions_df
  )
  
  # --- flat oppslagstabell: alle spørsmål etter id -------------------
  # Spørsmålsdata lagres ETT sted; leksjonene refererer bare til id-er.
  for (i in seq_len(nrow(questions_df))) {
    content$questions[[as.character(questions_df$id[i])]] <-
      as.list(questions_df[i, ])
  }
  
  # --- indeks: hvilke spørsmål hører til hvilken leksjon -------------
  content$questions_by_lesson <- split(
    as.character(questions_df$id),
    as.character(questions_df$lesson_id)
  )
  
  # --- bygg nivåene: les hver nivåfil, legg til leksjonene i orden ---
  for (i in seq_len(nrow(levels_df))) {
    lvl_id <- as.character(levels_df$level_id[i])
    
    lvl <- as.list(levels_df[i, ])      # alle kolonner blir felt
    lvl$level_id <- lvl_id
    lvl$label    <- level_label(levels_df$name[i], levels_df$filename[i])
    lvl$lessons  <- list()
    
    # les nivåets egen fil (f.eks. level_lett.csv)
    level_file <- levels_df$filename[i]
    level_lessons_df <- tryCatch(
      read_content_csv(level_file),
      error = function(e) stop(
        "Finner ikke eller kan ikke lese nivåfila '", level_file,
        "' (referert fra ", levels_file, ").\n",
        "Opprinnelig feil: ", conditionMessage(e)
      )
    )
    
    # kolonnen med leksjons-id-er heter 'lessons' ('lesson_id' godtas også)
    if ("lessons" %in% names(level_lessons_df)) {
      lvl_lesson_ids <- as.character(level_lessons_df$lessons)
    } else if ("lesson_id" %in% names(level_lessons_df)) {
      lvl_lesson_ids <- as.character(level_lessons_df$lesson_id)
    } else {
      stop(level_file, ": mangler kolonnen 'lessons' (én rad per leksjon).")
    }
    
    for (j in seq_along(lvl_lesson_ids)) {
      les_id <- lvl_lesson_ids[j]
      
      # hent leksjonsnavnet fra lessons.csv
      les_row <- lessons_df[as.character(lessons_df$lesson_id) == les_id, ,
                            drop = FALSE]
      if (nrow(les_row) == 0) {
        stop(level_file, ": leksjon '", les_id, "' finnes ikke i ",
             lessons_file, ".")
      }
      
      qids <- content$questions_by_lesson[[les_id]]
      lvl$lessons[[les_id]] <- list(
        lesson_id    = les_id,
        lesson_name  = les_row$lesson_name[1],
        order        = j,
        question_ids = if (is.null(qids)) character(0) else qids
      )
    }
    
    content$levels[[lvl_id]] <- lvl
  }
  
  content
}

# ----------------------------------------------------------------------
# Hjelpefunksjoner — enkle aksessorer, i samme stil overalt
# ----------------------------------------------------------------------

# ett helt nivå (info + leksjoner)
get_level <- function(content, level_id) {
  content$levels[[as.character(level_id)]]
}

# alle leksjoner på et nivå (navngitt liste, i rekkefølge fra nivåfila)
get_lessons <- function(content, level_id) {
  get_level(content, level_id)$lessons
}

# én leksjon
get_lesson <- function(content, level_id, lesson_id) {
  get_level(content, level_id)$lessons[[as.character(lesson_id)]]
}

# alle spørsmåls-id-er i én leksjon (nivå ikke nødvendig her)
get_questions <- function(content, lesson_id) {
  ids <- content$questions_by_lesson[[as.character(lesson_id)]]
  if (is.null(ids)) character(0) else ids
}

# alle spørsmåls-id-er på et helt nivå
get_level_questions <- function(content, level_id) {
  unlist(lapply(get_lessons(content, level_id),
                function(les) les$question_ids),
         use.names = FALSE)
}

# data om ett spørsmål
get_question <- function(content, question_id) {
  content$questions[[as.character(question_id)]]
}

# valg til nivåvelgeren: navngitt vektor, c("Lett" = "1", "Middels" = "2", ...)
# Bygges fra innholdet, så nye nivåer dukker opp av seg selv.
level_choices <- function(content) {
  ids <- names(content$levels)
  stats::setNames(ids, vapply(ids, function(id) content$levels[[id]]$label,
                              character(1)))
}

# id-en til nivået etter dette (NULL hvis dette er det siste)
next_level_id <- function(content, level_id) {
  ids <- names(content$levels)
  pos <- match(as.character(level_id), ids)
  if (is.na(pos) || pos == length(ids)) NULL else ids[pos + 1]
}

# neste leksjon på samme nivå (NULL hvis dette var den siste)
next_lesson <- function(content, level_id, lesson_id) {
  lessons <- get_lessons(content, level_id)
  ids  <- names(lessons)
  pos  <- match(as.character(lesson_id), ids)
  if (is.na(pos) || pos == length(ids)) NULL else lessons[[ids[pos + 1]]]
}

# posisjon i rekkefølgen, til f.eks. "Økt 2 av 6" i UI-et
lesson_position <- function(content, level_id, lesson_id) {
  ids <- names(get_lessons(content, level_id))
  pos <- match(as.character(lesson_id), ids)
  if (is.na(pos)) NULL else list(pos = pos, total = length(ids))
}

# ----------------------------------------------------------------------
# Kjør ved oppstart: bygger den globale strukturen
# ----------------------------------------------------------------------
app_content <- load_app_content()


# ----------------------------------------------------------------------
# EKSEMPLER PÅ AKSESS (kjøres ikke — bare dokumentasjon)
# ----------------------------------------------------------------------
if (FALSE) {
  
  # --- nivåer ---------------------------------------------------------
  names(app_content$levels)                  # "1" "2" "3"
  app_content$levels[["1"]]$name             # "lett"
  app_content$levels[["1"]]$filename         # "level_lett.csv"
  
  # --- leksjoner på et nivå -------------------------------------------
  lessons <- app_content$levels[["1"]]$lessons
  length(lessons)                            # antall leksjoner på nivå 1
  names(lessons)                             # lesson_id-er i rekkefølge
  app_content$levels[["1"]]$lessons[["1"]]$lesson_name
  app_content$levels[["1"]]$lessons[["1"]]$order
  app_content$levels[["1"]]$lessons[["1"]]$question_ids
  
  # --- spørsmål ---------------------------------------------------------
  app_content$questions[["question_006"]]$filename
  app_content$questions[["question_006"]]$lesson_id
  
  # --- med hjelpefunksjonene --------------------------------------------
  get_level(app_content, 1)
  get_lessons(app_content, 1)
  get_lesson(app_content, 1, 1)
  get_questions(app_content, "1")                     # leksjon 1s spørsmål
  get_question(app_content, "question_006")$filename
  get_level_questions(app_content, 1)
  next_lesson(app_content, 1, 1)                       # NULL hvis siste
  lesson_position(app_content, 1, 1)                   # list(pos = 1, total = 6)
  level_choices(app_content)                           # c(Lett = "1", ...)
  next_level_id(app_content, 1)                        # "2", NULL hvis siste
  
  # --- filtrere på tvers (via de rå dataene) ----------------------------
  subset(app_content$questions_df, lesson_id == 1)
  subset(app_content$lessons_df,   lesson_id %in% c(1, 2))
  
  # --- loope gjennom alt innhold -----------------------------------------
  for (lvl_id in names(app_content$levels)) {
    for (lesson_id in names(app_content$levels[[lvl_id]]$lessons)) {
      message("Nivå ", lvl_id, " / leksjon ", lesson_id, ": ",
              length(get_questions(app_content, lesson_id)), " spørsmål")
    }
  }
  
  # --- i server: posisjon og tittel til UI-et ----------------------------
  # current_level  <- reactiveVal("1")
  # current_lesson <- reactiveVal("1")
  #
  # output$lesson_heading <- renderText({
  #   les <- get_lesson(app_content, current_level(), current_lesson())
  #   p   <- lesson_position(app_content, current_level(), current_lesson())
  #   paste0("Økt ", p$pos, " av ", p$total, " — ", les$lesson_name)
  # })
  #
  # observeEvent(input$next_lesson, {
  #   nxt <- next_lesson(app_content, current_level(), current_lesson())
  #   if (!is.null(nxt)) current_lesson(nxt$lesson_id)
  # })
}