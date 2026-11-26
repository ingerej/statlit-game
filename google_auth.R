# google_auth.R
# ----------------------------------------------------------------------
# Automatisk, ikke-interaktiv innlogging mot Google Sheets med en
# SERVICEKONTO (en robotbruker). Da trengs ingen Gmail-godkjenning ved
# oppstart, og appen har bare tilgang til det ene arket du deler med den.
#
# Bruk: source("google_auth.R") og kall init_google_auth() ÉN gang,
# globalt (global.R / øverst i app.R), FØR user_state.R brukes.
#
# Nøkkelen (JSON-fila) hentes fra, i denne rekkefølgen:
#   1. miljøvariabelen GS4_SERVICE_ACCOUNT_JSON  (hele JSON-innholdet)
#      -> brukes på shinyapps.io / Posit Connect / server
#   2. miljøvariabelen GS4_KEY_FILE              (sti til JSON-fila)
#   3. fila secrets/service-account.json         (lokal utvikling)
#
# Nøkkelen skal ALDRI ligge i git eller i appmappa som publiseres.
# Se app/.gitignore og oppsettet i docs/google-oppsett.md.
# ----------------------------------------------------------------------

init_google_auth <- function() {
  key_json <- Sys.getenv("GS4_SERVICE_ACCOUNT_JSON")
  key_file <- Sys.getenv("GS4_KEY_FILE", unset = "secrets/service-account.json")
  
  # minste nødvendige tilgang: bare regneark, ikke hele Google Drive
  scopes <- "https://www.googleapis.com/auth/spreadsheets"
  
  if (nzchar(key_json)) {
    googlesheets4::gs4_auth(path = key_json, scopes = scopes)
  } else if (file.exists(key_file)) {
    googlesheets4::gs4_auth(path = key_file, scopes = scopes)
  } else {
    stop(
      "Finner ingen Google-nøkkel. Sett miljøvariabelen ",
      "GS4_SERVICE_ACCOUNT_JSON (JSON-innholdet), eller GS4_KEY_FILE ",
      "(sti til fila), eller legg nøkkelen i '", key_file, "'.\n",
      "Se docs/google-oppsett.md."
    )
  }
  
  # Ikke slå på nettleser-innlogging som reserve: hvis nøkkelen feiler,
  # skal appen stoppe tydelig, ikke be om Gmail-godkjenning.
  options(gargle_oauth_email = FALSE)
  invisible(TRUE)
}