# ----------------------------
# Nasjonal analyse – Git push
# ----------------------------

timestamp <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")
log_msg <- function(msg) cat(sprintf("[%s] %s\n", timestamp(), msg))

log_msg("🚀 Starter Git push for nasjonal analyse...")

# Sett arbeidskatalog til prosjektmappen
setwd("/Users/oystein/Desktop/wasteson/TrackSights/datating/looker/bigquery_4 auto nasjonalitet analyse")

# Funksjon for å kjøre git-kommandoer med logging
run_git <- function(cmd) {
  res <- system(cmd, intern = TRUE)
  log_msg(paste("→", cmd))
  if (length(res) > 0) log_msg(paste(res, collapse = "\n"))
}

# Legg til endringer
run_git("git add .")

# Sjekk om det er endringer før commit
status <- system("git status --porcelain", intern = TRUE)
if (length(status) == 0) {
  log_msg("⚠️ Ingen endringer å committe.")
} else {
  # Commit og push
  tryCatch({
    run_git("git commit -m 'Auto update national analysis via cron'")
    run_git("git push")
    log_msg("✅ Git push fullført.")
  }, error = function(e) {
    log_msg(paste("❌ Git push feilet:", e$message))
  })
}
