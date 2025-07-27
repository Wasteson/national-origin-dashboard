#!/usr/local/bin/Rscript

# --- Pakker ---
suppressPackageStartupMessages({
  library(rmarkdown)
  library(dplyr)
  library(bigrquery)
  library(ggplot2)
  library(scales)
  library(forcats)
  library(readr)
})

# --- Logging ---
timestamp <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")
log_msg <- function(msg) cat(sprintf("[%s] %s\n", timestamp(), msg))

log_msg("🚀 Starter national_share_update job...")

# --- Base path setup ---
base_dir <- "/Users/oystein/Desktop/wasteson/TrackSights/datating/looker"
project_dir <- file.path(base_dir, "bigquery_4 auto nasjonalitet analyse")

# Filbaner
csv_path <- file.path(project_dir, "national_model_data.csv")
rmd_path <- file.path(project_dir, "national_origin_dashboard.Rmd")
output_dir <- project_dir

# --- Pandoc-sti ---
Sys.setenv(RSTUDIO_PANDOC = "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64")

# --- BigQuery autentisering ---
tryCatch({
  bq_auth(path = "~/.gcp/oystein-service-account_2.json")
  log_msg("✅ Autentisering OK.")
}, error = function(e) {
  log_msg(paste("❌ Feil under autentisering:", e$message))
  quit(status = 1)
})

# --- BigQuery spørring ---
project <- "imposing-yen-426717-u4"
sql <- "SELECT * FROM `imposing-yen-426717-u4.wasteson_insight.daily_national_share`"

# --- Hent data ---
tryCatch({
  log_msg("📡 Henter data fra BigQuery...")
  query_job <- bq_project_query(project, sql)
  df <- bq_table_download(query_job)
  log_msg(paste("✅ Hentet", nrow(df), "rader."))
}, error = function(e) {
  log_msg(paste("❌ Feil under henting av data:", e$message))
  quit(status = 1)
})

# --- Lagre CSV ---
tryCatch({
  write_csv(df, csv_path)
  log_msg(paste("✅ Lagret data til", csv_path))
}, error = function(e) {
  log_msg(paste("❌ Feil under lagring av CSV:", e$message))
  quit(status = 1)
})

# --- Generer grafer ---
tryCatch({
  log_msg("📊 Genererer grafer for TOTAL og EV...")
  
  # Merkekart for opprinnelse
  brand_origin <- tribble(
    ~brand,             ~origin,
    # Tyske merker
    "BMW", "Tysk", "AUDI", "Tysk", "MERCEDES-BENZ", "Tysk",
    "VOLKSWAGEN", "Tysk", "VW", "Tysk", "PORSCHE", "Tysk",
    "OPEL", "Tysk", "SMART", "Tysk",
    # Kinesiske merker
    "BYD", "Kinesisk", "XPENG", "Kinesisk", "NIO", "Kinesisk",
    "GEELY", "Kinesisk", "ORA", "Kinesisk", "MG", "Kinesisk",
    "LEAPMOTOR", "Kinesisk"
  )
  
  df <- df %>%
    mutate(country_full = if_else(country_code == 0, "Denmark", country_full)) %>%
    mutate(country_full = str_to_title(country_full)) %>%
    left_join(brand_origin, by = "brand") %>%
    mutate(origin = replace_na(origin, "Annet"))
  
  # Funksjon for å lage og lagre plott
  plot_origin_share <- function(data, title_text, output_file) {
    df_focus <- data %>%
      filter(origin %in% c("Tysk", "Kinesisk")) %>%
      group_by(country_full, origin) %>%
      summarise(num_ads = sum(num_ads),
                total_ads = max(total_ads), .groups = "drop") %>%
      mutate(pct = num_ads / total_ads)
    
    p <- ggplot(df_focus, aes(x = fct_reorder(country_full, -pct), y = pct, fill = origin)) +
      geom_col(position = "dodge", width = 0.7) +
      geom_text(aes(label = percent(pct, accuracy = 0.1)),
                position = position_dodge(width = 0.7),
                vjust = -0.5, color = "white", size = 3.5) +
      scale_y_continuous(labels = percent_format(accuracy = 1),
                         breaks = seq(0, 1, 0.10),
                         expand = expansion(mult = c(0, 0.1))) +
      scale_fill_manual(values = c("Tysk" = "#1f78b4", "Kinesisk" = "#e6550d"),
                        labels = c("Tysk" = "German", "Kinesisk" = "Chinese")) +
      labs(title = title_text,
           subtitle = "Based on share of total ad volume per country",
           x = NULL, y = "Share of total market", fill = "Origin") +
      theme_minimal(base_size = 13) +
      theme(plot.background = element_rect(fill = "black", color = NA),
            panel.background = element_rect(fill = "black", color = NA),
            legend.background = element_rect(fill = "black", color = NA),
            legend.key = element_rect(fill = "black", color = NA),
            text = element_text(color = "white"),
            axis.text = element_text(color = "white"),
            axis.title = element_text(color = "white"),
            plot.title = element_text(color = "white", face = "bold"),
            plot.subtitle = element_text(color = "white"),
            panel.grid.major.y = element_line(color = "gray30"),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank())
    
    ggsave(file.path(output_dir, output_file), p, width = 10, height = 6, dpi = 300)
  }
  
  # Plott TOTAL
  df_total <- df %>% filter(segment == "TOTAL")
  plot_origin_share(df_total,
                    "Share of German and Chinese car brands in total market",
                    "brand_origin_total_dark.png")
  
  # Plott EV
  df_ev <- df %>% filter(segment == "EV")
  plot_origin_share(df_ev,
                    "Share of German and Chinese car brands in EV market",
                    "brand_origin_ev_dark.png")
  
  log_msg("✅ Grafer generert og lagret.")
}, error = function(e) {
  log_msg(paste("❌ Feil under grafgenerering:", e$message))
})

# --- Render dashboard (hvis filen finnes) ---
if (file.exists(rmd_path)) {
  tryCatch({
    log_msg("🖼 Kjører rmarkdown::render for national dashboard...")
    rmarkdown::render(input = rmd_path,
                      output_file = "index.html",
                      output_dir = output_dir)
    log_msg("✅ Dashboard generert OK.")
  }, error = function(e) {
    log_msg(paste("❌ Feil under rendering:", e$message))
  })
} else {
  log_msg("⚠️ Dashboardfil ikke funnet – hopper over rendering.")
}

log_msg("🎉 Jobb fullført uten feil!")
