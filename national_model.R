# SQL

-- 📦 Korrekt landekoding – overskriv Albania (kode 0) med "Denmark"
country_lookup AS (
  SELECT
  country_code,
  CASE
  WHEN country_code = 0 THEN 'Denmark'
  ELSE comment
  END AS country_full
  FROM `imposing-yen-426717-u4.enum_lookup_tables.countries`
)


-- 📦 Steg 2: Hent Mobile og AutoScout
base_data AS (
  SELECT
  IFNULL(sellerCountry, 51) AS country_code,
  UPPER(TRIM(make)) AS brand
  FROM `imposing-yen-426717-u4.mobile.staging_mobile_prod`
  WHERE make IS NOT NULL AND model IS NOT NULL
  AND (firstRegistration IS NOT NULL OR mileage > 100)
  
  UNION ALL
  
  SELECT
  sellerCountry AS country_code,
  UPPER(TRIM(make)) AS brand
  FROM `imposing-yen-426717-u4.autoscout.staging_autoscout_prod`
  WHERE make IS NOT NULL AND model IS NOT NULL
  AND (firstRegistration IS NOT NULL OR mileage > 100)
),

-- 📦 Steg 3: Hent Bilinfo (Danmark = 0)
bilinfo_data AS (
  SELECT
  0 AS country_code,
  UPPER(TRIM(Make)) AS brand
  FROM `imposing-yen-426717-u4.bilinfo.prod_bilinfo_prod`
  WHERE Make IS NOT NULL AND Model IS NOT NULL
  AND Mileage > 100
),

-- 📦 Steg 4: Kombiner
all_data AS (
  SELECT * FROM base_data
  UNION ALL
  SELECT * FROM bilinfo_data
),

-- 📦 Steg 5: Aggreger per land og merke
brand_counts AS (
  SELECT
  country_code,
  brand,
  COUNT(*) AS num_ads
  FROM all_data
  GROUP BY country_code, brand
),

-- 📦 Steg 6: Summer per land og beregn topp 8
country_totals AS (
  SELECT
  country_code,
  SUM(num_ads) AS total_ads
  FROM brand_counts
  GROUP BY country_code
),

top_8_countries AS (
  SELECT country_code
  FROM country_totals
  ORDER BY total_ads DESC
  LIMIT 8
),

-- 📦 Steg 7: Endelig output med andel
final AS (
  SELECT
  bc.country_code,
  cl.country_full,
  bc.brand,
  bc.num_ads,
  ct.total_ads,
  ROUND(bc.num_ads / ct.total_ads, 4) AS pct
  FROM brand_counts bc
  JOIN country_totals ct USING (country_code)
  JOIN country_lookup cl USING (country_code)
  WHERE bc.country_code IN (SELECT country_code FROM top_8_countries)
)

-- 📦 Steg 8: Sortér og returnér
SELECT *
  FROM final
ORDER BY total_ads DESC, country_full, num_ads DESC;


# ---------------

library(tidyverse)
library(scales)
library(ggrepel)
library(forcats)

# 1. Les data fra CSV (må være eksportert fra BigQuery med segment-kolonne)
df <- read_csv("national_model_data.csv")

# Sikrer at country_code = 0 → Denmark og pen formatering
df <- df %>%
  mutate(
    country_full = if_else(country_code == 0, "Denmark", country_full),
    country_full = str_to_title(country_full)
  )

# 2. Fjern NA og formater
df <- df %>%
  filter(!is.na(brand), brand != "")

# 3. Klassifiser merke-opprinnelse
brand_origin <- tribble(
  ~brand,             ~origin,
  # Tyske merker
  "BMW",              "Tysk",
  "AUDI",             "Tysk",
  "MERCEDES-BENZ",    "Tysk",
  "VOLKSWAGEN",       "Tysk",
  "VW",               "Tysk",
  "PORSCHE",          "Tysk",
  "OPEL",             "Tysk",
  "SMART",            "Tysk",
  # Kinesiske merker
  "BYD",              "Kinesisk",
  "XPENG",            "Kinesisk",
  "NIO",              "Kinesisk",
  "GEELY",            "Kinesisk",
  "ORA",              "Kinesisk",
  "MG",               "Kinesisk",
  "LEAPMOTOR",        "Kinesisk"
)

# 4. Slå på opprinnelse
df <- df %>%
  left_join(brand_origin, by = "brand") %>%
  mutate(origin = replace_na(origin, "Annet"))

# 🔹 Funksjon for å lage plot for TOTAL og EV
plot_origin_share <- function(data, title_text, output_file) {
  # Beregn fokusdata basert på andel av totalmarkedet
  df_focus <- data %>%
    filter(origin %in% c("Tysk", "Kinesisk")) %>%
    group_by(country_full, origin) %>%
    summarise(num_ads = sum(num_ads),
              total_ads = max(total_ads), .groups = "drop") %>%
    mutate(pct = num_ads / total_ads)
  
  # Lag plot
  p <- ggplot(df_focus, aes(x = fct_reorder(country_full, -pct), y = pct, fill = origin)) +
    geom_col(position = "dodge", width = 0.7) +
    geom_text(aes(label = percent(pct, accuracy = 0.1)),
              position = position_dodge(width = 0.7),
              vjust = -0.5, color = "white", size = 3.5) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      breaks = seq(0, 1, 0.10),
      expand = expansion(mult = c(0, 0.1))
    ) +
    scale_fill_manual(values = c("Tysk" = "#1f78b4", "Kinesisk" = "#e6550d"),
                      labels = c("Tysk" = "German", "Kinesisk" = "Chinese")) +
    labs(
      title = title_text,
      subtitle = "Based on share of total ad volume per country",
      x = NULL,
      y = "Share of total market",
      fill = "Origin"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.background = element_rect(fill = "black", color = NA),
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
      panel.grid.major.x = element_blank()
    )
  
  # Lagre plot
  ggsave(output_file, p, width = 10, height = 6, dpi = 300)
}

# 🔹 5. Generer graf for TOTAL
df_total <- df %>% filter(segment == "TOTAL")
plot_origin_share(df_total, 
                  "Share of German and Chinese car brands in total market", 
                  "brand_origin_total_dark.png")

# 🔹 6. Generer graf for EV
df_ev <- df %>% filter(segment == "EV")
plot_origin_share(df_ev, 
                  "Share of German and Chinese car brands in EV market", 
                  "brand_origin_ev_dark.png")
