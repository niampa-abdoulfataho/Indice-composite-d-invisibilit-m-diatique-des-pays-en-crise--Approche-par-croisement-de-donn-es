# ============================================================
# ucdp.R — Collecteur UCDP GED (Georeferenced Event Dataset)
# Source : https://ucdp.uu.se/downloads/index.html
# Version : 25.1 (1989-2024)
# ============================================================

library(httr2)
library(tidyverse)
library(lubridate)
library(countrycode)
library(glue)

# ------------------------------------------------------------
# Téléchargement et chargement
# ------------------------------------------------------------
#' @param url     URL du fichier zip RDS
#' @param destdir dossier de destination
#' @return tibble brut
fetch_ucdp <- function(url     = "https://ucdp.uu.se/downloads/ged/ged251-RDS.zip",
                       destdir = "data/raw/ucdp") {
  
  dir.create(destdir, recursive = TRUE, showWarnings = FALSE)
  
  zip_path  <- file.path(destdir, "ged251-RDS.zip")
  
  # CORRECTION : détection dynamique du .rds — le nom exact peut varier
  rds_files <- list.files(destdir, pattern = "\\.rds$",
                          full.names = TRUE, recursive = TRUE)
  
  if (length(rds_files) == 0) {
    message("Téléchargement UCDP GED v25.1...")
    request(url) |>
      req_timeout(120) |>
      req_retry(max_tries = 3, backoff = ~ 2 ^ .x) |>
      req_perform(path = zip_path)
    
    message("Décompression...")
    unzip(zip_path, exdir = destdir)
    
    # Relister après extraction
    rds_files <- list.files(destdir, pattern = "\\.rds$",
                            full.names = TRUE, recursive = TRUE)
    
    if (length(rds_files) == 0)
      stop("Aucun fichier .rds trouvé dans ", destdir,
           " après extraction. Vérifier le contenu du zip.")
    
    message(glue("✓ Fichier extrait : {basename(rds_files[1])}"))
  } else {
    message(glue("✓ Cache trouvé : {basename(rds_files[1])} — chargement direct"))
  }
  
  preferred_rds <- rds_files[stringr::str_detect(basename(rds_files), "^ged251.*\\.rds$")]
  rds_path <- if (length(preferred_rds) > 0) preferred_rds[1] else rds_files[1]
  
  df <- tryCatch(
    readRDS(rds_path),
    error = function(e) stop(glue("Impossible de lire {rds_path} : {e$message}"))
  )
  
  message(glue("✓ {nrow(df)} événements chargés"))
  as_tibble(df)
}

# ------------------------------------------------------------
# Nettoyage et filtrage
# ------------------------------------------------------------

#' Nettoie et filtre le dataset UCDP GED
#'
#' @param df        tibble brut UCDP
#' @param year_from année de début
#' @param year_to   année de fin
#' @return tibble nettoyé
clean_ucdp <- function(df,
                       year_from = 2020,
                       year_to   = 2024) {
  
  message(glue("Nettoyage UCDP ({year_from}-{year_to})..."))
  
  df_clean <- df |>
    filter(year >= year_from, year <= year_to) |>
    mutate(
      date_event       = as.Date(date_start),
      iso3             = countrycode::countrycode(
        as.character(country),
        origin       = "country.name",
        destination  = "iso3c",
        warn         = FALSE,
        custom_match = c(
          "Yemen (North Yemen)"   = "YEM",
          "DR Congo (Zaire)"      = "COD",
          "Russia (Soviet Union)" = "RUS",
          "Serbia (Yugoslavia)"   = "SRB",
          "Myanmar (Burma)"       = "MMR",
          "Sudan-North"           = "SDN",
          "South Yemen"           = "YEM"
        )
      ),
      conflict_id      = as.character(conflict_new_id),
      event_type       = type_of_violence,
      event_type_label = case_when(
        type_of_violence == 1 ~ "State-based conflict",
        type_of_violence == 2 ~ "Non-state conflict",
        type_of_violence == 3 ~ "One-sided violence",
        TRUE                  ~ "Unknown"
      ),
      deaths_best  = as.numeric(best),
      deaths_low   = as.numeric(low),
      deaths_high  = as.numeric(high),
      latitude     = as.numeric(latitude),
      longitude    = as.numeric(longitude)
    ) |>
    select(
      event_id     = id,
      conflict_id,
      iso3, country,
      region, date_event, year,
      event_type, event_type_label,
      deaths_best, deaths_low, deaths_high,
      latitude, longitude,
      side_a, side_b,
      where_description
    )
  
  message(glue("✓ {nrow(df_clean)} événements après filtrage"))
  df_clean
}

# ------------------------------------------------------------
# Agrégation par pays
# ------------------------------------------------------------

#' Agrège les événements UCDP par pays
#'
#' @param df tibble nettoyé
#' @return tibble agrégé par pays
summarise_ucdp_by_country <- function(df) {
  df |>
    group_by(iso3, country, region) |>
    summarise(
      nb_evenements  = n(),
      total_deaths   = sum(deaths_best, na.rm = TRUE),
      deaths_low     = sum(deaths_low,  na.rm = TRUE),
      deaths_high    = sum(deaths_high, na.rm = TRUE),
      nb_conflits    = n_distinct(conflict_id),
      annees_actives = n_distinct(year),
      types_violence = paste(unique(event_type_label), collapse = "; "),
      .groups = "drop"
    ) |>
    arrange(desc(total_deaths))
}

# ------------------------------------------------------------
# EXÉCUTION
# ------------------------------------------------------------

run_ucdp <- function(save = TRUE) {
  message("\n===== COLLECTE UCDP GED =====")
  
  df_raw     <- fetch_ucdp()
  df_ucdp    <- clean_ucdp(df_raw)
  df_country <- summarise_ucdp_by_country(df_ucdp)
  
  message(glue("\nPays avec conflits  : {nrow(df_country)}"))
  message(glue("Total événements    : {nrow(df_ucdp)}"))
  message(glue("Total morts (best)  : {sum(df_ucdp$deaths_best, na.rm = TRUE)}"))
  
  message("\nTop 10 pays — morts par conflits :")
  print(df_country |>
          head(10) |>
          select(iso3, country, nb_evenements, total_deaths, nb_conflits))
  
  if (save) {
    dir.create("data/raw/ucdp", recursive = TRUE, showWarnings = FALSE)
    write_csv(df_ucdp,    "data/raw/ucdp/ucdp_events.csv")
    write_csv(df_country, "data/raw/ucdp/ucdp_by_country.csv")
    message("\n✓ Sauvegardé dans data/raw/ucdp/")
  }
  
  list(events = df_ucdp, by_country = df_country)
}
