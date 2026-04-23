# ============================================================
# emdat.R — Chargement et nettoyage EM-DAT
# Source : https://www.emdat.be
# ============================================================

library(readxl)
library(tidyverse)
library(lubridate)
library(janitor)
library(glue)

# ------------------------------------------------------------
# Chargement et nettoyage
# ------------------------------------------------------------

#' Charge et nettoie le dataset EM-DAT
#'
#' @param path chemin vers le fichier Excel
#' @param year_from année de début
#' @param year_to   année de fin
#' @return tibble nettoyé
load_emdat <- function(path      = "data/raw/emdat/emdat.xlsx",
                       year_from = 2020,
                       year_to   = 2024) {
  
  message("Chargement EM-DAT...")
  
  df <- read_excel(path) |>
    clean_names() |>                         # normalise les noms de colonnes
    filter(start_year >= year_from,
           start_year <= year_to) |>
    filter(disaster_group == "Natural" |
             disaster_subgroup == "Complex Emergencies" |
             str_detect(disaster_type, "Violence|Conflict|Displacement")) |>
    mutate(
      # Reconstruction des dates
      date_start = make_date(start_year,
                             coalesce(start_month, 1L),
                             coalesce(start_day,   1L)),
      date_end   = make_date(end_year,
                             coalesce(end_month,   12L),
                             coalesce(end_day,     28L)),
      
      # Durée en jours
      duree_jours = as.numeric(date_end - date_start) + 1,
      duree_jours = pmax(duree_jours, 1, na.rm = TRUE),
      
      # Nettoyage colonnes clés
      iso3          = iso,
      crisis_id     = str_replace_all(dis_no, "[^A-Za-z0-9]", "_"),
      crisis_type   = disaster_type,
      crisis_subtype = disaster_subtype,
      country       = country,
      region        = region,
      subregion     = subregion,
      latitude      = latitude,
      longitude     = longitude,
      
      # Indicateurs humains
      total_deaths    = as.numeric(total_deaths),
      total_affected  = as.numeric(total_affected),
      no_homeless     = as.numeric(no_homeless),
      total_damage    = as.numeric(`total_damage_000_us`),
      
      # Réponse internationale (proxy visibilité humanitaire)
      had_appeal      = appeal == "Yes",
      had_declaration = declaration == "Yes",
      had_aid         = !is.na(aid_contribution_000_us)
    ) |>
    select(
      crisis_id, dis_no, iso3, country, region, subregion,
      crisis_type, crisis_subtype, event_name,
      date_start, date_end, duree_jours,
      total_deaths, total_affected, no_homeless, total_damage,
      latitude, longitude,
      had_appeal, had_declaration, had_aid,
      magnitude, magnitude_scale
    )
  
  message(glue("✓ {nrow(df)} catastrophes chargées ({year_from}-{year_to})"))
  df
}

# ------------------------------------------------------------
# Résumé par pays
# ------------------------------------------------------------

#' Agrège les catastrophes par pays
#'
#' @param df tibble issu de load_emdat()
#' @return tibble agrégé par pays
summarise_by_country <- function(df) {
  df |>
    group_by(iso3, country, region, subregion) |>
    summarise(
      nb_crises        = n(),
      total_deaths     = sum(total_deaths,   na.rm = TRUE),
      total_affected   = sum(total_affected, na.rm = TRUE),
      total_homeless   = sum(no_homeless,    na.rm = TRUE),
      total_damage_k   = sum(total_damage,   na.rm = TRUE),
      duree_moy        = mean(duree_jours,   na.rm = TRUE),
      pct_avec_appel   = mean(had_appeal,    na.rm = TRUE),
      types_crises     = paste(unique(crisis_type), collapse = "; "),
      .groups = "drop"
    ) |>
    arrange(desc(total_deaths))
}

# ------------------------------------------------------------
# EXÉCUTION
# ------------------------------------------------------------

run_emdat <- function(save = TRUE) {
  message("\n===== CHARGEMENT EM-DAT =====")
  
  df_emdat   <- load_emdat()
  df_country <- summarise_by_country(df_emdat)
  
  message(glue("Pays affectés  : {nrow(df_country)}"))
  message(glue("Types de crises: {paste(unique(df_emdat$crisis_type), collapse = ', ')}"))
  
  if (save) {
    dir.create("data/raw/emdat", recursive = TRUE, showWarnings = FALSE)
    write_csv(df_emdat,   "data/raw/emdat/emdat_clean.csv")
    write_csv(df_country, "data/raw/emdat/emdat_by_country.csv")
    message("✓ Sauvegardé dans data/raw/emdat/")
  }
  
  list(crises = df_emdat, by_country = df_country)
}
