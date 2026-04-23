# ============================================================
# merger.R — Fusion de toutes les sources
# ============================================================

library(tidyverse)
library(lubridate)
library(glue)

# ------------------------------------------------------------
# Construction du dataset crises (unité = crise par pays)
# ------------------------------------------------------------

#' Construit la table principale des crises
#' en fusionnant EM-DAT + UCDP + ReliefWeb
#'
#' @return tibble des crises avec données de gravité
build_crisis_table <- function() {
  message("Construction de la table des crises...")
  
  # Charger les sources nettoyées
  df_emdat  <- read_csv("data/processed/emdat_clean.csv",
                        show_col_types = FALSE)
  df_ucdp_c <- read_csv("data/processed/ucdp_country_clean.csv",
                        show_col_types = FALSE)
  df_rw     <- read_csv("data/processed/reliefweb_clean.csv",
                        show_col_types = FALSE)
  df_wb     <- read_csv("data/processed/worldbank_clean.csv",
                        show_col_types = FALSE)
  df_unhcr  <- read_csv("data/processed/unhcr_clean.csv",
                        show_col_types = FALSE)
  df_fts    <- read_csv("data/raw/fts/fts_aggregated.csv",
                        show_col_types = FALSE)
  
  # 1. Agrégation EM-DAT par pays
  df_emdat_agg <- df_emdat |>
    group_by(iso3, country, year) |>
    summarise(
      nb_disasters      = n(),
      total_deaths_nat  = sum(total_deaths,   na.rm = TRUE),
      total_affected_nat= sum(total_affected, na.rm = TRUE),
      total_homeless_nat= sum(no_homeless,    na.rm = TRUE),
      total_damage_nat  = sum(total_damage,   na.rm = TRUE),
      had_appeal        = any(had_appeal,     na.rm = TRUE),
      types_disasters   = paste(unique(crisis_type), collapse = "; "),
      .groups = "drop"
    )
  
  # 2. Agrégation UCDP par pays + année
  df_ucdp_yr <- read_csv("data/processed/ucdp_clean.csv",
                         show_col_types = FALSE) |>
    group_by(iso3, country, year) |>
    summarise(
      nb_conflict_events = n(),
      total_deaths_conf  = sum(deaths_best, na.rm = TRUE),
      nb_conflicts       = n_distinct(conflict_id),
      .groups = "drop"
    )
  
  # 3. Fusionner EM-DAT + UCDP par (iso3, year)
  df_emdat_agg <- df_emdat_agg |> rename(country_emdat = country)
  df_ucdp_yr   <- df_ucdp_yr   |> rename(country_ucdp  = country)
  
  df_crises <- full_join(
    df_emdat_agg,
    df_ucdp_yr |> select(iso3, year, country_ucdp,
                         nb_conflict_events,
                         total_deaths_conf, nb_conflicts),
    by = c("iso3", "year")
  ) |>
    mutate(
      country = coalesce(country_emdat, country_ucdp),
      country = ifelse(
        is.na(country),
        countrycode::countrycode(iso3, "iso3c", "country.name",
                                 warn = FALSE),
        country
      ),
      total_deaths_all   = coalesce(total_deaths_nat, 0) +
        coalesce(total_deaths_conf, 0),
      nb_disasters       = coalesce(nb_disasters, 0L),
      nb_conflict_events = coalesce(nb_conflict_events, 0L)
    ) |>
    select(-country_emdat, -country_ucdp) |>
    filter(!is.na(iso3), !is.na(year))
  
  # 4. Ajouter les infos ReliefWeb (nb de catastrophes enregistrées)
  df_rw_agg <- df_rw |>
    filter(!is.na(iso3), !is.na(year)) |>
    group_by(iso3, year) |>
    summarise(
      nb_rw_disasters = n(),
      rw_types        = paste(unique(disaster_type_label), collapse = "; "),
      rw_ongoing      = sum(status == "ongoing", na.rm = TRUE),
      .groups = "drop"
    )
  
  df_crises <- df_crises |>
    left_join(df_rw_agg, by = c("iso3", "year")) |>
    mutate(nb_rw_disasters = coalesce(nb_rw_disasters, 0L))
  
  # 5. Ajouter les données de déplacement UNHCR
  df_crises <- df_crises |>
    left_join(
      df_unhcr |> select(iso3, year, unhcr_idps, unhcr_refugees),
      by = c("iso3", "year")
    ) |>
    mutate(
      unhcr_idps     = coalesce(unhcr_idps, 0),
      unhcr_refugees = coalesce(unhcr_refugees, 0)
    )
  
  # 6. Ajouter les financements FTS
  df_crises <- df_crises |>
    left_join(
      df_fts |> select(iso3, year, fts_total_usd, fts_nb_flux, fts_nb_plans, fts_log_usd),
      by = c("iso3", "year")
    ) |>
    mutate(
      fts_total_usd = coalesce(fts_total_usd, 0),
      fts_nb_flux   = coalesce(fts_nb_flux, 0L),
      fts_nb_plans  = coalesce(fts_nb_plans, 0L),
      fts_log_usd   = coalesce(fts_log_usd, 0)
    )
  
  # 7. Enrichir avec données pays
  if (!"region_wb" %in% names(df_wb)) {
    df_wb$region_wb <- if ("region" %in% names(df_wb)) df_wb$region else NA_character_
  }
  if (!"continent_group" %in% names(df_wb)) {
    df_wb$continent_group <- case_when(
      df_wb$continent == "Africa"        ~ "Afrique",
      df_wb$continent == "Asia"          ~ "Asie",
      df_wb$continent == "Europe"        ~ "Europe",
      df_wb$continent == "North America" ~ "Amériques",
      df_wb$continent == "South America" ~ "Amériques",
      df_wb$continent == "Oceania"       ~ "Océanie",
      TRUE                               ~ "Autre"
    )
  }
  if (!"pop_est" %in% names(df_wb)) {
    df_wb$pop_est <- if ("population" %in% names(df_wb)) df_wb$population else NA_real_
  }
  if (!"gdp_md" %in% names(df_wb)) {
    pib_md <- if ("pib_md" %in% names(df_wb)) df_wb$pib_md else rep(NA_real_, nrow(df_wb))
    population <- if ("population" %in% names(df_wb)) df_wb$population else rep(NA_real_, nrow(df_wb))
    pib_habitant <- if ("pib_habitant" %in% names(df_wb)) df_wb$pib_habitant else rep(NA_real_, nrow(df_wb))
    df_wb$gdp_md <- dplyr::coalesce(
      pib_md,
      (pib_habitant * population) / 1e6
    )
  }
  if (!"proximity_score" %in% names(df_wb)) {
    df_wb$proximity_score <- if_else(
      !is.na(df_wb$dist_redactions_km) & df_wb$dist_redactions_km > 0,
      1 / log1p(df_wb$dist_redactions_km / 1000),
      1.0
    )
  }
  if (!"language_bias" %in% names(df_wb)) {
    df_wb$language_bias <- 1.0
  }

  cols_wb <- c("iso3", "continent", "subregion", "region_wb",
               "continent_group", "pop_est", "gdp_md",
               "dist_redactions_km", "proximity_score",
               "language_bias", "lon", "lat")
  
  df_crises <- df_crises |>
    left_join(
      df_wb |> select(all_of(cols_wb)),
      by = "iso3"
    )
  
  
  message(glue("✓ Table crises : {nrow(df_crises)} lignes ({n_distinct(df_crises$iso3)} pays)"))
  df_crises
}

# ------------------------------------------------------------
# Construction du dataset médias (articles par crise)
# ------------------------------------------------------------

#' Construit la table de couverture médiatique par pays/année
#'
#' @return tibble de couverture Guardian par pays/année
build_media_table <- function() {
  message("Construction de la table médias...")
  
  df_articles <- read_csv("data/processed/media_articles_clean.csv",
                          show_col_types = FALSE) |>
    mutate(
      date_pub = as.Date(date_pub),
      year     = lubridate::year(date_pub)
    )
  
  df_media <- df_articles |>
    filter(!is.na(iso3), !is.na(date_pub)) |>
    group_by(iso3, crisis_id, year) |>
    summarise(
      nb_articles     = n(),
      nb_sources      = n_distinct(source),
      wordcount_total = sum(wordcount, na.rm = TRUE),
      date_first_art  = min(date_pub, na.rm = TRUE),
      date_last_art   = max(date_pub, na.rm = TRUE),
      .groups = "drop"
    ) |>
    group_by(iso3, year) |>
    summarise(
      nb_articles_total   = sum(nb_articles),
      nb_sources_uniq     = max(nb_sources),
      nb_crises_couvertes = n_distinct(crisis_id),
      wordcount_total     = sum(wordcount_total, na.rm = TRUE),
      .groups = "drop"
    )
  
  message(glue("✓ Table médias : {nrow(df_media)} lignes pays/année"))
  df_media
}

# ------------------------------------------------------------
# Fusion finale
# ------------------------------------------------------------

#' Fusionne la table crises et la table médias
#'
#' @return tibble final prêt pour le scoring
build_final_dataset <- function() {
  message("Fusion finale...")
  
  df_crises <- build_crisis_table()
  df_media  <- build_media_table()
  
  df_final <- df_crises |>
    left_join(df_media, by = c("iso3", "year")) |>
    mutate(
      # Remplacer NA médias par 0
      nb_articles_total   = coalesce(nb_articles_total, 0L),
      nb_sources_uniq     = coalesce(nb_sources_uniq, 0L),
      nb_crises_couvertes = coalesce(nb_crises_couvertes, 0L),
      # Indicateur de crise grave (seuil minimal)
      is_crisis = total_deaths_all > 0 |
        nb_conflict_events > 0 |
        nb_disasters > 0
    ) |>
    filter(is_crisis) |>
    arrange(iso3, year)
  
  message(glue("✓ Dataset final : {nrow(df_final)} lignes"))
  message(glue("  Pays          : {n_distinct(df_final$iso3)}"))
  message(glue("  Années        : {min(df_final$year)}-{max(df_final$year)}"))
  message(glue("  Avec articles : {sum(df_final$nb_articles_total > 0)}"))
  
  df_final
}

# ------------------------------------------------------------
# EXÉCUTION
# ------------------------------------------------------------

run_merger <- function(save = TRUE) {
  message("\n===== FUSION DES SOURCES =====")
  
  dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
  
  df_final <- build_final_dataset()
  
  if (save) {
    write_csv(df_final, "data/processed/crises_merged.csv")
    message("✓ Sauvegardé dans data/processed/crises_merged.csv")
  }
  
  df_final
}
