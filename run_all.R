# ============================================================
# run_all.R — Pipeline complet crisis-media-index
# Exécuter ce script reproduit l'intégralité du projet
#
# ORDRE D'EXÉCUTION :
#   1. Collecte des données brutes (APIs + scraping)
#   2. Nettoyage et normalisation
#   3. Fusion des sources
#   4. Scoring et analyse
#   5. Visualisations
#
# PRÉREQUIS — variables dans .Renviron :
#   NYT_API_KEY=...
#   GUARDIAN_KEY=...
#
# USAGE :
#   source("run_all.R")                  # pipeline complet
#   source("run_all.R") ; run_collect()  # collecte seulement (df_crises construit auto)
# ============================================================

# ------------------------------------------------------------
# 0. Packages
# ------------------------------------------------------------

library(httr)
library(httr2)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(countrycode)
library(glue)

# ------------------------------------------------------------
# 1. Chargement des scripts
# ------------------------------------------------------------

message("\n📦 Chargement des scripts...")

# Collecteurs
source("R/collectors/emdat.R")
source("R/collectors/guardian.R")
source("R/collectors/newsapi.R")        # NYT + BBC RSS + AJE
source("R/collectors/ocha_fts.R")       # OCHA FTS + UNHCR
source("R/collectors/reliefweb_scraper.R")
source("R/collectors/ucdp.R")
source("R/collectors/worldbank.R")

# Traitement
source("R/processing/cleaner.R")
source("R/processing/geo_normalizer.R")
source("R/processing/merger.R")

# Analyse
source("R/analysis/scorer.R")
source("R/analysis/bias_analysis.R")

# Visualisations
source("R/viz/charts.R")
source("R/viz/maps.R")
source("R/viz/timeline_anim.R")
source("R/viz/wordcloud.R")

message("✓ Tous les scripts chargés\n")

# ------------------------------------------------------------
# Vérification des clés API
# ------------------------------------------------------------

check_api_keys <- function() {
  keys <- list(
    NYT_API_KEY  = Sys.getenv("NYT_API_KEY"),
    GUARDIAN_KEY = Sys.getenv("GUARDIAN_KEY")
  )
  missing <- names(keys)[keys == ""]
  if (length(missing) > 0) {
    warning(glue(
      "Clés API manquantes : {paste(missing, collapse=', ')}\n",
      "Ajoutez-les dans .Renviron puis redémarrez R.\n",
      "Les collecteurs concernés seront ignorés."
    ))
  }
  keys
}

# ------------------------------------------------------------
# Construction de df_crises depuis les résultats EM-DAT
# ------------------------------------------------------------

#' Construit le tibble df_crises à partir des données EM-DAT
#'
#' @param emdat_results résultat de run_emdat() (liste avec $crises)
#' @return tibble avec crisis_id, name, iso3_primary, crisis_type, date_created
build_df_crises <- function(emdat_results) {
  
  if (is.null(emdat_results) || is.null(emdat_results$crises)) {
    message("  ❌ Impossible de construire df_crises : résultats EM-DAT absents")
    return(NULL)
  }
  
  df <- emdat_results$crises |>
    transmute(
      crisis_id    = crisis_id,
      name         = coalesce(event_name, crisis_type, "Unknown"),
      iso3_primary = iso3,
      crisis_type  = crisis_type,
      date_created = date_start
    ) |>
    filter(!is.na(iso3_primary), !is.na(crisis_id)) |>
    distinct(crisis_id, .keep_all = TRUE)
  
  message(glue("  ✓ df_crises construit : {nrow(df)} crises"))
  df
}

# ------------------------------------------------------------
# 2. Collecte des données brutes
# ------------------------------------------------------------

#' Lance tous les collecteurs dans l'ordre
#'
#' @param df_crises  tibble des crises à analyser (optionnel — construit depuis
#'                   EM-DAT automatiquement si absent ou NULL)
#' @param date_from  date début
#' @param date_to    date fin
#' @param years      vecteur d'années pour FTS/UNHCR
run_collect <- function(df_crises = NULL,
                        date_from = "2020-01-01",
                        date_to   = "2024-12-31",
                        years     = 2020:2024) {
  
  keys <- check_api_keys()
  
  message("\n", strrep("=", 60))
  message("  ÉTAPE 1 — COLLECTE DES DONNÉES BRUTES")
  message(strrep("=", 60))
  
  results <- list()
  
  # 2a. EM-DAT (fichier local — pas d'API)
  message("\n[1/7] EM-DAT...")
  results$emdat <- tryCatch(
    run_emdat(save = TRUE),
    error = function(e) {
      message(glue("  ❌ EM-DAT : {e$message}"))
      NULL
    }
  )
  
  # CORRECTION : construction automatique de df_crises depuis EM-DAT
  # si l'appelant ne l'a pas fourni ou l'a passé NULL
  if (is.null(df_crises)) {
    message("\n  ℹ️  df_crises non fourni — construction automatique depuis EM-DAT...")
    df_crises <- build_df_crises(results$emdat)
    if (is.null(df_crises)) {
      message("  ⚠️  Guardian et NYT/RSS seront ignorés (df_crises introuvable)")
    }
  } else {
    # Vérifier les colonnes requises
    required_cols <- c("crisis_id", "name", "iso3_primary", "crisis_type", "date_created")
    missing_cols  <- setdiff(required_cols, names(df_crises))
    if (length(missing_cols) > 0) {
      message(glue("  ❌ df_crises fourni mais colonnes manquantes : {paste(missing_cols, collapse=', ')}"))
      df_crises <- NULL
    } else {
      message(glue("  ✓ df_crises fourni : {nrow(df_crises)} crises"))
    }
  }
  
  # 2b. ReliefWeb scraping
  message("\n[2/7] ReliefWeb...")
  results$reliefweb <- tryCatch(
    run_reliefweb_scraper(max_pages = 185, save = TRUE),
    error = function(e) {
      message(glue("  ❌ ReliefWeb : {e$message}"))
      NULL
    }
  )
  
  # 2c. UCDP GED
  message("\n[3/7] UCDP GED...")
  results$ucdp <- tryCatch(
    run_ucdp(save = TRUE),
    error = function(e) {
      message(glue("  ❌ UCDP : {e$message}"))
      NULL
    }
  )
  
  # 2d. World Bank / rnaturalearth
  message("\n[4/7] Données pays (rnaturalearth)...")
  results$worldbank <- tryCatch(
    run_worldbank(save = TRUE),
    error = function(e) {
      message(glue("  ❌ WorldBank/rnaturalearth : {e$message}"))
      NULL
    }
  )
  
  # 2e. OCHA FTS + UNHCR
  message("\n[5/7] OCHA FTS + UNHCR...")
  results$ocha <- tryCatch(
    run_ocha_unhcr(years = years, save = TRUE),
    error = function(e) {
      message(glue("  ❌ OCHA/UNHCR : {e$message}"))
      NULL
    }
  )
  
  # 2f. The Guardian (si clé disponible ET df_crises présent)
  message("\n[6/7] The Guardian...")
  if (keys$GUARDIAN_KEY != "" && !is.null(df_crises)) {
    results$guardian <- tryCatch(
      run_guardian(df_crises, save = TRUE),
      error = function(e) {
        message(glue("  ❌ Guardian : {e$message}"))
        NULL
      }
    )
  } else if (keys$GUARDIAN_KEY == "") {
    message("  ⚠️  GUARDIAN_KEY manquante — collecte ignorée")
    results$guardian <- NULL
  } else {
    message("  ⚠️  df_crises absent — Guardian ignoré")
    results$guardian <- NULL
  }
  
  # 2g. NYT + BBC RSS + Al Jazeera RSS
  message("\n[7/7] NYT + RSS...")
  if (keys$NYT_API_KEY != "" && !is.null(df_crises)) {
    results$newsapi <- tryCatch(
      run_newsapi(
        df_crises = df_crises,
        date_from = date_from,
        date_to   = date_to,
        api_key   = keys$NYT_API_KEY,
        max_pages = 10,
        save      = TRUE
      ),
      error = function(e) {
        message(glue("  ❌ NYT/RSS : {e$message}"))
        NULL
      }
    )
  } else if (keys$NYT_API_KEY == "") {
    message("  ⚠️  NYT_API_KEY manquante — collecte ignorée")
    results$newsapi <- NULL
  } else {
    message("  ⚠️  df_crises absent — NYT/RSS ignoré")
    results$newsapi <- NULL
  }
  
  message("\n✓ Collecte terminée\n")
  invisible(results)
}

# ------------------------------------------------------------
# 3. Traitement
# ------------------------------------------------------------

run_process <- function() {
  
  message("\n", strrep("=", 60))
  message("  ÉTAPE 2 — NETTOYAGE ET FUSION")
  message(strrep("=", 60))
  
  results <- list()
  
  message("\n[1/3] Nettoyage...")
  results$clean <- tryCatch(
    run_cleaner(),
    error = function(e) {
      message(glue("  ❌ Cleaner : {e$message}"))
      NULL
    }
  )
  
  message("\n[2/3] Normalisation géographique...")
  results$geo <- tryCatch(
    run_geo_normalizer(),
    error = function(e) {
      message(glue("  ❌ Geo normalizer : {e$message}"))
      NULL
    }
  )
  
  message("\n[3/3] Fusion des sources...")
  results$merged <- tryCatch(
    run_merger(),
    error = function(e) {
      message(glue("  ❌ Merger : {e$message}"))
      NULL
    }
  )
  
  message("\n✓ Traitement terminé\n")
  invisible(results)
}

# ------------------------------------------------------------
# 4. Analyse
# ------------------------------------------------------------

run_analyse <- function() {
  
  message("\n", strrep("=", 60))
  message("  ÉTAPE 3 — SCORING ET ANALYSE")
  message(strrep("=", 60))
  
  results <- list()
  
  message("\n[1/2] Scoring...")
  results$scores <- tryCatch(
    run_scorer(),
    error = function(e) {
      message(glue("  ❌ Scorer : {e$message}"))
      NULL
    }
  )
  
  message("\n[2/2] Analyse des biais...")
  if (!is.null(results$scores)) {
    results$bias <- tryCatch(
      run_bias_analysis(
        results$scores$indices,
        results$scores$vol
      ),
      error = function(e) {
        message(glue("  ❌ Bias analysis : {e$message}"))
        NULL
      }
    )
  } else {
    message("  ⚠️  Scores manquants — analyse des biais ignorée")
  }
  
  message("\n✓ Analyse terminée\n")
  invisible(results)
}

# ------------------------------------------------------------
# 5. Visualisations
# ------------------------------------------------------------

run_viz <- function(results_analyse) {
  
  message("\n", strrep("=", 60))
  message("  ÉTAPE 4 — VISUALISATIONS")
  message(strrep("=", 60))
  
  df_viz <- if (is.list(results_analyse) && !is.null(results_analyse$scores$indices)) {
    results_analyse$scores$indices
  } else {
    results_analyse
  }
  
  df_terms <- if (file.exists("data/final/top_terms_pays.csv")) {
    read_csv("data/final/top_terms_pays.csv", show_col_types = FALSE)
  } else {
    NULL
  }
  
  tryCatch(run_charts(df_viz),
           error = function(e) message(glue("  ❌ Charts : {e$message}")))
  
  tryCatch(run_maps(df_viz),
           error = function(e) message(glue("  ❌ Maps : {e$message}")))
  
  tryCatch(run_timeline(df_viz),
           error = function(e) message(glue("  ❌ Timeline : {e$message}")))
  
  tryCatch(run_wordclouds(df_terms = df_terms, df_indices = df_viz),
           error = function(e) message(glue("  ❌ Wordcloud : {e$message}")))
  
  message("\n✓ Visualisations terminées\n")
}

# ------------------------------------------------------------
# PIPELINE COMPLET
# ------------------------------------------------------------

#' Lance le pipeline complet
#'
#' @param df_crises     tibble des crises (optionnel — construit depuis EM-DAT si absent)
#'                      Colonnes requises : crisis_id, iso3_primary, name,
#'                      crisis_type, date_created
#' @param date_from     date début globale
#' @param date_to       date fin globale
#' @param years         années pour FTS/UNHCR
#' @param skip_collect  TRUE pour sauter la collecte (données déjà en cache)
run_pipeline <- function(df_crises     = NULL,
                         date_from     = "2020-01-01",
                         date_to       = "2024-12-31",
                         years         = 2020:2024,
                         skip_collect  = FALSE) {
  
  t_start <- Sys.time()
  
  message("\n", strrep("█", 60))
  message("  PIPELINE CRISIS-MEDIA-INDEX")
  message(glue("  Période : {date_from} → {date_to}"))
  if (!is.null(df_crises))
    message(glue("  Crises  : {nrow(df_crises)}"))
  else
    message("  Crises  : construction automatique depuis EM-DAT")
  message(strrep("█", 60))
  
  # Étape 1 — Collecte
  if (!skip_collect) {
    run_collect(df_crises, date_from, date_to, years)
  } else {
    message("\n⏭️  Collecte ignorée (skip_collect = TRUE)")
  }
  
  # Étape 2 — Traitement
  run_process()
  
  # Étape 3 — Analyse
  results_analyse <- run_analyse()
  
  # Étape 4 — Visualisations
  run_viz(results_analyse)
  
  t_end <- Sys.time()
  duree <- round(difftime(t_end, t_start, units = "mins"), 1)
  
  message("\n", strrep("█", 60))
  message(glue("  ✅ PIPELINE TERMINÉ en {duree} minutes"))
  message(strrep("█", 60))
  
  invisible(results_analyse)
}

# ------------------------------------------------------------
# MESSAGE DE DÉMARRAGE
# ------------------------------------------------------------

message("✓ run_all.R chargé")
message("\nUsage :")
message("  run_pipeline()                               # pipeline complet (df_crises auto)")
message("  run_pipeline(df_crises)                      # avec df_crises fourni")
message("  run_pipeline(df_crises, skip_collect = TRUE) # sans collecte")
message("  run_collect()                                # collecte seule (df_crises auto)")
message("  run_process()                                # traitement seul")
message("  run_analyse()                                # analyse seule")
