# ============================================================
# guardian.R — Collecteur The Guardian API
# Doc : https://open-platform.theguardian.com/documentation/
# ============================================================

library(httr2)
library(tidyverse)
library(lubridate)
library(glue)

# ------------------------------------------------------------
# Mots-clés par type de crise
# ------------------------------------------------------------

guardian_crisis_keywords <- list(
  "Flood"             = c("flood", "flooding"),
  "Drought"           = c("drought", "famine", "food crisis"),
  "Complex Emergency" = c("war", "conflict", "humanitarian crisis"),
  "Earthquake"        = c("earthquake", "seisme"),
  "Epidemic"          = c("epidemic", "outbreak", "disease"),
  "Cyclone"           = c("cyclone", "hurricane", "typhoon"),
  "Violence"          = c("violence", "massacre", "displacement")
)

#' Nettoie un nom de pays pour les requêtes HTTP
guardian_clean_country_name <- function(name) {
  name |>
    str_replace_all("[''`]",              " ") |>
    str_replace_all("[\\(\\)\\[\\]]",    " ") |>
    str_replace_all("[&]",               "and") |>
    str_replace_all("[^a-zA-Z0-9 \\-]", " ") |>
    str_squish()
}

#' Génère une requête Guardian : "pays" AND ("terme1" OR "terme2")
#'
#' au lieu de OR simple qui générait trop de bruit
guardian_generate_keywords <- function(country_name, crisis_type) {
  type_clean   <- str_trim(str_split(crisis_type, ";")[[1]][1])
  synonyms     <- guardian_crisis_keywords[[type_clean]] %||% c("humanitarian", "crisis")
  country_safe <- guardian_clean_country_name(country_name)
  synonyms_str <- paste0('("', paste(synonyms, collapse = '" OR "'), '")')
  glue('"{country_safe}" AND {synonyms_str}')
}

# ------------------------------------------------------------
# Collecteur Guardian avec pagination
# ------------------------------------------------------------

#' Récupère les articles Guardian pour une crise
#'
#' @param keywords   chaîne de recherche
#' @param date_from  date début (YYYY-MM-DD)
#' @param date_to    date fin
#' @param max_pages  nb max de pages (200 articles/page)
#' @return tibble des articles
fetch_guardian_crisis <- function(keywords,
                                  date_from = "2020-01-01",
                                  date_to   = "2024-12-31",
                                  max_pages = 3) {
  api_key     <- Sys.getenv("GUARDIAN_KEY")
  all_results <- list()
  
  for (page in seq_len(max_pages)) {
    
    resp <- tryCatch(
      request("https://content.guardianapis.com/search") |>
        req_url_query(
          `api-key`     = api_key,
          q             = keywords,
          `from-date`   = date_from,
          `to-date`     = date_to,
          `page-size`   = 200,
          page          = page,
          `show-fields` = "headline,byline,wordcount",
          `show-tags`   = "keyword"
        ) |>
        req_retry(max_tries = 3, backoff = ~ 2 ^ .x) |>
        req_perform(),
      error = function(e) {
        message(glue("    ⚠️  Erreur requête : {e$message}"))
        NULL
      }
    )
    
    if (is.null(resp) || resp_status(resp) != 200) break
    
    data    <- resp_body_json(resp, simplifyVector = FALSE)
    results <- data$response$results
    total_p <- data$response$pages %||% 1
    
    if (length(results) == 0) break
    
    all_results <- c(all_results, results)
    if (page >= total_p) break
    Sys.sleep(0.2)
  }
  
  if (length(all_results) == 0) return(tibble())
  
  map_dfr(all_results, function(item) {
    tibble(
      article_id = item$id        %||% NA_character_,
      title      = item$webTitle  %||% NA_character_,
      section    = item$sectionName %||% NA_character_,
      date_pub   = ymd(substr(item$webPublicationDate %||% "", 1, 10)),
      url        = item$webUrl    %||% NA_character_,
      wordcount  = as.integer(item$fields$wordcount %||% NA)
    )
  })
}

# ------------------------------------------------------------
# Collecte pour une liste de crises
# ------------------------------------------------------------

#' Lance la collecte Guardian pour toutes les crises du dataset
#'
#' @param df_crises tibble avec colonnes : crisis_id, name, iso3_primary,
#'                  crisis_type, date_created
#' @param save_checkpoint sauvegarder toutes les 20 crises
#' @return tibble des articles avec crisis_id associé
fetch_guardian_all <- function(df_crises, save_checkpoint = TRUE) {
  
  library(countrycode)
  
  df_crises <- df_crises |>
    mutate(
      country_name = countrycode(iso3_primary,
                                 origin      = "iso3c",
                                 destination = "country.name",
                                 warn        = FALSE),
      country_name = guardian_clean_country_name(country_name)
    )
  
  all_articles <- list()
  n <- nrow(df_crises)
  
  for (i in seq_len(n)) {
    row      <- df_crises[i, ]
    keywords <- guardian_generate_keywords(row$country_name, row$crisis_type)
    
    # CORRECTION : coalesce() au lieu de %||% — gère NA en plus de NULL
    d_from <- as.character(coalesce(row$date_created, as.Date("2020-01-01")))
    
    message(glue("[{i}/{n}] {row$name} — '{keywords}'"))
    
    articles <- fetch_guardian_crisis(
      keywords  = keywords,
      date_from = d_from,
      date_to   = "2024-12-31",
      max_pages = 3
    )
    
    if (nrow(articles) > 0) {
      articles$crisis_id    <- row$crisis_id
      articles$country_name <- row$country_name
      articles$crisis_type  <- row$crisis_type
      all_articles[[i]]     <- articles
    }
    
    if (save_checkpoint && i %% 20 == 0) {
      df_temp <- bind_rows(all_articles)
      dir.create("data/raw/guardian", recursive = TRUE, showWarnings = FALSE)
      write_csv(df_temp, "data/raw/guardian/articles_checkpoint.csv")
      message(glue("  💾 Checkpoint : {nrow(df_temp)} articles sauvegardés"))
    }
    
    Sys.sleep(0.1)
  }
  
  bind_rows(all_articles)
}

# ------------------------------------------------------------
# EXÉCUTION
# ------------------------------------------------------------

run_guardian <- function(df_crises, save = TRUE) {
  message("\n===== COLLECTE THE GUARDIAN =====")
  
  dir.create("data/raw/guardian", recursive = TRUE, showWarnings = FALSE)
  
  df_articles <- fetch_guardian_all(df_crises, save_checkpoint = TRUE)
  
  message(glue("Articles collectés : {nrow(df_articles)}"))
  message(glue("Crises couvertes   : {n_distinct(df_articles$crisis_id)}"))
  
  df_summary <- df_articles |>
    group_by(crisis_id, country_name, crisis_type) |>
    summarise(
      nb_articles = n(),
      nb_sources  = n_distinct(section),
      date_first  = min(date_pub, na.rm = TRUE),
      date_last   = max(date_pub, na.rm = TRUE),
      .groups     = "drop"
    )
  
  if (save) {
    write_csv(df_articles, "data/raw/guardian/articles.csv")
    write_csv(df_summary,  "data/raw/guardian/summary.csv")
    cp <- "data/raw/guardian/articles_checkpoint.csv"
    if (file.exists(cp)) file.remove(cp)
    message("✓ Sauvegardé dans data/raw/guardian/")
  }
  
  list(articles = df_articles, summary = df_summary)
}
