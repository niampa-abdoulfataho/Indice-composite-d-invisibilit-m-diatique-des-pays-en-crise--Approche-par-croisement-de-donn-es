# ============================================================
# cleaner.R — Nettoyage de chaque source
# VERSION AMÉLIORÉE :
#   - Intégration données UNHCR déplacés (si disponibles)
#   - Nettoyage guardian enrichi (multi-sources)
# ============================================================

library(tidyverse)
library(lubridate)
library(countrycode)
library(janitor)
library(glue)

# ------------------------------------------------------------
# Nettoyage EM-DAT — avec filtre COVID
# ------------------------------------------------------------

clean_emdat <- function(path            = "data/raw/emdat/emdat_clean.csv",
                        filter_covid    = TRUE,
                        gdp_covid_seuil = 20000) {
  message("Nettoyage EM-DAT...")
  
  df <- read_csv(path, show_col_types = FALSE) |>
    clean_names() |>
    mutate(
      source       = "emdat",
      iso3         = countrycode(iso3, "iso3c", "iso3c", warn = FALSE),
      crisis_type  = str_to_lower(str_trim(crisis_type)),
      date_start   = as.Date(date_start),
      date_end     = as.Date(date_end),
      duree_jours  = as.numeric(date_end - date_start) + 1,
      duree_jours  = pmax(duree_jours, 1, na.rm = TRUE),
      total_deaths    = as.numeric(total_deaths),
      total_affected  = as.numeric(total_affected),
      no_homeless     = as.numeric(no_homeless),
      total_damage    = as.numeric(total_damage),
      year         = year(date_start)
    ) |>
    filter(!is.na(iso3))
  
  # Les épidémies dans les pays à haut revenu (COVID, grippe…) faussent G
  # car les volumes absolus de décès sont élevés malgré un système de santé fort.
  # On les exclut pour ne pas sur-estimer la gravité de pays comme la France ou l'Italie.
  if (filter_covid) {
    n_avant <- nrow(df)
    
    # Calcul PIB/habitant à partir des données WorldBank si disponible
    # Sinon on utilise un mapping iso3;revenu statique (Banque Mondiale)
    codelist_df <- countrycode::codelist
    income_col <- intersect(
      c("income", "wb_income", "income_group", "wb_income_group"),
      names(codelist_df)
    )[1]
    
    if (!is.na(income_col)) {
      hic_iso3 <- codelist_df |>
        filter(.data[[income_col]] %in% c("High income")) |>
        pull(iso3c) |>
        na.omit()
      
      df <- df |>
        mutate(
          is_hic     = iso3 %in% hic_iso3,
          is_epidemic_covid = crisis_type %in% c("epidemic", "pandemic")
        ) |>
        filter(!(is_epidemic_covid & is_hic)) |>
        select(-is_hic, -is_epidemic_covid)
      
      n_retires <- n_avant - nrow(df)
      message(glue("  ⚠️  Filtre COVID/épidémie pays riches : {n_retires} lignes retirées"))
      message(glue("     (critère : crisis_type ∈ {{epidemic, pandemic}} & pays HIC)"))
    } else {
      message("  ℹ️  Filtre COVID ignoré : colonne de revenu indisponible dans countrycode::codelist")
    }
  }
  
  df <- df |>
    select(source, crisis_id, iso3, country, region, subregion,
           crisis_type, crisis_subtype, date_start, date_end,
           duree_jours, year, total_deaths, total_affected,
           no_homeless, total_damage, latitude, longitude,
           had_appeal, had_declaration, had_aid)
  
  message(glue("✓ EM-DAT nettoyé : {nrow(df)} lignes"))
  df
}

# ------------------------------------------------------------
# Nettoyage UCDP
# ------------------------------------------------------------

clean_ucdp <- function(path = "data/raw/ucdp/ucdp_events.csv") {
  message("Nettoyage UCDP...")
  
  df <- read_csv(path, show_col_types = FALSE) |>
    clean_names() |>
    mutate(
      source      = "ucdp",
      iso3        = countrycode(country, "country.name", "iso3c",
                                warn = FALSE,
                                custom_match = c(
                                  "Yemen (North Yemen)"   = "YEM",
                                  "DR Congo (Zaire)"      = "COD",
                                  "Russia (Soviet Union)" = "RUS",
                                  "Serbia (Yugoslavia)"   = "SRB",
                                  "Myanmar (Burma)"       = "MMR"
                                )),
      crisis_type  = "conflict",
      date_start   = as.Date(date_event),
      year         = as.integer(year),
      deaths_best  = as.numeric(deaths_best),
      deaths_low   = as.numeric(deaths_low),
      deaths_high  = as.numeric(deaths_high)
    ) |>
    filter(!is.na(iso3)) |>
    select(source, event_id, conflict_id, iso3, country, region,
           crisis_type, event_type_label, date_start, year,
           deaths_best, deaths_low, deaths_high,
           latitude, longitude, side_a, side_b)
  
  message(glue("✓ UCDP nettoyé : {nrow(df)} lignes"))
  df
}

# ------------------------------------------------------------
# Nettoyage UCDP agrégé par pays
# ------------------------------------------------------------

clean_ucdp_country <- function(path = "data/raw/ucdp/ucdp_by_country.csv") {
  message("Nettoyage UCDP (agrégé pays)...")
  
  df <- read_csv(path, show_col_types = FALSE) |>
    clean_names() |>
    mutate(
      iso3 = countrycode(country, "country.name", "iso3c",
                         warn = FALSE,
                         custom_match = c(
                           "Yemen (North Yemen)"   = "YEM",
                           "DR Congo (Zaire)"      = "COD",
                           "Russia (Soviet Union)" = "RUS",
                           "Serbia (Yugoslavia)"   = "SRB",
                           "Myanmar (Burma)"       = "MMR"
                         ))
    ) |>
    filter(!is.na(iso3))
  
  message(glue("✓ UCDP pays nettoyé : {nrow(df)} pays"))
  df
}

# ------------------------------------------------------------
# Nettoyage Guardian
# ------------------------------------------------------------

clean_guardian <- function(path_articles = "data/raw/guardian/articles.csv",
                           path_summary  = "data/raw/guardian/summary.csv") {
  message("Nettoyage Guardian...")
  
  df_articles <- read_csv(path_articles, show_col_types = FALSE) |>
    clean_names() |>
    mutate(
      source   = "guardian",
      date_pub = as.Date(date_pub),
      year     = year(date_pub),
      wordcount = as.integer(wordcount)
    ) |>
    filter(!is.na(date_pub))
  
  df_summary <- read_csv(path_summary, show_col_types = FALSE) |>
    clean_names() |>
    mutate(
      iso3 = countrycode(country_name, "country.name", "iso3c",
                         warn = FALSE),
      date_first = as.Date(date_first),
      date_last  = as.Date(date_last)
    )
  
  message(glue("✓ Guardian nettoyé : {nrow(df_articles)} articles, {nrow(df_summary)} crises"))
  list(articles = df_articles, summary = df_summary)
}

# ------------------------------------------------------------
# Nettoyage des articles agrégés (Guardian + NewsAPI + BBC/Al Jazeera RSS)
# Tous les collecteurs doivent produire le même schéma :
#   source, article_id, title, date_pub, url, iso3, crisis_id, crisis_type
# ------------------------------------------------------------

clean_media_all <- function(
    path_guardian  = "data/raw/guardian/articles.csv",
    path_newsapi   = "data/raw/nyt/articles.csv",       
    path_rss       = "data/raw/rss/articles.csv"        
) {
  message("Fusion et nettoyage des sources médias...")
  
  # Schéma minimal attendu de chaque source
  schema_cols <- c("source", "article_id", "title", "date_pub",
                   "url", "iso3", "crisis_id", "crisis_type", "wordcount")
  
  safe_read <- function(path, src_name) {
    if (!file.exists(path)) {
      message(glue("  ℹ️  {src_name} non disponible ({path})"))
      return(NULL)
    }
    read_csv(path, show_col_types = FALSE) |>
      clean_names() |>
      mutate(source = src_name)
  }
  
  df_guardian <- safe_read(path_guardian, "guardian")
  df_newsapi  <- safe_read(path_newsapi,  "newsapi")
  df_rss      <- safe_read(path_rss,      "rss")
  
  # Harmonisation des colonnes disponibles
  harmonize <- function(df, src_name = "unknown") {
    if (is.null(df)) return(NULL)
    
    if (!"article_id" %in% names(df))   df$article_id <- NA_character_
    if (!"date_pub" %in% names(df))     df$date_pub <- NA_character_
    if (!"wordcount" %in% names(df))    df$wordcount <- NA_integer_
    if (!"iso3" %in% names(df))         df$iso3 <- NA_character_
    if (!"country_name" %in% names(df)) df$country_name <- NA_character_
    if (!"country" %in% names(df))      df$country <- NA_character_
    if (!"url" %in% names(df))          df$url <- NA_character_
    if (!"crisis_id" %in% names(df))    df$crisis_id <- NA_character_
    if (!"crisis_type" %in% names(df))  df$crisis_type <- NA_character_
    if (!"title" %in% names(df))        df$title <- NA_character_

    n_avant <- nrow(df)

    df_out <- df |>
      mutate(
        article_id  = as.character(coalesce(article_id, as.character(row_number()))),
        date_pub    = as.Date(date_pub),
        year        = year(date_pub),
        wordcount   = as.integer(wordcount),
        country_name = coalesce(country_name, country),
        iso3        = coalesce(
          iso3,
          countrycode(country_name,
                      "country.name", "iso3c", warn = FALSE)
        )
      ) |>
      filter(!is.na(date_pub), !is.na(iso3))

    message(glue("  {src_name} : {n_avant - nrow(df_out)} articles perdus (iso3/date NA)"))
    df_out
  }
  
  df_all <- bind_rows(
    harmonize(df_guardian, "guardian"),
    harmonize(df_newsapi,  "newsapi"),
    harmonize(df_rss,      "rss")
  ) |>
    # Dédoublonner au niveau article × crise pour conserver
    # les articles multi-pays rattachés à plusieurs crises.
    distinct(url, crisis_id, .keep_all = TRUE)
  
  sources_dispo <- unique(df_all$source)
  message(glue("✓ Médias fusionnés : {nrow(df_all)} articles"))
  message(glue("  Sources actives  : {paste(sources_dispo, collapse = ', ')}"))
  
  df_all
}

# ------------------------------------------------------------
# API UNHCR : https://api.unhcr.org/population/v1/idps/
# Colonnes attendues : iso3, year, idps (déplacés internes), refugees
# ------------------------------------------------------------

clean_unhcr <- function(path = "data/raw/unhcr/unhcr_displacement.csv") {
  
  if (!file.exists(path)) {
    message("  ℹ️  UNHCR non disponible — run fetch_unhcr() d'abord")
    return(NULL)
  }
  
  message("Nettoyage UNHCR déplacés...")
  
  df <- read_csv(path, show_col_types = FALSE) |>
    clean_names() |>
    mutate(
      iso3            = countrycode(iso3, "iso3c", "iso3c", warn = FALSE),
      year            = as.integer(year),
      unhcr_idps      = as.numeric(unhcr_idps),
      unhcr_refugees  = as.numeric(unhcr_refugees)
    ) |>
    filter(!is.na(iso3), !is.na(year)) |>
    group_by(iso3, year) |>
    summarise(
      unhcr_idps     = sum(unhcr_idps,     na.rm = TRUE),
      unhcr_refugees = sum(unhcr_refugees, na.rm = TRUE),
      .groups = "drop"
    )
  
  message(glue("✓ UNHCR nettoyé : {nrow(df)} lignes (pays × année)"))
  df
}

# ------------------------------------------------------------
# Nettoyage WorldBank
# ------------------------------------------------------------

clean_worldbank <- function(path = "data/raw/worldbank/worldbank.csv") {
  message("Nettoyage WorldBank...")
  
  df <- read_csv(path, show_col_types = FALSE) |>
    clean_names() |>
    mutate(
      iso3         = countrycode(iso3, "iso3c", "iso3c", warn = FALSE),
      population   = as.numeric(population),
      pib_md       = as.numeric(pib_md),
      pib_habitant = as.numeric(pib_habitant),
      dist_redactions_km = as.numeric(dist_redactions_km),
      region_wb    = coalesce(region_wb, region),
      pop_est      = coalesce(pop_est, population),
      gdp_md       = coalesce(gdp_md, pib_md, (pib_habitant * population) / 1e6),
      continent_group = case_when(
        continent == "Africa"        ~ "Afrique",
        continent == "Asia"          ~ "Asie",
        continent == "Europe"        ~ "Europe",
        continent == "North America" ~ "Amériques",
        continent == "South America" ~ "Amériques",
        continent == "Oceania"       ~ "Océanie",
        TRUE                         ~ "Autre"
      ),
      proximity_score = if_else(
        !is.na(dist_redactions_km) & dist_redactions_km > 0,
        1 / log1p(dist_redactions_km / 1000),
        1.0
      ),
      language_group = case_when(
        iso3 %in% c("USA", "GBR", "AUS", "CAN", "NZL", "IRL",
                    "ZAF", "NGA", "GHA", "KEN", "UGA", "TZA",
                    "ZMB", "ZWE", "BWA", "NAM", "SLE", "LBR") ~ "anglophone",
        iso3 %in% c("FRA", "BEL", "CHE", "CAN", "SEN", "CIV",
                    "CMR", "COD", "MDG", "BFA", "MLI", "NER",
                    "TCD", "CAF", "COG", "GAB", "HTI", "MRT") ~ "francophone",
        TRUE ~ "other"
      ),
      language_bias = case_when(
        language_group == "anglophone"  ~ 1.4,
        language_group == "francophone" ~ 1.1,
        TRUE                            ~ 1.0
      )
    ) |>
    filter(!is.na(iso3)) |>
    select(iso3, country, continent, subregion, region, region_wb,
           continent_group, population, pop_est, pib_md, gdp_md,
           pib_habitant, dist_redactions_km, proximity_score,
           language_bias, lon, lat)
  
  message(glue("✓ WorldBank nettoyé : {nrow(df)} pays"))
  df
}

# ------------------------------------------------------------
# Nettoyage ReliefWeb (scraping)
# ------------------------------------------------------------

clean_reliefweb <- function(path = "data/raw/reliefweb/reliefweb_scraped.csv") {
  message("Nettoyage ReliefWeb...")
  
  type_labels <- c(
    FL = "flood",            EP = "epidemic",
    TC = "tropical_cyclone", CW = "cold_wave",
    VO = "volcano",          FR = "fire",
    FF = "flash_flood",      LS = "landslide",
    WF = "wildfire",         ST = "storm",
    EQ = "earthquake",       AC = "accident",
    DR = "drought",          HT = "heat_wave",
    SS = "storm_surge"
  )
  
  df <- read_csv(path, show_col_types = FALSE) |>
    janitor::clean_names() |>
    mutate(
      source              = "reliefweb",
      disaster_id         = as.character(disaster_id),
      disaster_type_label = recode(disaster_type, !!!type_labels,
                                   .default = "unknown"),
      status              = str_to_lower(str_trim(status)),
      
      country_clean = case_when(
        !is.na(country) & str_length(str_trim(country)) > 0
        ~ str_trim(country),
        TRUE
        ~ str_extract(title, "^[^:]+") |> str_trim()
      ),
      
      iso3 = countrycode::countrycode(
        country_clean,
        origin      = "country.name",
        destination = "iso3c",
        warn        = FALSE
      ),
      
      year = str_extract(title, "\\d{4}") |> as.integer(),
      year = ifelse(is.na(year) | year < 2000 | year > 2025,
                    NA_integer_, year)
    ) |>
    filter(!is.na(disaster_id)) |>
    distinct(disaster_id, .keep_all = TRUE) |>
    select(source, disaster_id, iso3, country = country_clean,
           disaster_type, disaster_type_label,
           status, title, url, year)
  
  message(glue::glue("✓ ReliefWeb nettoyé : {nrow(df)} catastrophes"))
  df
}

# ------------------------------------------------------------
# EXÉCUTION COMPLÈTE
# ------------------------------------------------------------

run_cleaner <- function(save              = TRUE,
                        filter_covid      = TRUE,
                        include_unhcr     = TRUE,
                        include_multi_media = TRUE) {
  message("\n===== NETTOYAGE DES SOURCES =====")
  
  dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
  
  # Sources de base
  df_emdat  <- clean_emdat(filter_covid = filter_covid)
  df_ucdp   <- clean_ucdp()
  df_ucdp_c <- clean_ucdp_country()
  df_wb     <- clean_worldbank()
  df_rw     <- clean_reliefweb()
  
  # Médias : Guardian seul ou multi-sources
  if (include_multi_media) {
    df_media  <- clean_media_all()
  } else {
    df_media_raw <- clean_guardian()
    df_media     <- df_media_raw$articles
  }
  
  # UNHCR — enrichit le score de déplacement dans G
  df_unhcr <- if (include_unhcr) clean_unhcr() else NULL
  
  if (save) {
    write_csv(df_emdat,  "data/processed/emdat_clean.csv")
    write_csv(df_ucdp,   "data/processed/ucdp_clean.csv")
    write_csv(df_ucdp_c, "data/processed/ucdp_country_clean.csv")
    write_csv(df_media,  "data/processed/media_articles_clean.csv")
    write_csv(df_wb,     "data/processed/worldbank_clean.csv")
    write_csv(df_rw,     "data/processed/reliefweb_clean.csv")
    if (!is.null(df_unhcr))
      write_csv(df_unhcr, "data/processed/unhcr_clean.csv")
    
    message("\n✓ Tous les fichiers nettoyés sauvegardés dans data/processed/")
  }
  
  list(
    emdat  = df_emdat,
    ucdp   = df_ucdp,
    ucdp_c = df_ucdp_c,
    media  = df_media,
    wb     = df_wb,
    rw     = df_rw,
    unhcr  = df_unhcr
  )
}
