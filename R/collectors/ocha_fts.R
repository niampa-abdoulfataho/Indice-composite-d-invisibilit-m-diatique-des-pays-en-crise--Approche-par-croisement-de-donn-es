# ============================================================
# ocha_fts.R - Collecteurs OCHA/FTS et UNHCR
#
#   - API FTS : flux de financement humanitaire par pays/annee
#     https://api.hpc.tools/v1/public/fts/flow
#   - API UNHCR : population figures (refugies / IDPs) par pays/annee
#     https://api.unhcr.org/population/v1/population/
# ============================================================

library(httr2)
library(tidyverse)
library(lubridate)
library(glue)
library(countrycode)

# ============================================================
# PARTIE 1 - OCHA Financial Tracking Service (FTS)
# ============================================================

#' Recupere les flux de financement FTS pour une annee
#'
#' @param year annee (ex: 2022)
#' @param limit nb max de resultats par requete
#' @return tibble des flux humanitaires
fetch_fts_year <- function(year, limit = 500) {
  base_url <- "https://api.hpc.tools/v1/public/fts/flow"
  page     <- 1
  all_data <- list()
  
  repeat {
    resp <- tryCatch(
      request(base_url) |>
        req_url_query(
          year    = year,
          limit   = limit,
          page    = page
        ) |>
        req_headers(Accept = "application/json") |>
        req_retry(max_tries = 3, backoff = ~ 2 ^ .x) |>
        req_perform(),
      error = function(e) {
        message(glue("  FTS {year} page {page} : {e$message}"))
        NULL
      }
    )
    
    if (is.null(resp) || resp_status(resp) != 200) break
    
    data  <- resp_body_json(resp, simplifyVector = FALSE)
    flows <- data$data$flows %||% list()
    
    if (length(flows) == 0) break
    all_data <- c(all_data, flows)
    
    total      <- data$meta$count %||% 0
    total_page <- ceiling(total / limit)
    if (page >= total_page) break
    page <- page + 1
    
    Sys.sleep(0.3)
  }
  
  if (length(all_data) == 0) {
    message(glue("  FTS {year} : aucune donnee"))
    return(tibble())
  }
  
  map_dfr(all_data, function(f) {
    find_object_name <- function(objects, object_type) {
      if (is.null(objects) || length(objects) == 0) return(NA_character_)
      matches <- keep(objects, ~ identical(.x$type %||% NA_character_, object_type))
      if (length(matches) == 0) return(NA_character_)
      matches[[1]]$name %||% NA_character_
    }
    
    dest_location <- find_object_name(f$destinationObjects, "Location")
    source_location <- find_object_name(f$sourceObjects, "Location")
    source_org <- find_object_name(f$sourceObjects, "Organization")
    dest_org <- find_object_name(f$destinationObjects, "Organization")
    dest_plan <- find_object_name(f$destinationObjects, "Plan")
    
    iso3 <- countrycode::countrycode(
      dest_location,
      origin = "country.name",
      destination = "iso3c",
      warn = FALSE
    )
    
    tibble(
      fts_id          = as.character(f$id %||% NA),
      year            = as.integer(year),
      iso3            = iso3,
      destination     = dest_location,
      source_location = source_location,
      amount_usd      = as.numeric(f$amountUSD %||% NA),
      status          = f$status %||% NA_character_,
      boundary        = f$boundary %||% NA_character_,
      flow_type       = f$flowType %||% NA_character_,
      source_org      = source_org,
      dest_org        = dest_org,
      dest_plan       = dest_plan
    )
  })
}

#' Collecte FTS sur plusieurs annees
#'
#' @param years vecteur d'annees
#' @param save sauvegarder les resultats intermediaires
#' @return tibble brut de tous les flux
fetch_fts_all <- function(years = 2020:2024, save = TRUE) {
  message("\n===== COLLECTE OCHA/FTS =====")
  dir.create("data/raw/fts", recursive = TRUE, showWarnings = FALSE)
  
  df_all <- map_dfr(years, function(yr) {
    message(glue("  FTS {yr}..."))
    df <- fetch_fts_year(yr)
    if (save && nrow(df) > 0) {
      write_csv(df, glue("data/raw/fts/fts_{yr}.csv"))
    }
    df
  })
  
  message(glue("FTS total : {nrow(df_all)} flux ({length(years)} annees)"))
  df_all
}

#' Agrege les flux FTS par pays/annee
#'
#' @param df_fts tibble brut de fetch_fts_all()
#' @return tibble agrege
aggregate_fts <- function(df_fts) {
  if (nrow(df_fts) == 0 || !"iso3" %in% names(df_fts)) {
    message("  FTS : aucune donnee a agreger")
    return(tibble(
      iso3          = character(),
      year          = integer(),
      fts_total_usd = numeric(),
      fts_nb_flux   = integer(),
      fts_nb_plans  = integer(),
      fts_log_usd   = numeric()
    ))
  }
  
  df_fts |>
    filter(
      !is.na(iso3),
      !is.na(amount_usd),
      amount_usd > 0,
      boundary == "incoming"
    ) |>
    group_by(iso3, year) |>
    summarise(
      fts_total_usd = sum(amount_usd, na.rm = TRUE),
      fts_nb_flux   = n(),
      fts_nb_plans  = n_distinct(dest_plan, na.rm = TRUE),
      fts_log_usd   = log1p(sum(amount_usd, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    arrange(desc(fts_total_usd))
}

# ============================================================
# PARTIE 2 - UNHCR Population Data
# ============================================================

UNHCR_BASE <- "https://api.unhcr.org/population/v1"

extract_unhcr_items <- function(data) {
  if (!is.null(data$items) && length(data$items) > 0) {
    data$items
  } else if (!is.null(data$results) && length(data$results) > 0) {
    data$results
  } else if (!is.null(data$data) && length(data$data) > 0) {
    data$data
  } else {
    list()
  }
}

pick_first_existing <- function(df, candidates, default = NA_character_) {
  match <- candidates[candidates %in% names(df)][1]
  if (is.na(match)) {
    rep(default, nrow(df))
  } else {
    df[[match]]
  }
}

fetch_unhcr_population_type <- function(pop_col, years = 2020:2024, limit = 1000) {
  map_dfr(years, function(yr) {
    page    <- 1
    rows_yr <- list()
    
    repeat {
      resp <- tryCatch(
        request(glue("{UNHCR_BASE}/population/")) |>
          req_url_query(
            year     = yr,
            page     = page,
            limit    = limit,
            coo_all  = "true",
            cf_type  = "ISO",
            columns  = pop_col
          ) |>
          req_headers(Accept = "application/json") |>
          req_retry(max_tries = 3) |>
          req_perform(),
        error = function(e) {
          message(glue("    UNHCR population {pop_col} {yr} page {page} : {e$message}"))
          NULL
        }
      )
      
      if (is.null(resp) || resp_status(resp) != 200) break
      
      data <- resp_body_json(resp, simplifyVector = TRUE)
      items <- extract_unhcr_items(data)
      
      if (length(items) == 0) break
      
      page_df <- as_tibble(items)
      if (nrow(page_df) == 0) break
      rows_yr[[length(rows_yr) + 1]] <- page_df
      
      returned_n <- nrow(page_df)
      total_pages <- dplyr::coalesce(
        suppressWarnings(as.integer(data$total_pages)),
        suppressWarnings(as.integer(data$pages)),
        suppressWarnings(as.integer(data$meta$pages)),
        NA_integer_
      )
      
      if (returned_n < limit) break
      if (!is.na(total_pages) && page >= total_pages) break
      
      page <- page + 1
      
      Sys.sleep(0.2)
    }
    
    if (length(rows_yr) == 0) {
      return(tibble(
        year  = integer(),
        iso3  = character(),
        value = numeric()
      ))
    }
    
    df_year <- bind_rows(rows_yr)
    iso3_col <- pick_first_existing(df_year, c("coo_iso", "coo", "coa_iso", "coa"))
    value_col <- pick_first_existing(df_year, c(pop_col, "value", "total"))
    
    tibble(
      year  = as.integer(yr),
      iso3  = as.character(iso3_col),
      value = as.numeric(value_col)
    ) |>
      filter(!is.na(iso3))
  })
}

#' Recupere les deplaces internes (IDPs) par pays/annee
#'
#' @param years vecteur d'annees
#' @return tibble IDPs
fetch_unhcr_idps <- function(years = 2020:2024) {
  message("  Collecte UNHCR IDPs...")
  
  fetch_unhcr_population_type("idps", years = years) |>
    rename(unhcr_idps = value)
}

#' Recupere les refugies par pays d'origine/annee
#'
#' @param years vecteur d'annees
#' @return tibble refugies par pays d'origine
fetch_unhcr_refugees <- function(years = 2020:2024) {
  message("  Collecte UNHCR refugies...")
  
  fetch_unhcr_population_type("refugees", years = years) |>
    rename(unhcr_refugees = value)
}

#' Fusionne IDPs + refugies en une seule table par pays/annee
#'
#' @param years annees a collecter
#' @return tibble avec unhcr_idps, unhcr_refugees, unhcr_total_deplacement
fetch_unhcr_all <- function(years = 2020:2024) {
  message("\n===== COLLECTE UNHCR =====")
  
  df_idps     <- fetch_unhcr_idps(years)
  df_refugees <- fetch_unhcr_refugees(years)
  
  df_unhcr <- full_join(df_idps, df_refugees, by = c("iso3", "year")) |>
    mutate(
      unhcr_idps              = coalesce(unhcr_idps, 0),
      unhcr_refugees          = coalesce(unhcr_refugees, 0),
      unhcr_total_deplacement = unhcr_idps + unhcr_refugees
    ) |>
    arrange(desc(unhcr_total_deplacement))
  
  message(glue("UNHCR : {nrow(df_unhcr)} lignes (pays x annee)"))
  
  if (nrow(df_unhcr) > 0) {
    message("  Top 5 pays deplaces :")
    df_unhcr |>
      group_by(iso3) |>
      summarise(total = sum(unhcr_total_deplacement), .groups = "drop") |>
      slice_max(total, n = 5) |>
      mutate(
        country = countrycode::countrycode(iso3, "iso3c", "country.name",
                                           warn = FALSE),
        total   = scales::number(total, big.mark = " ")
      ) |>
      select(pays = country, total) |>
      print()
  }
  
  df_unhcr
}

# ============================================================
# EXECUTION COMPLETE
# ============================================================

run_ocha_unhcr <- function(years = 2020:2024, save = TRUE) {
  message("\n===== OCHA/FTS + UNHCR =====")
  
  df_fts_raw <- fetch_fts_all(years = years, save = FALSE)
  df_fts     <- aggregate_fts(df_fts_raw)
  df_unhcr   <- fetch_unhcr_all(years = years)
  
  if (save) {
    dir.create("data/raw/fts", recursive = TRUE, showWarnings = FALSE)
    dir.create("data/raw/unhcr", recursive = TRUE, showWarnings = FALSE)
    
    write_csv(df_fts_raw, "data/raw/fts/fts_raw.csv")
    write_csv(df_fts,     "data/raw/fts/fts_aggregated.csv")
    write_csv(df_unhcr,   "data/raw/unhcr/unhcr_displacement.csv")
    
    message("\nSauvegardes dans data/raw/fts/ et data/raw/unhcr/")
  }
  
  list(fts = df_fts, unhcr = df_unhcr, fts_raw = df_fts_raw)
}
