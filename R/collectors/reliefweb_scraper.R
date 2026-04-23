# ============================================================
# reliefweb_scraper.R — Scraping ReliefWeb (rvest)
# Source : https://reliefweb.int/disasters
# Méthode : scraping HTML avec rvest + pagination 
# ============================================================

library(rvest)
library(httr2)
library(tidyverse)
library(lubridate)
library(glue)

build_reliefweb_key <- function(disaster_id, title, url, country, disaster_type) {
  paste(
    coalesce(disaster_id, ""),
    coalesce(title, ""),
    coalesce(url, ""),
    coalesce(country, ""),
    coalesce(disaster_type, ""),
    sep = "||"
  )
}

# ------------------------------------------------------------
# Scraper une page de la liste des catastrophes
# ------------------------------------------------------------

#' Scrappe une page de la liste des catastrophes ReliefWeb
#'
#' @param page numéro de page (commence à 0)
#' @return tibble des catastrophes sur cette page, ou NULL si vide
scrape_reliefweb_page <- function(page = 0) {
  
  url <- glue("https://reliefweb.int/disasters?page={page}")
  
  Sys.sleep(1)
  
  html <- tryCatch(
    request(url) |>
      req_headers(
        `User-Agent` = "Mozilla/5.0 (academic research project)",
        `Accept`     = "text/html"
      ) |>
      req_retry(max_tries = 3, backoff = ~ 2 ^ .x) |>
      req_perform() |>
      resp_body_html(),
    error = function(e) {
      message(glue(" ERREUR requête : {e$message}"))
      NULL
    }
  )
  
  if (is.null(html)) return(NULL)
  
  articles <- html |>
    html_elements("article.rw-river-article--disaster")
  
  if (length(articles) == 0) return(NULL)
  
  map_dfr(articles, function(art) {
    
    # CORRECTION : gestion de href NA avant startsWith()
    href <- art |> html_element("a") |> html_attr("href")
    url_full <- if (!is.na(href) && startsWith(href, "http")) {
      href
    } else if (!is.na(href)) {
      paste0("https://reliefweb.int", href)
    } else {
      NA_character_
    }
    
    tibble(
      disaster_id   = art |> html_attr("data-id"),
      disaster_type = art |> html_attr("data-disaster-type"),
      status        = art |> html_attr("data-disaster-status"),
      title         = art |> html_element(".rw-river-article__title") |>
        html_text2() |> str_trim(),
      url           = url_full,
      country       = art |> html_element(".rw-entity-country-slug__link") |>
        html_text2() |> str_trim()
    )
  })
}

# ------------------------------------------------------------
# Scraper les détails d'une catastrophe individuelle
# ------------------------------------------------------------

#' Scrappe la page détail d'une catastrophe
#'
#' @param url URL de la catastrophe
#' @return liste avec description, date, pays affectés
scrape_reliefweb_detail <- function(url) {
  
  Sys.sleep(0.5)
  
  tryCatch({
    html <- request(url) |>
      req_headers(`User-Agent` = "Mozilla/5.0 (academic research)") |>
      req_timeout(15) |>
      req_perform() |>
      resp_body_html()
    
    title <- html |>
      html_element("h1.rw-page-title") |>
      html_text2() |> str_trim()
    
    status <- html |>
      html_element("[class*='tag-value--status']") |>
      html_text2() |> str_trim()
    
    glide <- html |>
      html_element("[class*='tag-value--glide']") |>
      html_text2() |> str_trim()
    
    description <- html |>
      html_element("#overview-content") |>
      html_text2() |> str_trim() |>
      (\(x) substr(x, 1, 500))()
    
    countries <- html |>
      html_elements(".rw-river-article--country .rw-river-article__title a") |>
      html_text2() |>
      paste(collapse = "; ")
    
    list(
      title       = title,
      status      = status,
      glide       = glide,
      description = description,
      countries   = countries
    )
  }, error = function(e) {
    list(title = NA, status = NA, glide = NA,
         description = NA, countries = NA)
  })
}

# ------------------------------------------------------------
# Scraping complet avec pagination
# ------------------------------------------------------------

#' Lance le scraping complet de ReliefWeb
#'
#' @param max_pages       nb max de pages à scraper
#' @param get_details     scraper les pages détail (plus lent)
#' @param save_checkpoint sauvegarder toutes les 10 pages
#' @return tibble des catastrophes dédupliquées
scrape_reliefweb_all <- function(max_pages        = 185,
                                 get_details      = FALSE,
                                 save_checkpoint  = TRUE) {
  
  dir.create("data/raw/reliefweb", recursive = TRUE, showWarnings = FALSE)
  
  message(glue("Scraping ReliefWeb — {max_pages} pages prévues..."))
  
  all_results <- list()
  seen_keys   <- character(0)
  pages_vides <- 0
  
  for (page in 0:(max_pages - 1)) {
    message(glue("  Page {page + 1}/{max_pages}..."), appendLF = FALSE)
    
    result <- tryCatch(
      scrape_reliefweb_page(page),
      error = function(e) {
        message(glue(" ERREUR : {e$message}"))
        NULL
      }
    )
    
    if (is.null(result) || nrow(result) == 0) {
      pages_vides <- pages_vides + 1
      message(" (vide)")
      # Arrêter après 3 pages vides consécutives
      if (pages_vides >= 3) {
        message("\n  → Fin de la pagination détectée (3 pages vides consécutives)")
        break
      }
      next
    }
    
    pages_vides <- 0  # réinitialiser compteur
    
    page_keys <- build_reliefweb_key(
      disaster_id   = result$disaster_id,
      title         = result$title,
      url           = result$url,
      country       = result$country,
      disaster_type = result$disaster_type
    )
    overlap <- sum(page_keys %in% seen_keys)
    
    if (length(page_keys) > 0 && overlap == length(page_keys)) {
      message(glue(" {nrow(result)} entrées — TOUTES déjà vues (clé composite)"))
      message("\n  → Arrêt : le site renvoie les mêmes données en boucle")
      break
    }
    
    seen_keys <- c(seen_keys, page_keys)
    all_results[[page + 1]] <- result
    message(glue(" {nrow(result)} catastrophes ({overlap} doublons)"))
    
    # Checkpoint toutes les 10 pages
    if (save_checkpoint && (page + 1) %% 10 == 0) {
      df_temp <- bind_rows(all_results)
      write_csv(df_temp,
                "data/raw/reliefweb/reliefweb_scraped_checkpoint.csv")
      message(glue("  💾 Checkpoint sauvegardé ({nrow(df_temp)} lignes)"))
    }
  }
  
  df <- bind_rows(all_results)
  message(glue("\n✓ Total brut : {nrow(df)} lignes"))
  
  # Optionnel : enrichir avec les détails de chaque page
  if (get_details && nrow(df) > 0) {
    message("Scraping des pages détail...")
    details <- map(df$url, function(u) {
      if (is.na(u)) return(list(description = NA, countries = NA, glide = NA))
      message(glue("  → {u}"))
      scrape_reliefweb_detail(u)
    })
    
    df <- df |>
      mutate(
        description = map_chr(details, ~ .x$description %||% NA_character_),
        countries   = map_chr(details, ~ .x$countries   %||% NA_character_),
        glide       = map_chr(details, ~ .x$glide        %||% NA_character_)
      )
  }
  
  df
}

# ------------------------------------------------------------
# EXÉCUTION
# ------------------------------------------------------------

run_reliefweb_scraper <- function(max_pages   = 185,
                                  get_details = FALSE,
                                  save        = TRUE) {
  
  message("\n===== SCRAPING RELIEFWEB =====")
  
  dir.create("data/raw/reliefweb", recursive = TRUE, showWarnings = FALSE)
  
  df <- scrape_reliefweb_all(
    max_pages       = max_pages,
    get_details     = get_details,
    save_checkpoint = TRUE
  )
  
  # Dédupliquer sur une clé composite pour éviter d'écraser
  # des contenus distincts partageant le même disaster_id
  n_avant <- nrow(df)
  df <- df |>
    mutate(
      reliefweb_key = build_reliefweb_key(
        disaster_id   = disaster_id,
        title         = title,
        url           = url,
        country       = country,
        disaster_type = disaster_type
      )
    ) |>
    distinct(reliefweb_key, .keep_all = TRUE) |>
    select(-reliefweb_key)
  message(glue("Dédoublonnage : {n_avant} → {nrow(df)} catastrophes uniques"))
  
  # Statistiques
  message(glue("\nTypes   : {paste(unique(df$disaster_type), collapse=', ')}"))
  message(glue("Statuts : {paste(unique(df$status), collapse=', ')}"))
  
  if (save) {
    output_path <- "data/raw/reliefweb/reliefweb_scraped.csv"
    write_csv(df, output_path)
    message(glue("✓ Sauvegardé dans {output_path}"))
    
    checkpoint <- "data/raw/reliefweb/reliefweb_scraped_checkpoint.csv"
    if (file.exists(checkpoint)) file.remove(checkpoint)
  }
  
  df
}
