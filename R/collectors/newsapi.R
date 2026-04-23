# ============================================================
# newsapi.R — Collecteurs médias multi-sources
# Sources : NYT Article Search API | BBC RSS | Al Jazeera RSS
#
# NYT Article Search API :
#   - Gratuit, clé simple
#   - Historique depuis 1851
#   - 500 req/jour, 5 req/minute → Sys.sleep(12) entre requêtes
#   - 10 articles/page, jusqu'à 100 pages = 1000 articles/requête
#   - Doc : https://developer.nytimes.com/docs/articlesearch-product/1/overview
#
# Clé API : stocker dans .Renviron 
# ============================================================

library(httr)
library(httr2)
library(tidyverse)
library(lubridate)
library(glue)
library(xml2)

# ------------------------------------------------------------
# Clé API NYT
# Stockez votre clé dans .Renviron : NYT_API_KEY=votre_clé
# Ou passez-la directement en argument de run_newsapi()
# ------------------------------------------------------------

NYT_BASE <- "https://api.nytimes.com/svc/search/v2/articlesearch.json"

# ------------------------------------------------------------
# Mots-clés par type de crise
# ------------------------------------------------------------

news_crisis_keywords <- list(
  "Flood"             = c("flood", "flooding"),
  "Drought"           = c("drought", "famine", "food crisis"),
  "Complex Emergency" = c("war", "conflict", "humanitarian crisis"),
  "Earthquake"        = c("earthquake"),
  "Epidemic"          = c("epidemic", "outbreak", "disease"),
  "Cyclone"           = c("cyclone", "hurricane", "typhoon"),
  "Violence"          = c("violence", "massacre", "displacement")
)

news_clean_country_name <- function(name) {
  name |>
    str_replace_all("[''`]",              " ") |>
    str_replace_all("[\\(\\)\\[\\]]",    " ") |>
    str_replace_all("[&]",               "and") |>
    str_replace_all("[^a-zA-Z0-9 \\-]", " ") |>
    str_squish()
}

#' Génère une requête NYT : "pays" AND ("terme1" OR "terme2")
news_generate_keywords <- function(country_name, crisis_type) {
  type_clean   <- str_trim(str_split(crisis_type, ";")[[1]][1])
  synonyms     <- news_crisis_keywords[[type_clean]] %||% c("humanitarian", "crisis")
  country_safe <- news_clean_country_name(country_name)
  synonyms_str <- paste0('("', paste(synonyms, collapse = '" OR "'), '")')
  glue('"{country_safe}" AND {synonyms_str}')
}

build_rss_patterns <- function(country_name, crisis_type) {
  country_safe <- news_clean_country_name(country_name)
  type_clean   <- str_trim(str_split(crisis_type, ";")[[1]][1])
  synonyms     <- news_crisis_keywords[[type_clean]] %||% c("humanitarian", "crisis")
  
  escape_term <- function(x) {
    str_replace_all(x, "([.+*?^${}()|\\[\\]\\\\])", "\\\\\\1")
  }
  
  list(
    country_pattern = escape_term(str_to_lower(country_safe)),
    synonym_pattern = paste(
      vapply(str_to_lower(synonyms), escape_term, character(1)),
      collapse = "|"
    )
  )
}

# ------------------------------------------------------------
# SOURCE 1 — NYT Article Search API
# ------------------------------------------------------------

#' Formate une date pour NYT (YYYYMMDD)
nyt_fmt <- function(date) format(as.Date(date), "%Y%m%d")

#' Collecte une page NYT (10 articles)
#'
#' @param keywords  requête (ex: '"Yemen" AND ("war" OR "conflict")')
#' @param date_from date début YYYY-MM-DD
#' @param date_to   date fin   YYYY-MM-DD
#' @param page      numéro de page (0-indexé, max 99)
#' @param api_key   clé API NYT
#' @return tibble ou tibble vide
fetch_nyt_page <- function(keywords, date_from, date_to, page = 0, api_key) {
  
  resp <- tryCatch(
    httr::GET(
      NYT_BASE,
      query = list(
        q          = keywords,
        begin_date = nyt_fmt(date_from),
        end_date   = nyt_fmt(date_to),
        sort       = "newest",
        page       = page,
        `api-key`  = api_key
      ),
      httr::timeout(30),
      httr::user_agent("Mozilla/5.0 (academic research)")
    ),
    error = function(e) {
      message(glue("    ⚠️  NYT erreur : {e$message}"))
      NULL
    }
  )
  
  status <- if (is.null(resp)) 0L else httr::status_code(resp)
  
  if (status == 429) {
    message("    ⏳ 429 reçu — pause 60s...")
    Sys.sleep(60)
    return(list(articles = tibble(), hits = 0L))
  }
  
  if (is.null(resp) || status != 200) {
    message(glue("    ⚠️  NYT HTTP {status}"))
    return(list(articles = tibble(), hits = 0L))
  }
  
  body <- tryCatch(
    httr::content(resp, as = "parsed", type = "application/json"),
    error = function(e) NULL
  )
  
  if (is.null(body) || is.null(body$response) ||
      length(body$response$docs) == 0) {
    return(list(articles = tibble(), hits = 0L))
  }
  
  hits <- body$response$meta$hits %||% 0L
  
  df <- map_dfr(body$response$docs, function(doc) {
    tibble(
      title       = doc$headline$main        %||% NA_character_,
      outlet      = "New York Times",
      date_pub    = tryCatch(
        as.Date(str_sub(doc$pub_date %||% "", 1, 10)),
        error = function(e) NA_Date_
      ),
      url         = doc$web_url              %||% NA_character_,
      source_type = "nyt",
      note        = doc$section_name         %||% NA_character_
    )
  }) |>
    filter(!is.na(title), !is.na(date_pub), !is.na(url))
  
  list(articles = df, hits = hits)
}

#' Collecte NYT pour une crise — pagination automatique
#'
#' NYT : 10 articles/page, max 100 pages (1000 articles)
#' Pause 12s entre pages (limite : 5 req/minute)
#'
#' @param keywords  requête
#' @param date_from date début
#' @param date_to   date fin
#' @param api_key   clé API NYT
#' @param max_pages nombre max de pages (défaut : 10 = 100 articles)
#' @return tibble dédupliqué
fetch_nyt_crisis <- function(keywords,
                             date_from  = "2020-01-01",
                             date_to    = "2024-12-31",
                             api_key,
                             max_pages  = 10) {
  
  all_dfs <- list()
  
  for (page in seq(0, max_pages - 1)) {
    
    res <- fetch_nyt_page(keywords, date_from, date_to, page, api_key)
    df_page <- res$articles
    hits    <- res$hits
    
    if (page == 0) {
      n_pages_total <- min(ceiling(hits / 10), 100, max_pages)
      message(glue("     {hits} articles disponibles — collecte de {n_pages_total} page(s)"))
      if (hits == 0) break
    }
    
    if (nrow(df_page) == 0) break
    
    all_dfs[[length(all_dfs) + 1]] <- df_page
    message(glue("     page {page + 1}/{n_pages_total} : {nrow(df_page)} articles"))
    
    # Stop si on a tout récupéré
    if (page >= n_pages_total - 1) break
    
    Sys.sleep(12)   # 5 req/minute max
  }
  
  df <- bind_rows(all_dfs)
  if (nrow(df) == 0 || !"url" %in% names(df)) return(tibble())
  
  df |>
    distinct(url, .keep_all = TRUE) |>
    arrange(desc(date_pub))
}

# ------------------------------------------------------------
# SOURCE 2 — BBC RSS
# ------------------------------------------------------------

BBC_FEEDS <- c(
  world   = "https://feeds.bbci.co.uk/news/world/rss.xml",
  africa  = "https://feeds.bbci.co.uk/news/world/africa/rss.xml",
  asia    = "https://feeds.bbci.co.uk/news/world/asia/rss.xml",
  mideast = "https://feeds.bbci.co.uk/news/world/middle_east/rss.xml",
  latin   = "https://feeds.bbci.co.uk/news/world/latin_america/rss.xml"
)

fetch_bbc_rss <- function() {
  message("  Collecte BBC RSS...")
  all_items <- list()
  
  for (feed_name in names(BBC_FEEDS)) {
    tryCatch({
      resp <- request(BBC_FEEDS[[feed_name]]) |>
        req_headers(`User-Agent` = "Mozilla/5.0 (academic research)") |>
        req_timeout(15) |>
        req_perform()
      
      if (resp_status(resp) != 200) next
      
      xml   <- resp_body_string(resp) |> read_xml()
      items <- xml_find_all(xml, ".//item")
      if (length(items) == 0) next
      
      df <- map_dfr(items, function(item) {
        tibble(
          title       = xml_text(xml_find_first(item, "title")),
          outlet      = glue("BBC {str_to_title(feed_name)}"),
          date_pub    = xml_text(xml_find_first(item, "pubDate")) |>
            parse_date_time(
              orders = c("a, d b Y H:M:S z", "d b Y H:M:S z"),
              quiet  = TRUE
            ) |> as.Date(),
          url         = xml_text(xml_find_first(item, "link")),
          source_type = "bbc_rss",
          note        = "rss_recent"
        )
      })
      
      all_items[[feed_name]] <- df
      message(glue("    BBC {feed_name} : {nrow(df)} articles"))
      
    }, error = function(e) {
      message(glue("    ⚠️  BBC {feed_name} : {e$message}"))
    })
    Sys.sleep(0.3)
  }
  
  bind_rows(all_items)
}

# ------------------------------------------------------------
# SOURCE 3 — Al Jazeera RSS
# ------------------------------------------------------------

AJE_FEED <- "https://www.aljazeera.com/xml/rss/all.xml"

fetch_aljazeera_rss <- function() {
  message("  Collecte Al Jazeera RSS...")
  
  tryCatch({
    resp <- request(AJE_FEED) |>
      req_headers(`User-Agent` = "Mozilla/5.0 (academic research)") |>
      req_timeout(15) |>
      req_perform()
    
    if (resp_status(resp) != 200) {
      message(glue("  ⚠️  AJE : HTTP {resp_status(resp)}"))
      return(tibble())
    }
    
    xml   <- resp_body_string(resp) |> read_xml()
    items <- xml_find_all(xml, ".//item")
    if (length(items) == 0) return(tibble())
    
    df <- map_dfr(items, function(item) {
      tibble(
        title       = xml_text(xml_find_first(item, "title")),
        outlet      = "Al Jazeera",
        date_pub    = xml_text(xml_find_first(item, "pubDate")) |>
          parse_date_time(
            orders = c("a, d b Y H:M:S z", "d b Y H:M:S z"),
            quiet  = TRUE
          ) |> as.Date(),
        url         = xml_text(xml_find_first(item, "link")),
        source_type = "aljazeera_rss",
        note        = "rss_recent"
      )
    })
    
    message(glue("    Al Jazeera : {nrow(df)} articles"))
    df
    
  }, error = function(e) {
    message(glue("  ⚠️  AJE : {e$message}"))
    tibble()
  })
}

# ------------------------------------------------------------
# Filtrage RSS par pays/crise
# ------------------------------------------------------------

filter_rss_by_crisis <- function(df_rss, country_name, crisis_type) {
  if (nrow(df_rss) == 0) return(tibble())
  
  patterns <- build_rss_patterns(country_name, crisis_type)
  
  df_rss |>
    mutate(title_lower = str_to_lower(title)) |>
    filter(
      !is.na(title),
      str_detect(title_lower, patterns$country_pattern),
      str_detect(title_lower, patterns$synonym_pattern)
    ) |>
    select(-title_lower)
}

# ------------------------------------------------------------
# Collecte complète multi-sources
# ------------------------------------------------------------

#' Lance la collecte NYT + BBC RSS + AJE pour toutes les crises
#'
#' @param df_crises       tibble des crises (crisis_id, iso3_primary,
#'                        name, crisis_type, date_created)
#' @param date_from       date début globale
#' @param date_to         date fin globale
#' @param api_key         clé API NYT (défaut : variable NYT_API_KEY)
#' @param max_pages       pages NYT par crise (10 = 100 articles, max 100)
#' @param save_checkpoint sauvegarder toutes les 10 crises
fetch_multisource_all <- function(df_crises,
                                  date_from        = "2020-01-01",
                                  date_to          = "2024-12-31",
                                  api_key          = Sys.getenv("NYT_API_KEY"),
                                  max_pages        = 10,
                                  save_checkpoint  = TRUE,
                                  start_index      = 1,
                                  end_index        = NULL,
                                  checkpoint_path  = "data/raw/nyt/articles_checkpoint.csv",
                                  resume           = FALSE) {
  library(countrycode)
  
  if (api_key == "") stop("Clé API NYT manquante. Définir NYT_API_KEY dans .Renviron")
  
  df_crises <- df_crises |>
    mutate(
      country_name = countrycode(iso3_primary, "iso3c",
                                 "country.name", warn = FALSE),
      country_name = news_clean_country_name(country_name)
    )
  
  # Charger les flux RSS une seule fois
  message("Chargement des flux RSS...")
  df_bbc <- fetch_bbc_rss()
  df_aje <- fetch_aljazeera_rss()
  df_rss <- bind_rows(df_bbc, df_aje)
  message(glue("  RSS total : {nrow(df_rss)} articles chargés\n"))
  
  all_articles <- list()
  n <- nrow(df_crises)
  if (is.null(end_index)) end_index <- n
  start_index <- max(1L, as.integer(start_index))
  end_index   <- min(n, as.integer(end_index))
  indices     <- seq.int(start_index, end_index)
  
  if (length(indices) == 0) {
    message("Aucune crise a traiter pour cette plage.")
    return(tibble())
  }
  
  if (resume && file.exists(checkpoint_path)) {
    df_checkpoint <- read_csv(checkpoint_path, show_col_types = FALSE)
    if (nrow(df_checkpoint) > 0) {
      all_articles[[1]] <- df_checkpoint
      done_ids <- unique(df_checkpoint$crisis_id)
      indices  <- indices[!(df_crises$crisis_id[indices] %in% done_ids)]
      message(glue("Reprise depuis checkpoint : {length(done_ids)} crises deja presentes"))
    }
  }
  
  if (length(indices) == 0) {
    message("Aucune nouvelle crise a traiter apres reprise.")
    return(bind_rows(all_articles))
  }
  
  message(glue("Collecte batch : crises {min(indices)} a {max(indices)} sur {n}"))
  
  for (i in indices) {
    row      <- df_crises[i, ]
    keywords <- news_generate_keywords(row$country_name, row$crisis_type)
    d_from   <- as.character(coalesce(row$date_created, as.Date(date_from)))
    d_to     <- date_to
    
    message(glue("[{i}/{n}] {row$name}  ({d_from} → {d_to})"))
    message(glue("  Requête NYT : {keywords}"))
    
    # NYT
    message("  → NYT...")
    df_nyt <- fetch_nyt_crisis(
      keywords  = keywords,
      date_from = d_from,
      date_to   = d_to,
      api_key   = api_key,
      max_pages = max_pages
    )
    message(glue("     {nrow(df_nyt)} articles NYT retenus"))
    
    # RSS
    df_rss_crisis <- filter_rss_by_crisis(
      df_rss,
      country_name = row$country_name,
      crisis_type  = row$crisis_type
    )
    message(glue("     {nrow(df_rss_crisis)} articles RSS"))
    
    df_combined <- bind_rows(df_nyt, df_rss_crisis)
    
    if (nrow(df_combined) == 0) {
      message("  → 0 articles au total\n")
      next
    }
    
    if (!"url"  %in% names(df_combined)) df_combined$url  <- NA_character_
    if (!"note" %in% names(df_combined)) df_combined$note <- NA_character_
    
    df_crisis <- df_combined |>
      mutate(
        crisis_id    = row$crisis_id,
        country_name = row$country_name,
        crisis_type  = row$crisis_type
      ) |>
      select(crisis_id, country_name, crisis_type,
             title, date_pub, outlet, source_type, url, note) |>
      distinct(url, title, .keep_all = TRUE) |>
      filter(!is.na(title), !is.na(date_pub))
    
    message(glue("  → {nrow(df_crisis)} articles retenus\n"))
    all_articles[[i]] <- df_crisis
    
    if (save_checkpoint && i %% 10 == 0) {
      df_temp <- bind_rows(all_articles)
      dir.create("data/raw/nyt", recursive = TRUE, showWarnings = FALSE)
      write_csv(df_temp, checkpoint_path)
      message(glue("  💾 Checkpoint : {nrow(df_temp)} articles"))
    }
    
    # Pause entre crises (respecter 5 req/min)
    if (i < max(indices)) Sys.sleep(12)
  }
  
  bind_rows(all_articles)
}

# ------------------------------------------------------------
# EXÉCUTION
# ------------------------------------------------------------

run_newsapi <- function(df_crises,
                        date_from = "2020-01-01",
                        date_to   = "2024-12-31",
                        api_key   = Sys.getenv("NYT_API_KEY"),
                        max_pages = 10,
                        save      = TRUE,
                        start_index = 1,
                        end_index   = NULL,
                        resume      = FALSE,
                        checkpoint_path = "data/raw/nyt/articles_checkpoint.csv") {
  
  message("\n===== COLLECTE MÉDIAS MULTI-SOURCES (NYT + RSS) =====")
  
  dir.create("data/raw/nyt", recursive = TRUE, showWarnings = FALSE)
  
  df_raw <- fetch_multisource_all(
    df_crises,
    date_from       = date_from,
    date_to         = date_to,
    api_key         = api_key,
    max_pages       = max_pages,
    save_checkpoint = save,
    start_index     = start_index,
    end_index       = end_index,
    checkpoint_path = checkpoint_path,
    resume          = resume
  )
  
  if (nrow(df_raw) == 0) {
    message("  ⚠️  Aucun article collecté")
    return(list(
      articles = tibble(
        crisis_id = character(), country_name = character(),
        crisis_type = character(), title = character(),
        date_pub = as.Date(NA), outlet = character(),
        source_type = character(), url = character(),
        note = character()
      ),
      summary = tibble()
    ))
  }
  
  # Déduplication finale par (crisis_id + url)
  df_articles <- df_raw |>
    distinct(crisis_id, url, .keep_all = TRUE) |>
    filter(!is.na(title), !is.na(date_pub))
  
  message(glue("\n✓ Articles collectés : {nrow(df_articles)}"))
  
  message("\n  Répartition par source :")
  df_articles |>
    count(source_type, sort = TRUE) |>
    mutate(pct = round(n / sum(n) * 100, 1)) |>
    print()
  
  df_summary <- df_articles |>
    group_by(crisis_id, country_name, crisis_type) |>
    summarise(
      nb_articles = n(),
      nb_sources  = n_distinct(source_type),
      date_first  = min(date_pub, na.rm = TRUE),
      date_last   = max(date_pub, na.rm = TRUE),
      .groups     = "drop"
    )
  
  if (save) {
    articles_path <- "data/raw/nyt/articles.csv"
    summary_path  <- "data/raw/nyt/summary.csv"
    
    if (file.exists(articles_path)) {
      df_existing <- read_csv(articles_path, show_col_types = FALSE)
      df_articles <- bind_rows(df_existing, df_articles) |>
        distinct(crisis_id, url, title, .keep_all = TRUE) |>
        filter(!is.na(title), !is.na(date_pub))
    }
    
    df_summary <- df_articles |>
      group_by(crisis_id, country_name, crisis_type) |>
      summarise(
        nb_articles = n(),
        nb_sources  = n_distinct(source_type),
        date_first  = min(date_pub, na.rm = TRUE),
        date_last   = max(date_pub, na.rm = TRUE),
        .groups     = "drop"
      )
    
    write_csv(df_articles, articles_path)
    write_csv(df_summary,  summary_path)
    cp <- "data/raw/nyt/articles_checkpoint.csv"
    if (file.exists(cp)) file.remove(cp)
    message("✓ Sauvegardé dans data/raw/nyt/")
  }
  
  list(articles = df_articles, summary = df_summary)
}
