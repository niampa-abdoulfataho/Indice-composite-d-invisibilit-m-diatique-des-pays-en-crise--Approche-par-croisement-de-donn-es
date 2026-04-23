# ============================================================
# scorer.R — Construction des indices
# G (Gravité) | M (Médiatique) | I (Invisibilité)
# B (Biais géo) | V (Volatilité)
# ============================================================

library(tidyverse)
library(glue)

# ------------------------------------------------------------
# Fonctions utilitaires
# ------------------------------------------------------------

normalize_minmax <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (rng[1] == rng[2]) return(rep(50, length(x)))
  (x - rng[1]) / (rng[2] - rng[1]) * 100
}

normalize_robust <- function(x) {
  med <- median(x, na.rm = TRUE)
  mad <- mad(x, na.rm = TRUE)
  if (mad == 0) return(normalize_minmax(x))
  score <- (x - med) / (mad + 1e-6)
  normalize_minmax(score)
}

fill_score <- function(x) replace(x, is.na(x), 0)

# ------------------------------------------------------------
# Score de Gravité (G) — AMÉLIORÉ avec UNHCR
# ------------------------------------------------------------

#' Calcule le Score de Gravité pour chaque pays/année
#' Composantes :
#'   - Mortalité relative    : total_deaths_all / pop          (35%)
#'   - Déplacement enrichi   : max(emdat, unhcr) / pop         (20%)
#'   - Personnes affectées   : total_affected_nat / pop        (15%)
#'   - Intensité conflits    : nb_conflict_events / pop        (20%)
#'   - Nb de crises          : log1p(nb_disasters + nb_rw)     (10%)
#'
#' @param df tibble issu de merger.R (avec colonnes unhcr_idps optionnelles)
#' @return df enrichi avec G et ses composantes
compute_gravity_score <- function(df) {
  message("Calcul du Score de Gravité (G)...")
  

  has_unhcr <- all(c("unhcr_idps", "unhcr_refugees") %in% names(df))
  
  if (has_unhcr) {
    message("  ✓ Données UNHCR disponibles — déplacement enrichi")
    df <- df |>
      mutate(
        deplacement_enrichi = pmax(
          fill_score(total_homeless_nat),
          fill_score(unhcr_idps) + fill_score(unhcr_refugees),
          na.rm = TRUE
        )
      )
  } else {
    message("  ℹ️  Pas de données UNHCR — utilisation EM-DAT seul pour déplacement")
    df <- df |>
      mutate(deplacement_enrichi = fill_score(total_homeless_nat))
  }
  
  df <- df |>
    mutate(
      pop = pmax(dplyr::coalesce(.data$pop_est, 1), 1, na.rm = TRUE),
      
      # Composantes brutes
      raw_mortalite   = fill_score(total_deaths_all) / pop * 1e6,
      raw_deplacement = deplacement_enrichi / pop * 1e6,
      raw_affectes    = fill_score(total_affected_nat) / pop * 1e6,
      raw_conflits    = fill_score(nb_conflict_events) / pop * 1e6,
      raw_nb_crises   = log1p(fill_score(nb_disasters) +
                                fill_score(nb_rw_disasters)),
      
      # Normalisation robuste
      score_mortalite   = normalize_robust(raw_mortalite),
      score_deplacement = normalize_robust(raw_deplacement),
      score_affectes    = normalize_robust(raw_affectes),
      score_conflits    = normalize_robust(raw_conflits),
      score_nb_crises   = normalize_minmax(raw_nb_crises),
      
      # Score composite G pondéré (poids théoriques)
      G = 0.35 * score_mortalite   +
          0.20 * score_deplacement +
          0.15 * score_affectes    +
          0.20 * score_conflits    +
          0.10 * score_nb_crises
    )
  
  message(glue("  G moyen : {round(mean(df$G, na.rm=TRUE), 2)}"))
  message(glue("  G max   : {round(max(df$G, na.rm=TRUE), 2)} ({df$country[which.max(df$G)]}, {df$year[which.max(df$G)]})"))
  df
}



#' @param df tibble avec colonnes raw_* calculées par compute_gravity_score()
#' @return liste : df enrichi avec G_acp + tibble des poids ACP
compute_gravity_pca <- function(df) {
  
  if (!requireNamespace("FactoMineR", quietly = TRUE)) {
    message("  ℹ️  FactoMineR non installé — skip ACP (install.packages('FactoMineR'))")
    return(list(df = df, poids = NULL))
  }
  
  message("\n  ACP sur les composantes de G...")
  
  df_acp <- df |>
    select(raw_mortalite, raw_deplacement, raw_affectes,
           raw_conflits, raw_nb_crises) |>
    na.omit()
  
  res_acp <- FactoMineR::PCA(df_acp, scale.unit = TRUE, graph = FALSE)
  
  # Poids = contribution des variables au PC1 (somme = 100%)
  contrib_pc1    <- res_acp$var$contrib[, 1]
  poids_acp      <- contrib_pc1 / sum(contrib_pc1)
  
  # Affichage comparatif
  poids_theoriques <- c(
    raw_mortalite   = 0.35,
    raw_deplacement = 0.20,
    raw_affectes    = 0.15,
    raw_conflits    = 0.20,
    raw_nb_crises   = 0.10
  )
  
  df_poids <- tibble(
    composante         = names(poids_acp),
    poids_theorique    = poids_theoriques[names(poids_acp)],
    poids_acp          = round(poids_acp, 3),
    variance_pc1_pct   = round(contrib_pc1, 1)
  )
  
  message("\n  Comparaison poids théoriques vs ACP :")
  print(df_poids)
  message(glue("  Variance expliquée PC1 : {round(res_acp$eig[1,2], 1)}%"))
  
  # Recompute G avec poids ACP
  df <- df |>
    mutate(
      G_acp = poids_acp["raw_mortalite"]   * score_mortalite   +
              poids_acp["raw_deplacement"]  * score_deplacement +
              poids_acp["raw_affectes"]     * score_affectes    +
              poids_acp["raw_conflits"]     * score_conflits    +
              poids_acp["raw_nb_crises"]    * score_nb_crises,
      
      # Corrélation G vs G_acp : si > 0.95, les poids théoriques sont validés
      .after = G
    )
  
  cor_g <- cor(df$G, df$G_acp, use = "complete.obs")
  message(glue("  Corrélation G (théorique) vs G_acp : r = {round(cor_g, 3)}"))
  if (cor_g > 0.95)
    message("  → Poids théoriques validés empiriquement ✅")
  else
    message("  → Écart notable — envisager les poids ACP ⚠️")
  
  list(df = df, poids = df_poids, res_acp = res_acp)
}

# ------------------------------------------------------------
# Score Médiatique (M)
# ------------------------------------------------------------

compute_media_score <- function(df) {
  message("Calcul du Score Médiatique (M)...")
  
  df <- df |>
    mutate(
      M_brut = log1p(fill_score(nb_articles_total)) *
               log1p(fill_score(nb_sources_uniq) + 1),
      
      facteur_proximite    = pmax(dplyr::coalesce(proximity_score, 1.0), 0.01),
      facteur_linguistique = pmax(dplyr::coalesce(language_bias,   1.0), 1.0),
      
      M_corrige = M_brut / (facteur_proximite * facteur_linguistique),
      M = normalize_minmax(M_corrige)
    )
  
  message(glue("  M moyen      : {round(mean(df$M, na.rm=TRUE), 2)}"))
  message(glue("  Pays avec M > 0 : {sum(df$M > 0, na.rm=TRUE)}"))
  df
}

# ------------------------------------------------------------
# Indice d'Invisibilité (I)
# ------------------------------------------------------------

compute_invisibility_index <- function(df) {
  message("Calcul de l'Indice d'Invisibilité (I)...")
  
  epsilon <- 0.5
  
  df <- df |>
    mutate(
      I = G / (M + epsilon),
      
      zone = case_when(
        I > 5  ~ "Rouge (invisible)",
        I > 2  ~ "Orange (sous-couvert)",
        I > 1  ~ "Verte (proportionnel)",
        TRUE   ~ "Bleue (sur-médiatisé)"
      ),
      zone = factor(zone, levels = c(
        "Rouge (invisible)",
        "Orange (sous-couvert)",
        "Verte (proportionnel)",
        "Bleue (sur-médiatisé)"
      ))
    )
  
  message("\n  Distribution par zone :")
  df |>
    count(zone) |>
    mutate(pct = round(n / sum(n) * 100, 1)) |>
    mutate(msg = glue("    {zone} : {n} ({pct}%)")) |>
    pull(msg) |>
    walk(message)
  
  df
}

# ------------------------------------------------------------
# Indice de Biais Géographique (B)
# ------------------------------------------------------------

compute_geographic_bias <- function(df) {
  message("\nCalcul de l'Indice de Biais Géographique (B)...")
  
  # Intégrer FTS si disponible
  has_fts <- "fts_total_usd" %in% names(df)
  
  df_bias <- df |>
    filter(!is.na(continent_group)) |>
    group_by(continent_group) |>
    summarise(
      mean_G         = mean(G,                  na.rm = TRUE),
      mean_M         = mean(M,                  na.rm = TRUE),
      mean_I         = mean(I,                  na.rm = TRUE),
      nb_pays        = n_distinct(iso3),
      total_deaths   = sum(total_deaths_all,    na.rm = TRUE),
      total_articles = sum(nb_articles_total,   na.rm = TRUE),
      # FTS si disponible
      total_fts_usd  = if (has_fts) sum(fts_total_usd, na.rm = TRUE) else NA_real_,
      .groups = "drop"
    ) |>
    mutate(
      B = mean_M / (mean_G + 0.01),
      biais_label = case_when(
        B > 1.5 ~ "Très sur-couvert",
        B > 1.0 ~ "Sur-couvert",
        B > 0.5 ~ "Sous-couvert",
        TRUE    ~ "Très sous-couvert"
      )
    ) |>
    arrange(desc(mean_I))
  
  print(df_bias |>
    select(continent_group, mean_G, mean_M, B, biais_label) |>
    mutate(across(where(is.numeric), ~ round(.x, 2))))
  
  df_bias
}


#' @param df_articles tibble des articles avec colonnes : iso3, date_pub
#' @param df_annuel   tibble annuel (fallback si trop peu d'articles mensuels)
#' @return tibble de volatilité par pays : iso3, V, V_stl, peak_forget
compute_volatility_score <- function(df_annuel, df_articles = NULL) {
  message("\nCalcul du Score de Volatilité (V)...")
  
  # ---- Méthode STL (si articles mensuels disponibles) ----
  df_vol_stl <- NULL
  
  if (!is.null(df_articles) &&
      all(c("iso3", "date_pub") %in% names(df_articles)) &&
      requireNamespace("forecast", quietly = TRUE)) {
    
    message("  Méthode STL (séries mensuelles)...")
    
    df_monthly <- df_articles |>
      filter(!is.na(date_pub), !is.na(iso3)) |>
      mutate(month = lubridate::floor_date(as.Date(date_pub), "month")) |>
      count(iso3, month, name = "n_articles") |>
      tidyr::complete(iso3, month, fill = list(n_articles = 0))
    
    # Calculer STL uniquement pour les pays avec >= 24 mois de données
    iso3_eligible <- df_monthly |>
      count(iso3) |>
      filter(n >= 24) |>
      pull(iso3)
    
    message(glue("  {length(iso3_eligible)} pays éligibles pour STL"))
    
    df_vol_stl <- df_monthly |>
      filter(iso3 %in% iso3_eligible) |>
      group_by(iso3) |>
      arrange(month, .by_group = TRUE) |>
      summarise(
        ts_data  = list(ts(n_articles, frequency = 12)),
        .groups  = "drop"
      ) |>
      mutate(
        stl_fit = purrr::map(ts_data, function(x) {
          tryCatch(
            stl(x, s.window = "periodic", robust = TRUE),
            error = function(e) NULL
          )
        }),
        # Variance du terme remainder = bruit résiduel après tendance + saison
        V_stl = purrr::map_dbl(stl_fit, function(fit) {
          if (is.null(fit)) return(NA_real_)
          remainder_vals <- forecast::remainder(fit)[, 1]
          sd(remainder_vals, na.rm = TRUE) /
            (mean(abs(remainder_vals), na.rm = TRUE) + 1e-6)
        })
      ) |>
      select(iso3, V_stl) |>
      filter(!is.na(V_stl))
    
    message(glue("  STL calculé pour {nrow(df_vol_stl)} pays"))
  }
  
  # ---- Méthode classique sd/mean (fallback ou comparaison) ----
  df_vol <- df_annuel |>
    filter(!is.na(nb_articles_total)) |>
    group_by(iso3, country, continent_group) |>
    summarise(
      mean_articles = mean(nb_articles_total, na.rm = TRUE),
      sd_articles   = sd(nb_articles_total,   na.rm = TRUE),
      nb_annees     = n(),
      .groups = "drop"
    ) |>
    mutate(
      V = ifelse(mean_articles > 0,
                 sd_articles / (mean_articles + 1e-6),
                 NA_real_)
    )
  
  # ---- Fusion : STL si disponible, sinon sd/mean ----
  if (!is.null(df_vol_stl) && nrow(df_vol_stl) > 0) {
    df_vol <- df_vol |>
      left_join(df_vol_stl, by = "iso3") |>
      mutate(
        # Utiliser STL si disponible, sd/mean sinon
        V_final      = coalesce(V_stl, V),
        peak_forget  = V_final > 1.0 & mean_articles > 0,
        methode_V    = ifelse(!is.na(V_stl), "STL", "sd/mean")
      )
    
    n_stl <- sum(df_vol$methode_V == "STL",   na.rm = TRUE)
    n_cv  <- sum(df_vol$methode_V == "sd/mean", na.rm = TRUE)
    message(glue("  Méthode STL    : {n_stl} pays"))
    message(glue("  Méthode sd/mean: {n_cv} pays (< 24 mois de données)"))
  } else {
    df_vol <- df_vol |>
      mutate(
        V_final     = V,
        peak_forget = V > 1.0 & mean_articles > 0,
        methode_V   = "sd/mean"
      )
  }
  
  n_pf <- sum(df_vol$peak_forget, na.rm = TRUE)
  message(glue("  Pays avec effet 'Peak & Forget' (V > 1) : {n_pf}"))
  
  df_vol
}


#' @param df      tibble final des indices
#' @param df_fts  tibble agrégé FTS (iso3, year, fts_total_usd, fts_log_usd)
#' @return df enrichi avec les colonnes FTS
merge_fts_indices <- function(df, df_fts) {
  
  if (is.null(df_fts) || nrow(df_fts) == 0) {
    message("  ℹ️  FTS non disponible — H4 utilisera had_appeal")
    return(df)
  }
  
  df |>
    left_join(
      df_fts |> select(iso3, year, fts_total_usd, fts_log_usd, fts_nb_plans),
      by = c("iso3", "year")
    ) |>
    mutate(
      has_fts_data    = !is.na(fts_total_usd) & fts_total_usd > 0,
      fts_log_usd     = coalesce(fts_log_usd, 0),
      fts_total_usd   = coalesce(fts_total_usd, 0)
    )
}

# ------------------------------------------------------------
# Validation
# ------------------------------------------------------------

validate_index <- function(df) {
  message("\n===== VALIDATION DE L'INDICE =====")
  
  cas_test <- c("UKR", "YEM", "ETH", "SDN", "TUR")
  
  df |>
    filter(iso3 %in% cas_test) |>
    group_by(iso3, country) |>
    summarise(
      G_moy = round(mean(G, na.rm = TRUE), 2),
      M_moy = round(mean(M, na.rm = TRUE), 2),
      I_moy = round(mean(I, na.rm = TRUE), 2),
      zone  = names(sort(table(zone), decreasing = TRUE))[1],
      .groups = "drop"
    ) |>
    mutate(
      attendu = case_when(
        iso3 == "UKR" ~ "I faible (sur-médiatisé)",
        iso3 == "YEM" ~ "I élevé (invisible)",
        iso3 == "ETH" ~ "I élevé (invisible)",
        iso3 == "SDN" ~ "I élevé (invisible)",
        iso3 == "TUR" ~ "I moyen"
      ),
      coherent = case_when(
        iso3 == "UKR" & I_moy < 5  ~ "✅",
        iso3 %in% c("YEM","ETH","SDN") & I_moy > 3 ~ "✅",
        iso3 == "TUR" ~ "ℹ️",
        TRUE ~ "⚠️  À revoir"
      )
    ) |>
    print()
}

print_top_invisible <- function(df) {
  message("\n===== TOP 10 CRISES LES PLUS INVISIBLES =====")
  
  df |>
    group_by(iso3, country, continent_group) |>
    summarise(
      G_moy          = round(mean(G, na.rm = TRUE), 1),
      M_moy          = round(mean(M, na.rm = TRUE), 1),
      I_moy          = round(mean(I, na.rm = TRUE), 1),
      morts_total    = sum(total_deaths_all,   na.rm = TRUE),
      articles_total = sum(nb_articles_total,  na.rm = TRUE),
      gdp_habitant   = mean(
        .data$gdp_md * 1e6 / pmax(.data$pop_est, 1, na.rm = TRUE),
        na.rm = TRUE
      ),
      pop_moy        = mean(.data$pop_est, na.rm = TRUE),
      .groups = "drop"
    ) |>
    filter(
      G_moy > 2,
      continent_group %in% c("Afrique", "Asie", "Amériques"),
      gdp_habitant < 15000,
      pop_moy      < 200e6,
      morts_total  > 500
    ) |>
    slice_max(I_moy, n = 10) |>
    select(pays = country, continent = continent_group,
           G_moy, M_moy, I_moy, morts_total, articles_total) |>
    print(n = 10)
}

# ------------------------------------------------------------
# EXÉCUTION COMPLÈTE
# ------------------------------------------------------------

run_scorer <- function(save             = TRUE,
                       run_pca          = FALSE,
                       df_articles_raw  = NULL,   # pour STL
                       df_fts           = NULL) {  # pour H4
  
  message("\n===== CONSTRUCTION DES INDICES =====")
  
  dir.create("data/final", recursive = TRUE, showWarnings = FALSE)
  
  df <- read_csv("data/processed/crises_merged.csv",
                 show_col_types = FALSE)
  message(glue("Dataset chargé : {nrow(df)} lignes, {n_distinct(df$iso3)} pays"))
  
  # Calcul des indices
  df <- compute_gravity_score(df)
  df <- compute_media_score(df)
  df <- compute_invisibility_index(df)
  
  # ACP optionnelle pour validation des poids G
  if (run_pca) {
    pca_res <- compute_gravity_pca(df)
    df      <- pca_res$df
  }
  
  # Intégrer FTS pour H4
  if (!is.null(df_fts))
    df <- merge_fts_indices(df, df_fts)
  
  # Indices agrégés
  df_bias <- compute_geographic_bias(df)
  df_vol  <- compute_volatility_score(df, df_articles_raw)
  
  # Validation + Top 10
  validate_index(df)
  print_top_invisible(df)
  
  # Sauvegarde
  if (save) {
    write_csv(df,      "data/final/crises_indices.csv")
    write_csv(df_bias, "data/final/biais_continental.csv")
    write_csv(df_vol,  "data/final/volatilite_media.csv")
    message("\n✓ Fichiers sauvegardés dans data/final/")
  }
  
  list(
    indices = df,
    biais   = df_bias,
    vol     = df_vol
  )
}
