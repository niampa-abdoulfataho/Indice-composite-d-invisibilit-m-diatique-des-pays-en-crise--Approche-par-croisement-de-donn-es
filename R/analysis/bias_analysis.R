# ============================================================
# bias_analysis.R — Test des 4 hypothèses
#   H1 : Biais Sud global vs Nord global  (+modèle mixte)
#   H2 : Biais de proximité géographique  (+modèle mixte)
#   H3 : Peak & Forget (crises aiguës vs chroniques)
#   H4 : Couverture / financement OCHA/FTS (remplace had_appeal)
# ============================================================

library(tidyverse)
library(glue)

# ------------------------------------------------------------
# H1 — Biais continental
# ------------------------------------------------------------

#' Test H1 : biais Sud global vs Nord global

#' univariés (Wilcoxon) qui peuvent créer de faux positifs.
test_H1_biais_continental <- function(df) {
  message("\n===== H1 : BIAIS CONTINENTAL =====")
  message("H1 : Les crises du Sud global sont sous-représentées")
  message("     médiatiquement par rapport à leur gravité.\n")
  
  df_h1 <- df |>
    filter(!is.na(continent_group), !is.na(I)) |>
    mutate(
      continent_group = fct_reorder(continent_group, I, .fun = median),
      nord_sud = case_when(
        continent_group == "Europe"                        ~ "Nord global",
        continent_group == "Amériques" &
          iso3 %in% c("USA", "CAN")                       ~ "Nord global",
        TRUE                                               ~ "Sud global"
      )
    )
  
  # Test de Kruskal-Wallis par continent
  kw_test <- kruskal.test(I ~ continent_group, data = df_h1)
  message(glue("  Kruskal-Wallis : p-value = {round(kw_test$p.value, 4)}"))
  
  if (kw_test$p.value < 0.05)
    message("  → Différences significatives entre continents ✅")
  else
    message("  → Pas de différences significatives ⚠️")
  
  # Médiane de I par continent
  message("\n  Indice d'Invisibilité médian par continent :")
  df_h1 |>
    group_by(continent_group) |>
    summarise(
      I_median = round(median(I, na.rm = TRUE), 2),
      I_mean   = round(mean(I,   na.rm = TRUE), 2),
      G_mean   = round(mean(G,   na.rm = TRUE), 2),
      M_mean   = round(mean(M,   na.rm = TRUE), 2),
      n_obs    = n(), .groups = "drop"
    ) |>
    arrange(desc(I_median)) |>
    print()
  
  # Wilcoxon Sud vs Nord
  wilcox_ns <- wilcox.test(I ~ nord_sud, data = df_h1,
                            alternative = "greater")
  message(glue("\n  Wilcoxon Sud vs Nord : p-value = {round(wilcox_ns$p.value, 4)}"))
  
  if (wilcox_ns$p.value < 0.05)
    message("  → H1 CONFIRMÉE : le Sud global est significativement plus invisible ✅")
  else
    message("  → H1 NON CONFIRMÉE ⚠️")
  
  model_h1 <- run_mixed_model(
    df       = df_h1,
    formule  = "I ~ nord_sud + log(gdp_habitant_calc + 1) + G",
    label    = "H1 (Sud/Nord contrôlé)"
  )
  
  list(kruskal = kw_test, wilcoxon_ns = wilcox_ns,
       model = model_h1, data = df_h1)
}

# ------------------------------------------------------------
# H2 — Biais de proximité géographique  (+modèle mixte)
# ------------------------------------------------------------

#' Test H2 : biais de proximité
test_H2_biais_proximite <- function(df) {
  message("\n===== H2 : BIAIS DE PROXIMITÉ =====")
  message("H2 : La couverture médiatique est corrélée négativement")
  message("     à la distance des grandes rédactions.\n")
  
  df_h2 <- df |>
    filter(!is.na(dist_redactions_km), !is.na(M), !is.na(G)) |>
    mutate(
      gdp_habitant_calc = gdp_md * 1e6 / pmax(pop_est, 1, na.rm = TRUE)
    )
  
  # Corrélations Spearman
  cor_M <- cor.test(df_h2$dist_redactions_km, df_h2$M, method = "spearman")
  cor_I <- cor.test(df_h2$dist_redactions_km, df_h2$I, method = "spearman")
  
  message(glue("  Corrélation Spearman (distance ~ M) : rho = {round(cor_M$estimate, 3)}, p = {round(cor_M$p.value, 4)}"))
  message(glue("  Corrélation Spearman (distance ~ I) : rho = {round(cor_I$estimate, 3)}, p = {round(cor_I$p.value, 4)}"))
  
  if (cor_I$estimate > 0 & cor_I$p.value < 0.05)
    message("  → H2 CONFIRMÉE via I : plus c'est loin, plus c'est invisible ✅")
  else if (cor_M$estimate < 0 & cor_M$p.value < 0.05)
    message("  → H2 CONFIRMÉE via M : plus c'est loin, moins c'est couvert ✅")
  else
    message("  → H2 NON CONFIRMÉE ⚠️")
  

  model_h2 <- run_mixed_model(
    df      = df_h2,
    formule = "I ~ dist_redactions_km + continent_group + log(gdp_habitant_calc + 1) + G",
    label   = "H2 (distance + confondeurs)"
  )
  
  # Groupes distance
  message("\n  Score M et I moyen par groupe de distance :")
  df_h2 |>
    mutate(
      groupe_distance = case_when(
        dist_redactions_km < 4000  ~ "Proche (< 4 000 km)",
        dist_redactions_km < 8000  ~ "Moyen (4-8 000 km)",
        TRUE                        ~ "Loin (> 8 000 km)"
      ),
      groupe_distance = factor(groupe_distance, levels = c(
        "Proche (< 4 000 km)", "Moyen (4-8 000 km)", "Loin (> 8 000 km)"
      ))
    ) |>
    group_by(groupe_distance) |>
    summarise(
      M_mean = round(mean(M, na.rm = TRUE), 2),
      I_mean = round(mean(I, na.rm = TRUE), 2),
      G_mean = round(mean(G, na.rm = TRUE), 2),
      n = n(), .groups = "drop"
    ) |>
    print()
  
  list(cor_distance_M = cor_M, cor_distance_I = cor_I,
       model = model_h2)
}

#' @param df      tibble
#' @param formule chaîne de caractères (sans effet aléatoire iso3)
#' @param label   label pour le message
#' @return objet lmer ou NULL si lme4 indisponible
run_mixed_model <- function(df, formule, label = "") {
  
  if (!requireNamespace("lme4", quietly = TRUE)) {
    message("  ℹ️  lme4 non installé — modèle mixte ignoré (install.packages('lme4'))")
    return(NULL)
  }
  
  formule_full <- paste0(formule, " + (1 | iso3)")
  
  tryCatch({
    model <- lme4::lmer(
      as.formula(formule_full),
      data    = df,
      REML    = FALSE,
      control = lme4::lmerControl(optimizer = "bobyqa")
    )
    
    message(glue("\n  Modèle mixte {label} :"))
    message(glue("  Formule : {formule_full}"))
    
    coefs <- summary(model)$coefficients
    message("  Coefficients :")
    print(round(coefs, 4))
    
    # Variance expliquée approx (r² marginale)
    if (requireNamespace("MuMIn", quietly = TRUE)) {
      r2 <- MuMIn::r.squaredGLMM(model)
      message(glue("  R² marginal (effets fixes) : {round(r2[1], 3)}"))
      message(glue("  R² conditionnel (+ aléat.) : {round(r2[2], 3)}"))
    }
    
    model
  }, error = function(e) {
    message(glue("  ⚠️  Modèle mixte {label} : {e$message}"))
    # Fallback : régression simple sans effet aléatoire
    message("  → Fallback : lm() sans effet aléatoire")
    tryCatch(
      lm(as.formula(formule), data = df),
      error = function(e2) NULL
    )
  })
}

# ------------------------------------------------------------
# H3 — Peak & Forget
# ------------------------------------------------------------

test_H3_peak_forget <- function(df, df_vol) {
  message("\n===== H3 : PEAK & FORGET =====")
  message("H3 : Les crises aiguës reçoivent une couverture intense")
  message("     mais éphémère (Peak & Forget).\n")
  
  df_types <- df |>
    group_by(iso3, country) |>
    summarise(
      type_crise = ifelse(
        any(str_detect(
          coalesce(types_disasters, ""),
          "earthquake|cyclone|flood|storm|wildfire"
        ), na.rm = TRUE),
        "Aiguë", "Chronique"
      ),
      .groups = "drop"
    )
  
  # Utiliser V_final (STL si dispo, sinon sd/mean)
  v_col <- if ("V_final" %in% names(df_vol)) "V_final" else "V"
  
  df_vol_distinct <- df_vol |>
    select(iso3, V = all_of(v_col), peak_forget) |>
    distinct(iso3, .keep_all = TRUE)
  
  df_h3 <- df_types |>
    left_join(df_vol_distinct, by = "iso3") |>
    filter(!is.na(V))
  
  message(glue("  Crises aiguës     : {sum(df_h3$type_crise == 'Aiguë')}"))
  message(glue("  Crises chroniques : {sum(df_h3$type_crise == 'Chronique')}"))
  
  message("\n  Volatilité médiane par type de crise :")
  df_h3 |>
    group_by(type_crise) |>
    summarise(
      V_median        = round(median(V,           na.rm = TRUE), 3),
      V_mean          = round(mean(V,             na.rm = TRUE), 3),
      pct_peak_forget = round(mean(peak_forget,   na.rm = TRUE) * 100, 1),
      n               = n(), .groups = "drop"
    ) |>
    print()
  
  wilcox <- wilcox.test(V ~ type_crise, data = df_h3,
                         alternative = "two.sided")
  
  message(glue("\n  Wilcoxon aiguë vs chronique : p-value = {round(wilcox$p.value, 4)}"))
  
  if (wilcox$p.value < 0.05)
    message("  → H3 CONFIRMÉE : différence significative de volatilité ✅")
  else
    message("  → H3 NON CONFIRMÉE ⚠️")
  
  list(data = df_h3, wilcoxon = wilcox)
}

#' Test H4 : corrélation couverture / financement humanitaire
test_H4_financement <- function(df) {
  message("\n===== H4 : COUVERTURE ET FINANCEMENT =====")
  message("H4 : Les crises avec plus de couverture médiatique")
  message("     reçoivent plus de financement humanitaire.\n")
  
  has_fts <- "fts_total_usd" %in% names(df) &&
              sum(!is.na(df$fts_total_usd) & df$fts_total_usd > 0) > 50
  
  if (has_fts) {
    
    message("  ✓ Données FTS disponibles — analyse continue")
    
    df_h4 <- df |>
      filter(!is.na(fts_log_usd), !is.na(M), !is.na(G)) |>
      mutate(fts_log_usd = as.numeric(fts_log_usd))
    
    message(glue("  N observations avec FTS : {nrow(df_h4)}"))
    
    # Corrélation Spearman M ~ financement
    cor_mf <- cor.test(df_h4$M, df_h4$fts_log_usd, method = "spearman")
    message(glue("\n  Corrélation Spearman (M ~ log FTS) :"))
    message(glue("    rho = {round(cor_mf$estimate, 3)}"))
    message(glue("    p-value = {round(cor_mf$p.value, 4)}"))
    
    # Corrélation Spearman I ~ financement (crises invisibles = moins financées ?)
    cor_if <- cor.test(df_h4$I, df_h4$fts_log_usd, method = "spearman")
    message(glue("\n  Corrélation Spearman (I ~ log FTS) :"))
    message(glue("    rho = {round(cor_if$estimate, 3)} (négatif attendu)"))
    message(glue("    p-value = {round(cor_if$p.value, 4)}"))
    
    # Régression contrôlant la gravité : financement ~ M + G
    model_h4 <- tryCatch({
      lm(fts_log_usd ~ M + G, data = df_h4)
    }, error = function(e) NULL)
    
    if (!is.null(model_h4)) {
      message("\n  Régression log(FTS) ~ M + G :")
      print(round(summary(model_h4)$coefficients, 4))
      r2 <- summary(model_h4)$r.squared
      message(glue("  R² = {round(r2, 3)}"))
    }
    
    if (cor_mf$p.value < 0.05 && cor_mf$estimate > 0)
      message("  → H4 CONFIRMÉE (FTS) : couverture M corrélée au financement ✅")
    else
      message("  → H4 NON CONFIRMÉE via FTS ⚠️")
    
    return(list(cor_M_fts = cor_mf, cor_I_fts = cor_if, model = model_h4))
  }
  
  # ---- Fallback : had_appeal (méthode originale) ----
  message("  ℹ️  FTS non disponible — utilisation de had_appeal (méthode originale)")
  message(glue("  Note : seulement 23 crises avec appel → faible puissance statistique"))
  message(glue("  → Exécuter run_ocha_unhcr() pour enrichir H4\n"))
  
  df_h4 <- df |>
    filter(!is.na(had_appeal), !is.na(M), !is.na(G)) |>
    mutate(had_appeal = as.logical(had_appeal))
  
  message(glue("  Crises avec appel humanitaire : {sum(df_h4$had_appeal, na.rm=TRUE)}"))
  message(glue("  Crises sans appel             : {sum(!df_h4$had_appeal, na.rm=TRUE)}"))
  
  df_h4 |>
    group_by(had_appeal) |>
    summarise(
      M_median = round(median(M, na.rm = TRUE), 2),
      M_mean   = round(mean(M,   na.rm = TRUE), 2),
      G_mean   = round(mean(G,   na.rm = TRUE), 2),
      I_mean   = round(mean(I,   na.rm = TRUE), 2),
      n        = n(), .groups = "drop"
    ) |>
    print()
  
  wilcox <- wilcox.test(M ~ had_appeal, data = df_h4,
                         alternative = "two.sided")
  
  message(glue("\n  Wilcoxon (avec appel vs sans) : p-value = {round(wilcox$p.value, 4)}"))
  
  cor_test <- cor.test(as.numeric(df_h4$had_appeal), df_h4$I,
                        method = "spearman")
  
  list(wilcoxon = wilcox, cor_appel_I = cor_test)
}

# ------------------------------------------------------------
# EXÉCUTION COMPLÈTE
# ------------------------------------------------------------

run_bias_analysis <- function(df, df_vol) {
  message("\n===== ANALYSE DES BIAIS & HYPOTHÈSES =====")
  
  # Calcul PIB/habitant si absent
  if (!"gdp_habitant_calc" %in% names(df)) {
    df <- df |>
      mutate(gdp_habitant_calc = gdp_md * 1e6 / pmax(pop_est, 1, na.rm = TRUE))
  }
  
  res_H1 <- test_H1_biais_continental(df)
  res_H2 <- test_H2_biais_proximite(df)
  res_H3 <- test_H3_peak_forget(df, df_vol)
  res_H4 <- test_H4_financement(df)
  
  # Résumé final
  message("\n===== RÉSUMÉ DES HYPOTHÈSES =====")
  
  # Extraire la p-value de H4 selon la méthode utilisée
  p_h4 <- if (!is.null(res_H4$cor_M_fts))
    round(res_H4$cor_M_fts$p.value, 4)
  else
    round(res_H4$wilcoxon$p.value, 4)
  
  dir_h4 <- if (!is.null(res_H4$cor_M_fts))
    glue("rho(M~FTS) = {round(res_H4$cor_M_fts$estimate, 3)}")
  else
    glue("rho = {round(res_H4$cor_appel_I$estimate, 3)}")
  
  resultats <- tibble(
    hypothese = c(
      "H1 - Biais Sud global",
      "H2 - Biais proximité (via I)",
      "H3 - Peak & Forget",
      "H4 - Couverture/Financement"
    ),
    p_value = c(
      round(res_H1$wilcoxon_ns$p.value,    4),
      round(res_H2$cor_distance_I$p.value, 4),
      round(res_H3$wilcoxon$p.value,       4),
      p_h4
    ),
    direction = c(
      "Sud I_moy > Nord I_moy",
      glue("rho = {round(res_H2$cor_distance_I$estimate, 3)}"),
      "V aiguë > V chronique",
      dir_h4
    )
  ) |>
    mutate(
      significatif = p_value < 0.05,
      conclusion   = ifelse(significatif, "✅ Confirmée", "⚠️ Non confirmée")
    )
  
  print(resultats)
  
  dir.create("data/final", recursive = TRUE, showWarnings = FALSE)
  write_csv(resultats, "data/final/resultats_hypotheses.csv")
  message("\n✓ Résultats sauvegardés dans data/final/resultats_hypotheses.csv")
  
  list(H1 = res_H1, H2 = res_H2, H3 = res_H3, H4 = res_H4,
       resultats = resultats)
}
