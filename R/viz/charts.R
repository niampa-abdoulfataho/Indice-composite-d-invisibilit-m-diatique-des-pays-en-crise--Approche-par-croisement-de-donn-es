# ============================================================
# charts.R — Visualisations ggplot2 + plotly
# ============================================================

library(tidyverse)
library(plotly)
library(ggrepel)
library(patchwork)
library(scales)
library(glue)

# Palette de couleurs du projet
COLORS <- list(
  rouge    = "#c0392b",
  orange   = "#e67e22",
  vert     = "#27ae60",
  bleu     = "#2980b9",
  gris     = "#7f8c8d",
  fond     = "#1a1a2e",
  texte    = "#ecf0f1",
  accent   = "#e74c3c"
)

# Thème ggplot2 personnalisé
theme_crisis <- function() {
  theme_minimal() +
    theme(
      plot.background  = element_rect(fill = "#1a1a2e", color = NA),
      panel.background = element_rect(fill = "#1a1a2e", color = NA),
      panel.grid.major = element_line(color = "#2c3e50", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      text             = element_text(color = "#ecf0f1", family = "sans"),
      plot.title       = element_text(size = 14, face = "bold",
                                      color = "#e74c3c", hjust = 0),
      plot.subtitle    = element_text(size = 10, color = "#bdc3c7"),
      axis.text        = element_text(color = "#bdc3c7", size = 8),
      axis.title       = element_text(color = "#ecf0f1", size = 9),
      legend.background= element_rect(fill = "#1a1a2e", color = NA),
      legend.text      = element_text(color = "#ecf0f1"),
      legend.title     = element_text(color = "#ecf0f1"),
      plot.caption     = element_text(color = "#7f8c8d", size = 7)
    )
}

# ============================================================
# VIZ 1 — Scatter plot G vs M
# ============================================================

#' Scatter plot Gravité vs Couverture médiatique
#'
#' @param df tibble avec G, M, I calculés
#' @param interactive retourner un plotly interactif
plot_scatter_gm <- function(df, interactive = TRUE) {
  message("VIZ 1 — Scatter G vs M...")
  
  # Agréger par pays
  df_agg <- df |>
    group_by(iso3, country, continent_group) |>
    summarise(
      G_moy          = mean(G, na.rm = TRUE),
      M_moy          = mean(M, na.rm = TRUE),
      I_moy          = mean(I, na.rm = TRUE),
      morts_total    = sum(total_deaths_all, na.rm = TRUE),
      articles_total = sum(nb_articles_total, na.rm = TRUE),
      .groups = "drop"
    ) |>
    filter(!is.na(continent_group))
  
  # Cas extrêmes à annoter
  extremes <- df_agg |>
    filter(
      I_moy > quantile(I_moy, 0.92, na.rm = TRUE) |
        I_moy < quantile(I_moy, 0.08, na.rm = TRUE) |
        iso3 %in% c("UKR", "YEM", "ETH", "SDN", "AFG", "COD")
    )
  
  p <- ggplot(df_agg, aes(x = M_moy, y = G_moy,
                          color = continent_group,
                          size  = log1p(morts_total),
                          text  = glue("{country}\nG: {round(G_moy,1)} | M: {round(M_moy,1)} | I: {round(I_moy,1)}\nMorts: {scales::comma(morts_total)}"))) +
    # Ligne diagonale = représentation idéale
    geom_abline(slope = 1, intercept = 0,
                linetype = "dashed", color = "#7f8c8d", alpha = 0.5) +
    geom_point(alpha = 0.7) +
    geom_text_repel(
      data      = extremes,
      aes(label = country),
      size      = 2.5,
      color     = "#ecf0f1",
      max.overlaps = 15,
      box.padding  = 0.3
    ) +
    scale_color_manual(values = c(
      "Afrique"   = COLORS$rouge,
      "Asie"      = COLORS$orange,
      "Amériques" = COLORS$vert,
      "Europe"    = COLORS$bleu,
      "Océanie"   = "#9b59b6",
      "Autre"     = COLORS$gris
    )) +
    scale_size_continuous(range = c(1, 8), guide = "none") +
    labs(
      title    = "Gravité réelle vs Couverture médiatique par pays (2020-2024)",
      subtitle = "Les pays au-dessus de la diagonale sont sous-couverts par rapport à leur gravité",
      x        = "Score Médiatique (M)",
      y        = "Score de Gravité (G)",
      color    = "Continent",
      caption  = "Sources : EM-DAT, UCDP, The Guardian | Indice d'Invisibilité"
    ) +
    theme_crisis()
  
  if (interactive) {
    return(ggplotly(p, tooltip = "text") |>
             layout(
               paper_bgcolor = "#1a1a2e",
               plot_bgcolor  = "#1a1a2e",
               font          = list(color = "#ecf0f1")
             ))
  }
  p
}

# ============================================================
# VIZ 2 — Lollipop Top 10 crises invisibles
# ============================================================

#' Lollipop chart des 10 crises les plus invisibles
#'
#' @param df tibble avec indices calculés
plot_top_invisible <- function(df) {
  message("VIZ 2 — Top 10 crises invisibles...")
  
  df_top <- df |>
    group_by(iso3, country, continent_group) |>
    summarise(
      G_moy          = mean(G, na.rm = TRUE),
      M_moy          = mean(M, na.rm = TRUE),
      I_moy          = mean(I, na.rm = TRUE),
      morts_total    = sum(total_deaths_all, na.rm = TRUE),
      articles_total = sum(nb_articles_total, na.rm = TRUE),
      gdp_habitant   = mean(gdp_md * 1e6 / pop_est, na.rm = TRUE),
      pop_moy        = mean(pop_est, na.rm = TRUE),
      .groups = "drop"
    ) |>
    filter(
      G_moy > 2,
      continent_group %in% c("Afrique", "Asie", "Amériques"),
      gdp_habitant < 15000,
      pop_moy < 200e6,
      morts_total > 500
    ) |>
    slice_max(I_moy, n = 10) |>
    mutate(
      country = fct_reorder(country, I_moy),
      label   = glue("{scales::comma(round(morts_total))} morts\n{scales::comma(articles_total)} articles")
    )
  
  ggplot(df_top, aes(x = I_moy, y = country)) +
    geom_segment(aes(x = 0, xend = I_moy,
                     y = country, yend = country),
                 color = "#2c3e50", linewidth = 0.8) +
    geom_point(aes(color = continent_group), size = 5) +
    geom_text(aes(label = round(I_moy, 1)),
              hjust = -0.5, color = "#ecf0f1", size = 3) +
    geom_text(aes(x = 0.3, label = label),
              hjust = 0, color = "#7f8c8d", size = 2.2) +
    scale_color_manual(values = c(
      "Afrique"   = COLORS$rouge,
      "Asie"      = COLORS$orange,
      "Amériques" = COLORS$vert
    )) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
      title    = "Top 10 — Crises les plus invisibles (2020-2024)",
      subtitle = "Indice d'Invisibilité I = Gravité / Couverture médiatique",
      x        = "Indice d'Invisibilité (I)",
      y        = NULL,
      color    = "Continent",
      caption  = "Sources : EM-DAT, UCDP, The Guardian"
    ) +
    theme_crisis() +
    theme(panel.grid.major.y = element_blank())
}

# ============================================================
# VIZ 3 — Heatmap biais géographique
# ============================================================

#' Heatmap du biais médiatique par continent
#'
#' @param df tibble avec indices calculés
plot_heatmap_bias <- function(df) {
  message("VIZ 3 — Heatmap biais géographique...")
  
  df_heat <- df |>
    filter(!is.na(continent_group)) |>
    group_by(continent_group) |>
    summarise(
      I_mean       = mean(I, na.rm = TRUE),
      G_mean       = mean(G, na.rm = TRUE),
      M_mean       = mean(M, na.rm = TRUE),
      total_morts  = sum(total_deaths_all, na.rm = TRUE),
      total_arts   = sum(nb_articles_total, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      continent_group = fct_reorder(continent_group, I_mean),
      ratio_G_monde   = G_mean / mean(G_mean),
      ratio_M_monde   = M_mean / mean(M_mean)
    ) |>
    pivot_longer(
      cols      = c(ratio_G_monde, ratio_M_monde),
      names_to  = "indicateur",
      values_to = "valeur"
    ) |>
    mutate(
      indicateur = recode(indicateur,
                          ratio_G_monde = "Gravité relative\n(vs moyenne mondiale)",
                          ratio_M_monde = "Couverture relative\n(vs moyenne mondiale)"
      )
    )
  
  ggplot(df_heat, aes(x = indicateur, y = continent_group,
                      fill = valeur)) +
    geom_tile(color = "#1a1a2e", linewidth = 0.5) +
    geom_text(aes(label = round(valeur, 2)),
              color = "white", size = 4, fontface = "bold") +
    scale_fill_gradient2(
      low      = COLORS$bleu,
      mid      = "#2c3e50",
      high     = COLORS$rouge,
      midpoint = 1,
      name     = "Ratio vs\nmoyenne"
    ) +
    labs(
      title    = "Biais médiatique par continent",
      subtitle = "Ratio > 1 = sur-représenté | Ratio < 1 = sous-représenté",
      x        = NULL,
      y        = NULL,
      caption  = "Sources : EM-DAT, UCDP, The Guardian"
    ) +
    theme_crisis() +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(size = 8)
    )
}

# ============================================================
# VIZ 4 — Distribution par zone (donut)
# ============================================================

#' Donut chart de la distribution par zone d'invisibilité
#'
#' @param df tibble avec zone calculée
plot_donut_zones <- function(df) {
  message("VIZ 4 — Distribution zones...")
  
  df_zones <- df |>
    count(zone) |>
    mutate(
      pct   = n / sum(n) * 100,
      label = glue("{zone}\n{round(pct,1)}%"),
      color = case_when(
        str_detect(zone, "Rouge")  ~ COLORS$rouge,
        str_detect(zone, "Orange") ~ COLORS$orange,
        str_detect(zone, "Verte")  ~ COLORS$vert,
        str_detect(zone, "Bleue")  ~ COLORS$bleu
      )
    )
  
  ggplot(df_zones, aes(x = 2, y = pct, fill = zone)) +
    geom_col(color = "#1a1a2e", linewidth = 0.5) +
    geom_text(aes(label = glue("{round(pct,1)}%\n({n})")),
              position = position_stack(vjust = 0.5),
              color = "white", size = 3, fontface = "bold") +
    coord_polar(theta = "y") +
    xlim(0.5, 2.5) +
    scale_fill_manual(values = c(
      "Rouge (invisible)"      = COLORS$rouge,
      "Orange (sous-couvert)"  = COLORS$orange,
      "Verte (proportionnel)"  = COLORS$vert,
      "Bleue (sur-médiatisé)"  = COLORS$bleu
    )) +
    labs(
      title    = "Distribution de l'Indice d'Invisibilité",
      subtitle = "Répartition des 722 observations pays/année",
      fill     = NULL,
      caption  = "Sources : EM-DAT, UCDP, The Guardian"
    ) +
    theme_crisis() +
    theme(
      axis.text        = element_blank(),
      axis.title       = element_blank(),
      panel.grid       = element_blank()
    )
}

# ============================================================
# VIZ 5 — Série temporelle Peak & Forget
# ============================================================

#' Courbe Peak & Forget pour des crises sélectionnées
#'
#' @param df tibble avec indices calculés
#' @param pays_selec vecteur d'iso3 à afficher
plot_peak_forget <- function(df,
                             pays_selec = c("UKR", "YEM",
                                            "ETH", "SDN",
                                            "AFG")) {
  message("VIZ 5 — Peak & Forget timeline...")
  
  df_timeline <- df |>
    filter(iso3 %in% pays_selec) |>
    select(iso3, country, year, nb_articles_total,
           G, M, I, total_deaths_all) |>
    arrange(iso3, year)
  
  p <- ggplot(df_timeline,
              aes(x = year, y = nb_articles_total,
                  color = country, group = country)) +
    geom_line(linewidth = 1) +
    geom_point(aes(size = total_deaths_all / 1000), alpha = 0.8) +
    scale_color_manual(values = c(
      "Ukraine"  = COLORS$bleu,
      "Yemen"    = COLORS$rouge,
      "Ethiopia" = COLORS$orange,
      "Sudan"    = "#9b59b6",
      "Afghanistan" = COLORS$vert
    )) +
    scale_size_continuous(
      range  = c(2, 10),
      name   = "Morts (milliers)",
      guide  = guide_legend(override.aes = list(color = "#ecf0f1"))
    ) +
    scale_x_continuous(breaks = 2020:2024) +
    labs(
      title    = "Évolution de la couverture médiatique (2020-2024)",
      subtitle = "Taille des points = nombre de morts | Effet Peak & Forget visible",
      x        = "Année",
      y        = "Nombre d'articles (The Guardian)",
      color    = "Pays",
      caption  = "Source : The Guardian API"
    ) +
    theme_crisis()
  
  ggplotly(p) |>
    layout(
      paper_bgcolor = "#1a1a2e",
      plot_bgcolor  = "#1a1a2e",
      font          = list(color = "#ecf0f1")
    )
}

# ============================================================
# VIZ 6 — Radar chart comparatif
# ============================================================

#' Radar chart pour comparer 5 crises clés
#'
#' @param df tibble avec indices calculés
plot_radar <- function(df) {
  message("VIZ 6 — Radar chart comparatif...")
  
  # Nécessite le package fmsb
  if (!requireNamespace("fmsb", quietly = TRUE)) {
    install.packages("fmsb")
  }
  library(fmsb)
  
  cas <- c("UKR", "YEM", "ETH", "SDN", "AFG")
  
  df_radar <- df |>
    filter(iso3 %in% cas) |>
    group_by(iso3, country) |>
    summarise(
      Gravite       = mean(G,                    na.rm = TRUE),
      Couverture    = mean(M,                    na.rm = TRUE),
      Invisibilite  = mean(I,                    na.rm = TRUE),
      Mortalite     = mean(score_mortalite,      na.rm = TRUE),
      Conflits      = mean(score_conflits,       na.rm = TRUE),
      .groups = "drop"
    ) |>
    column_to_rownames("country") |>
    select(-iso3)
  
  # Ajouter lignes max/min requises par fmsb
  df_radar <- rbind(
    rep(100, ncol(df_radar)),
    rep(0,   ncol(df_radar)),
    df_radar
  )
  
  # Couleurs par pays
  colors_radar <- c(
    COLORS$bleu, COLORS$rouge, COLORS$orange,
    "#9b59b6",   COLORS$vert
  )
  
  op <- par(mar = c(1, 1, 2, 1),
            bg  = "#1a1a2e",
            col.main = "#e74c3c")
  
  radarchart(
    df_radar,
    axistype  = 1,
    pcol      = colors_radar,
    pfcol     = scales::alpha(colors_radar, 0.15),
    plwd      = 2,
    cglcol    = "#2c3e50",
    cglty     = 1,
    axislabcol= "#bdc3c7",
    vlcex     = 0.8,
    vlabels   = colnames(df_radar),
    title     = "Comparaison multidimensionnelle — 5 crises clés"
  )
  
  legend(
    x      = "bottomright",
    legend = rownames(df_radar)[3:nrow(df_radar)],
    col    = colors_radar,
    lty    = 1, lwd = 2,
    bty    = "n",
    text.col = "#ecf0f1",
    cex    = 0.8
  )
  
  par(op)
}

# ============================================================
# VIZ 7 — Barres empilées sentiment (placeholder)
# ============================================================

#' Barres empilées de distribution des zones par continent
#'
#' @param df tibble avec zone et continent calculés
plot_zones_by_continent <- function(df) {
  message("VIZ 7 — Zones par continent...")
  
  df_cont <- df |>
    filter(!is.na(continent_group), !is.na(zone)) |>
    count(continent_group, zone) |>
    group_by(continent_group) |>
    mutate(pct = n / sum(n) * 100) |>
    ungroup()
  
  ggplot(df_cont,
         aes(x    = continent_group,
             y    = pct,
             fill = zone)) +
    geom_col(color = "#1a1a2e", linewidth = 0.3) +
    geom_text(
      aes(label = ifelse(pct > 8,
                         glue("{round(pct,0)}%"), "")),
      position = position_stack(vjust = 0.5),
      color    = "white",
      size     = 3,
      fontface = "bold"
    ) +
    scale_fill_manual(values = c(
      "Rouge (invisible)"      = COLORS$rouge,
      "Orange (sous-couvert)"  = COLORS$orange,
      "Verte (proportionnel)"  = COLORS$vert,
      "Bleue (sur-médiatisé)"  = COLORS$bleu
    )) +
    scale_y_continuous(labels = label_percent(scale = 1)) +
    labs(
      title    = "Distribution de l'invisibilité par continent",
      subtitle = "% d'observations dans chaque zone d'invisibilité",
      x        = NULL,
      y        = "% des observations",
      fill     = "Zone",
      caption  = "Sources : EM-DAT, UCDP, The Guardian"
    ) +
    theme_crisis() +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
}

# ============================================================
# EXÉCUTION — Générer et sauvegarder toutes les viz
# ============================================================

run_charts <- function(df, save = TRUE) {
  message("\n===== GÉNÉRATION DES VISUALISATIONS =====")
  
  dir.create("report/assets", recursive = TRUE,
             showWarnings = FALSE)
  
  # VIZ 1 — Scatter G vs M (statique pour Quarto)
  p1 <- plot_scatter_gm(df, interactive = FALSE)
  if (save) ggsave("report/assets/viz1_scatter_gm.png",
                   p1, width = 12, height = 8, dpi = 150,
                   bg = "#1a1a2e")
  message("  ✓ VIZ 1 sauvegardée")
  
  # VIZ 2 — Top 10 invisibles
  p2 <- plot_top_invisible(df)
  if (save) ggsave("report/assets/viz2_top_invisible.png",
                   p2, width = 10, height = 7, dpi = 150,
                   bg = "#1a1a2e")
  message("  ✓ VIZ 2 sauvegardée")
  
  # VIZ 3 — Heatmap biais
  p3 <- plot_heatmap_bias(df)
  if (save) ggsave("report/assets/viz3_heatmap_bias.png",
                   p3, width = 9, height = 6, dpi = 150,
                   bg = "#1a1a2e")
  message("  ✓ VIZ 3 sauvegardée")
  
  # VIZ 4 — Donut zones
  p4 <- plot_donut_zones(df)
  if (save) ggsave("report/assets/viz4_donut_zones.png",
                   p4, width = 8, height = 8, dpi = 150,
                   bg = "#1a1a2e")
  message("  ✓ VIZ 4 sauvegardée")
  
  # VIZ 7 — Zones par continent
  p7 <- plot_zones_by_continent(df)
  if (save) ggsave("report/assets/viz7_zones_continent.png",
                   p7, width = 10, height = 6, dpi = 150,
                   bg = "#1a1a2e")
  message("  ✓ VIZ 7 sauvegardée")
  
  # VIZ 5 — Peak & Forget (plotly — HTML)
  p5 <- plot_peak_forget(df)
  if (save) {
    htmlwidgets::saveWidget(
      p5,
      "report/assets/viz5_peak_forget.html",
      selfcontained = TRUE
    )
    message("  ✓ VIZ 5 sauvegardée (HTML interactif)")
  }
  
  # VIZ 6 — Radar (PNG)
  if (save) {
    png("report/assets/viz6_radar.png",
        width = 800, height = 800, bg = "#1a1a2e")
    plot_radar(df)
    dev.off()
    message("  ✓ VIZ 6 sauvegardée")
  }
  
  message("\n✓ Toutes les visualisations sauvegardées dans report/assets/")
  
  list(p1 = p1, p2 = p2, p3 = p3,
       p4 = p4, p5 = p5, p7 = p7)
}