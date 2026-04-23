# ============================================================
# timeline_anim.R — Timeline animée G vs M par année
#
# Montre l'évolution de la position de chaque pays dans
# l'espace Gravité × Couverture médiatique de 2020 à 2024.
# Taille des bulles ∝ nombre de morts.
# Couleur = continent.
# Packages requis : gganimate, gifski (ou av)
# Fallback si gganimate absent : plotly avec slider d'année
# ============================================================

library(tidyverse)
library(plotly)
library(glue)

# Pays à annoter dans l'animation (crises représentatives)
PAYS_LABELS <- c(
  "UKR", "YEM", "ETH", "SDN", "AFG",
  "SYR", "COD", "MMR", "HTI", "NGA"
)

# Palette continents (cohérente avec charts.R)
CONTINENT_COLORS <- c(
  "Afrique"   = "#c0392b",
  "Asie"      = "#e67e22",
  "Amériques" = "#27ae60",
  "Europe"    = "#2980b9",
  "Océanie"   = "#9b59b6",
  "Autre"     = "#7f8c8d"
)

# ============================================================
# Préparation des données
# ============================================================

#' Prépare le dataset pour l'animation
#'
#' @param df tibble final des indices (pays × année)
#' @return tibble nettoyé avec les colonnes nécessaires
prepare_animation_data <- function(df) {
  
  df |>
    filter(!is.na(G), !is.na(M), !is.na(continent_group)) |>
    group_by(iso3, country, continent_group, year) |>
    summarise(
      G              = mean(G,               na.rm = TRUE),
      M              = mean(M,               na.rm = TRUE),
      I              = mean(I,               na.rm = TRUE),
      total_deaths   = sum(total_deaths_all, na.rm = TRUE),
      nb_articles    = sum(nb_articles_total,na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      # Taille des bulles : log des morts pour éviter les outliers
      bubble_size  = log1p(total_deaths + 1),
      # Label pour les pays clés
      label_anim   = if_else(iso3 %in% PAYS_LABELS, country, ""),
      # Tooltip enrichi
      tooltip_text = glue(
        "<b>{country}</b> ({year})<br>",
        "Gravité (G)    : {round(G, 1)}<br>",
        "Couverture (M) : {round(M, 1)}<br>",
        "Invisibilité (I): {round(I, 2)}<br>",
        "Morts : {scales::comma(total_deaths)}<br>",
        "Articles : {scales::comma(nb_articles)}"
      )
    )
}

# ============================================================
# Option A — Animation GIF avec gganimate
# ============================================================

#' Crée l'animation GIF (nécessite gganimate + gifski)
#'
#' @param df_anim  tibble de prepare_animation_data()
#' @param fps      images par seconde
#' @param nframes  nb total de frames (plus = plus fluide mais plus lourd)
#' @param output   chemin de sortie du GIF
#' @return objet gganimate ou NULL si package absent
plot_animation_gif <- function(df_anim,
                               fps     = 5,
                               nframes = 50,
                               output  = "report/assets/timeline_anim.gif") {
  
  if (!requireNamespace("gganimate", quietly = TRUE)) {
    message("  ℹ️  gganimate non installé (install.packages('gganimate'))")
    message("  → Utilisation du fallback plotly interactif")
    return(NULL)
  }
  
  if (!requireNamespace("gifski", quietly = TRUE) &&
      !requireNamespace("av",     quietly = TRUE)) {
    message("  ℹ️  gifski ou av requis : install.packages('gifski')")
    return(NULL)
  }
  
  message("  Génération de l'animation GIF...")
  
  p <- ggplot2::ggplot(
    df_anim,
    ggplot2::aes(
      x     = M,
      y     = G,
      size  = bubble_size,
      color = continent_group,
      group = iso3           # nécessaire pour gganimate (suivi des pays)
    )
  ) +
    # Ligne de représentation idéale G = M
    ggplot2::geom_abline(
      slope     = 1, intercept = 0,
      linetype  = "dashed",
      color     = "#7f8c8d",
      alpha     = 0.4
    ) +
    # Zones d'invisibilité en fond
    ggplot2::annotate("rect",
      xmin = -Inf, xmax = 30, ymin = 50, ymax = Inf,
      fill = "#c0392b", alpha = 0.04
    ) +
    ggplot2::annotate("text",
      x = 5, y = 95, label = "Zone invisible",
      color = "#c0392b", size = 2.5, alpha = 0.6, hjust = 0
    ) +
    ggplot2::geom_point(alpha = 0.72) +
    # Annotations pays clés
    ggrepel::geom_text_repel(
      ggplot2::aes(label = label_anim),
      size         = 2.5,
      color        = "#ecf0f1",
      max.overlaps = 10,
      box.padding  = 0.3,
      force        = 1.5
    ) +
    ggplot2::scale_color_manual(values = CONTINENT_COLORS) +
    ggplot2::scale_size_continuous(range = c(1, 12), guide = "none") +
    ggplot2::scale_x_continuous(limits = c(0, 100)) +
    ggplot2::scale_y_continuous(limits = c(0, 100)) +
    ggplot2::labs(
      title    = "Gravité vs Couverture médiatique — {frame_time}",
      subtitle = "Taille ∝ nombre de morts | Pays au-dessus de la diagonale = sous-couverts",
      x        = "Score Médiatique (M)",
      y        = "Score de Gravité (G)",
      color    = "Continent",
      caption  = "Sources : EM-DAT, UCDP, The Guardian/NewsAPI"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = "#1a1a2e", color = NA),
      panel.background = ggplot2::element_rect(fill = "#1a1a2e", color = NA),
      panel.grid.major = ggplot2::element_line(color = "#2c3e50", linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      text             = ggplot2::element_text(color = "#ecf0f1", family = "sans"),
      plot.title       = ggplot2::element_text(size = 14, face = "bold",
                                                color = "#e74c3c"),
      plot.subtitle    = ggplot2::element_text(size = 9, color = "#bdc3c7"),
      axis.text        = ggplot2::element_text(color = "#bdc3c7"),
      legend.background= ggplot2::element_rect(fill = "#1a1a2e", color = NA),
      legend.text      = ggplot2::element_text(color = "#ecf0f1"),
      plot.caption     = ggplot2::element_text(color = "#7f8c8d", size = 7)
    ) +
    # ---- Transition gganimate ----
    gganimate::transition_time(as.integer(year)) +
    gganimate::ease_aes("cubic-in-out") +
    gganimate::shadow_wake(
      wake_length = 0.2,   # traînée des pays (effect "ghost")
      alpha       = TRUE,
      wrap        = FALSE
    )
  
  # Rendu
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  
  anim <- gganimate::animate(
    p,
    fps     = fps,
    nframes = nframes,
    width   = 900,
    height  = 650,
    renderer = if (requireNamespace("gifski", quietly = TRUE))
                 gganimate::gifski_renderer(output)
               else
                 gganimate::av_renderer(sub("\\.gif$", ".mp4", output))
  )
  
  message(glue("  ✓ Animation sauvegardée : {output}"))
  anim
}

# ============================================================
# Option B — Plotly interactif avec slider d'année (fallback)
# ============================================================
#' Timeline interactive plotly avec slider d'année
#'
#' @param df_anim  tibble de prepare_animation_data()
#' @param output   chemin HTML de sortie (optionnel)
#' @return objet plotly
plot_timeline_plotly <- function(df_anim, output = NULL) {
  
  message("  Génération de la timeline plotly (slider d'année)...")
  
  years <- sort(unique(df_anim$year))
  
  p <- plotly::plot_ly(
    data   = df_anim,
    x      = ~M,
    y      = ~G,
    size   = ~bubble_size,
    color  = ~continent_group,
    colors = CONTINENT_COLORS,
    frame  = ~year,               # slider d'animation
    text   = ~tooltip_text,
    ids    = ~iso3,               # important pour les transitions fluides
    type   = "scatter",
    mode   = "markers",
    marker = list(
      sizemode = "diameter",
      sizeref  = 0.12,
      opacity  = 0.72,
      line     = list(width = 0.5, color = "#1a1a2e")
    ),
    hoverinfo = "text"
  ) |>
    plotly::add_segments(
      x = 0, xend = 100, y = 0, yend = 100,
      line = list(color = "#7f8c8d", dash = "dash", width = 1),
      showlegend = FALSE,
      hoverinfo = "none"
    ) |>
    plotly::layout(
      title = list(
        text = "Évolution Gravité vs Couverture médiatique (2020-2024)",
        font = list(color = "#e74c3c", size = 15)
      ),
      xaxis = list(
        title      = "Score Médiatique (M)",
        range      = c(0, 105),
        gridcolor  = "#2c3e50",
        zerolinecolor = "#2c3e50",
        tickfont   = list(color = "#bdc3c7")
      ),
      yaxis = list(
        title      = "Score de Gravité (G)",
        range      = c(0, 105),
        gridcolor  = "#2c3e50",
        zerolinecolor = "#2c3e50",
        tickfont   = list(color = "#bdc3c7")
      ),
      legend = list(
        font       = list(color = "#ecf0f1"),
        bgcolor    = "rgba(26,26,46,0.8)"
      ),
      paper_bgcolor = "#1a1a2e",
      plot_bgcolor  = "#1a1a2e",
      font          = list(color = "#ecf0f1"),
      # Annotations zone invisible
      annotations = list(
        list(
          x = 8, y = 92,
          text = "Zone invisible",
          showarrow = FALSE,
          font = list(color = "#c0392b", size = 11)
        ),
        list(
          x = 75, y = 10,
          text = "Sur-médiatisé",
          showarrow = FALSE,
          font = list(color = "#2980b9", size = 11)
        )
      )
    ) |>
    plotly::animation_opts(
      frame      = 800,    # durée par frame (ms)
      transition = 500,    # durée de transition
      easing     = "cubic-in-out",
      redraw     = FALSE
    ) |>
    plotly::animation_slider(
      currentvalue = list(
        prefix = "Année : ",
        font   = list(color = "#ecf0f1", size = 13)
      )
    ) |>
    plotly::animation_button(
      x = 1.05, y = 0,
      label = "▶ Play"
    )
  
  if (!is.null(output)) {
    dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
    htmlwidgets::saveWidget(p, output, selfcontained = TRUE)
    message(glue("  ✓ Timeline plotly sauvegardée : {output}"))
  }
  
  p
}

# ============================================================
# EXÉCUTION
# ============================================================

run_timeline <- function(df, save = TRUE,
                          mode = "auto") {
  # mode : "auto" | "gif" | "plotly"
  message("\n===== TIMELINE ANIMÉE =====")
  
  df_anim <- prepare_animation_data(df)
  
  years_dispo <- sort(unique(df_anim$year))
  message(glue("  Années disponibles : {paste(years_dispo, collapse = ', ')}"))
  message(glue("  Pays : {n_distinct(df_anim$iso3)}"))
  
  results <- list()
  
  # GIF (gganimate)
  if (mode %in% c("auto", "gif")) {
    gif <- plot_animation_gif(
      df_anim,
      output = "report/assets/timeline_anim.gif"
    )
    if (!is.null(gif)) {
      results$gif <- gif
    } else if (mode == "gif") {
      message("  → Basculement sur plotly (gganimate indisponible)")
      mode <- "plotly"
    }
  }
  
  # Plotly (fallback ou mode explicite)
  if (mode %in% c("auto", "plotly") || is.null(results$gif)) {
    p_plotly <- plot_timeline_plotly(
      df_anim,
      output = if (save) "report/assets/timeline_anim.html" else NULL
    )
    results$plotly <- p_plotly
  }
  
  message("✓ Timeline animée générée")
  results
}
