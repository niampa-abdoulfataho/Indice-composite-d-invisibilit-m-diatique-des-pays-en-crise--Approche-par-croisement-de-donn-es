# ============================================================
# maps.R — Cartes interactives leaflet + ggplot2
# ============================================================

library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)
library(rnaturalearth)
library(htmlwidgets)
library(glue)
library(scales)

# ============================================================
# VIZ MAP 1 — Carte choroplèthe leaflet (interactive)
# ============================================================

#' Carte mondiale interactive de l'Indice d'Invisibilité
#'
#' @param df tibble avec indices calculés
#' @return objet leaflet
plot_map_leaflet <- function(df) {
  message("MAP 1 — Carte choroplèthe leaflet...")
  
  # Agréger par pays
  df_map <- df |>
    group_by(iso3, country) |>
    summarise(
      I_moy          = mean(I,               na.rm = TRUE),
      G_moy          = mean(G,               na.rm = TRUE),
      M_moy          = mean(M,               na.rm = TRUE),
      morts_total    = sum(total_deaths_all, na.rm = TRUE),
      articles_total = sum(nb_articles_total,na.rm = TRUE),
      zone           = names(sort(table(zone), decreasing = TRUE))[1],
      .groups = "drop"
    )
  
  # Charger les géométries
  world <- ne_countries(scale = "medium", returnclass = "sf") |>
    select(iso3 = iso_a3, geometry)
  
  # Jointure
  world_data <- world |>
    left_join(df_map, by = "iso3") |>
    filter(!is.na(I_moy))
  
  # Palette de couleurs
  pal <- colorNumeric(
    palette = c("#2980b9", "#27ae60", "#e67e22", "#c0392b"),
    domain  = world_data$I_moy,
    na.color= "#2c3e50"
  )
  
  # Popup détaillé
  popup_content <- glue(
    "<div style='font-family:sans-serif; background:#1a1a2e;
                 color:#ecf0f1; padding:10px; border-radius:6px;'>
       <b style='color:#e74c3c; font-size:14px;'>{world_data$country}</b><br/>
       <hr style='border-color:#2c3e50; margin:5px 0;'/>
       <b>Indice d'Invisibilité (I) :</b> {round(world_data$I_moy, 2)}<br/>
       <b>Score de Gravité (G) :</b> {round(world_data$G_moy, 2)}<br/>
       <b>Score Médiatique (M) :</b> {round(world_data$M_moy, 2)}<br/>
       <hr style='border-color:#2c3e50; margin:5px 0;'/>
       <b>Morts (2020-2024) :</b> {scales::comma(world_data$morts_total)}<br/>
       <b>Articles Guardian :</b> {scales::comma(world_data$articles_total)}<br/>
       <b>Zone :</b> {world_data$zone}
     </div>"
  )
  
  # Carte leaflet
  map <- leaflet(world_data,
                 options = leafletOptions(
                   minZoom = 2, maxZoom = 8
                 )) |>
    # Fond sombre
    addProviderTiles(
      providers$CartoDB.DarkMatter,
      options = tileOptions(opacity = 0.8)
    ) |>
    # Couche choroplèthe
    addPolygons(
      fillColor   = ~pal(I_moy),
      fillOpacity = 0.8,
      color       = "#1a1a2e",
      weight      = 0.5,
      popup       = popup_content,
      highlight   = highlightOptions(
        weight        = 2,
        color         = "#e74c3c",
        fillOpacity   = 0.9,
        bringToFront  = TRUE
      ),
      label = ~glue("{country} — I: {round(I_moy, 1)}")
    ) |>
    # Légende
    addLegend(
      position  = "bottomright",
      pal       = pal,
      values    = ~I_moy,
      title     = "Indice<br>d'Invisibilité",
      opacity   = 0.9,
      labFormat = labelFormat(digits = 1)
    ) |>
    # Titre
    addControl(
      html = "<div style='background:rgba(26,26,46,0.9);
                           padding:10px; border-radius:6px;
                           color:#ecf0f1; font-family:sans-serif;'>
                <b style='color:#e74c3c; font-size:16px;'>
                  L'Indice d'Invisibilité
                </b><br/>
                <span style='font-size:11px; color:#bdc3c7;'>
                  Crises humanitaires 2020-2024
                </span>
              </div>",
      position = "topleft"
    ) |>
    setView(lng = 10, lat = 20, zoom = 2)
  
  message("  ✓ Carte leaflet créée")
  map
}

# ============================================================
# VIZ MAP 2 — Carte statique ggplot2 (pour Quarto)
# ============================================================

#' Carte mondiale statique pour le rapport Quarto
#'
#' @param df tibble avec indices calculés
plot_map_static <- function(df) {
  message("MAP 2 — Carte statique ggplot2...")
  
  df_map <- df |>
    group_by(iso3) |>
    summarise(
      I_moy       = mean(I, na.rm = TRUE),
      G_moy       = mean(G, na.rm = TRUE),
      M_moy       = mean(M, na.rm = TRUE),
      morts_total = sum(total_deaths_all, na.rm = TRUE),
      .groups = "drop"
    )
  
  world <- ne_countries(scale = "medium", returnclass = "sf") |>
    select(iso3 = iso_a3, name, geometry)
  
  world_data <- world |>
    left_join(df_map, by = "iso3")
  
  ggplot(world_data) +
    geom_sf(aes(fill = I_moy), color = "#1a1a2e", linewidth = 0.1) +
    scale_fill_gradientn(
      colors   = c("#2980b9", "#27ae60", "#e67e22", "#c0392b"),
      values   = scales::rescale(c(0, 2, 5, max(df_map$I_moy, na.rm=TRUE))),
      na.value = "#2c3e50",
      name     = "Indice\nd'Invisibilité",
      guide    = guide_colorbar(
        barwidth  = 1,
        barheight = 8,
        title.position = "top"
      )
    ) +
    coord_sf(crs = "+proj=robin") +
    labs(
      title    = "L'Indice d'Invisibilité — Crises humanitaires 2020-2024",
      subtitle = "Plus la couleur est rouge, plus la crise est grave et ignorée des médias",
      caption  = "Sources : EM-DAT, UCDP, The Guardian API | Projection Robinson"
    ) +
    theme_void() +
    theme(
      plot.background  = element_rect(fill = "#1a1a2e", color = NA),
      panel.background = element_rect(fill = "#1a1a2e", color = NA),
      plot.title       = element_text(color = "#e74c3c", size = 14,
                                      face = "bold", hjust = 0.5,
                                      margin = margin(b = 5)),
      plot.subtitle    = element_text(color = "#bdc3c7", size = 9,
                                      hjust = 0.5,
                                      margin = margin(b = 10)),
      plot.caption     = element_text(color = "#7f8c8d", size = 7,
                                      hjust = 0.5),
      legend.position  = c(0.08, 0.35),
      legend.background= element_rect(fill = "transparent", color = NA),
      legend.title     = element_text(color = "#ecf0f1", size = 8),
      legend.text      = element_text(color = "#bdc3c7", size = 7)
    )
}

# ============================================================
# VIZ MAP 3 — Carte bulles proportionnelles
# ============================================================

#' Carte avec bulles proportionnelles aux morts
#'
#' @param df tibble avec indices calculés
plot_map_bubbles <- function(df) {
  message("MAP 3 — Carte bulles proportionnelles...")
  
  df_map <- df |>
    group_by(iso3, country) |>
    summarise(
      I_moy       = mean(I, na.rm = TRUE),
      G_moy       = mean(G, na.rm = TRUE),
      M_moy       = mean(M, na.rm = TRUE),
      morts_total = sum(total_deaths_all, na.rm = TRUE),
      .groups = "drop"
    ) |>
    filter(morts_total > 100)
  
  # Récupérer les centroides
  world_centroids <- ne_countries(scale = "medium",
                                  returnclass = "sf") |>
    mutate(
      centroid = st_centroid(geometry),
      lon      = st_coordinates(centroid)[, 1],
      lat      = st_coordinates(centroid)[, 2]
    ) |>
    as_tibble() |>
    select(iso3 = iso_a3, lon, lat)
  
  df_bubbles <- df_map |>
    left_join(world_centroids, by = "iso3") |>
    filter(!is.na(lon), !is.na(lat))
  
  # Fond carte
  world_bg <- ne_countries(scale = "medium", returnclass = "sf")
  
  ggplot() +
    geom_sf(data    = world_bg,
            fill    = "#2c3e50",
            color   = "#1a1a2e",
            linewidth = 0.1) +
    geom_point(
      data  = df_bubbles |> arrange(morts_total),
      aes(x     = lon,
          y     = lat,
          size  = morts_total,
          color = I_moy),
      alpha = 0.75
    ) +
    scale_size_continuous(
      range  = c(1, 15),
      name   = "Morts totaux",
      labels = scales::comma,
      breaks = c(1000, 10000, 50000, 100000)
    ) +
    scale_color_gradientn(
      colors   = c("#2980b9", "#27ae60", "#e67e22", "#c0392b"),
      name     = "Indice\nd'Invisibilité",
      guide    = guide_colorbar(barwidth = 1, barheight = 6)
    ) +
    coord_sf(crs = "+proj=robin") +
    labs(
      title    = "Gravité des crises humanitaires vs invisibilité médiatique",
      subtitle = "Taille = nombre de morts | Couleur = Indice d'Invisibilité",
      caption  = "Sources : EM-DAT, UCDP, The Guardian API"
    ) +
    theme_void() +
    theme(
      plot.background  = element_rect(fill = "#1a1a2e", color = NA),
      panel.background = element_rect(fill = "#1a1a2e", color = NA),
      plot.title       = element_text(color = "#e74c3c", size = 13,
                                      face = "bold", hjust = 0.5),
      plot.subtitle    = element_text(color = "#bdc3c7", size = 9,
                                      hjust = 0.5),
      plot.caption     = element_text(color = "#7f8c8d", size = 7,
                                      hjust = 0.5),
      legend.position  = "right",
      legend.background= element_rect(fill = "transparent", color = NA),
      legend.title     = element_text(color = "#ecf0f1", size = 8),
      legend.text      = element_text(color = "#bdc3c7", size = 7)
    )
}

# ============================================================
# EXÉCUTION
# ============================================================

run_maps <- function(df, save = TRUE) {
  message("\n===== GÉNÉRATION DES CARTES =====")
  
  dir.create("report/assets", recursive = TRUE,
             showWarnings = FALSE)
  
  # MAP 1 — Leaflet interactive
  map1 <- plot_map_leaflet(df)
  if (save) {
    saveWidget(
      map1,
      "report/assets/map1_leaflet.html",
      selfcontained = TRUE
    )
    message("  ✓ MAP 1 sauvegardée (HTML interactif)")
  }
  
  # MAP 2 — Statique Quarto
  map2 <- plot_map_static(df)
  if (save) {
    ggsave("report/assets/map2_static.png",
           map2, width = 14, height = 8,
           dpi = 150, bg = "#1a1a2e")
    message("  ✓ MAP 2 sauvegardée (PNG)")
  }
  
  # MAP 3 — Bulles
  map3 <- plot_map_bubbles(df)
  if (save) {
    ggsave("report/assets/map3_bubbles.png",
           map3, width = 14, height = 8,
           dpi = 150, bg = "#1a1a2e")
    message("  ✓ MAP 3 sauvegardée (PNG)")
  }
  
  message("\n✓ Toutes les cartes sauvegardées dans report/assets/")
  list(map1 = map1, map2 = map2, map3 = map3)
}