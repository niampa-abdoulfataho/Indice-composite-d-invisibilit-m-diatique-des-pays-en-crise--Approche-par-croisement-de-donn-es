# ============================================================
# worldbank.R — Données contextuelles par pays (Plan B)
# Source : rnaturalearth (données intégrées)
# ============================================================

library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(tidyverse)
library(glue)

# ------------------------------------------------------------
# Extraction des données pays
# ------------------------------------------------------------

#' Récupère les données contextuelles de tous les pays
#' via rnaturalearth
#'
#' @return tibble avec 1 ligne par pays
fetch_country_data <- function() {
  message("Extraction données pays (rnaturalearth)...")
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Centroides
  centroids <- world |>
    st_centroid() |>
    st_coordinates() |>
    as_tibble() |>
    rename(lon = X, lat = Y)
  
  # Superficie calculée depuis la géométrie (indépendant des noms de colonnes)
  superficie_km2 <- as.numeric(st_area(world)) / 1e6
  
  df <- world |>
    as_tibble() |>
    bind_cols(centroids) |>
    mutate(superficie = superficie_km2) |>
    select(
      iso3        = iso_a3,
      iso2        = iso_a2,
      country     = name,
      subregion,
      region      = region_wb,
      continent,
      population  = pop_est,
      pib_md      = gdp_md,
      superficie,          # maintenant disponible via mutate()
      lon, lat
    ) |>
    filter(!is.na(iso3), iso3 != "-99", nchar(iso3) == 3) |>
    mutate(
      pib_habitant = (pib_md * 1e6) / population,
      pop_millions = population / 1e6
    )
  
  message(glue("✓ {nrow(df)} pays extraits"))
  df
}
# ------------------------------------------------------------
# Calcul des distances vers les grandes rédactions
# ------------------------------------------------------------

#' Ajoute la distance moyenne vers Paris, Londres, New York
#' proxy du biais de proximité médiatique
#'
#' @param df tibble avec colonnes lon, lat
#' @return df enrichi avec dist_redactions_km
add_media_distance <- function(df) {
  
  message("Calcul des distances vers les grandes rédactions...")
  
  # Les 5 grandes capitales médiatiques mondiales
  redactions <- tibble(
    ville = c("Paris", "Londres", "New_York", "Washington", "Berlin"),
    lon   = c( 2.35,   -0.12,    -74.01,     -77.04,        13.40),
    lat   = c(48.85,   51.51,     40.71,      38.91,         52.52)
  ) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  # Convertir les pays en sf
  df_sf <- df |>
    filter(!is.na(lon), !is.na(lat)) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  # Matrice de distances (pays × rédactions)
  dist_matrix <- st_distance(df_sf, redactions)
  
  # Distance moyenne en km pour chaque pays
  dist_moyenne <- apply(dist_matrix, 1, mean) / 1000
  
  df_sf <- df_sf |>
    mutate(dist_redactions_km = as.numeric(dist_moyenne))
  
  # Réintégrer lon/lat et retirer la géométrie
  coords <- st_coordinates(df_sf)
  df_result <- df_sf |>
    st_drop_geometry() |>
    mutate(lon = coords[, 1], lat = coords[, 2])
  
  # Joindre avec les pays sans coordonnées
  df |>
    select(-lon, -lat) |>
    left_join(
      df_result |> select(iso3, lon, lat, dist_redactions_km),
      by = "iso3"
    )
}

# ------------------------------------------------------------
# EXÉCUTION
# ------------------------------------------------------------

run_worldbank <- function(save = TRUE) {
  message("\n===== DONNÉES CONTEXTUELLES PAYS =====")
  
  df_wb <- fetch_country_data()
  df_wb <- add_media_distance(df_wb)
  
  # Stats de couverture
  message(glue("Pays avec population      : {sum(!is.na(df_wb$population))}"))
  message(glue("Pays avec PIB/hab estimé  : {sum(!is.na(df_wb$pib_habitant))}"))
  message(glue("Pays avec distance media  : {sum(!is.na(df_wb$dist_redactions_km))}"))
  
  # Top 10 pays les plus éloignés des rédactions
  top_loin <- df_wb |>
    arrange(desc(dist_redactions_km)) |>
    select(iso3, country, dist_redactions_km) |>
    head(10)
  message("\nTop 10 pays les plus éloignés des rédactions :")
  print(top_loin)
  
  if (save) {
    dir.create("data/raw/worldbank", recursive = TRUE, showWarnings = FALSE)
    write_csv(df_wb, "data/raw/worldbank/worldbank.csv")
    message("\n✓ Sauvegardé dans data/raw/worldbank/worldbank.csv")
  }
  
  df_wb
}
