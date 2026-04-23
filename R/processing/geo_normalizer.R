# ============================================================
# geo_normalizer.R — Normalisation géographique
# ============================================================

library(tidyverse)
library(countrycode)
library(sf)
library(rnaturalearth)
library(glue)

# ------------------------------------------------------------
# Table des pays
# ------------------------------------------------------------
#' Construit la table de référence géographique complète
#'
#' @return tibble avec 1 ligne par pays ISO3
build_country_reference <- function() {
  message("Construction de la table de référence pays...")
  
  # Base : rnaturalearth
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  centroids <- world |>
    st_centroid() |>
    st_coordinates() |>
    as_tibble() |>
    rename(lon = X, lat = Y)
  
  df_ref <- world |>
    as_tibble() |>
    bind_cols(centroids) |>
    transmute(
      iso3        = iso_a3,
      iso2        = iso_a2,
      country_en  = name,
      continent   = continent,
      subregion   = subregion,
      region_wb   = region_wb,
      pop_est     = pop_est,
      gdp_md      = gdp_md,
      lon, lat
    ) |>
    filter(!is.na(iso3), iso3 != "-99", nchar(iso3) == 3)
  
  # Enrichir avec countrycode
  df_ref <- df_ref |>
    mutate(
      country_fr = countrycode(iso3, "iso3c", "cldr.short.fr",
                               warn = FALSE),
      region_un  = countrycode(iso3, "iso3c", "region",
                               warn = FALSE),
      # Groupes continentaux simplifiés
      continent_group = case_when(
        continent == "Africa"         ~ "Afrique",
        continent == "Asia"           ~ "Asie",
        continent %in% c("Europe")   ~ "Europe",
        continent == "North America"  ~ "Amériques",
        continent == "South America"  ~ "Amériques",
        continent == "Oceania"        ~ "Océanie",
        TRUE                          ~ "Autre"
      )
    )
  
  message(glue("✓ Table de référence : {nrow(df_ref)} pays"))
  df_ref
}

# ------------------------------------------------------------
# Calcul des distances vers les grandes rédactions
# ------------------------------------------------------------

#' Calcule la distance moyenne vers les 5 grandes rédactions mondiales
#'
#' @param df_ref tibble avec colonnes lon, lat, iso3
#' @return df_ref enrichi avec dist_redactions_km
add_media_distances <- function(df_ref) {
  message("Calcul des distances vers les grandes rédactions...")
  
  redactions <- tibble(
    ville = c("Paris", "Londres", "New_York", "Washington", "Berlin"),
    lon   = c( 2.35,   -0.12,    -74.01,     -77.04,        13.40),
    lat   = c(48.85,   51.51,     40.71,      38.91,         52.52)
  ) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  df_sf <- df_ref |>
    filter(!is.na(lon), !is.na(lat)) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  dist_matrix  <- st_distance(df_sf, redactions)
  dist_moyenne <- apply(dist_matrix, 1, mean) / 1000
  
  coords <- st_coordinates(df_sf)
  
  df_result <- df_sf |>
    st_drop_geometry() |>
    mutate(
      lon                = coords[, 1],
      lat                = coords[, 2],
      dist_redactions_km = as.numeric(dist_moyenne),
      # Score de proximité normalisé (0 = très loin, 1 = très proche)
      proximity_score    = 1 / log1p(dist_redactions_km / 1000)
    )
  
  # Rejoindre avec les pays sans coordonnées
  df_ref |>
    select(-lon, -lat) |>
    left_join(
      df_result |> select(iso3, lon, lat,
                          dist_redactions_km, proximity_score),
      by = "iso3"
    )
}

# ------------------------------------------------------------
# Facteur de biais linguistique
# ------------------------------------------------------------

#' Ajoute un facteur de biais linguistique
#' (pays anglophones reçoivent plus de couverture)
#'
#' @param df_ref tibble de référence pays
#' @return df_ref enrichi avec language_bias
add_language_bias <- function(df_ref) {
  
  # Pays principalement anglophones
  english_speaking <- c(
    "USA", "GBR", "AUS", "CAN", "NZL", "IRL",
    "ZAF", "NGA", "GHA", "KEN", "UGA", "TZA",
    "ZMB", "ZWE", "BWA", "NAM", "SLE", "LBR"
  )
  
  # Pays francophones
  french_speaking <- c(
    "FRA", "BEL", "CHE", "CAN", "SEN", "CIV",
    "CMR", "COD", "MDG", "BFA", "MLI", "NER",
    "TCD", "CAF", "COG", "GAB", "HTI", "MRT"
  )
  
  df_ref |>
    mutate(
      language_group = case_when(
        iso3 %in% english_speaking ~ "anglophone",
        iso3 %in% french_speaking  ~ "francophone",
        TRUE                        ~ "other"
      ),
      # Facteur de biais : les anglophones reçoivent ~40% plus de couverture
      language_bias = case_when(
        language_group == "anglophone" ~ 1.4,
        language_group == "francophone"~ 1.1,
        TRUE                            ~ 1.0
      )
    )
}

# ------------------------------------------------------------
# EXÉCUTION
# ------------------------------------------------------------

run_geo_normalizer <- function(save = TRUE) {
  message("\n===== NORMALISATION GÉOGRAPHIQUE =====")
  
  dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
  
  df_ref <- build_country_reference()
  df_ref <- add_media_distances(df_ref)
  df_ref <- add_language_bias(df_ref)
  
  message(glue("\nPays avec distance calculée : {sum(!is.na(df_ref$dist_redactions_km))}"))
  message(glue("Distance moy. Europe       : {round(mean(df_ref$dist_redactions_km[df_ref$continent == 'Europe'], na.rm=TRUE))} km"))
  message(glue("Distance moy. Afrique      : {round(mean(df_ref$dist_redactions_km[df_ref$continent == 'Africa'], na.rm=TRUE))} km"))
  message(glue("Distance moy. Asie         : {round(mean(df_ref$dist_redactions_km[df_ref$continent == 'Asia'], na.rm=TRUE))} km"))
  
  if (save) {
    write_csv(df_ref, "data/processed/country_reference.csv")
    message("✓ Sauvegardé dans data/processed/country_reference.csv")
  }
  
  df_ref
}