# ============================================================
# app.R — Dashboard Shiny
# Indice d'Invisibilité — Crises humanitaires 2020-2024
# ============================================================

library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(leaflet)
library(DT)
library(glue)
library(scales)
library(here)

# ------------------------------------------------------------
# Chargement des données
# ------------------------------------------------------------

#  Utiliser here() pour tous les chemins
df_indices <- read_csv(here("data/final/crises_indices.csv"),
                       show_col_types = FALSE)
df_biais   <- read_csv(here("data/final/biais_continental.csv"),
                       show_col_types = FALSE)
df_vol     <- read_csv(here("data/final/volatilite_media.csv"),
                       show_col_types = FALSE)

df_hypotheses <- tryCatch(
  read_csv(here("data/final/resultats_hypotheses.csv"),
           show_col_types = FALSE),
  error = function(e) {
    tibble(
      hypothese  = c("H1","H2","H3","H4"),
      p_value    = NA_real_,
      conclusion = "Fichier non disponible"
    )
  }
)

# scorer.R produit V_final (STL) ou V (sd/mean)
if (!"V" %in% names(df_vol) && "V_final" %in% names(df_vol)) {
  df_vol <- df_vol |> rename(V = V_final)
}

# worldbank.R produit population/pib_md, merger.R peut avoir pop_est/gdp_md
if (!"pop_est" %in% names(df_indices) && "population" %in% names(df_indices)) {
  df_indices <- df_indices |> rename(pop_est = population)
}
if (!"gdp_md" %in% names(df_indices) && "pib_md" %in% names(df_indices)) {
  df_indices <- df_indices |> rename(gdp_md = pib_md)
}

# Agrégation par pays (pour la carte et le tableau)
df_pays <- df_indices |>
  group_by(iso3, country, continent_group) |>
  summarise(
    I_moy          = round(mean(I,               na.rm = TRUE), 2),
    G_moy          = round(mean(G,               na.rm = TRUE), 2),
    M_moy          = round(mean(M,               na.rm = TRUE), 2),
    morts_total    = sum(total_deaths_all,        na.rm = TRUE),
    articles_total = sum(nb_articles_total,       na.rm = TRUE),
    # Zone modale sur la période
    zone           = names(sort(table(zone), decreasing = TRUE))[1],
    .groups = "drop"
  )

# Couleurs du projet
COLORS <- list(
  rouge  = "#c0392b", orange = "#e67e22",
  vert   = "#27ae60", bleu   = "#2980b9",
  fond   = "#1a1a2e", texte  = "#ecf0f1"
)

# Années disponibles
annees_dispo <- sort(unique(df_indices$year))

# ------------------------------------------------------------
# Thème bslib
# ------------------------------------------------------------

theme_app <- bs_theme(
  bootswatch   = "darkly",
  primary      = "#e74c3c",
  secondary    = "#2980b9",
  success      = "#27ae60",
  warning      = "#e67e22",
  base_font    = font_google("Inter"),
  heading_font = font_google("Inter"),
  bg           = "#1a1a2e",
  fg           = "#ecf0f1"
)

# ------------------------------------------------------------
# Fonctions helpers UI
# ------------------------------------------------------------

kpi_card <- function(title, value, subtitle = NULL,
                     color = "#e74c3c") {
  div(
    class = "card h-100",
    style = glue("border-left: 4px solid {color};
                  background: #16213e;"),
    div(
      class = "card-body p-3",
      p(class = "text-muted mb-1",
        style = "font-size: 0.75rem; text-transform: uppercase;
                 letter-spacing: 1px;",
        title),
      h3(class = "mb-0 fw-bold",
         style = glue("color: {color};"),
         value),
      if (!is.null(subtitle))
        p(class = "text-muted mb-0",
          style = "font-size: 0.8rem; margin-top: 4px;",
          subtitle
        )
    )
  )
}

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------

ui <- page_navbar(
  title = div(
    span("🌍", style = "margin-right: 8px;"),
    span("Indice d'Invisibilité",
         style = "color: #e74c3c; font-weight: bold;"),
    span(" (Crises humanitaires 2020-2024)",
         style = "color: #bdc3c7; font-size: 0.85em;")
  ),
  theme    = theme_app,
  bg       = "#1a1a2e",
  fillable = TRUE,
  
  # ── Onglet 1 : Vue globale ────────────────────────────────
  nav_panel(
    title = "🗺️ Vue globale",
    value = "vue_globale",
    
    layout_sidebar(
      sidebar = sidebar(
        width = 280,
        bg    = "#16213e",
        
        h6("Filtres", class = "text-danger fw-bold mb-3"),
        
        selectInput(
          "filtre_continent",
          "Continent",
          choices  = c("Tous",
                       sort(na.omit(unique(df_pays$continent_group)))),
          selected = "Tous"
        ),
        
        # Zones dynamiques — ordre logique, uniquement zones présentes
        selectInput(
          "filtre_zone",
          "Zone d'invisibilité",
          choices  = c("Toutes", {
            zones_ordre <- c("Rouge (invisible)", "Orange (sous-couvert)",
                             "Verte (proportionnel)", "Bleue (sur-médiatisé)")
            zones_ordre[zones_ordre %in% unique(na.omit(df_indices$zone))]
          }),
          selected = "Toutes"
        ),
        
        # CORRECTION : bornes dynamiques depuis les données
        sliderInput(
          "filtre_annee",
          "Période",
          min   = min(annees_dispo),
          max   = max(annees_dispo),
          value = c(min(annees_dispo), max(annees_dispo)),
          step  = 1, sep = ""
        ),
        
        hr(style = "border-color: #2c3e50;"),
        h6("À propos", class = "text-muted"),
        p(class = "text-muted",
          style = "font-size: 0.75rem;",
          "L'Indice d'Invisibilité mesure le décalage entre
           la gravité réelle d'une crise et sa couverture
           médiatique internationale.")
      ),
      
      # KPIs
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        uiOutput("kpi_crises"),
        uiOutput("kpi_invisible"),
        uiOutput("kpi_morts"),
        uiOutput("kpi_articles")
      ),
      
      # Carte
      card(
        full_screen = TRUE,
        card_header("Carte mondiale — Indice d'Invisibilité",
                    class = "bg-danger text-white"),
        leafletOutput("carte_mondiale", height = "450px")
      )
    )
  ),
  
  # ── Onglet 2 : Analyse comparative ───────────────────────
  nav_panel(
    title = "📊 Analyse comparative",
    value = "analyse",
    
    layout_columns(
      col_widths = c(8, 4),
      
      card(
        full_screen = TRUE,
        card_header("Gravité vs Couverture médiatique",
                    class = "bg-danger text-white"),
        plotlyOutput("scatter_gm", height = "450px")
      ),
      
      card(
        full_screen = TRUE,
        card_header("Top 10 crises invisibles",
                    class = "bg-warning text-dark"),
        plotlyOutput("top_invisible", height = "450px")
      )
    ),
    
    layout_columns(
      col_widths = c(6, 6),
      
      card(
        card_header("Distribution par zone"),
        plotlyOutput("donut_zones", height = "350px")
      ),
      
      card(
        card_header("Invisibilité par continent"),
        plotlyOutput("zones_continent", height = "350px")
      )
    )
  ),
  
  # ── Onglet 3 : Temporalité ────────────────────────────────
  nav_panel(
    title = "📈 Temporalité",
    value = "temporalite",
    
    layout_sidebar(
      sidebar = sidebar(
        width = 250,
        bg    = "#16213e",
        
        h6("Sélection", class = "text-danger fw-bold"),
        
        selectizeInput(
          "pays_timeline",
          "Pays à comparer",
          choices  = sort(unique(df_pays$country)),
          selected = c("Ukraine", "Yemen", "Ethiopia",
                       "Sudan", "Afghanistan"),
          multiple = TRUE,
          options  = list(maxItems = 8)
        )
      ),
      
      card(
        full_screen = TRUE,
        card_header("Évolution de la couverture médiatique",
                    class = "bg-danger text-white"),
        plotlyOutput("timeline", height = "400px")
      ),
      
      card(
        card_header("Peak & Forget — Volatilité médiatique"),
        plotlyOutput("peak_forget", height = "350px")
      )
    )
  ),
  
  # ── Onglet 4 : Biais médiatique ───────────────────────────
  nav_panel(
    title = "⚖️ Biais médiatique",
    value = "biais",
    
    layout_columns(
      col_widths = c(6, 6),
      
      card(
        full_screen = TRUE,
        card_header("Biais par continent"),
        plotlyOutput("heatmap_biais", height = "400px")
      ),
      
      card(
        full_screen = TRUE,
        card_header("Résultats des hypothèses"),
        # CORRECTION : tableOutput → DTOutput pour meilleur rendu
        DTOutput("table_hypotheses")
      )
    ),
    
    card(
      card_header("Données complètes — Tableau interactif"),
      DTOutput("table_donnees")
    )
  ),
  
  # ── Onglet 5 : Cas d'étude ────────────────────────────────
  nav_panel(
    title = "🔍 Cas d'étude",
    value = "cas_etude",
    
    layout_sidebar(
      sidebar = sidebar(
        width = 250,
        bg    = "#16213e",
        
        h6("Sélectionner un pays",
           class = "text-danger fw-bold"),
        
        selectInput(
          "pays_detail",
          "Pays",
          choices  = sort(unique(df_pays$country)),
          selected = "Yemen"
        )
      ),
      
      uiOutput("fiche_pays"),
      
      card(
        card_header("Évolution des indices"),
        plotlyOutput("evolution_pays", height = "350px")
      )
    )
  )
)

# ------------------------------------------------------------
# SERVER
# ------------------------------------------------------------

server <- function(input, output, session) {
  
  # ── Données réactives ──────────────────────────────────────
  
  # CORRECTION : df_filtre filtre df_pays (agrégat fixe)
  # pour la carte et les KPIs
  df_filtre <- reactive({
    df <- df_pays
    
    if (input$filtre_continent != "Tous")
      df <- df |> filter(continent_group == input$filtre_continent)
    
    if (input$filtre_zone != "Toutes")
      df <- df |> filter(zone == input$filtre_zone)
    
    df
  })
  
  # CORRECTION : df_indices_filtre filtre par année ET continent ET zone
  # pour que le slider "Période" affecte tous les graphiques
  df_indices_filtre <- reactive({
    df <- df_indices |>
      filter(year >= input$filtre_annee[1],
             year <= input$filtre_annee[2])
    
    if (input$filtre_continent != "Tous")
      df <- df |> filter(continent_group == input$filtre_continent)
    
    if (input$filtre_zone != "Toutes")
      df <- df |> filter(zone == input$filtre_zone)
    
    df
  })
  
  # ── KPI Cards ─────────────────────────────────────────────
  
  output$kpi_crises <- renderUI({
    # CORRECTION : KPIs basés sur df_indices_filtre (respecte la période)
    df <- df_indices_filtre()
    n  <- n_distinct(df$iso3)
    kpi_card("Pays analysés", n,
             glue("{n_distinct(df$year)} année(s)"),
             "#e74c3c")
  })
  
  output$kpi_invisible <- renderUI({
    df <- df_indices_filtre()
    n  <- sum(df$zone == "Rouge (invisible)", na.rm = TRUE)
    kpi_card("Obs. invisibles", n,
             "Zone rouge (I > 5)", "#c0392b")
  })
  
  output$kpi_morts <- renderUI({
    df    <- df_indices_filtre()
    total <- sum(df$total_deaths_all, na.rm = TRUE)
    kpi_card("Morts totaux", scales::comma(total),
             glue("{input$filtre_annee[1]}–{input$filtre_annee[2]}"),
             "#e67e22")
  })
  
  output$kpi_articles <- renderUI({
    df    <- df_indices_filtre()
    total <- sum(df$nb_articles_total, na.rm = TRUE)
    kpi_card("Articles médias", scales::comma(total),
             "collectés", "#2980b9")
  })
  
  # ── Carte mondiale ─────────────────────────────────────────
  
  output$carte_mondiale <- renderLeaflet({
    library(rnaturalearth)
    library(sf)
    
    # CORRECTION : la carte agrège df_indices_filtre (respect période + filtres)
    df_carte <- df_indices_filtre() |>
      group_by(iso3, country, continent_group) |>
      summarise(
        I_moy          = round(mean(I,             na.rm = TRUE), 2),
        G_moy          = round(mean(G,             na.rm = TRUE), 2),
        M_moy          = round(mean(M,             na.rm = TRUE), 2),
        morts_total    = sum(total_deaths_all,     na.rm = TRUE),
        articles_total = sum(nb_articles_total,    na.rm = TRUE),
        zone           = names(sort(table(zone), decreasing = TRUE))[1],
        .groups = "drop"
      )
    
    world <- ne_countries(scale = "medium", returnclass = "sf") |>
      select(iso3 = iso_a3, geometry)
    
    world_data <- world |>
      left_join(df_carte, by = "iso3") |>
      filter(!is.na(I_moy))
    
    pal <- colorNumeric(
      palette  = c("#2980b9", "#27ae60", "#e67e22", "#c0392b"),
      domain   = world_data$I_moy,
      na.color = "#2c3e50"
    )
    
    leaflet(world_data,
            options = leafletOptions(minZoom = 2)) |>
      addProviderTiles(providers$CartoDB.DarkMatter) |>
      addPolygons(
        fillColor   = ~pal(I_moy),
        fillOpacity = 0.8,
        color       = "#1a1a2e",
        weight      = 0.5,
        popup = ~glue(
          "<b style='color:#e74c3c'>{country}</b><br/>
           I: <b>{I_moy}</b> | G: {G_moy} | M: {M_moy}<br/>
           Morts: {scales::comma(morts_total)}<br/>
           Articles: {scales::comma(articles_total)}<br/>
           Zone: {zone}"
        ),
        highlight = highlightOptions(
          weight       = 2,
          color        = "#e74c3c",
          fillOpacity  = 0.9,
          bringToFront = TRUE
        ),
        label = ~glue("{country} — I: {I_moy}")
      ) |>
      addLegend(
        position = "bottomright",
        pal      = pal,
        values   = ~I_moy,
        title    = "Indice<br>d'Invisibilité",
        opacity  = 0.9
      ) |>
      setView(lng = 10, lat = 20, zoom = 2)
  })
  
  # ── Scatter G vs M ─────────────────────────────────────────
  
  output$scatter_gm <- renderPlotly({
    df <- df_indices_filtre() |>
      filter(!is.na(continent_group)) |>
      group_by(iso3, country, continent_group) |>
      summarise(
        G_moy       = mean(G, na.rm = TRUE),
        M_moy       = mean(M, na.rm = TRUE),
        I_moy       = mean(I, na.rm = TRUE),
        morts_total = sum(total_deaths_all, na.rm = TRUE),
        .groups = "drop"
      )
    
    p <- ggplot(df, aes(
      x    = M_moy, y = G_moy,
      color = continent_group,
      size  = log1p(morts_total),
      text  = glue("{country}\nI: {round(I_moy,2)} | G: {round(G_moy,2)} | M: {round(M_moy,2)}\nMorts: {scales::comma(morts_total)}")
    )) +
      geom_abline(slope = 1, intercept = 0,
                  linetype = "dashed",
                  color = "#7f8c8d", alpha = 0.5) +
      geom_point(alpha = 0.7) +
      scale_color_manual(values = c(
        "Afrique"   = "#c0392b", "Asie"      = "#e67e22",
        "Amériques" = "#27ae60", "Europe"    = "#2980b9",
        "Océanie"   = "#9b59b6", "Autre"     = "#7f8c8d"
      )) +
      scale_size_continuous(range = c(2, 8), guide = "none") +
      labs(x = "Score Médiatique (M)",
           y = "Score de Gravité (G)",
           color = NULL) +
      theme_minimal() +
      theme(
        plot.background  = element_rect(fill = "#1a1a2e", color = NA),
        panel.background = element_rect(fill = "#1a1a2e", color = NA),
        panel.grid.major = element_line(color = "#2c3e50"),
        text             = element_text(color = "#ecf0f1"),
        axis.text        = element_text(color = "#bdc3c7")
      )
    
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#1a1a2e",
             plot_bgcolor  = "#1a1a2e",
             font          = list(color = "#ecf0f1"),
             legend        = list(font = list(color = "#ecf0f1")))
  })
  
  # ── Top 10 invisibles ──────────────────────────────────────
  
  output$top_invisible <- renderPlotly({
    # Déterminer les colonnes disponibles avant de construire le tibble
    col_gdp_hab <- dplyr::case_when(
      "gdp_md"      %in% names(df_indices) &
        "pop_est"   %in% names(df_indices) ~ "gdp_md_pop",
      "pib_habitant" %in% names(df_indices) ~ "pib_habitant",
      TRUE ~ "none"
    )
    col_pop <- if ("pop_est"    %in% names(df_indices)) "pop_est"
    else if ("population" %in% names(df_indices)) "population"
    else "none"
    
    df <- df_indices_filtre() |>
      group_by(iso3, country, continent_group) |>
      summarise(
        I_moy       = mean(I,               na.rm = TRUE),
        G_moy       = mean(G,               na.rm = TRUE),
        morts_total = sum(total_deaths_all, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Ajouter gdp_hab selon les colonnes disponibles
    df$gdp_hab <- switch(
      col_gdp_hab,
      "gdp_md_pop"   = mean(df_indices$gdp_md * 1e6 /
                              pmax(df_indices$pop_est, 1), na.rm = TRUE),
      "pib_habitant" = mean(df_indices$pib_habitant, na.rm = TRUE),
      NA_real_
    )
    
    # Ajouter pop_moy selon les colonnes disponibles
    df$pop_moy <- if (col_pop != "none")
      mean(df_indices[[col_pop]], na.rm = TRUE)
    else
      NA_real_
    
    df <- df |>
      filter(
        G_moy > 2,
        continent_group %in% c("Afrique", "Asie", "Amériques"),
        morts_total > 500
      )
    
    # Filtre PIB si disponible
    if (!all(is.na(df$gdp_hab))) {
      df <- df |> filter(is.na(gdp_hab) | gdp_hab < 15000)
    }
    
    df <- df |>
      slice_max(I_moy, n = 10) |>
      mutate(country = fct_reorder(country, I_moy))
    
    if (nrow(df) == 0) {
      return(plot_ly() |>
               layout(paper_bgcolor = "#1a1a2e",
                      plot_bgcolor  = "#1a1a2e",
                      title = list(text = "Aucune donnée pour ces filtres",
                                   font = list(color = "#ecf0f1"))))
    }
    
    plot_ly(df,
            x           = ~I_moy,
            y           = ~country,
            type        = "bar",
            orientation = "h",
            color       = ~continent_group,
            colors      = c("Afrique"   = "#c0392b",
                            "Asie"      = "#e67e22",
                            "Amériques" = "#27ae60"),
            text        = ~glue("{round(I_moy,1)} | {scales::comma(morts_total)} morts"),
            hoverinfo   = "text") |>
      layout(
        paper_bgcolor = "#1a1a2e",
        plot_bgcolor  = "#1a1a2e",
        font          = list(color = "#ecf0f1"),
        xaxis         = list(title = "Indice d'Invisibilité",
                             gridcolor = "#2c3e50"),
        yaxis         = list(title = "", gridcolor = "#2c3e50"),
        showlegend    = TRUE
      )
  })
  
  # ── Donut zones ────────────────────────────────────────────
  
  output$donut_zones <- renderPlotly({
    df_z <- df_indices_filtre() |>
      count(zone) |>
      mutate(pct = round(n / sum(n) * 100, 1))
    
    plot_ly(df_z,
            labels    = ~zone,
            values    = ~n,
            type      = "pie",
            hole      = 0.5,
            marker    = list(colors = c(
              "#c0392b", "#e67e22", "#27ae60", "#2980b9"
            )),
            text      = ~glue("{pct}%"),
            hoverinfo = "label+value+percent") |>
      layout(
        paper_bgcolor = "#1a1a2e",
        font          = list(color = "#ecf0f1"),
        showlegend    = TRUE,
        legend        = list(font = list(color = "#ecf0f1"))
      )
  })
  
  # ── Zones par continent ────────────────────────────────────
  
  output$zones_continent <- renderPlotly({
    df_c <- df_indices_filtre() |>
      filter(!is.na(continent_group)) |>
      count(continent_group, zone) |>
      group_by(continent_group) |>
      mutate(pct = round(n / sum(n) * 100, 1)) |>
      ungroup()
    
    plot_ly(df_c,
            x     = ~continent_group,
            y     = ~pct,
            color = ~zone,
            colors = c("#c0392b", "#e67e22", "#27ae60", "#2980b9"),
            type  = "bar") |>
      layout(
        barmode       = "stack",
        paper_bgcolor = "#1a1a2e",
        plot_bgcolor  = "#1a1a2e",
        font          = list(color = "#ecf0f1"),
        xaxis         = list(title = ""),
        yaxis         = list(title = "%", gridcolor = "#2c3e50"),
        legend        = list(font = list(color = "#ecf0f1"))
      )
  })
  
  # ── Timeline ───────────────────────────────────────────────
  
  output$timeline <- renderPlotly({
    pays <- input$pays_timeline
    if (length(pays) == 0) return(NULL)
    
    df_t <- df_indices |>
      filter(country %in% pays) |>
      select(country, year, nb_articles_total,
             total_deaths_all, I, G, M)
    
    plot_ly(df_t,
            x     = ~year,
            y     = ~nb_articles_total,
            color = ~country,
            type  = "scatter",
            mode  = "lines+markers",
            text  = ~glue("{country} ({year})\nArticles: {nb_articles_total}\nMorts: {scales::comma(total_deaths_all)}\nI: {round(I,2)}"),
            hoverinfo = "text") |>
      layout(
        paper_bgcolor = "#1a1a2e",
        plot_bgcolor  = "#1a1a2e",
        font          = list(color = "#ecf0f1"),
        xaxis         = list(title = "Année",
                             gridcolor = "#2c3e50", dtick = 1),
        yaxis         = list(title = "Nb articles",
                             gridcolor = "#2c3e50"),
        legend        = list(font = list(color = "#ecf0f1"))
      )
  })
  
  # ── Peak & Forget ──────────────────────────────────────────
  
  output$peak_forget <- renderPlotly({
    pays <- input$pays_timeline
    if (length(pays) == 0) return(NULL)
    
    # CORRECTION : utiliser la colonne V (harmonisée au chargement)
    df_pf <- df_vol |>
      filter(country %in% pays) |>
      filter(!is.na(V)) |>
      mutate(country = fct_reorder(country, V))
    
    if (nrow(df_pf) == 0) return(NULL)
    
    plot_ly(df_pf,
            x           = ~V,
            y           = ~country,
            type        = "bar",
            orientation = "h",
            marker      = list(
              color = ~ifelse(peak_forget, "#c0392b", "#2980b9")
            ),
            text        = ~glue("V = {round(V, 2)}"),
            hoverinfo   = "text+y") |>
      layout(
        paper_bgcolor = "#1a1a2e",
        plot_bgcolor  = "#1a1a2e",
        font          = list(color = "#ecf0f1"),
        xaxis         = list(title = "Score de Volatilité (V)",
                             gridcolor = "#2c3e50"),
        yaxis         = list(title = ""),
        shapes        = list(list(
          type = "line", x0 = 1, x1 = 1,
          y0   = 0,      y1  = 1, yref = "paper",
          line = list(color = "#e67e22",
                      dash  = "dash", width = 1.5)
        ))
      )
  })
  
  # ── Heatmap biais ──────────────────────────────────────────
  
  output$heatmap_biais <- renderPlotly({
    df_h <- df_biais |>
      select(continent_group, mean_G, mean_M, mean_I, B) |>
      pivot_longer(c(mean_G, mean_M, mean_I, B),
                   names_to  = "indicateur",
                   values_to = "valeur") |>
      mutate(indicateur = recode(indicateur,
                                 mean_G = "Gravité (G)",
                                 mean_M = "Couverture (M)",
                                 mean_I = "Invisibilité (I)",
                                 B      = "Biais (B)"
      ))
    
    plot_ly(df_h,
            x    = ~indicateur,
            y    = ~continent_group,
            z    = ~round(valeur, 2),
            type = "heatmap",
            colorscale = list(
              c(0,   "#2980b9"),
              c(0.5, "#2c3e50"),
              c(1,   "#c0392b")
            ),
            text      = ~round(valeur, 2),
            hoverinfo = "x+y+text") |>
      layout(
        paper_bgcolor = "#1a1a2e",
        plot_bgcolor  = "#1a1a2e",
        font          = list(color = "#ecf0f1"),
        xaxis         = list(title = ""),
        yaxis         = list(title = "")
      )
  })
  
  # ── Table hypothèses ───────────────────────────────────────
  
  # CORRECTION : données chargées au démarrage avec here()
  # + DTOutput pour meilleur rendu dark mode
  output$table_hypotheses <- renderDT({
    df_hypotheses |>
      select(
        Hypothèse  = hypothese,
        `p-value`  = p_value,
        Direction  = direction,
        Conclusion = conclusion
      ) |>
      datatable(
        options  = list(dom = "t", pageLength = 10),
        rownames = FALSE,
        class    = "table-dark table-hover"
      ) |>
      formatRound("p-value", digits = 4) |>
      formatStyle(
        "Conclusion",
        color = styleEqual(
          c("✅ Confirmée", "⚠️ Non confirmée"),
          c("#27ae60",      "#e67e22")
        )
      )
  })
  
  # ── Table données complètes ────────────────────────────────
  
  output$table_donnees <- renderDT({
    df_pays |>
      select(
        Pays       = country,
        Continent  = continent_group,
        `I (moy)`  = I_moy,
        `G (moy)`  = G_moy,
        `M (moy)`  = M_moy,
        Morts      = morts_total,
        Articles   = articles_total,
        Zone       = zone
      ) |>
      arrange(desc(`I (moy)`)) |>
      datatable(
        options  = list(pageLength = 15, scrollX = TRUE, dom = "ftip"),
        rownames = FALSE,
        class    = "table-dark table-hover"
      ) |>
      formatRound(c("I (moy)", "G (moy)", "M (moy)"), digits = 2) |>
      formatCurrency("Morts",    currency = "", digits = 0) |>
      formatCurrency("Articles", currency = "", digits = 0)
  })
  
  # ── Fiche pays ─────────────────────────────────────────────
  
  output$fiche_pays <- renderUI({
    pays <- input$pays_detail
    info <- df_pays |> filter(country == pays)
    if (nrow(info) == 0) return(NULL)
    
    zone_color <- case_when(
      str_detect(info$zone, "Rouge")  ~ "#c0392b",
      str_detect(info$zone, "Orange") ~ "#e67e22",
      str_detect(info$zone, "Verte")  ~ "#27ae60",
      TRUE                             ~ "#2980b9"
    )
    
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      kpi_card("Indice Invisibilité",
               info$I_moy, info$zone, zone_color),
      kpi_card("Score Gravité",
               info$G_moy, info$continent_group, "#e67e22"),
      kpi_card("Score Médiatique",
               info$M_moy, "Médias collectés", "#2980b9"),
      kpi_card("Morts totaux",
               scales::comma(info$morts_total),
               "2020-2024", "#9b59b6")
    )
  })
  
  # ── Évolution pays sélectionné ─────────────────────────────
  
  output$evolution_pays <- renderPlotly({
    pays <- input$pays_detail
    df_p <- df_indices |>
      filter(country == pays) |>
      select(year, G, M, I, nb_articles_total, total_deaths_all)
    
    if (nrow(df_p) == 0) return(NULL)
    
    plot_ly(df_p, x = ~year) |>
      add_lines(y = ~I, name = "Invisibilité (I)",
                line = list(color = "#c0392b", width = 2)) |>
      add_lines(y = ~G, name = "Gravité (G)",
                line = list(color = "#e67e22", width = 2)) |>
      add_lines(y = ~M, name = "Couverture (M)",
                line = list(color = "#2980b9", width = 2)) |>
      layout(
        paper_bgcolor = "#1a1a2e",
        plot_bgcolor  = "#1a1a2e",
        font          = list(color = "#ecf0f1"),
        xaxis         = list(title = "Année",
                             gridcolor = "#2c3e50", dtick = 1),
        yaxis         = list(title = "Score",
                             gridcolor = "#2c3e50"),
        legend        = list(font = list(color = "#ecf0f1"))
      )
  })
}

# ------------------------------------------------------------
# LANCEMENT
# ------------------------------------------------------------

shinyApp(ui = ui, server = server)