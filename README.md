# Indice composite d'invisibilité - Approche par croisement de données multisources

Modélisation de l'indice d'invisibilité médiatique des crises humanitaires. Une approche par croisement de données multisources : catastrophes (EM-DAT), conflits (UCDP), déplacements de populations (UNHCR) et flux financiers (OCHA FTS).


## Description

Ce projet vise à mesurer le décalage entre :

-   la gravité observée d'une crise humanitaire
-   la visibilité médiatique internationale qu'elle reçoit

Le cœur du projet est un indicateur composite, l'**Indice d'Invisibilité (`I`)**, calculé à l'échelle **pays-année** sur la période **2020-2024**.

Le pipeline combine notamment :

-   `EM-DAT` pour les catastrophes
-   `UCDP` pour les conflits
-   `The Guardian` et `New York Times` pour la couverture médiatique
-   `ReliefWeb` pour les signaux humanitaires
-   `UNHCR` pour les déplacements forcés
-   `OCHA FTS` pour le financement humanitaire
-   `rnaturalearth` pour les données pays et les distances

## Motivation

Toutes les crises ne reçoivent pas la même attention médiatique. Certaines occupent durablement l'espace public international, alors que d'autres restent peu visibles malgré des impacts humains majeurs.

L'objectif du projet est donc de produire une mesure reproductible de cette sous-couverture médiatique, afin de :

-   comparer les crises entre pays et dans le temps
-   tester plusieurs hypothèses sur les biais de couverture
-   appuyer un rapport final et un dashboard d'exploration

## Ce que produit le projet

Le pipeline génère principalement :

-   un fichier final d'indices par `pays × année`
-   des résultats statistiques sur les hypothèses du projet
-   des visualisations
-   un dashboard Shiny pour explorer les résultats

Les analyses actuellement conservées sont :

-   scoring des indices `G`, `M`, `I`
-   biais géographique / continental
-   volatilité médiatique et effet `Peak & Forget`
-   relation entre visibilité médiatique et financement FTS

Les modules de **clustering** et d'**analyse de sentiment** ont été retirés du pipeline courant.

## Arborescence

``` text
crisis-media-index/
├── dashboard/
│   └── app.R
├── data/
│   ├── raw/
│   │   ├── emdat/   # Catastrophes naturelles
│   │   ├── fts/     # Financement humanitaire
│   │   ├── guardian/  # Articles de presse internationale
│   │   ├── nyt/       # Articles de presse internationale
│   │   ├── reliefweb/  # Rapports et catastrophes humanitaires
│   │   ├── ucdp/     # Événements de conflit
│   │   ├── unhcr/   # Déplacements forcés (IDPs, réfugiés)
│   │   └── worldbank/ # Population, géographie, distances
│   ├── processed/
│   │   ├── emdat_clean.csv
│   │   ├── media_articles_clean.csv
│   │   ├── reliefweb_clean.csv
│   │   ├── ucdp_clean.csv
│   │   ├── ucdp_country_clean.csv
│   │   ├── unhcr_clean.csv
│   │   ├── worldbank_clean.csv
│   │   └── crises_merged.csv
│   └── final/      # Données finales
│       ├── crises_indices.csv
│       ├── biais_continental.csv
│       ├── volatilite_media.csv
│       └── resultats_hypotheses.csv
├── R/
│   ├── analysis/         # Volet analyse
│   │   ├── bias_analysis.R    # Biais médiatique
│   │   └── scorer.R           # Calcul des scores
│   ├── collectors/           # Pipeline de collecte
│   │   ├── emdat.R
│   │   ├── guardian.R
│   │   ├── newsapi.R
│   │   ├── ocha_fts.R
│   │   ├── reliefweb_scraper.R
│   │   ├── ucdp.R
│   │   └── worldbank.R
│   ├── processing/       # Pipeline de nettoyage et de traitement
│   │   ├── cleaner.R
│   │   ├── geo_normalizer.R
│   │   └── merger.R
│   └── viz/      # Pipeline de visualisation
│       ├── charts.R
│       ├── maps.R
│       ├── timeline_anim.R
│       └── wordcloud.R
├── report/       
│   ├── index.qmd # Rapport
│   └── assets/   # Visualisations
├── run_all.R
├── renv.lock
└── crisis-media-index.Rproj
```

## Pipeline

Le pipeline est organisé en 4 grandes étapes :

1.  `Collecte`
2.  `Nettoyage / harmonisation`
3.  `Fusion`
4.  `Scoring / analyses`

Le script d'orchestration principal est [run_all.R](/F:/crisis-media-index/run_all.R:1).

Fonctions principales :

-   `run_collect()` : lance les collecteurs
-   `run_process()` : nettoie et fusionne les données
-   `run_analyse()` : exécute le scoring et les analyses conservées

## Prérequis

### Environnement R

Le projet est structuré comme un projet R avec `renv`.

Packages clés :

-   `tidyverse`
-   `httr`, `httr2`
-   `jsonlite`
-   `lubridate`
-   `countrycode`
-   `janitor`
-   `plotly`
-   `leaflet`
-   `kableExtra`
-   `shiny`
-   `bslib`

### Variables d'environnement

Renseigner les clés API dans `.Renviron` :

``` env
NYT_API_KEY=...
GUARDIAN_KEY=...
```

Selon les usages, d'autres variables peuvent être présentes, mais celles-ci sont les plus importantes pour la collecte médias.

## Comment exécuter le projet

### 1. Ouvrir le projet

Ouvrir [crisis-media-index.Rproj](/F:/crisis-media-index/crisis-media-index.Rproj:1) dans RStudio.

### 2. Restaurer l'environnement

Dans R :

``` r
renv::restore()
```

### 3. Lancer le pipeline complet

``` r
source("run_all.R")
run_all()
```

### 4. Lancer seulement certaines étapes

Collecte seule :

``` r
source("run_all.R")
run_collect()
```

Traitement seul :

``` r
source("run_all.R")
run_process()
```

Analyse seule :

``` r
source("run_all.R")
run_analyse()
```

## Rapport final

Le rapport principal est [report/index.qmd](/crisis-media-index/report/index.qmd:1).

Il a été recentré sur :

-   les résultats globaux
-   les biais géographiques
-   le test `Peak & Forget`
-   la relation couverture / financement

## Dashboard

Le dashboard Shiny est [dashboard/app.R](/F:/crisis-media-index/dashboard/app.R:1).

Pour le lancer :

``` r
shiny::runApp("dashboard")
```

## Fichiers de sortie importants

-   [data/processed/media_articles_clean.csv](/F:/crisis-media-index/data/processed/media_articles_clean.csv:1)
-   [data/processed/crises_merged.csv](/F:/crisis-media-index/data/processed/crises_merged.csv:1)
-   [data/final/crises_indices.csv](/F:/crisis-media-index/data/final/crises_indices.csv:1)
-   [data/final/biais_continental.csv](/F:/crisis-media-index/data/final/biais_continental.csv:1)
-   [data/final/volatilite_media.csv](/F:/crisis-media-index/data/final/volatilite_media.csv:1)
-   [data/final/resultats_hypotheses.csv](/F:/crisis-media-index/data/final/resultats_hypotheses.csv:1)

## Limites actuelles

-   la couverture médiatique reste peu suffisante
-   certaines requêtes médias restent sensibles au bruit sémantique

## Auteur

**Abdoul Fataho NIAMPA**
