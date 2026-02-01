# Package index

## Fonctions Principales

Fonctions principales pour nettoyer les données de rendement

- [`clean_yield()`](https://cedricbouffard.github.io/yieldcleanr/reference/clean_yield.md)
  : Fonction unifiee de nettoyage des donnees de rendement
- [`clean_yield_fast()`](https://cedricbouffard.github.io/yieldcleanr/reference/clean_yield_fast.md)
  : Nettoyage rapide des données de rendement avec mise en cache
- [`clean_yield_with_tracking()`](https://cedricbouffard.github.io/yieldcleanr/reference/clean_yield_with_tracking.md)
  : Nettoyage des donnees de rendement avec suivi des suppressions
- [`read_yield_data()`](https://cedricbouffard.github.io/yieldcleanr/reference/read_yield_data.md)
  : Lire des donnees de rendement brutes depuis un fichier texte
- [`launch_shiny_app()`](https://cedricbouffard.github.io/yieldcleanr/reference/launch_shiny_app.md)
  : Lancer l'application Shiny de nettoyage des rendements

## AYCE - Auto Yield Cleaning Engine

Fonctions du moteur de nettoyage automatique AYCE

- [`ayce_clean()`](https://cedricbouffard.github.io/yieldcleanr/reference/ayce_clean.md)
  : AYCE : Auto Yield Cleaning Engine (sortie imperiale)
- [`ayce_sf()`](https://cedricbouffard.github.io/yieldcleanr/reference/ayce_sf.md)
  : Pipeline AYCE complet avec sortie SF
- [`preprocess_yield_data()`](https://cedricbouffard.github.io/yieldcleanr/reference/preprocess_yield_data.md)
  : Pré-traitement des données de rendement

## Application des Filtres

Fonctions pour appliquer les filtres de nettoyage

- [`apply_flow_delay()`](https://cedricbouffard.github.io/yieldcleanr/reference/apply_flow_delay.md)
  : Appliquer la correction de delai de flux
- [`apply_local_sd_filter()`](https://cedricbouffard.github.io/yieldcleanr/reference/apply_local_sd_filter.md)
  : Filtre d'ecart-type localise
- [`apply_moisture_delay()`](https://cedricbouffard.github.io/yieldcleanr/reference/apply_moisture_delay.md)
  : Appliquer la correction de delai d'humidite
- [`apply_overlap_filter()`](https://cedricbouffard.github.io/yieldcleanr/reference/apply_overlap_filter.md)
  : Filtre de chevauchement base sur un bitmap
- [`apply_pcdi()`](https://cedricbouffard.github.io/yieldcleanr/reference/apply_pcdi.md)
  : PCDI : Phase Correlation Delay Identification (Version Rapide)
- [`apply_position_filter()`](https://cedricbouffard.github.io/yieldcleanr/reference/apply_position_filter.md)
  : Appliquer le filtre de position (POS)
- [`apply_yield_filters()`](https://cedricbouffard.github.io/yieldcleanr/reference/apply_yield_filters.md)
  : Application des filtres de rendement

## Filtres de Données

Fonctions de filtrage des données de rendement

- [`filter_bounds()`](https://cedricbouffard.github.io/yieldcleanr/reference/filter_bounds.md)
  : Filtrer selon les limites geographiques
- [`filter_dop()`](https://cedricbouffard.github.io/yieldcleanr/reference/filter_dop.md)
  : Filtrer selon le DOP (Dilution of Precision)
- [`filter_gps_status()`](https://cedricbouffard.github.io/yieldcleanr/reference/filter_gps_status.md)
  : Filtrer selon le statut GPS
- [`filter_header_status()`](https://cedricbouffard.github.io/yieldcleanr/reference/filter_header_status.md)
  : Filtrer selon le statut du header
- [`filter_heading_anomalies()`](https://cedricbouffard.github.io/yieldcleanr/reference/filter_heading_anomalies.md)
  : Filtre pour variations brusques de direction du header
- [`filter_local_std()`](https://cedricbouffard.github.io/yieldcleanr/reference/filter_local_std.md)
  : Appliquer le filtre ET local
- [`filter_moisture_range()`](https://cedricbouffard.github.io/yieldcleanr/reference/filter_moisture_range.md)
  : Filtrer selon la plage d'humidite
- [`filter_position_outliers()`](https://cedricbouffard.github.io/yieldcleanr/reference/filter_position_outliers.md)
  : Filtre de position pour eliminer les points hors champ
- [`filter_sliding_window()`](https://cedricbouffard.github.io/yieldcleanr/reference/filter_sliding_window.md)
  : Appliquer le filtre a fenetre glissante
- [`filter_velocity()`](https://cedricbouffard.github.io/yieldcleanr/reference/filter_velocity.md)
  : Filtrer selon la plage de vitesse
- [`filter_velocity_jumps()`](https://cedricbouffard.github.io/yieldcleanr/reference/filter_velocity_jumps.md)
  : Filtre pour changements brusques de vitesse
- [`filter_yield_range()`](https://cedricbouffard.github.io/yieldcleanr/reference/filter_yield_range.md)
  : Filtrer selon la plage de rendement

## Calculs et Statistiques

Fonctions de calcul et analyse des données

- [`calculate_auto_thresholds()`](https://cedricbouffard.github.io/yieldcleanr/reference/calculate_auto_thresholds.md)
  : Calculer les seuils automatiques (methode quantiles-IQR)
- [`calculate_filter_counts()`](https://cedricbouffard.github.io/yieldcleanr/reference/calculate_filter_counts.md)
  : Calculer le nombre de points retires par chaque filtre

## Utilitaires

Fonctions utilitaires pour la manipulation des données

- [`convert_flow_to_yield()`](https://cedricbouffard.github.io/yieldcleanr/reference/convert_flow_to_yield.md)
  : Convertir le flux de grain en rendement (boisseaux/acre)
- [`export_raster()`](https://cedricbouffard.github.io/yieldcleanr/reference/export_raster.md)
  : Exporter les donnees nettoyees en raster
- [`latlon_to_utm()`](https://cedricbouffard.github.io/yieldcleanr/reference/latlon_to_utm.md)
  : Convertir Latitude/Longitude en coordonnees UTM
- [`list_fields_from_zip()`](https://cedricbouffard.github.io/yieldcleanr/reference/list_fields_from_zip.md)
  : Lister les champs disponibles dans un fichier ZIP
- [`read_yield_from_zip()`](https://cedricbouffard.github.io/yieldcleanr/reference/read_yield_from_zip.md)
  : Lire les donnees de rendement depuis un fichier ZIP
- [`remove_overlap()`](https://cedricbouffard.github.io/yieldcleanr/reference/remove_overlap.md)
  : Supprimer les points en chevauchement
- [`save_raster()`](https://cedricbouffard.github.io/yieldcleanr/reference/save_raster.md)
  : Sauvegarder un raster en fichier
