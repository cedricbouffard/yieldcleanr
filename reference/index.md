# Package index

## Fonctions Principales

Fonctions principales pour nettoyer les données de rendement

- [`clean_yield()`](clean_yield.md) : Fonction unifiee de nettoyage des
  donnees de rendement
- [`clean_yield_with_tracking()`](clean_yield_with_tracking.md) :
  Nettoyage des donnees de rendement avec suivi des suppressions
- [`read_yield_data()`](read_yield_data.md) : Lire des donnees de
  rendement brutes depuis un fichier texte
- [`launch_shiny_app()`](launch_shiny_app.md) : Lancer l'application
  Shiny de nettoyage des rendements

## AYCE - Auto Yield Cleaning Engine

Fonctions du moteur de nettoyage automatique AYCE

- [`ayce_clean()`](ayce_clean.md) : AYCE : Auto Yield Cleaning Engine
  (sortie imperiale)
- [`ayce_sf()`](ayce_sf.md) : Pipeline AYCE complet avec sortie SF

## Application des Filtres

Fonctions pour appliquer les filtres de nettoyage

- [`apply_flow_delay()`](apply_flow_delay.md) : Appliquer la correction
  de delai de flux
- [`apply_local_sd_filter()`](apply_local_sd_filter.md) : Filtre
  d'ecart-type localise
- [`apply_moisture_delay()`](apply_moisture_delay.md) : Appliquer la
  correction de delai d'humidite
- [`apply_overlap_filter()`](apply_overlap_filter.md) : Filtre de
  chevauchement base sur un bitmap
- [`apply_pcdi()`](apply_pcdi.md) : PCDI : Phase Correlation Delay
  Identification
- [`apply_position_filter()`](apply_position_filter.md) : Appliquer le
  filtre de position (POS)

## Filtres de Données

Fonctions de filtrage des données de rendement

- [`filter_bounds()`](filter_bounds.md) : Filtrer selon les limites
  geographiques
- [`filter_dop()`](filter_dop.md) : Filtrer selon le DOP (Dilution of
  Precision)
- [`filter_gps_status()`](filter_gps_status.md) : Filtrer selon le
  statut GPS
- [`filter_header_status()`](filter_header_status.md) : Filtrer selon le
  statut du header
- [`filter_heading_anomalies()`](filter_heading_anomalies.md) : Filtre
  pour variations brusques de direction du header
- [`filter_local_std()`](filter_local_std.md) : Appliquer le filtre ET
  local
- [`filter_moisture_range()`](filter_moisture_range.md) : Filtrer selon
  la plage d'humidite
- [`filter_sliding_window()`](filter_sliding_window.md) : Appliquer le
  filtre a fenetre glissante
- [`filter_velocity()`](filter_velocity.md) : Filtrer selon la plage de
  vitesse
- [`filter_velocity_jumps()`](filter_velocity_jumps.md) : Filtre pour
  changements brusques de vitesse
- [`filter_yield_range()`](filter_yield_range.md) : Filtrer selon la
  plage de rendement

## Calculs et Statistiques

Fonctions de calcul et analyse des données

- [`calculate_auto_thresholds()`](calculate_auto_thresholds.md) :
  Calculer les seuils automatiques (methode quantiles-IQR)
- [`calculate_filter_counts()`](calculate_filter_counts.md) : Calculer
  le nombre de points retires par chaque filtre

## Utilitaires

Fonctions utilitaires pour la manipulation des données

- [`convert_flow_to_yield()`](convert_flow_to_yield.md) : Convertir le
  flux de grain en rendement (boisseaux/acre)
- [`export_raster()`](export_raster.md) : Exporter les donnees nettoyees
  en raster
- [`latlon_to_utm()`](latlon_to_utm.md) : Convertir Latitude/Longitude
  en coordonnees UTM
- [`list_fields_from_zip()`](list_fields_from_zip.md) : Lister les
  champs disponibles dans un fichier ZIP
- [`read_yield_from_zip()`](read_yield_from_zip.md) : Lire les donnees
  de rendement depuis un fichier ZIP
- [`remove_overlap()`](remove_overlap.md) : Supprimer les points en
  chevauchement
- [`save_raster()`](save_raster.md) : Sauvegarder un raster en fichier
