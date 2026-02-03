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

## Filtres et Anomalies

Fonctions pour appliquer les filtres et détecter les anomalies

- [`filter_data()`](https://cedricbouffard.github.io/yieldcleanr/reference/filter_data.md)
  : Méta-fonction de filtrage unifiée
- [`detect_anomalies()`](https://cedricbouffard.github.io/yieldcleanr/reference/detect_anomalies.md)
  : Méta-fonction de détection d'anomalies
- [`apply_delay_adjustment()`](https://cedricbouffard.github.io/yieldcleanr/reference/apply_delay_adjustment.md)
  : Delay Adjustment : Delay Adjustment (Version Rapide)
- [`optimize_delays()`](https://cedricbouffard.github.io/yieldcleanr/reference/optimize_delays.md)
  : Méta-fonction d'optimisation des délais
- [`calculate_thresholds()`](https://cedricbouffard.github.io/yieldcleanr/reference/calculate_thresholds.md)
  : Méta-fonction de calcul des seuils
- [`calculate_auto_thresholds()`](https://cedricbouffard.github.io/yieldcleanr/reference/calculate_auto_thresholds.md)
  : Calculer les seuils automatiques (methode quantiles-IQR)
- [`apply_moisture_delay()`](https://cedricbouffard.github.io/yieldcleanr/reference/apply_moisture_delay.md)
  : Appliquer la correction de delai d'humidite
- [`calculate_filter_counts()`](https://cedricbouffard.github.io/yieldcleanr/reference/calculate_filter_counts.md)
  : Calculer le nombre de points retires par chaque filtre

## Conversion et Import/Export

Fonctions de conversion et manipulation des données

- [`convert_flow_to_yield()`](https://cedricbouffard.github.io/yieldcleanr/reference/convert_flow_to_yield.md)
  : Convertir le flux de grain en rendement (boisseaux/acre)
- [`convert_coordinates()`](https://cedricbouffard.github.io/yieldcleanr/reference/convert_coordinates.md)
  : Méta-fonction de conversion des coordonnées
- [`convert_yield_units()`](https://cedricbouffard.github.io/yieldcleanr/reference/convert_yield_units.md)
  : Méta-fonction de conversion des unités de rendement
- [`latlon_to_utm()`](https://cedricbouffard.github.io/yieldcleanr/reference/latlon_to_utm.md)
  : Convertir Latitude/Longitude en coordonnees UTM
- [`export_data()`](https://cedricbouffard.github.io/yieldcleanr/reference/export_data.md)
  : Méta-fonction d'export des données

## Lecture de Fichiers

Fonctions pour lire les données depuis différentes sources

- [`read_yield_from_zip()`](https://cedricbouffard.github.io/yieldcleanr/reference/read_yield_from_zip.md)
  : Lire les donnees de rendement depuis un fichier ZIP
- [`list_fields_from_zip()`](https://cedricbouffard.github.io/yieldcleanr/reference/list_fields_from_zip.md)
  : Lister les champs disponibles dans un fichier ZIP

## Anonymisation

Fonctions pour anonymiser les données sensibles

- [`anonymize_coordinates()`](https://cedricbouffard.github.io/yieldcleanr/reference/anonymize_coordinates.md)
  : Anonymiser les coordonnées GPS des données de rendement
- [`anonymize_data()`](https://cedricbouffard.github.io/yieldcleanr/reference/anonymize_data.md)
  : Méta-fonction d'anonymisation des données
- [`anonymize_yield_data()`](https://cedricbouffard.github.io/yieldcleanr/reference/anonymize_yield_data.md)
  : Pipeline complet d'anonymisation des donnees de rendement
- [`remove_sensitive_attributes()`](https://cedricbouffard.github.io/yieldcleanr/reference/remove_sensitive_attributes.md)
  : Supprimer les attributs sensibles des donnees de rendement
- [`restore_coordinates()`](https://cedricbouffard.github.io/yieldcleanr/reference/restore_coordinates.md)
  : Restaurer les coordonnées originales à partir de données anonymisées
