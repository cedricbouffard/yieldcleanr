# Package index

## 🚀 Fonctions Principales

Fonctions principales pour nettoyer les données de rendement. Ces
fonctions constituent le point d’entrée principal pour le nettoyage des
données de moissonneuses-batteuses. Elles supportent différents formats
de sortie (métrique/imperial, polygones/tibble) et incluent une
interface Shiny interactive.

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

## ⚙️ AYCE - Auto Yield Cleaning Engine

Fonctions du moteur de nettoyage automatique AYCE. Ces fonctions
implémentent les algorithmes de nettoyage automatisé basés sur les
standards USDA Yield Editor.

- [`ayce_clean()`](https://cedricbouffard.github.io/yieldcleanr/reference/ayce_clean.md)
  : AYCE : Auto Yield Cleaning Engine (sortie tibble)
- [`ayce_sf()`](https://cedricbouffard.github.io/yieldcleanr/reference/ayce_sf.md)
  : Pipeline AYCE complet avec sortie SF

## 🔍 Filtres et Détection d’Anomalies

Fonctions pour appliquer les filtres et détecter les anomalies dans les
données de rendement. Ces filtres suivent les recommandations USDA pour
identifier et éliminer les valeurs aberrantes.

- [`filter_data()`](https://cedricbouffard.github.io/yieldcleanr/reference/filter_data.md)
  : Méta-fonction de filtrage unifiée
- [`detect_anomalies()`](https://cedricbouffard.github.io/yieldcleanr/reference/detect_anomalies.md)
  : Méta-fonction de détection d'anomalies
- [`calculate_filter_counts()`](https://cedricbouffard.github.io/yieldcleanr/reference/calculate_filter_counts.md)
  : Calculer le nombre de points retires par chaque filtre

## ⏱️ Ajustement des Délais (Delay Adjustment)

Fonctions pour optimiser l’ajustement des délais temporels entre la
récolte et l’enregistrement des données GPS. Cet ajustement est crucial
pour la précision des données de rendement.

- [`apply_delay_adjustment()`](https://cedricbouffard.github.io/yieldcleanr/reference/apply_delay_adjustment.md)
  : Delay Adjustment : Delay Adjustment (Version Rapide)
- [`optimize_delays()`](https://cedricbouffard.github.io/yieldcleanr/reference/optimize_delays.md)
  : Méta-fonction d'optimisation des délais
- [`apply_moisture_delay()`](https://cedricbouffard.github.io/yieldcleanr/reference/apply_moisture_delay.md)
  : Appliquer la correction de delai d'humidite

## 📊 Calcul des Seuils

Fonctions pour calculer les seuils de filtrage automatiquement ou
manuellement. Ces seuils déterminent quelles données sont considérées
comme aberrantes.

- [`calculate_thresholds()`](https://cedricbouffard.github.io/yieldcleanr/reference/calculate_thresholds.md)
  : Méta-fonction de calcul des seuils
- [`calculate_auto_thresholds()`](https://cedricbouffard.github.io/yieldcleanr/reference/calculate_auto_thresholds.md)
  : Calculer les seuils automatiques (methode quantiles-IQR)

## 🔄 Conversion et Transformation

Fonctions de conversion entre différents formats et unités. Conversion
des coordonnées, des unités de rendement, et transformation des données
brutes en rendement calculé.

- [`convert_flow_to_yield()`](https://cedricbouffard.github.io/yieldcleanr/reference/convert_flow_to_yield.md)
  : Convertir le flux de grain en rendement (boisseaux/acre)
- [`convert_coordinates()`](https://cedricbouffard.github.io/yieldcleanr/reference/convert_coordinates.md)
  : Méta-fonction de conversion des coordonnées
- [`convert_yield_units()`](https://cedricbouffard.github.io/yieldcleanr/reference/convert_yield_units.md)
  : Méta-fonction de conversion des unités de rendement
- [`latlon_to_utm()`](https://cedricbouffard.github.io/yieldcleanr/reference/latlon_to_utm.md)
  : Convertir Latitude/Longitude en coordonnees UTM

## 💾 Import et Export de Données

Fonctions pour importer et exporter les données depuis/vers différents
formats (CSV, TXT, GeoJSON, GeoPackage, Shapefile et ZIP).

- [`export_data()`](https://cedricbouffard.github.io/yieldcleanr/reference/export_data.md)
  : Méta-fonction d'export des données
- [`read_yield_from_zip()`](https://cedricbouffard.github.io/yieldcleanr/reference/read_yield_from_zip.md)
  : Lire les donnees de rendement depuis un fichier ZIP
- [`read_yield_from_vector()`](https://cedricbouffard.github.io/yieldcleanr/reference/read_yield_from_vector.md)
  : Lire des donnees depuis un fichier vectoriel (shapefile, GeoPackage,
  GeoJSON)
- [`read_jd_to_polygons()`](https://cedricbouffard.github.io/yieldcleanr/reference/read_jd_to_polygons.md)
  : Lire des donnees John Deere et convertir en polygones metriques
- [`create_polygons_from_data()`](https://cedricbouffard.github.io/yieldcleanr/reference/create_polygons_from_data.md)
  : Creer des polygones rectangulaires a partir de donnees ponctuelles
- [`list_fields_from_zip()`](https://cedricbouffard.github.io/yieldcleanr/reference/list_fields_from_zip.md)
  : Lister les champs disponibles dans un fichier ZIP

## 🔒 Anonymisation et Sécurité

Fonctions pour anonymiser les données sensibles (coordonnées
géographiques, identifiants) tout en préservant l’utilité des données
pour l’analyse.

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

## 📈 Rapports et Visualisations

Fonctions pour générer des rapports PDF/HTML professionnels et créer des
visualisations des données de rendement sous forme de cartes
interactives.

- [`generate_batch_report()`](https://cedricbouffard.github.io/yieldcleanr/reference/generate_batch_report.md)
  : Générer un rapport multi-champs à partir de fichiers de données
- [`generate_batch_report_html()`](https://cedricbouffard.github.io/yieldcleanr/reference/generate_batch_report_html.md)
  : Generate a multi-field HTML report
- [`render_pdf_report()`](https://cedricbouffard.github.io/yieldcleanr/reference/render_pdf_report.md)
  : Render report to PDF using Chrome
- [`geom_yield_map_polygon()`](https://cedricbouffard.github.io/yieldcleanr/reference/geom_yield_map_polygon.md)
  : Creer une carte de rendement avec des polygones ggplot

## 🛠️ Fonctions Internes

Fonctions utilitaires internes utilisées par les autres fonctions du
package.

- [`.translate_crop_to_french()`](https://cedricbouffard.github.io/yieldcleanr/reference/dot-translate_crop_to_french.md)
  : Generate PDF report from cleaned yield data
