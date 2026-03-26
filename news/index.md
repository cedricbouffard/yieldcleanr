# Changelog

## yieldcleanr 0.1.0

### Première version

#### Nouvelles fonctionnalités

- **Fonctions principales de nettoyage**:
  - [`clean_yield()`](https://cedricbouffard.github.io/yieldcleanr/reference/clean_yield.md):
    Pipeline complet de nettoyage avec support métrique/imperial
  - [`clean_yield_fast()`](https://cedricbouffard.github.io/yieldcleanr/reference/clean_yield_fast.md):
    Version optimisée pour de grands jeux de données
  - [`clean_yield_with_tracking()`](https://cedricbouffard.github.io/yieldcleanr/reference/clean_yield_with_tracking.md):
    Nettoyage avec suivi des points filtrés
  - [`read_yield_data()`](https://cedricbouffard.github.io/yieldcleanr/reference/read_yield_data.md):
    Lecture des fichiers de données de rendement
  - [`launch_shiny_app()`](https://cedricbouffard.github.io/yieldcleanr/reference/launch_shiny_app.md):
    Interface graphique interactive Shiny
- **Moteur AYCE (Auto Yield Cleaning Engine)**:
  - [`ayce_clean()`](https://cedricbouffard.github.io/yieldcleanr/reference/ayce_clean.md):
    Nettoyage automatique basé sur les standards USDA
  - [`ayce_sf()`](https://cedricbouffard.github.io/yieldcleanr/reference/ayce_sf.md):
    Sortie au format sf pour les analyses spatiales
- **Filtres et détection d’anomalies**:
  - [`filter_data()`](https://cedricbouffard.github.io/yieldcleanr/reference/filter_data.md):
    Application des filtres standards (rendement, humidité, vitesse)
  - [`detect_anomalies()`](https://cedricbouffard.github.io/yieldcleanr/reference/detect_anomalies.md):
    Détection automatique des valeurs aberrantes
  - [`calculate_filter_counts()`](https://cedricbouffard.github.io/yieldcleanr/reference/calculate_filter_counts.md):
    Comptage des points filtrés par type
- **Ajustement des délais**:
  - [`apply_delay_adjustment()`](https://cedricbouffard.github.io/yieldcleanr/reference/apply_delay_adjustment.md):
    Correction des délais temporels
  - [`optimize_delays()`](https://cedricbouffard.github.io/yieldcleanr/reference/optimize_delays.md):
    Optimisation automatique des paramètres de délai
  - [`apply_moisture_delay()`](https://cedricbouffard.github.io/yieldcleanr/reference/apply_moisture_delay.md):
    Ajustement basé sur l’humidité
- **Calcul des seuils**:
  - [`calculate_thresholds()`](https://cedricbouffard.github.io/yieldcleanr/reference/calculate_thresholds.md):
    Calcul manuel des seuils
  - [`calculate_auto_thresholds()`](https://cedricbouffard.github.io/yieldcleanr/reference/calculate_auto_thresholds.md):
    Détection automatique des seuils optimaux
- **Conversion et transformation**:
  - [`convert_flow_to_yield()`](https://cedricbouffard.github.io/yieldcleanr/reference/convert_flow_to_yield.md):
    Conversion du débit en rendement
  - [`convert_coordinates()`](https://cedricbouffard.github.io/yieldcleanr/reference/convert_coordinates.md):
    Conversion entre systèmes de coordonnées
  - [`convert_yield_units()`](https://cedricbouffard.github.io/yieldcleanr/reference/convert_yield_units.md):
    Conversion entre unités métriques et impériales
  - [`latlon_to_utm()`](https://cedricbouffard.github.io/yieldcleanr/reference/latlon_to_utm.md):
    Conversion latitude/longitude vers UTM
- **Import/Export**:
  - [`export_data()`](https://cedricbouffard.github.io/yieldcleanr/reference/export_data.md):
    Export vers CSV ou GeoJSON
  - [`read_yield_from_zip()`](https://cedricbouffard.github.io/yieldcleanr/reference/read_yield_from_zip.md):
    Lecture de shapefiles compressés
  - [`list_fields_from_zip()`](https://cedricbouffard.github.io/yieldcleanr/reference/list_fields_from_zip.md):
    Liste des champs dans un ZIP
- **Anonymisation et sécurité**:
  - [`anonymize_coordinates()`](https://cedricbouffard.github.io/yieldcleanr/reference/anonymize_coordinates.md):
    Anonymisation des coordonnées GPS
  - [`anonymize_data()`](https://cedricbouffard.github.io/yieldcleanr/reference/anonymize_data.md):
    Anonymisation générale des données
  - [`anonymize_yield_data()`](https://cedricbouffard.github.io/yieldcleanr/reference/anonymize_yield_data.md):
    Anonymisation spécifique aux données de rendement
  - [`remove_sensitive_attributes()`](https://cedricbouffard.github.io/yieldcleanr/reference/remove_sensitive_attributes.md):
    Suppression des attributs sensibles
  - [`restore_coordinates()`](https://cedricbouffard.github.io/yieldcleanr/reference/restore_coordinates.md):
    Restauration des coordonnées originales
- **Rapports et visualisations**:
  - [`generate_batch_report()`](https://cedricbouffard.github.io/yieldcleanr/reference/generate_batch_report.md):
    Génération de rapports multi-champs (PDF/HTML)
  - [`generate_batch_report_html()`](https://cedricbouffard.github.io/yieldcleanr/reference/generate_batch_report_html.md):
    Rapport HTML interactif
  - [`render_pdf_report()`](https://cedricbouffard.github.io/yieldcleanr/reference/render_pdf_report.md):
    Rendu PDF avec mise en page professionnelle
  - [`geom_yield_map_polygon()`](https://cedricbouffard.github.io/yieldcleanr/reference/geom_yield_map_polygon.md):
    Visualisation cartographique ggplot2

#### Documentation

- 7 vignettes détaillées couvrant:
  - Les filtres de rendement, humidité, vitesse
  - Le filtre d’écart-type local
  - Le filtre de chevauchement
  - L’ajustement des délais
  - Le guide complet avec exemples
  - Les méta-fonctions
  - La sécurité et confidentialité des données
- Documentation pkgdown complète avec:
  - Configuration Bootstrap 5 personnalisée
  - Navigation structurée par catégories
  - Support multilingue (FR/EN)
  - SEO optimisé

#### Tests

- Suite de tests complète avec testthat 3e édition
- Tests unitaires pour toutes les fonctions principales
- Couverture de code en cours d’amélioration

#### Standards

- Basé sur les recommandations USDA Yield Editor
- Support des unités métriques (kg/ha) et impériales (bu/acre)
- Compatible avec les données des principaux fabricants de moissonneuses
