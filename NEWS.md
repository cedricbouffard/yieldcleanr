# yieldcleanr 0.1.0

## Première version

### Nouvelles fonctionnalités

* **Fonctions principales de nettoyage**:
  - `clean_yield()`: Pipeline complet de nettoyage avec support métrique/imperial
  - `clean_yield_fast()`: Version optimisée pour de grands jeux de données
  - `clean_yield_with_tracking()`: Nettoyage avec suivi des points filtrés
  - `read_yield_data()`: Lecture des fichiers de données de rendement
  - `launch_shiny_app()`: Interface graphique interactive Shiny

* **Moteur AYCE (Auto Yield Cleaning Engine)**:
  - `ayce_clean()`: Nettoyage automatique basé sur les standards USDA
  - `ayce_sf()`: Sortie au format sf pour les analyses spatiales

* **Filtres et détection d'anomalies**:
  - `filter_data()`: Application des filtres standards (rendement, humidité, vitesse)
  - `detect_anomalies()`: Détection automatique des valeurs aberrantes
  - `calculate_filter_counts()`: Comptage des points filtrés par type

* **Ajustement des délais**:
  - `apply_delay_adjustment()`: Correction des délais temporels
  - `optimize_delays()`: Optimisation automatique des paramètres de délai
  - `apply_moisture_delay()`: Ajustement basé sur l'humidité

* **Calcul des seuils**:
  - `calculate_thresholds()`: Calcul manuel des seuils
  - `calculate_auto_thresholds()`: Détection automatique des seuils optimaux

* **Conversion et transformation**:
  - `convert_flow_to_yield()`: Conversion du débit en rendement
  - `convert_coordinates()`: Conversion entre systèmes de coordonnées
  - `convert_yield_units()`: Conversion entre unités métriques et impériales
  - `latlon_to_utm()`: Conversion latitude/longitude vers UTM

* **Import/Export**:
  - `export_data()`: Export vers CSV ou GeoJSON
  - `read_yield_from_zip()`: Lecture de shapefiles compressés
  - `list_fields_from_zip()`: Liste des champs dans un ZIP

* **Anonymisation et sécurité**:
  - `anonymize_coordinates()`: Anonymisation des coordonnées GPS
  - `anonymize_data()`: Anonymisation générale des données
  - `anonymize_yield_data()`: Anonymisation spécifique aux données de rendement
  - `remove_sensitive_attributes()`: Suppression des attributs sensibles
  - `restore_coordinates()`: Restauration des coordonnées originales

* **Rapports et visualisations**:
  - `generate_batch_report()`: Génération de rapports multi-champs (PDF/HTML)
  - `generate_batch_report_html()`: Rapport HTML interactif
  - `render_pdf_report()`: Rendu PDF avec mise en page professionnelle
  - `geom_yield_map_polygon()`: Visualisation cartographique ggplot2

### Documentation

* 7 vignettes détaillées couvrant:
  - Les filtres de rendement, humidité, vitesse
  - Le filtre d'écart-type local
  - Le filtre de chevauchement
  - L'ajustement des délais
  - Le guide complet avec exemples
  - Les méta-fonctions
  - La sécurité et confidentialité des données

* Documentation pkgdown complète avec:
  - Configuration Bootstrap 5 personnalisée
  - Navigation structurée par catégories
  - Support multilingue (FR/EN)
  - SEO optimisé

### Tests

* Suite de tests complète avec testthat 3e édition
* Tests unitaires pour toutes les fonctions principales
* Couverture de code en cours d'amélioration

### Standards

* Basé sur les recommandations USDA Yield Editor
* Support des unités métriques (kg/ha) et impériales (bu/acre)
* Compatible avec les données des principaux fabricants de moissonneuses
