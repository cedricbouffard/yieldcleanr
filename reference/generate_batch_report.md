# Générer un rapport multi-champs à partir de fichiers de données

Cette fonction traite un ou plusieurs fichiers de données de rendement
(ZIP, CSV, TXT, ou GeoJSON), nettoie les données de chaque champ, et
génère un rapport professionnel avec tous les champs organisés par
année.

## Usage

``` r
generate_batch_report(
  file_paths,
  output_file = NULL,
  title = NULL,
  output_format = c("pdf", "html")
)
```

## Arguments

- file_paths:

  chemin(s) vers le(s) fichier(s) de données. Formats supportés: ZIP
  (contenant des shapefiles), CSV, TXT, GeoJSON

- output_file:

  chemin du fichier de sortie (optionnel). Par défaut, le rapport est
  créé dans le même répertoire que le premier fichier.

- title:

  Titre du rapport (optionnel)

- output_format:

  format de sortie: "pdf" (défaut) ou "html"

## Value

Chemin du fichier généré (invisible)

## Examples

``` r
if (FALSE) { # \dontrun{
# Un seul ZIP vers PDF
generate_batch_report("RDT2025.zip")

# Vers HTML
generate_batch_report("RDT2025.zip", output_format = "html")

# Plusieurs fichiers vers HTML
generate_batch_report(c("field1.geojson", "field2.geojson"), output_format = "html")
} # }
```
