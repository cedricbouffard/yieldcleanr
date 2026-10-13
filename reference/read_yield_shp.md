# Lire un shapefile de rendement John Deere depuis un repertoire

Lit un shapefile John Deere exporte (fichiers .shp/.dbf/.shx/.prj/.cpg)
ainsi que son fichier JSON de metadonnees (optionnel) et retourne les
donnees standardisees au format yieldcleanr. Equivalent a
[`read_yield_from_zip()`](https://cedricbouffard.github.io/yieldcleanr/reference/read_yield_from_zip.md)
mais pour des fichiers deja extraits (hors ZIP).

## Usage

``` r
read_yield_shp(shp_path, metadata_path = NULL, as_polygons = FALSE)
```

## Arguments

- shp_path:

  Chemin vers le fichier .shp, ou vers le repertoire qui le contient (un
  fichier .shp attendu).

- metadata_path:

  Chemin optionnel vers le fichier JSON de metadonnees
  (\*-Deere-Metadata.json). Si NULL, detection automatique a cote du
  .shp.

- as_polygons:

  Si TRUE, retourne un objet SF avec polygones (comme
  [`read_jd_to_polygons()`](https://cedricbouffard.github.io/yieldcleanr/reference/read_jd_to_polygons.md))
  au lieu d'une data.frame standardisee.

## Value

Une data.frame standardisee (colonnes Longitude, Latitude, Flow, Swath,
Distance, etc.) ou un objet SF si as_polygons = TRUE.

## Examples

``` r
if (FALSE) { # \dontrun{
# Lire un export John Deere deja extrait dans un repertoire
data <- read_yield_shp("chemin/vers/Delfland_..._00.shp")

# Ou directement le repertoire contenant le .shp et le JSON
data <- read_yield_shp("chemin/vers/repertoire")

# Pour obtenir des polygones metriques
sf_data <- read_yield_shp("chemin/vers/repertoire", as_polygons = TRUE)
} # }
```
