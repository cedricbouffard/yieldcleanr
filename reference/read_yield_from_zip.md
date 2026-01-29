# Lire les donnees de rendement depuis un fichier ZIP

Cette fonction lit les donnees de rendement depuis un fichier ZIP
contenant des shapefiles (format John Deere, etc.)

## Usage

``` r
read_yield_from_zip(zip_path, field_name)
```

## Arguments

- zip_path:

  Chemin vers le fichier ZIP

- field_name:

  Nom du champ a lire

## Value

Un objet sf avec les donnees de rendement
