# Generer un rapport PDF de nettoyage des donnees de rendement

Cree un rapport PDF pagine professionnel a partir des donnees nettoyees
par le pipeline yieldcleanr.

## Usage

``` r
generate_yield_report(
  data_clean,
  data_raw = NULL,
  deletions = NULL,
  stats = NULL,
  output_file = NULL,
  title = NULL,
  author = NULL,
  template_path = NULL,
  verbose = TRUE,
  metadata = NULL,
  style = c("irda", "ced")
)

generate_report(
  data_clean,
  data_raw = NULL,
  deletions = NULL,
  stats = NULL,
  output_file = NULL,
  title = NULL,
  author = NULL,
  template_path = NULL,
  verbose = TRUE,
  metadata = NULL,
  style = c("irda", "ced")
)
```

## Arguments

- data_clean:

  Donnees de rendement nettoyees (objet sf ou tibble)

- data_raw:

  Donnees brutes avant nettoyage (optionnel)

- deletions:

  Donnees des points supprimes avec motifs (optionnel)

- stats:

  Statistiques de nettoyage (n_raw, n_clean, etc.)

- output_file:

  Chemin du fichier PDF de sortie

- title:

  Titre du rapport (defaut: "Rapport de nettoyage des rendements")

- author:

  Nom de l'auteur (defaut: extrait du DESCRIPTION ou "YieldCleanr")

- template_path:

  Chemin du gabarit R Markdown (optionnel, auto-detecte)

- verbose:

  Afficher la progression (defaut: TRUE)

- metadata:

  Metadonnees optionnelles

- style:

  Style du rapport: "irda" (defaut) ou "ced"

## Value

Chemin vers le fichier genere
