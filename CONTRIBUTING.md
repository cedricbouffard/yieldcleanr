# Guide de Contribution

Merci de votre interet pour contribuer a yieldcleanr ! Ce document
fournit les directives pour contribuer au package.

## Comment contribuer

### Signaler des bugs

Si vous trouvez un bug, veuillez ouvrir une issue sur GitHub avec : -
Une description claire du probleme - Les etapes pour reproduire le bug -
Le comportement attendu vs observe - Votre environnement (version de R,
OS, etc.) - Un exemple minimal reproductible si possible

### Proposer des fonctionnalites

Pour proposer une nouvelle fonctionnalite : - Ouvrez une issue avec le
label “enhancement” - Decrivez la fonctionnalite souhaitee - Expliquez
le cas d’usage - Discutez des alternatives envisagees

### Soumettre des modifications

1.  **Fork** le repository
2.  **Clone** votre fork localement
3.  **Creez une branche** pour votre fonctionnalite
    (`git checkout -b feature/nom-de-la-fonctionnalite`)
4.  **Committez** vos changements
    (`git commit -m 'Description claire des changements'`)
5.  **Push** vers votre fork
    (`git push origin feature/nom-de-la-fonctionnalite`)
6.  **Ouvrez une Pull Request**

## Standards de code

### Style de code

- Suivez le [tidyverse style guide](https://style.tidyverse.org/)
- Utilisez des noms de fonctions en snake_case
- Utilisez des noms de variables explicites et descriptifs
- Commentez le code complexe
- Limitez les lignes a 80 caracteres quand possible

### Documentation

- Toutes les fonctions doivent avoir une documentation roxygen2 complete
- Incluez des exemples dans la documentation (@examples)
- Ajoutez des tests pour les nouvelles fonctionnalites
- Mettez a jour le fichier NEWS.md

### Tests

- Utilisez testthat pour les tests
- Assurez-vous que tous les tests passent avant de soumettre
- Visez une couverture de code d’au moins 80%
- Testez les cas limites et les erreurs

``` r
# Executer les tests
devtools::test()

# Verifier la couverture
covr::package_coverage()
```

### Documentation des fonctions

``` r
#' Titre court de la fonction
#'
#' @description
#' Description detaillee de ce que fait la fonction.
#'
#' @details
#' Details supplementaires si necessaire.
#'
#' @param param1 Description du premier parametre
#' @param param2 Description du deuxieme parametre
#' @return Description de la valeur de retour
#' @export
#'
#' @seealso \code{\link{fonction_liee}}
#'
#' @examples
#' \dontrun{
#' resultat <- ma_fonction(donnees, param1 = TRUE)
#' }
ma_fonction <- function(donnees, param1 = FALSE) {
  # Implementation
}
```

## Processus de revision

1.  Les PR seront examinees par les mainteneurs
2.  Les tests CI doivent passer (GitHub Actions)
3.  Les conflits doivent etre resolus
4.  Les modifications suggerees doivent etre integrees

## Workflows GitHub Actions

Le projet utilise plusieurs workflows GitHub Actions pour automatiser
les tests et le deploiement :

### Workflows principaux

- **R-CMD-check** : Execute `R CMD check` sur plusieurs plateformes
  (Ubuntu, macOS, Windows) et versions de R
- **test-coverage** : Genere et envoie le rapport de couverture de code
  a Codecov
- **pkgdown** : Construit et deploie automatiquement le site de
  documentation sur GitHub Pages
- **lint** : Verifie le style du code avec lintr
- **render-readme** : Regenere le README.md a partir de README.Rmd si
  modifie

### Commandes PR

Les mainteneurs peuvent utiliser des commandes dans les PR : -
`/document` : Regenere la documentation roxygen2 - `/style` : Reformate
le code avec styler

### Dependabot

Dependabot est configure pour mettre a jour automatiquement les actions
GitHub chaque semaine.

## Developpement local

### Configuration de l’environnement

``` r
# Installer les dependances
devtools::install_deps(dependencies = TRUE)

# Charger le package en developpement
devtools::load_all()

# Executer les tests
devtools::test()

# Generer la documentation
devtools::document()

# Verifier le package
devtools::check()
```

### Construction du site de documentation

``` r
# Generer le site pkgdown
pkgdown::build_site()

# Deployer sur GitHub Pages
pkgdown::deploy_to_branch()
```

## Questions ?

Si vous avez des questions, n’hesitez pas a : - Ouvrir une issue sur
GitHub - Contacter les mainteneurs

Merci de contribuer a yieldcleanr !
