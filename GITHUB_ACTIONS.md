# Configuration GitHub Actions

Ce document decrit la configuration des workflows GitHub Actions pour le
package yieldcleanr.

## Workflows disponibles

### 1. R-CMD-check.yaml

**Fichier** : `.github/workflows/R-CMD-check.yaml`

Execute R CMD check sur plusieurs plateformes et versions de R : - macOS
(R release) - Windows (R release)  
- Ubuntu (R devel, release, oldrel-1)

**Declencheurs** : push et pull_request sur main, master, dev

**Actions** : - Verifie le package avec `rcmdcheck` - Publie les
snapshots en cas d’echec

### 2. test-coverage.yaml

**Fichier** : `.github/workflows/test-coverage.yaml`

Genere et envoie le rapport de couverture de code.

**Declencheurs** : push et pull_request sur main, master, dev

**Actions** : - Execute les tests avec couverture - Envoie les resultats
a Codecov - Genere un rapport HTML de couverture - Archive les resultats
en cas d’echec

**Secrets requis** : `CODECOV_TOKEN`

### 3. pkgdown.yaml

**Fichier** : `.github/workflows/pkgdown.yaml`

Construit et deploie le site de documentation pkgdown.

**Declencheurs** : - push sur main/master - pull_request sur
main/master - release publiee - workflow_dispatch (manuel)

**Actions** : - Construit le site avec
[`pkgdown::build_site_github_pages()`](https://pkgdown.r-lib.org/reference/build_site_github_pages.html) -
Deploie sur la branche `gh-pages` - Publie sur GitHub Pages

**Permissions** : `contents: write`

### 4. lint.yaml

**Fichier** : `.github/workflows/lint.yaml`

Verifie le style du code avec lintr.

**Declencheurs** : push et pull_request sur main, master, dev

**Actions** : - Execute
[`lintr::lint_package()`](https://lintr.r-lib.org/reference/lint.html) -
Echoue si des problemes de style sont detectes

**Configuration** : Voir `.lintr`

### 5. render-readme.yaml

**Fichier** : `.github/workflows/render-readme.yaml`

Regenere README.md a partir de README.Rmd.

**Declencheurs** : modification de README.Rmd

**Actions** : - Render README.Rmd avec rmarkdown - Commit et push les
changements

### 6. pr-commands.yaml

**Fichier** : `.github/workflows/pr-commands.yaml`

Permet aux mainteneurs d’executer des commandes dans les PR.

**Commandes disponibles** : - `/document` : Regenere la documentation
roxygen2 - `/style` : Reformate le code avec styler

**Conditions** : Uniquement pour MEMBER ou OWNER

## Configuration dependabot

**Fichier** : `.github/dependabot.yaml`

Met a jour automatiquement les actions GitHub.

**Configuration** : - Verification hebdomadaire - Maximum 10 PR
ouvertes - Labels : dependencies, github-actions

## Configuration lintr

**Fichier** : `.lintr`

Configuration du linter R : - Longueur de ligne maximale : 120
caracteres - Commentaires de code autorises - Noms d’objets non verifies

## Secrets requis

Les secrets suivants doivent etre configures dans les parametres du
repository GitHub :

### CODECOV_TOKEN

Token pour envoyer les rapports de couverture a Codecov.

**Obtention** : 1. Aller sur <https://codecov.io> 2. Connecter le
repository 3. Copier le token 4. Ajouter dans Settings \> Secrets \>
Actions

## Utilisation

### Declenchement manuel

Le workflow pkgdown peut etre declenche manuellement via l’onglet
“Actions” sur GitHub.

### Verification des workflows

``` bash
# Verifier la syntaxe des workflows
gh workflow view R-CMD-check

# Executer un workflow manuellement
gh workflow run pkgdown.yaml
```

### Badges

Les badges dans le README.md reflètent l’etat des workflows : -
R-CMD-check : Etat de la verification du package - test-coverage : Etat
de la couverture de code - pkgdown : Etat du deploiement de la
documentation - lint : Etat de la verification du style

## Resolution des problemes

### Workflows en attente

Si un workflow reste en attente : 1. Verifier les restrictions dans
Settings \> Actions 2. S’assurer que les secrets sont configures 3.
Verifier les permissions du token GITHUB_TOKEN

### Echecs frequents

**R-CMD-check** : - Verifier les dependances systeme (GDAL, PROJ,
GEOS) - Consulter les logs d’artefacts

**test-coverage** : - Verifier que CODECOV_TOKEN est configure -
S’assurer que les tests passent localement

**pkgdown** : - Verifier que le site se construit localement
([`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html)) -
S’assurer que la branche gh-pages existe

## Ressources

- [GitHub Actions for R](https://github.com/r-lib/actions)
- [pkgdown documentation](https://pkgdown.r-lib.org/)
- [Codecov documentation](https://docs.codecov.io/)
- [lintr documentation](https://lintr.r-lib.org/)
