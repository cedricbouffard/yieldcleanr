# Script de construction du site pkgdown
# A executer avec : source("build-pkgdown.R")

if (!requireNamespace("pkgdown", quietly = TRUE)) {
  install.packages("pkgdown")
}

library(pkgdown)

# Construire le site
build_site()

message("Site pkgdown genere avec succes!")
message("Ouvrir docs/index.html dans votre navigateur pour visualiser.")

# Option: deployer sur GitHub Pages (si configure)
# deploy_to_branch()
