# Script de generation du rapport de coverage
# A executer avec : source("coverage.R")

if (!requireNamespace("covr", quietly = TRUE)) {
  install.packages("covr")
}

library(covr)

# Generer le rapport de coverage
report <- package_coverage()

# Afficher un resume dans la console
print(report)

# Generer un rapport HTML interactif
report_html <- report
class(report_html) <- c("coverage", "list")

# Sauvegarder le rapport
report_file <- file.path("coverage-report.html")
file_report(report, report_file)

message("Rapport de coverage genere : ", normalizePath(report_file))

# Option: generer un badge de coverage (pour GitHub)
# badge <- covr::badge_codecov(report)
# cat(badge)
