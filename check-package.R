# Script de verification du package R
# A executer avec : source("check-package.R")

if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

library(devtools)

# Verifier le package
cat("=== Verification du package yieldcleanr ===\n\n")

# 1. Charger le package
 cat("1. Chargement du package...\n")
load_all()

# 2. Executer les tests
 cat("\n2. Execution des tests...\n")
test_results <- test()
if (length(test_results) == 0) {
  cat("Tous les tests ont reussi!\n")
} else {
  cat("Certains tests ont echoue. Verifier les details ci-dessus.\n")
}

# 3. Generer la documentation
cat("\n3. Generation de la documentation...\n")
document()
cat("Documentation generee avec succes!\n")

# 4. Verifier le package avec R CMD check
cat("\n4. Verification R CMD check...\n")
check_results <- check()

# Afficher le resume
cat("\n=== Resume de la verification ===\n")
if (check_results$errors == 0 && check_results$warnings == 0 && check_results$notes == 0) 
{
  cat("Parfait ! Aucune erreur, warning ou note.\n")
} else {
  cat("Erreurs:", check_results$errors, "\n")
  cat("Warnings:", check_results$warnings, "\n")
  cat("Notes:", check_results$notes, "\n")
}

cat("\nVerification terminee!\n")
