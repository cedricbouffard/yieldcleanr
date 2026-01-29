# Test shiny_diagnostics.R ----
library(dplyr)
library(yieldcleanr)

test_that("build_filter_diagnostics construit les diagnostics par filtre", {
  data_raw <- tibble::tibble(
    orig_row_id = 1:4,
    Flow = c(1, 2, 3, 4)
  )

  deletions <- tibble::tibble(
    orig_row_id = c(2, 3),
    step = c("Filtre vitesse", "Filtre vitesse"),
    reason = c("Vitesse hors plage", "Vitesse hors plage")
  )

  diagnostics <- yieldcleanr:::build_filter_diagnostics(data_raw, deletions, metrique = TRUE)

  expect_true("Filtre vitesse" %in% names(diagnostics))

  diag_data <- diagnostics[["Filtre vitesse"]]
  expect_true(all(c("etape", "statut", "valeur", "unite") %in% names(diag_data)))
  expect_equal(unique(diag_data$unite), "kg/s")
  expect_true(any(diag_data$statut == "Supprime"))
  expect_equal(diag_data$statut[diag_data$orig_row_id == 2], "Supprime")
})

test_that("build_filter_diagnostics respecte les unites imperiales", {
  data_raw <- tibble::tibble(
    orig_row_id = 1:2,
    Flow = c(1, 2)
  )

  deletions <- tibble::tibble(
    orig_row_id = 2,
    step = "Filtre GPS",
    reason = "GPSStatus < 4"
  )

  diagnostics <- yieldcleanr:::build_filter_diagnostics(data_raw, deletions, metrique = FALSE)
  diag_data <- diagnostics[["Filtre GPS"]]

  expect_equal(unique(diag_data$unite), "lb/s")
})
