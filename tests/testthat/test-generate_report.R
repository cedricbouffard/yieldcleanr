# Test suite for generate_report.R ----
library(dplyr)
library(yieldcleanr)

test_that(".translate_crop_to_french exists", {
  expect_true(exists(".translate_crop_to_french"))
  expect_true(is.function(.translate_crop_to_french))
})

test_that(".translate_crop_to_french translates common crops", {
  expect_equal(as.character(.translate_crop_to_french("corn")), "Maïs")
  expect_equal(as.character(.translate_crop_to_french("maize")), "Maïs")
  expect_equal(as.character(.translate_crop_to_french("soybean")), "Soya")
  expect_equal(as.character(.translate_crop_to_french("wheat")), "Blé")
  expect_equal(as.character(.translate_crop_to_french("barley")), "Orge")
})

test_that(".translate_crop_to_french handles case insensitivity", {
  expect_equal(as.character(.translate_crop_to_french("CORN")), "Maïs")
  expect_equal(as.character(.translate_crop_to_french("Corn")), "Maïs")
  expect_equal(as.character(.translate_crop_to_french("SOYBEAN")), "Soya")
})

test_that(".translate_crop_to_french handles unknown crops", {
  expect_equal(as.character(.translate_crop_to_french("unknown_crop")), "unknown_crop")
  expect_equal(as.character(.translate_crop_to_french("custom")), "custom")
})

test_that(".translate_crop_to_french handles NA and NULL", {
  expect_true(is.na(.translate_crop_to_french(NA)))
  expect_true(is.na(.translate_crop_to_french(NULL)))
  expect_true(is.na(.translate_crop_to_french("")))
})

test_that(".translate_crop_to_french handles plurals", {
  expect_equal(as.character(.translate_crop_to_french("soybeans")), "Soya")
  expect_equal(as.character(.translate_crop_to_french("potatoes")), "Pomme de terre")
  expect_equal(as.character(.translate_crop_to_french("carrots")), "Carotte")
})
