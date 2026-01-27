# Donnees de test pour le package yieldcleanr
# Ce fichier est auto-genere et ne doit pas etre modifie manuellement

yield_test_data <- structure(list(
  ID = c("00001", "00002", "00003", "00004", "00005"),
  Longitude = c(-69.856661, -69.856681, -69.856701, -69.856726, -69.856749),
  Latitude = c(47.506122, 47.506136, 47.506152, 47.506168, 47.506185),
  Flow = c(1.53, 3.7, 7.56, 10.36, 15.48),
  GPS_Time = c(1762958157, 1762958159, 1762958161, 1762958163, 1762958165),
  Interval = c(2L, 2L, 2L, 2L, 2L),
  Distance = c(77L, 87L, 93L, 97L, 103L),
  Swath = c(240L, 240L, 240L, 240L, 240L),
  Moisture = c(30.8, 30.9, 30.8, 30.8, 30.8),
  HeaderStatus = c(33L, 33L, 33L, 33L, 33L),
  Pass = c(1L, 1L, 1L, 1L, 1L),
  Serial = c(2410019049L, 2410019049L, 2410019049L, 2410019049L, 2410019049L),
  FieldID = c("F0:1", "F0:1", "F0:1", "F0:1", "F0:1"),
  LoadID = c("L0:<1>", "L0:<1>", "L0:<1>", "L0:<1>", "L0:<1>"),
  GrainType = c("Maïs", "Maïs", "Maïs", "Maïs", "Maïs"),
  GPSStatus = c(7L, 7L, 7L, 7L, 7L),
  DOP = c(0, 0, 0, 0, 0),
  Altitude = c(61.3, 61.5, 61.5, 61.4, 61.3),
  .row_id = 1:5
), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -5L))

# Donnees de test avec header mixte (1 = actif, 33 = header bas, 0 = header haut)
yield_test_data_header_mixed <- structure(list(
  ID = c("00001", "00002", "00003", "00004", "00005"),
  Longitude = c(-69.856661, -69.856681, -69.856701, -69.856726, -69.856749),
  Latitude = c(47.506122, 47.506136, 47.506152, 47.506168, 47.506185),
  Flow = c(1.53, 3.7, 7.56, 10.36, 15.48),
  GPS_Time = c(1762958157, 1762958159, 1762958161, 1762958163, 1762958165),
  Interval = c(2L, 2L, 2L, 2L, 2L),
  Distance = c(77L, 87L, 93L, 97L, 103L),
  Swath = c(240L, 240L, 240L, 240L, 240L),
  Moisture = c(30.8, 30.9, 30.8, 30.8, 30.8),
  HeaderStatus = c(1L, 33L, 33L, 0L, 33L),  # 1=harvesting, 33=header down, 0=header up (should filter)
  Pass = c(1L, 1L, 1L, 1L, 1L),
  Serial = c(2410019049L, 2410019049L, 2410019049L, 2410019049L, 2410019049L),
  FieldID = c("F0:1", "F0:1", "F0:1", "F0:1", "F0:1"),
  LoadID = c("L0:<1>", "L0:<1>", "L0:<1>", "L0:<1>", "L0:<1>"),
  GrainType = c("Maïs", "Maïs", "Maïs", "Maïs", "Maïs"),
  GPSStatus = c(7L, 7L, 7L, 7L, 7L),
  DOP = c(0, 0, 0, 0, 0),
  Altitude = c(61.3, 61.5, 61.5, 61.4, 61.3),
  .row_id = 1:5
), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -5L))

# Donnees de test avec outliers
yield_test_data_outliers <- structure(list(
  ID = c("00001", "00002", "00003", "00004", "00005"),
  Longitude = c(-69.856661, -69.856681, -69.856701, -69.856726, -69.856749),
  Latitude = c(47.506122, 47.506136, 47.506152, 47.506168, 47.506185),
  Flow = c(50, 100, 150, 600, 180),  # 600 is outlier
  GPS_Time = c(1762958157, 1762958159, 1762958161, 1762958163, 1762958165),
  Interval = c(2L, 2L, 2L, 2L, 2L),
  Distance = c(77L, 87L, 93L, 97L, 103L),
  Swath = c(240L, 240L, 240L, 240L, 240L),
  Moisture = c(30.8, 30.9, 30.8, 30.8, 30.8),
  HeaderStatus = c(33L, 33L, 33L, 33L, 33L),
  Pass = c(1L, 1L, 1L, 1L, 1L),
  Serial = c(2410019049L, 2410019049L, 2410019049L, 2410019049L, 2410019049L),
  FieldID = c("F0:1", "F0:1", "F0:1", "F0:1", "F0:1"),
  LoadID = c("L0:<1>", "L0:<1>", "L0:<1>", "L0:<1>", "L0:<1>"),
  GrainType = c("Maïs", "Maïs", "Maïs", "Maïs", "Maïs"),
  GPSStatus = c(7L, 7L, 7L, 7L, 7L),
  DOP = c(0, 0, 0, 0, 0),
  Altitude = c(61.3, 61.5, 61.5, 61.4, 61.3),
  .row_id = 1:5
), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -5L))

# Donnees de test pour le filtre de chevauchement
yield_test_data_overlap <- structure(list(
  ID = c("00001", "00002", "00003", "00004", "00005"),
  Longitude = c(-69.856661, -69.856681, -69.856701, -69.856726, -69.856749),
  Latitude = c(47.506122, 47.506136, 47.506152, 47.506168, 47.506185),
  Flow = c(1.53, 3.7, 7.56, 10.36, 15.48),
  GPS_Time = c(1762958157, 1762958159, 1762958161, 1762958163, 1762958165),
  Interval = c(2L, 2L, 2L, 2L, 2L),
  Distance = c(77L, 87L, 93L, 97L, 103L),
  Swath = c(240L, 240L, 240L, 240L, 240L),
  Moisture = c(30.8, 30.9, 30.8, 30.8, 30.8),
  HeaderStatus = c(33L, 33L, 33L, 33L, 33L),
  Pass = c(1L, 1L, 1L, 1L, 1L),
  Serial = c(2410019049L, 2410019049L, 2410019049L, 2410019049L, 2410019049L),
  FieldID = c("F0:1", "F0:1", "F0:1", "F0:1", "F0:1"),
  LoadID = c("L0:<1>", "L0:<1>", "L0:<1>", "L0:<1>", "L0:<1>"),
  GrainType = c("Maïs", "Maïs", "Maïs", "Maïs", "Maïs"),
  GPSStatus = c(7L, 7L, 7L, 7L, 7L),
  DOP = c(0, 0, 0, 0, 0),
  Altitude = c(61.3, 61.5, 61.5, 61.4, 61.3),
  .row_id = 1:5
), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -5L))
