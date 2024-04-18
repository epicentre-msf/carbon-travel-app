source(here::here("R", "set_paths.R"))

# Set paths -------------------------------------------------------------

paths <- set_paths()

# Path to clean data
if (Sys.info()[["user"]] == "paul") {
  path_to_data <- fs::path(Sys.getenv("SHAREPOINT_PATH"), "Maelle CHARRIER - Carbon-travel-App", "Data")
} else {
  path_to_data <- fs::path(paths$maelle_charrier_tool, "Data")
}

if (FALSE) {
  fs::file_copy(
    fs::path(path_to_data, "clean", "air_msf.rds"),
    here::here("data", "clean", "air_msf.rds"),
    overwrite = TRUE
  )
  fs::file_copy(
    fs::path(path_to_data, "distance-matrix", "airports_distance_matrix.rds"),
    here::here("data", "distance-matrix", "airports_distance_matrix.rds"),
    overwrite = TRUE
  )
  fs::file_copy(
    fs::path(path_to_data, "clean", "conversion_df.rds"),
    here::here("data", "clean", "conversion_df.rds"),
    overwrite = TRUE
  )
  fs::file_copy(
    fs::path(path_to_data, "clean", "amex_clean_lon_lat.rds"),
    here::here("data", "clean", "amex_clean_lon_lat.rds"),
    overwrite = TRUE
  )
}