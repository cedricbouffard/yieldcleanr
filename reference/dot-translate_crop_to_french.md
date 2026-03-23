# Generate PDF report from cleaned yield data

Creates a professional paged PDF report using the yield data that was
cleaned using clean_yield functions.

## Usage

``` r
.translate_crop_to_french(crop_name)
```

## Arguments

- data_clean:

  Cleaned yield data (sf object or data frame with geometry)

- data_raw:

  Raw yield data before cleaning

- deletions:

  Data frame of deleted points with reasons

- stats:

  List containing cleaning statistics (n_raw, n_clean, n_deleted,
  retention_rate, etc.)

- output_file:

  Path where the PDF should be saved

- title:

  Report title (default: "Rapport de nettoyage des rendements")

- author:

  Author name (default: from DESCRIPTION or "YieldCleanr")

- template_path:

  Path to the R Markdown template (default: auto-detected)

- verbose:

  Print progress messages (default: TRUE)

## Value

Path to the generated PDF file
