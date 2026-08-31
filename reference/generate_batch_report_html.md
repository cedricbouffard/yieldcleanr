# Generate a multi-field HTML report

Generate a multi-field HTML report

## Usage

``` r
generate_batch_report_html(
  file_paths,
  output_file = NULL,
  title = NULL,
  style = c("irda", "ced")
)
```

## Arguments

- file_paths:

  Vector of file paths (GeoJSON)

- output_file:

  Output HTML file path

- title:

  Report title

- style:

  Style visuel du rapport: "irda" (défaut) ou "ced" (Cedric Bouffard)

## Value

Path to generated HTML file
