#' Generate PDF report from cleaned yield data
#'
#' Creates a professional paged PDF report using the yield data that was
#' cleaned using clean_yield functions.
#'
#' @param data_clean Cleaned yield data (sf object or data frame with geometry)
#' @param data_raw Raw yield data before cleaning
#' @param deletions Data frame of deleted points with reasons
#' @param stats List containing cleaning statistics (n_raw, n_clean, n_deleted, retention_rate, etc.)
#' @param output_file Path where the PDF should be saved
#' @param title Report title (default: "Rapport de nettoyage des rendements")
#' @param author Author name (default: from DESCRIPTION or "YieldCleanr")
#' @param template_path Path to the R Markdown template (default: auto-detected)
#' @param verbose Print progress messages (default: TRUE)
#' @param style Style theme for the report. Options: "irda" (default) or "ced" (Cedric Bouffard style)
#' @return Path to the generated PDF file
#' @export

.translate_crop_to_french <- function(crop_name) {
  if (is.na(crop_name) || is.null(crop_name) || crop_name == "") {
    return(NA_character_)
  }

  crop_lower <- tolower(crop_name)

  crop_translations <- c(
    "corn" = "Maïs",
    "maize" = "Maïs",
    "soybean" = "Soya",
    "soybeans" = "Soya",
    "soy beans" = "Soya",
    "soy" = "Soya",
    "wheat" = "Blé",
    "barley" = "Orge",
    "oat" = "Avoine",
    "oats" = "Avoine",
    "rice" = "Riz",
    "sorghum" = "Sorgho",
    "millet" = "Millet",
    "canola" = "Colza",
    "rapeseed" = "Colza",
    "sunflower" = "Tournesol",
    "potato" = "Pomme de terre",
    "potatoes" = "Pomme de terre",
    "onion" = "Oignon",
    "onions" = "Oignon",
    "shallot" = "Échalote",
    "shallots" = "Échalote",
    "echalot" = "Échalote",
    "echalote" = "Échalote",
    "carrot" = "Carotte",
    "carrots" = "Carotte",
    "beet" = "Betterave",
    "beets" = "Betterave",
    "sugar beet" = "Betterave sucrière",
    "celery" = "Céleri",
    "turnip" = "Navet",
    "radish" = "Radis",
    "cabbage" = "Chou",
    "lettuce" = "Laitue",
    "tomato" = "Tomate",
    "tomatoes" = "Tomate",
    "pepper" = "Poivron",
    "beans" = "Haricots",
    "pea" = "Pois",
    "peas" = "Pois",
    "cotton" = "Coton",
    "sugarcane" = "Canne à sucre",
    "coffee" = "Café",
    "cocoa" = "Cacao",
    "cotton" = "Coton",
    "alfalfa" = "Luzerne",
    "clover" = "Trèfle",
    "rye" = "Seigle",
    "triticale" = "Triticale",
    "buckwheat" = "Sarrasin",
    "quinoa" = "Quinoa"
  )

  if (crop_lower %in% names(crop_translations)) {
    return(crop_translations[crop_lower])
  }

  return(crop_name)
}

generate_yield_report <- function(data_clean,
                                   data_raw = NULL,
                                   deletions = NULL,
                                   stats = NULL,
                                   output_file = NULL,
                                   title = NULL,
                                   author = NULL,
                                   template_path = NULL,
                                   verbose = TRUE,
                                   metadata = NULL,
                                   style = c("irda", "ced")) {
  
  style <- match.arg(style)
  if (verbose) cat("Generating yield report with style:", style, "...\n")
  
  # Check required packages
  required_packages <- c("rmarkdown", "pagedown", "ggplot2", "sf", "dplyr")
  missing_packages <- setdiff(required_packages, rownames(utils::installed.packages()))
  if (length(missing_packages) > 0) {
    stop(paste("Required packages not installed:", paste(missing_packages, collapse = ", "), 
               ". Install with: install.packages(c('rmarkdown', 'pagedown', 'ggplot2', 'sf', 'dplyr'))"))
  }
  
  # Default output file
  if (is.null(output_file)) {
    output_file <- tempfile(fileext = ".pdf")
  }
  
  # Find template if not provided
  if (is.null(template_path)) {
    # Try to find template in the package
    template_path <- system.file("rapport", "yield_report_template.Rmd", package = "yieldcleanr")
    if (template_path == "") {
      # Try relative path from current directory
      template_path <- file.path(getwd(), "inst", "rapport", "yield_report_template.Rmd")
    }
  }
  
  if (!file.exists(template_path)) {
    stop("Report template not found at: ", template_path)
  }
  
  # Get default author from DESCRIPTION if not provided
  if (is.null(author)) {
    desc_path <- system.file("DESCRIPTION", package = "yieldcleanr")
    if (file.exists(desc_path)) {
      desc <- readLines(desc_path)
      author_line <- grep("^Author:", desc, value = TRUE)
      if (length(author_line) > 0) {
        author <- sub("^Author: ", "", author_line[1])
      }
    }
  }
  if (is.null(author)) {
    author <- "YieldCleanr"
  }
  
  # Prepare data - handle list output from clean_yield_fast
  if (is.list(data_clean) && !inherits(data_clean, "sf")) {
    # Check if this is the result from clean_yield_fast (has $data component)
    if (!is.null(data_clean$data)) {
      data_clean_sf <- data_clean$data
      data_clean_df <- sf::st_drop_geometry(data_clean_sf)
    } else {
      data_clean_df <- data_clean
      data_clean_sf <- NULL
    }
  } else if (inherits(data_clean, "sf")) {
    data_clean_df <- sf::st_drop_geometry(data_clean)
    data_clean_sf <- data_clean
  } else {
    data_clean_df <- data_clean
    # Try to convert to sf
    if ("Longitude" %in% names(data_clean) && "Latitude" %in% names(data_clean)) {
      data_clean_sf <- sf::st_as_sf(data_clean, coords = c("Longitude", "Latitude"), crs = 4326)
    } else {
      data_clean_sf <- NULL
    }
  }

  # Helper pour echapper les chaines pour du code R
  escape_r_string <- function(s) {
    if (is.null(s)) return("NULL")
    if (is.na(s)) return("NA_character_")
    s <- as.character(s)
    s <- gsub("\\\\", "\\\\\\\\", s, fixed = TRUE)
    s <- gsub("'", "\\'", s, fixed = TRUE)
    s <- gsub("\n", "\\n", s, fixed = TRUE)
    s <- gsub("\r", "\\r", s, fixed = TRUE)
    s <- gsub("\t", "\\t", s, fixed = TRUE)
    return(s)
  }

  # Helper pour formater une valeur pour R
  format_r_value <- function(x) {
    if (is.na(x)) return("NA")
    if (is.null(x)) return("NULL")
    return(x)
  }

  # Extraire le metadata des attributs de data_clean si non fourni
  if (is.null(metadata)) {
    metadata <- attr(data_clean, "jd_metadata")
  }

  # Extraire les informations de champ et culture
  field_name <- NA_character_
  season_year <- NA_integer_
  crop_name <- NA_character_
  farm_name <- NA_character_

  if (!is.null(metadata)) {
    if (!is.null(metadata$field_info)) {
      field_name <- metadata$field_info$field %||% NA_character_
      season_year <- metadata$field_info$season %||% NA_integer_
      farm_name <- metadata$field_info$farm %||% NA_character_
    }
    if (!is.null(metadata$crop_info)) {
      crop_name <- metadata$crop_info$crop_name %||% NA_character_
    }
  }

  # Traduire le nom de la culture en francais
  crop_name <- .translate_crop_to_french(crop_name)

  # Titre par defaut base sur le metadata
  if (is.null(title)) {
    if (!is.na(field_name) && !is.na(season_year)) {
      title <- paste0("Champ ", field_name, " - ", season_year)
    } else if (!is.na(field_name)) {
      title <- paste0("Champ ", field_name)
    } else {
      title <- "Rapport de nettoyage des rendements"
    }
  }

  # Calculate statistics if not provided
  if (is.null(stats)) {
    stats <- list(
      n_raw = if (!is.null(data_raw)) nrow(data_raw) else NA,
      n_clean = nrow(data_clean_df),
      n_deleted = if (!is.null(data_raw)) nrow(data_raw) - nrow(data_clean_df) else NA,
      retention_rate = if (!is.null(data_raw)) nrow(data_clean_df) / nrow(data_raw) * 100 else NA
    )
  }
  
  # Extract yield statistics
  yield_col <- "Yield_kg_ha"
  if (yield_col %in% names(data_clean_df)) {
    yield_mean <- mean(data_clean_df[[yield_col]], na.rm = TRUE)
    yield_sd <- sd(data_clean_df[[yield_col]], na.rm = TRUE)
    yield_min <- min(data_clean_df[[yield_col]], na.rm = TRUE)
    yield_max <- max(data_clean_df[[yield_col]], na.rm = TRUE)
    yield_median <- median(data_clean_df[[yield_col]], na.rm = TRUE)
  } else {
    yield_mean <- yield_sd <- yield_min <- yield_max <- yield_median <- NA
  }
  
  # Create temp directory for report generation
  report_dir <- tempfile("yield_report_")
  dir.create(report_dir, recursive = TRUE)
  on.exit(unlink(report_dir, recursive = TRUE), add = TRUE)
  
  # Copy template and assets
  report_template <- file.path(report_dir, "report.Rmd")
  file.copy(template_path, report_template, overwrite = TRUE)
  
  # Determine style directory based on selected style
  template_dir <- dirname(template_path)
  if (style == "ced") {
    style_dir <- system.file("rapport", "ced", package = "yieldcleanr")
    if (style_dir == "") {
      style_dir <- file.path(getwd(), "inst", "rapport", "ced")
    }
    if (!dir.exists(style_dir)) {
      warning("Style 'ced' directory not found, falling back to 'irda' style")
      style_dir <- template_dir
      style <- "irda"
    }
  } else {
    style_dir <- template_dir
  }
  
  if (verbose) cat("Using style directory:", style_dir, "\n")
  
  # Copy CSS and images from style directory
  css_files <- list.files(style_dir, pattern = "\\.css$", full.names = TRUE)
  html_files <- list.files(style_dir, pattern = "\\.html$", full.names = TRUE)
  img_files <- list.files(style_dir, pattern = "\\.(png|jpg|jpeg)$", full.names = TRUE)
  
  for (f in c(css_files, html_files, img_files)) {
    file.copy(f, file.path(report_dir, basename(f)), overwrite = TRUE)
  }
  
  # For IRDA style, also copy files from template_dir if different from style_dir
  if (style == "irda" && style_dir != template_dir) {
    css_files_tpl <- list.files(template_dir, pattern = "\\.css$", full.names = TRUE)
    html_files_tpl <- list.files(template_dir, pattern = "\\.html$", full.names = TRUE)
    img_files_tpl <- list.files(template_dir, pattern = "\\.(png|jpg|jpeg)$", full.names = TRUE)
    for (f in c(css_files_tpl, html_files_tpl, img_files_tpl)) {
      file.copy(f, file.path(report_dir, basename(f)), overwrite = TRUE)
    }
  }
  
  # Fonction pour convertir une image en base64
  image_to_base64 <- function(image_path) {
    if (!file.exists(image_path)) return(NULL)
    img_data <- readBin(image_path, "raw", file.size(image_path))
    base64_data <- base64enc::base64encode(img_data)
    base64_data <- gsub("\n", "", base64_data)
    base64_data <- gsub("\r", "", base64_data)
    ext <- tolower(tools::file_ext(image_path))
    mime_type <- switch(ext, "png" = "image/png", "jpg" = "image/jpeg", "jpeg" = "image/jpeg", "gif" = "image/gif", "image/png")
    paste0("data:", mime_type, ";base64,", base64_data)
  }
  
  # Convertir les images en base64 pour le CSS (style-specific)
  css_file <- file.path(report_dir, "brochure.css")
  if (file.exists(css_file)) {
    css_content <- readLines(css_file, warn = FALSE)
    # Pour le style IRDA, integrer bandeaugauche.png
    if (style == "irda") {
      bandeau_path <- file.path(style_dir, "bandeaugauche.png")
      if (file.exists(bandeau_path)) {
        bandeau_base64 <- image_to_base64(bandeau_path)
        if (!is.null(bandeau_base64)) {
          css_content <- gsub("url\\('bandeaugauche.png'\\)", paste0("url('", bandeau_base64, "')"), css_content)
        }
      }
    }
    # Pour le style CED, integrer background.png
    if (style == "ced") {
      background_path <- file.path(style_dir, "background.png")
      if (file.exists(background_path)) {
        background_base64 <- image_to_base64(background_path)
        if (!is.null(background_base64)) {
          css_content <- gsub("url\\('background.png'\\)", paste0("url('", background_base64, "')"), css_content)
        }
      }
    }
    writeLines(css_content, css_file)
  }
  
  # Preparer les images base64 pour la page de couverture
  logo_base64 <- ""
  couverture_base64 <- ""
  logo_path <- file.path(style_dir, "logo.png")
  if (file.exists(logo_path)) {
    logo_base64 <- image_to_base64(logo_path)
  }
  # Pour IRDA: image_couverture.png, pour CED: background.png
  if (style == "irda") {
    couverture_path <- file.path(style_dir, "image_couverture.png")
  } else {
    couverture_path <- file.path(style_dir, "background.png")
  }
  if (file.exists(couverture_path)) {
    couverture_base64 <- image_to_base64(couverture_path)
  }
  
  # Prepare YAML header with custom values
  current_date <- format(Sys.Date(), "%B %Y")

  # Construire le sous-titre avec les infos de champ
  subtitle_parts <- c("Analyse des donnees de rendement")
  if (!is.na(crop_name) && crop_name != "") {
    subtitle_parts <- c(subtitle_parts, paste0("Culture: ", crop_name))
  }
  if (!is.na(farm_name) && farm_name != "") {
    subtitle_parts <- c(subtitle_parts, paste0("Ferme: ", farm_name))
  }
  subtitle <- paste(subtitle_parts, collapse = " | ")

  # Nom de l'organisation selon le style

  org_name <- if (style == "ced") "Cedric Bouffard" else "IRDA"
  
  yaml_updates <- c(
    paste0("title: \"", title, "\""),
    paste0("subtitle: \"", subtitle, "\""),
    paste0("author: \"", author, "\""),
    paste0("date: \"Date: ", current_date, "\""),
    paste0("header-left: \"", ifelse(!is.na(farm_name), farm_name, "Rapport agricole"), "\""),
    paste0("header-right: \"", org_name, "\""),
    paste0("footer-right: \"", current_date, "\""),
    "page-number-position: \"alternate\"",
    "output:",
    "  pagedown::html_paged:",
    "    css:",
    "      - \"https://cdnjs.cloudflare.com/ajax/libs/font-awesome-6.0.0/css/all.min.css\"",
    "      - \"default-page\"",
    "      - \"default\"",
    "      - \"brochure.css\"",
    "    number_sections: false",
    "    toc: true",
    "    toc_title: \"Table des matieres\"",
    "    includes:",
    "      in_header: header_overrides.html"
  )
  
  # Read the template and modify YAML
  template_content <- readLines(report_template)
  
  # Find YAML boundaries
  yaml_start <- which(grepl("^---$", template_content))
  if (length(yaml_start) >= 2) {
    # Replace YAML section
    yaml_end <- yaml_start[2]
    template_content <- template_content[-seq(yaml_start[1], yaml_end)]
  }
  
  # Add new YAML at the beginning
  new_yaml <- c("---", yaml_updates, "---", "", template_content)
  writeLines(new_yaml, report_template)
  
  # Prepare data for R code in template
  # We'll modify the template to use our data
  
  # Create data files in the temp directory
  if (!is.null(data_clean_sf)) {
    geojson_file <- file.path(report_dir, "rendement_nettoye.geojson")
    sf::st_write(data_clean_sf, geojson_file, driver = "GeoJSON", quiet = TRUE)
  }
  
  # Build the R code that will be inserted into the template
  # This will replace the hardcoded data loading
  
  # Find where to insert the data preparation code
  setup_idx <- which(grepl("^```.*setup", template_content))
  if (length(setup_idx) > 0) {
    # Find the end of the setup chunk
    for (i in setup_idx:length(template_content)) {
      if (grepl("^```$", template_content[i]) && i > setup_idx) {
        setup_end <- i
        break
      }
    }
    
    # Create new setup content (do NOT include opening line - it's already in template_content[setup_idx])
    setup_content <- c(
      "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, fig.width = 10, fig.height = 7, out.width = \"100%\")",
      "library(ggplot2)",
      "library(sf)",
      "library(readr)",
      "library(dplyr)",
      "library(knitr)",
      "library(terra)",
      "library(fontawesome)",
      "",
      "# Data prepared by generate_yield_report()",
      paste0("gdf <- st_read('", gsub("\\\\", "/", normalizePath(file.path(report_dir, "rendement_nettoye.geojson"))), "', quiet = TRUE)"),
      "",
      "# Field metadata",
      if (is.na(field_name)) {
        "field_name <- NA_character_"
      } else {
        paste0("field_name <- '", escape_r_string(field_name), "'")
      },
      if (is.na(season_year)) {
        "season_year <- NA_integer_"
      } else {
        paste0("season_year <- ", as.integer(season_year))
      },
      if (is.na(crop_name)) {
        "crop_name <- NA_character_"
      } else {
        paste0("crop_name <- '", escape_r_string(crop_name), "'")
      },
      if (is.na(farm_name)) {
        "farm_name <- NA_character_"
      } else {
        paste0("farm_name <- '", escape_r_string(farm_name), "'")
      },
      "# Preparer les libelles pour l'affichage",
      "field_label <- if (!is.na(field_name) && field_name != '') paste0('Champ ', field_name) else 'Champ'",
      "year_label <- if (!is.na(season_year)) as.character(season_year) else ''",
      "crop_label <- if (!is.na(crop_name) && crop_name != '') crop_name else ''",
      "",
      "# Statistics",
      paste0("n_raw <- ", ifelse(is.na(stats$n_raw), "NA", stats$n_raw)),
      paste0("n_clean <- ", stats$n_clean),
      paste0("n_deleted <- ", ifelse(is.na(stats$n_deleted), "NA", stats$n_deleted)),
      paste0("retention_rate <- ", ifelse(is.na(stats$retention_rate), "NA", round(stats$retention_rate, 1))),
      paste0("yield_mean <- ", round(yield_mean, 1)),
      paste0("yield_sd <- ", round(yield_sd, 1)),
      paste0("yield_min <- ", round(yield_min, 1)),
      paste0("yield_max <- ", round(yield_max, 1)),
      paste0("yield_median <- ", round(yield_median, 1)),
      "```",
      "",
      "<style>",
      "/* Prevent blank page after TOC */",
      ".toc, #TOC { break-after: avoid !important; page-break-after: avoid !important; }",
      "</style>",
      "",
      "<div class=\"front-page\">",
      "",
      "<div class=\"bandeau-droit\"></div>",
      "",
      paste0("<img class=\"logo-irda\" src=\"", if (nchar(logo_base64) > 0) logo_base64 else "logo.png", "\" alt=\"Logo IRDA\">"),
      "",
      paste0("<img class=\"image-couverture\" src=\"",
             if (nchar(couverture_base64) > 0) couverture_base64 else "image_couverture.png",
             "\" alt=\"Image de couverture\">"),
      "",
      "<div class=\"subtitle\"><span>Rapport de rendement</span></div>",
      "",
      paste0("<div class=\"title\">", title, "</div>"),
      "",
      paste0("<div class=\"date\">", current_date, "</div>"),
      "",
      paste0("<div class=\"author\">", author, "</div>"),
      "",
      "</div>",
      ""
    )
    
    # Replace setup chunk
    template_content <- c(
      template_content[1:setup_idx],
      setup_content,
      template_content[(setup_end + 1):length(template_content)]
    )
  }
  
  # Write modified template
  writeLines(template_content, report_template)
  
  # Render the report
  if (verbose) cat("Rendering PDF report...\n")

  # Create output HTML first, then convert to PDF
  # Use absolute paths since rmarkdown renders in report_dir
  output_html_abs <- normalizePath(output_file, mustWork = FALSE)
  output_html <- file.path(getwd(), sub("\\.pdf$", ".html", basename(output_file)))

  tryCatch({
    rmarkdown::render(
      input = report_template,
      output_file = output_html,
      quiet = FALSE,
      encoding = "UTF-8"
    )

    # Copy HTML to the requested output location if different
    if (output_html != output_html_abs && file.exists(output_html)) {
      file.copy(output_html, output_html_abs, overwrite = TRUE)
    }

    # Convert to PDF using pagedown
    if (grepl("\\.pdf$", output_file)) {
      tryCatch({
        pagedown::chrome_print(output_html, output_file, verbose = FALSE)
      }, error = function(pdf_error) {
        warning("PDF conversion failed (HTML still available): ", pdf_error$message)
      })
    }

    if (verbose) cat("Report generated successfully:", output_file, "\n")

    return(output_file)

  }, error = function(e) {
    stop("Error generating report: ", e$message)
  })
}


#' Render report to PDF using Chrome
#'
#' Helper function to convert HTML to PDF using Chrome browser
#'
#' @param html_file Path to HTML file
#' @param pdf_file Path for output PDF file
#' @param verbose Print progress messages
#' @return Path to the PDF file
render_pdf_report <- function(html_file, pdf_file, verbose = TRUE) {
  if (!file.exists(html_file)) {
    stop("HTML file not found: ", html_file)
  }
  
  if (verbose) cat("Converting HTML to PDF...\n")
  
  pagedown::chrome_print(html_file, pdf_file, verbose = verbose)
  
  return(pdf_file)
}