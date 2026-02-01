library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(ggplot2)
library(DT)
library(yieldcleanr)

# Augmenter la taille maximale de fichier uploadé à 100MB
options(shiny.maxRequestSize = 100 * 1024^2)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Fraunces:wght@500;700&family=Source+Sans+3:wght@400;500;600&display=swap');

      :root {
        --bg: #f4f1ec;
        --panel: #ffffff;
        --ink: #1f2a32;
        --muted: #5b6470;
        --accent: #2f6f6d;
        --accent-2: #b7832f;
        --line: #e2d9cf;
        --soft: #f8f4ee;
        --shadow: 0 14px 30px rgba(31, 42, 50, 0.08);
      }

      body {
        font-family: \"Source Sans 3\", \"Trebuchet MS\", sans-serif;
        color: var(--ink);
        background: radial-gradient(circle at top left, #fdf8f1 0%, #f4f1ec 45%, #efe8dd 100%);
      }

      .app-title {
        font-family: \"Fraunces\", \"Times New Roman\", serif;
        font-size: 28px;
        font-weight: 700;
        color: var(--ink);
        margin-bottom: 4px;
      }

      .app-subtitle {
        font-size: 14px;
        color: var(--muted);
      }

      .sidebar-panel {
        background: var(--panel);
        padding: 18px;
        border-radius: 16px;
        border: 1px solid var(--line);
        box-shadow: var(--shadow);
        max-height: 90vh;
        overflow-y: auto;
      }

      .section-title {
        color: var(--accent);
        font-weight: 700;
        margin-top: 18px;
        border-bottom: 2px solid rgba(47, 111, 109, 0.2);
        padding-bottom: 6px;
        text-transform: uppercase;
        letter-spacing: 0.04em;
        font-size: 12px;
      }

      .section-title:first-child { margin-top: 0; }

      .param-label {
        font-size: 12px;
        margin-bottom: 4px;
        color: var(--muted);
      }

      .shiny-slider-input { margin-bottom: 10px; }

      #map {
        border: 1px solid var(--line);
        border-radius: 14px;
        overflow: hidden;
        box-shadow: var(--shadow);
      }

      .status-bar {
        background: linear-gradient(120deg, rgba(47, 111, 109, 0.12), rgba(183, 131, 47, 0.12));
        padding: 12px 14px;
        border-radius: 12px;
        margin-bottom: 16px;
        border: 1px solid rgba(47, 111, 109, 0.2);
        font-weight: 600;
      }

      .btn { border-radius: 10px; font-weight: 600; }
      .btn-success { background: var(--accent); border-color: var(--accent); }
      .btn-info { background: #496276; border-color: #496276; }
      .btn-warning { background: var(--accent-2); border-color: var(--accent-2); color: #1a1a1a; }

      .nav-tabs { border-bottom: 1px solid var(--line); }
      .nav-tabs > li > a { color: var(--muted); font-weight: 600; }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover {
        color: var(--accent);
        border-color: var(--line) var(--line) transparent;
      }

      .diag-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(320px, 1fr));
        gap: 16px;
      }

      .diag-card {
        background: var(--panel);
        border-radius: 16px;
        border: 1px solid var(--line);
        padding: 14px;
        box-shadow: var(--shadow);
        transition: transform 0.2s ease;
      }

      .diag-card:hover {
        transform: translateY(-2px);
        box-shadow: 0 20px 40px rgba(31, 42, 50, 0.12);
      }

      .diag-card h4 {
        font-family: \"Fraunces\", \"Times New Roman\", serif;
        font-size: 16px;
        margin-top: 0;
        margin-bottom: 10px;
        color: var(--ink);
      }

      .plot-controls {
        background: var(--soft);
        padding: 10px 14px;
        border-radius: 10px;
        margin-bottom: 12px;
        border: 1px solid var(--line);
      }

      .plot-controls label {
        font-size: 12px;
        color: var(--muted);
        margin-right: 10px;
      }

      .info-box {
        background: linear-gradient(135deg, rgba(47, 111, 109, 0.08), rgba(183, 131, 47, 0.08));
        padding: 12px 16px;
        border-radius: 12px;
        margin-bottom: 16px;
        border: 1px solid var(--line);
      }

      .info-box h5 {
        margin: 0 0 8px 0;
        color: var(--accent);
        font-weight: 600;
      }

      .stat-row {
        display: flex;
        justify-content: space-between;
        margin: 4px 0;
        font-size: 13px;
      }

      .stat-value {
        font-weight: 600;
        color: var(--ink);
      }
      
       .progress-modal .modal-content {
         background: var(--panel);
         border-radius: 16px;
         border: 1px solid var(--line);
         box-shadow: var(--shadow);
         min-width: 500px;
       }
       
       .progress-modal .modal-header {
         border-bottom: 1px solid var(--line);
         background: linear-gradient(135deg, rgba(47, 111, 109, 0.08), rgba(183, 131, 47, 0.08));
         padding: 20px;
       }
       
       .progress-modal .modal-title {
         font-size: 20px !important;
         font-weight: bold;
       }
       
       .progress-modal .modal-body {
         padding: 30px;
         font-size: 16px;
       }
      
       .progress-bar {
         background-color: var(--accent);
         border-radius: 6px;
         height: 30px !important;
         font-size: 16px !important;
         font-weight: bold;
         line-height: 30px;
       }
       
       .progress {
         height: 30px !important;
         margin-bottom: 15px;
       }
        
        .field-selector {
          background: var(--soft);
          padding: 12px;
          border-radius: 10px;
          margin-bottom: 12px;
          border: 1px solid var(--line);
          max-height: 250px;
          overflow-y: auto;
        }
 
        .progress-text {
          color: var(--accent);
          font-size: 18px !important;
          margin-top: 8px;
          font-style: italic;
          min-height: 24px;
          font-weight: 600;
        }
      
      .field-selector label {
        font-weight: 600;
        color: var(--accent);
      }
      
      .field-list {
        margin-top: 8px;
      }
      
      .field-item {
        padding: 8px 10px;
        border: 1px solid var(--line);
        border-radius: 8px;
        margin-bottom: 6px;
        cursor: pointer;
        transition: all 0.2s ease;
        background: var(--panel);
      }
      
      .field-item:hover {
        border-color: var(--accent);
        background: rgba(47, 111, 109, 0.05);
      }
      
      .field-item.selected {
        border-color: var(--accent);
        background: rgba(47, 111, 109, 0.1);
      }
      
      .field-item .field-name {
        font-weight: 600;
        color: var(--ink);
        font-size: 13px;
      }
      
      .field-item .field-meta {
        font-size: 11px;
        color: var(--muted);
        margin-top: 2px;
      }
      
      .view-toggle {
        display: flex;
        gap: 8px;
        margin-bottom: 12px;
      }
      
      .view-toggle .btn {
        flex: 1;
        font-size: 12px;
        padding: 8px;
      }
      
      .view-toggle .btn.active {
        background: var(--accent);
        color: white;
      }
      
      .debug-info {
        background: #fff3cd;
        border: 1px solid #ffc107;
        border-radius: 8px;
        padding: 10px;
        margin-bottom: 10px;
        font-size: 11px;
        font-family: monospace;
      }
      
      .debug-info .col-list {
        max-height: 60px;
        overflow-y: auto;
        font-size: 9px;
        margin-top: 5px;
        color: #666;
      }
    "))
  ),
  
  # Modal de progression
  div(id = "progress_modal", class = "modal fade progress-modal", tabindex = "-1",
      div(class = "modal-dialog",
          div(class = "modal-content",
              div(class = "modal-header",
                  tags$h4(class = "modal-title", "Importation des champs"),
                  tags$button(type = "button", class = "close", `data-dismiss` = "modal",
                              tags$span("×"))
              ),
              div(class = "modal-body",
                  uiOutput("progress_content")
              )
          )
      )
  ),
  
  titlePanel(
    tags$div(
      tags$div(class = "app-title", "Explorateur de nettoyage des rendements"),
      tags$div(class = "app-subtitle", "Analyse, filtrage et export des donnees de moissonneuse")
    )
  ),
  
   fluidRow(
     column(3,
       div(class = "sidebar-panel",
         div(class = "status-bar",
           textOutput("status")
         ),
         div(class = "progress-text",
           textOutput("progress_step")
         ),
         div(class = "section-title", "1. Importation"),
        
        # Import fichier unique (détection automatique du type)
        fileInput("file_input", "Choisir un fichier",
                 accept = c(".txt", ".csv", ".zip")),
        
        # Selection des champs (visible uniquement pour ZIP)
        conditionalPanel(
          condition = "output.zip_loaded == true",
          div(class = "field-selector",
              tags$label("Champs disponibles :"),
              uiOutput("field_list"),
              checkboxInput("select_all_fields", "Selectionner tous les champs", value = FALSE),
              actionButton("import_selected", "Importer la selection", 
                         class = "btn-success btn-block", icon = icon("download"))
          )
        ),
        
        div(class = "section-title", "2. Parametres d'affichage"),
        selectInput("display_var", "Variable a afficher :",
                   choices = c(
                     "Rendement sec" = "dry_yield",
                     "Rendement humide" = "wet_yield",
                     "Humidite" = "moisture"
                   ),
                    selected = "dry_yield"),

        # Section Filtres avec checkboxes statiques
        div(class = "section-title", "3. Filtres a appliquer"),
         checkboxInput("apply_pcdi_flow", "PCDI flux", value = TRUE),
         checkboxInput("apply_pcdi_moisture", "PCDI humidite", value = TRUE),
         checkboxInput("apply_position", "Filtre position (hors champ)", value = TRUE),
         checkboxInput("apply_header", "Filtre header", value = TRUE),
         checkboxInput("apply_gps", "Filtre GPS", value = TRUE),
         checkboxInput("apply_velocity", "Filtre vitesse", value = TRUE),
         checkboxInput("apply_velocity_jump", "Filtre changement de vitesse", value = TRUE),
         checkboxInput("apply_heading_anomaly", "Filtre anomalies de direction", value = TRUE),
         checkboxInput("apply_null_yield", "Retirer rendements nuls", value = TRUE),
         checkboxInput("apply_yield_range", "Filtre plage de rendement", value = TRUE),
         checkboxInput("apply_moisture", "Filtre humidite", value = TRUE),
         checkboxInput("apply_overlap", "Filtre chevauchement", value = TRUE),
         checkboxInput("apply_local_sd", "Filtre ecart-type local", value = TRUE),

         # Vue carte brute/nettoyee
          div(class = "section-title", "4. Visualisation"),
          shinyWidgets::switchInput(
            inputId = "view_mode_switch",
            label = "Carte",
            value = TRUE,
            onLabel = "Nettoyee",
            offLabel = "Brute",
            onStatus = "success",
            offStatus = "primary",
            size = "large",
            width = "100%"
          ),
         
         # Mode d'affichage : points ou rectangles
         radioButtons("display_mode", "Mode d'affichage :",
                     c("Points" = "points",
                       "Rectangles" = "rectangles"),
                     selected = "points"),
         
         # Bouton pour ouvrir les parametres avances
        div(class = "section-title", "5. Parametres avances"),
        actionButton("show_params", "Ouvrir les parametres",
                    class = "btn btn-primary btn-block",
                    icon = icon("cog")),

        div(class = "section-title", "6. Carte et telechargement"),
        selectInput("map_type", "Vue :",
                   choices = c("Carte des rendements" = "yield",
                             "Points supprimes" = "deleted",
                             "Raster" = "raster",
                             "Comparaison" = "comparison")),
        
        conditionalPanel(
          condition = "input.map_type == 'deleted' || input.map_type == 'comparison'",
          selectInput("filter_step", "Etape de filtre :",
                     choices = "Toutes les etapes", selected = "Toutes les etapes")
        ),
        
        conditionalPanel(
          condition = "input.map_type == 'raster'",
          sliderInput("raster_resolution", "Resolution raster (m) :",
                     min = 0.5, max = 5, value = 1, step = 0.5)
        ),
        
        radioButtons("download_format", "Format :",
                    c("GeoJSON" = "geojson",
                      "CSV" = "csv",
                      "Raster (1m)" = "raster")),
        downloadButton("download_data", "Telecharger les donnees", class = "btn-success btn-block")
      )
    ),
    
    column(9,
      tabsetPanel(
        tabPanel("Carte",
                 leafletOutput("map", height = "650px")),
        tabPanel("Statistiques",
                 fluidRow(
                   column(6,
                          h4("Resume du nettoyage"),
                          tableOutput("summary")),
                   column(6,
                          h4("Suppressions par etape"),
                          DT::dataTableOutput("deletions_table"))
                  ),
                 hr(),
                 fluidRow(
                   column(12,
                          h4("Seuils calcules"),
                          verbatimTextOutput("thresholds_display"))
                  )),
        tabPanel("Distribution",
                 plotOutput("yield_distribution", height = "500px")),
        tabPanel("Diagnostics",
                 div(class = "plot-controls",
                     fluidRow(
                       column(4, 
                              sliderInput("diag_plot_height", "Hauteur des graphiques :", 
                                         200, 600, 350, 50)),
                       column(4,
                              sliderInput("diag_base_size", "Taille de police :",
                                         8, 16, 11, 1)),
                       column(4,
                              selectInput("diag_layout", "Disposition :",
                                         choices = c("Auto" = "auto",
                                                   "1 colonne" = "1",
                                                   "2 colonnes" = "2",
                                                   "3 colonnes" = "3")))
                     )
                 ),
                 uiOutput("diagnostics_ui"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Nettoyer les callbacks later lors de la fermeture de la session
  session$onEnded(function() {
    tryCatch({
      if (requireNamespace("later", quietly = TRUE)) {
        later::later(function() NULL, 0)
      }
    }, error = function(e) NULL)
  })

  # Operateur pour valeurs par defaut si NULL
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
   rv <- reactiveValues(
     result = NULL,
     processed = FALSE,
     zip_data = NULL,
     zip_fields = NULL,
     selected_fields = NULL,
     view_mode = "clean",  # "raw" ou "clean" - par defaut: carte nettoyee
     import_progress = list(total = 0, current = 0, field_name = ""),
     raster_data = NULL,  # Pour stocker le raster genere
     deletions_sf = NULL,  # Pour stocker les points supprimes avec raisons
     progress_step = "",  # Pour afficher l'etape actuelle
     progress_detail = "",  # Pour afficher le detail de l'etape
     # Stockage des resultats intermediaires pour eviter de recalculer
     pcdi_result = NULL,  # Resultat du PCDI (delai optimal)
     pcdi_params = NULL,  # Parametres utilises pour le PCDI
     overlap_result = NULL,  # Resultat de l'analyse d'overlap
     overlap_params = NULL,  # Parametres utilises pour l'overlap
     preprocessed_data = NULL  # Donnees pretraitees (apres UTM, position, PCDI)
   )
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 0, lat = 0, zoom = 2)
  })
  
  # Indicateur de donnees chargees
  output$has_data <- reactive({
    !is.null(rv$raw_data) && nrow(rv$raw_data) > 0
  })
  outputOptions(output, "has_data", suspendWhenHidden = FALSE)
  
  # Info de debug
  output$debug_info <- renderText({
    if (!is.null(rv$raw_data) && nrow(rv$raw_data) > 0) {
      cols <- names(rv$raw_data)
      flow_info <- ""
      if ("Flow" %in% cols) {
        valid_flow <- sum(!is.na(rv$raw_data$Flow))
        flow_info <- paste0(" | Flow: ", valid_flow, " valeurs valides")
      }
      if ("Yield_kg_ha" %in% cols) {
        valid_yield <- sum(!is.na(rv$raw_data$Yield_kg_ha))
        flow_info <- paste0(flow_info, " | Yield_kg_ha: ", valid_yield)
      }
      paste0("Lignes: ", nrow(rv$raw_data), 
             " | Colonnes: ", length(cols),
             flow_info)
    } else {
      "Aucune donnee"
    }
  })
  
  # Modal pour les parametres avances
  observeEvent(input$show_params, {
    showModal(modalDialog(
      title = "Parametres avances",
      size = "l",
      
      tabsetPanel(
        tabPanel("PCDI",
          div(class = "param-label", "Plage de delai (secondes)"),
          fluidRow(
            column(6, sliderInput("delay_min", "Min :", -50, 0, -25, 1)),
            column(6, sliderInput("delay_max", "Max :", 0, 50, 20, 1))
          ),
          div(class = "param-label", "Iterations"),
          sliderInput("n_iterations", "Iterations PCDI :", 1, 20, 10, 1),
          div(class = "param-label", "Niveau de bruit"),
          sliderInput("noise_level", "Bruit :", 0, 0.2, 0.03, 0.01),
          div(class = "param-label", "Echantillonnage (pour grandes donnees)"),
          sliderInput("sample_fraction", "Fraction des points :", 0.05, 1, 1, 0.05)
        ),
        tabPanel("Seuils",
          div(class = "param-label", "Quantiles de rendement"),
          fluidRow(
            column(6, sliderInput("yllim", "Bas :", 0, 0.3, 0.10, 0.01)),
            column(6, sliderInput("yulim", "Haut :", 0.7, 1.0, 0.90, 0.01))
          ),
          div(class = "param-label", "Multiplicateur IQR rendement"),
          sliderInput("yscale", "IQR rendement :", 0.5, 2.0, 1.1, 0.1),
          div(class = "param-label", "Quantiles de vitesse"),
          fluidRow(
            column(6, sliderInput("vlim", "Bas :", 0, 0.1, 0.02, 0.01)),
            column(6, sliderInput("vulim", "Haut :", 0.9, 1.0, 0.98, 0.01))
          ),
          div(class = "param-label", "Multiplicateur IQR vitesse"),
          sliderInput("vscale", "IQR vitesse :", 0.5, 2.0, 1.5, 0.1),
          div(class = "param-label", "Vitesse minimale absolue"),
          sliderInput("minv", "Min vitesse (m/s) :", 0.1, 2.0, 0.5, 0.1)
        ),
        tabPanel("Filtres",
          div(class = "param-label", "Chevauchement"),
          fluidRow(
            column(6, sliderInput("cellsize_overlap", "Taille cellule (m) :", 0.1, 1.0, 0.3, 0.1)),
            column(6, sliderInput("overlap_threshold", "Seuil :", 0.1, 1.0, 0.4, 0.1))
          ),
          div(class = "param-label", "Ecart-type local"),
          fluidRow(
            column(6, sliderInput("nswaths", "Passages :", 1, 20, 5, 1)),
            column(6, sliderInput("lsd_limit", "Limite (ET) :", 1.0, 5.0, 2.4, 0.1))
          ),
          div(class = "param-label", "Cellules minimales par grille"),
          sliderInput("min_cells", "Cellules min :", 1, 10, 3, 1),
          div(class = "param-label", "Ecart-type auto"),
          sliderInput("nstd", "Ecarts-types :", 1, 5, 3, 0.5)
        ),
        tabPanel("Mouvement",
          div(class = "param-label", "Changement de vitesse"),
          fluidRow(
            column(6, sliderInput("max_acceleration", "Acceleration max (m/s) :", 1, 20, 3, 1)),
            column(6, sliderInput("max_deceleration", "Deceleration max (m/s) :", -20, -1, -5, 1))
          ),
          div(class = "param-label", "Anomalies de direction"),
          sliderInput("max_heading_change", "Variation max direction (deg) :", 5, 90, 60, 5)
        )
      ),
      
      footer = tagList(
        modalButton("Fermer"),
        actionButton("apply_params", "Appliquer", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  })
  
  # Fermer le modal et relancer le traitement quand on clique sur Appliquer
  observeEvent(input$apply_params, {
    removeModal()
    if (!is.null(rv$raw_data)) {
      process_data()
    }
  })
  
  # Chargement du fichier (détection automatique du type)
  observeEvent(input$file_input, {
    req(input$file_input)
    
    file_path <- input$file_input$datapath
    file_name <- input$file_input$name
    file_ext <- tolower(tools::file_ext(file_name))
    
    tryCatch({
      if (file_ext == "zip") {
        # C'est un fichier ZIP - lister les champs disponibles
        fields <- yieldcleanr::list_fields_from_zip(file_path)
        rv$zip_fields <- fields
        rv$zip_data <- file_path
        rv$selected_fields <- NULL
        rv$raw_data <- NULL  # Réinitialiser les données brutes
        
        showNotification(paste(nrow(fields), "champs trouves dans le ZIP"), type = "message")
      } else {
        # C'est un fichier texte (txt/csv) - importer directement
        rv$zip_fields <- NULL  # Réinitialiser les champs ZIP
        rv$zip_data <- NULL
        rv$selected_fields <- NULL
        
        # Importer le fichier texte
        data <- yieldcleanr::read_yield_data(file_path)
        
        # Calculer le rendement en kg/ha pour l'affichage des donnees brutes
        # Les donnees brutes ont Flow en lbs/sec, il faut convertir en kg/ha
        if (all(c("Flow", "Interval", "Swath", "Distance") %in% names(data))) {
          message("Conversion du flux en rendement (kg/ha) pour l'affichage...")
          data <- yieldcleanr::convert_flow_to_yield(data)
          message(paste("Rendement calcule:", round(mean(data$Yield_kg_ha, na.rm = TRUE), 1), "kg/ha"))
        }
        
        rv$raw_data <- data
         
         # Garder la vue nettoyee si les donnees nettoyees existent deja
         if (is.null(rv$result) || is.null(rv$result$data_clean)) {
           rv$view_mode <- "raw"
         }
         
         # Reinitialiser les etiquettes des checkboxes
         resetCheckboxLabels()
         
         # Afficher sur la carte
        if (nrow(data) > 0) {
          raw_sf <- data %>%
            dplyr::select(Longitude, Latitude) %>%
            sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
          
          center_lng <- mean(sf::st_coordinates(raw_sf)[, 1])
          center_lat <- mean(sf::st_coordinates(raw_sf)[, 2])
          
          leafletProxy("map") %>%
            clearMarkers() %>%
            clearShapes() %>%
            addCircleMarkers(data = raw_sf,
                            radius = 3,
                            fillColor = "gray",
                            fillOpacity = 0.5,
                            weight = 1,
                            color = "black") %>%
            setView(lng = center_lng, lat = center_lat, zoom = 15)
        }
        
        showNotification(paste("Fichier importe:", file_name), type = "message")
        
        # Lancer le nettoyage automatiquement
        process_data()
      }
    }, error = function(e) {
      showNotification(paste("Erreur:", e$message), type = "error")
    })
  })
  
  # Indicateur que le ZIP est charge
  output$zip_loaded <- reactive({
    !is.null(rv$zip_fields) && nrow(rv$zip_fields) > 0
  })
  outputOptions(output, "zip_loaded", suspendWhenHidden = FALSE)
  
  # Liste des champs disponibles
  output$field_list <- renderUI({
    req(rv$zip_fields)
    
    fields <- rv$zip_fields$field_name
    
    tags$div(
      class = "field-list",
      lapply(seq_along(fields), function(i) {
        field_name <- fields[i]
        is_selected <- !is.null(rv$selected_fields) && field_name %in% rv$selected_fields
        
        tags$div(
          class = paste("field-item", ifelse(is_selected, "selected", "")),
          onclick = sprintf("Shiny.setInputValue('toggle_field_%d', %d, {priority: 'event'})", i, i),
          tags$div(class = "field-name", field_name),
          tags$div(class = "field-meta", paste("Taille:", round(rv$zip_fields$size_bytes[i] / 1024, 1), "KB"))
        )
      })
    )
  })
  
  # Gestion de la selection des champs
  observe({
    req(rv$zip_fields)
    
    fields <- rv$zip_fields$field_name
    
    for (i in seq_along(fields)) {
      local({
        idx <- i
        field_name <- fields[idx]
        input_name <- paste0("toggle_field_", idx)
        
        observeEvent(input[[input_name]], {
          if (is.null(rv$selected_fields)) {
            rv$selected_fields <- field_name
          } else if (field_name %in% rv$selected_fields) {
            rv$selected_fields <- setdiff(rv$selected_fields, field_name)
          } else {
            rv$selected_fields <- c(rv$selected_fields, field_name)
          }
        }, ignoreInit = TRUE)
      })
    }
  })
  
  # Selectionner tous les champs
  observeEvent(input$select_all_fields, {
    req(rv$zip_fields)
    
    if (input$select_all_fields) {
      rv$selected_fields <- rv$zip_fields$field_name
    } else {
      rv$selected_fields <- NULL
    }
  })
  
  # Contenu du modal de progression
  output$progress_content <- renderUI({
    total <- rv$import_progress$total
    current <- rv$import_progress$current
    field_name <- rv$import_progress$field_name
    
    if (total == 0) {
      return(tags$p("Preparation de l'importation..."))
    }
    
    progress_pct <- ifelse(total > 0, round((current / total) * 100), 0)
    
    tags$div(
      tags$p(tags$strong(paste0("Champ ", current, " sur ", total))),
      tags$p(paste("Traitement:", field_name)),
      div(class = "progress",
          div(class = "progress-bar", role = "progressbar",
              style = paste0("width: ", progress_pct, "%"),
              `aria-valuenow` = progress_pct,
              `aria-valuemin` = "0",
              `aria-valuemax` = "100",
              paste0(progress_pct, "%")))
    )
  })
  
  # Import des champs selectionnes
  observeEvent(input$import_selected, {
    req(rv$zip_data)
    req(rv$selected_fields)
    
    if (length(rv$selected_fields) == 0) {
      showNotification("Aucun champ selectionne", type = "warning")
      return()
    }
    
    # Si un seul champ, pas besoin de modal
    if (length(rv$selected_fields) == 1) {
      import_single_field(rv$selected_fields[1])
    } else {
      # Afficher le modal de progression
      rv$import_progress <- list(total = length(rv$selected_fields), current = 0, field_name = "")
      shinyjs::runjs("$('#progress_modal').modal('show');")
      
      # Importer les champs un par un
      for (i in seq_along(rv$selected_fields)) {
        rv$import_progress$current <- i
        rv$import_progress$field_name <- rv$selected_fields[i]
        
        # Forcer le rafraichissement de l'UI
        output$progress_content <- renderUI({
          total <- rv$import_progress$total
          current <- rv$import_progress$current
          field_name <- rv$import_progress$field_name
          progress_pct <- ifelse(total > 0, round((current / total) * 100), 0)
          
          tags$div(
            tags$p(tags$strong(paste0("Champ ", current, " sur ", total))),
            tags$p(paste("Traitement:", field_name)),
            div(class = "progress",
                div(class = "progress-bar", role = "progressbar",
                    style = paste0("width: ", progress_pct, "%"),
                    paste0(progress_pct, "%")))
          )
        })
        
        # Importer le champ
        import_single_field(rv$selected_fields[i], show_notification = FALSE)
        
        # Petite pause pour permettre l'affichage
        Sys.sleep(0.5)
      }
      
      # Fermer le modal
      shinyjs::runjs("$('#progress_modal').modal('hide');")
      showNotification(paste(length(rv$selected_fields), "champs importes avec succes"), type = "message")
    }
  })
  
  # Fonction pour importer un seul champ
  import_single_field <- function(field_name, show_notification = TRUE) {
    tryCatch({
      message(paste("Importation du champ:", field_name))
      
      data <- yieldcleanr::read_yield_from_zip(rv$zip_data, field_name = field_name)
      
      message(paste("Champ importe:", nrow(data), "lignes"))
      message(paste("Colonnes:", paste(names(data), collapse = ", ")))
      
      # Stocker les donnees brutes
      rv$raw_data <- data
      # Garder la vue nettoyee si les donnees nettoyees existent deja
      if (is.null(rv$result) || is.null(rv$result$data_clean)) {
        rv$view_mode <- "raw"
      }
      
      # Reinitialiser les etiquettes des checkboxes
      resetCheckboxLabels()
      
      # Afficher sur la carte
      if (!is.null(data) && nrow(data) > 0) {
        # Calculer le centre sans sf (donnees brutes = data frame)
        if ("Longitude" %in% names(data) && "Latitude" %in% names(data)) {
          center <- c(
            mean(data$Longitude, na.rm = TRUE),
            mean(data$Latitude, na.rm = TRUE)
          )
        } else if ("X" %in% names(data) && "Y" %in% names(data)) {
          center <- c(
            mean(data$X, na.rm = TRUE),
            mean(data$Y, na.rm = TRUE)
          )
        } else {
          center <- c(0, 0)
        }
        
         # Determiner la colonne de rendement
         # Prioriser Yield_kg_ha (rendement calcule) sur Flow (flux brut)
         yield_col <- NULL
         if ("Yield_kg_ha" %in% names(data) && !all(is.na(data$Yield_kg_ha))) {
           yield_col <- "Yield_kg_ha"
         } else if ("Flow" %in% names(data) && !all(is.na(data$Flow))) {
           yield_col <- "Flow"
         } else {
           # Chercher une colonne avec des valeurs numeriques
           for (col in names(data)) {
             if (is.numeric(data[[col]]) && !all(is.na(data[[col]]))) {
               yield_col <- col
               break
             }
           }
         }
        
        message(paste("Colonne de rendement:", yield_col))
        
         # Verifier que la colonne existe et contient des valeurs valides
         if (!is.null(yield_col) && yield_col %in% names(data) && !all(is.na(data[[yield_col]]))) {
           valid_values <- data[[yield_col]][!is.na(data[[yield_col]])]
           
           if (length(valid_values) > 0) {
             message(paste("Valeurs valides:", length(valid_values)))
             
             # Utiliser les quantiles 2% et 98% pour l'echelle
             q02 <- quantile(valid_values, 0.02, na.rm = TRUE)
             q98 <- quantile(valid_values, 0.98, na.rm = TRUE)
             
             # Clamper les valeurs pour la visualisation (2% -> 98%)
             data[[paste0(yield_col, "_clamped")]] <- pmin(pmax(data[[yield_col]], q02), q98)
             
             pal <- colorNumeric(c("#b04a3b", "#e9b44c", "#3d8a6b"), c(q02, q98))
            
             leafletProxy("map") %>%
               clearMarkers() %>%
               clearShapes() %>%
               clearControls() %>%
               addCircleMarkers(data = data,
                               radius = 3,
                               fillColor = ~pal(get(paste0(yield_col, "_clamped"))),
                               fillOpacity = 0.7,
                               weight = 0.5,
                               color = "black") %>%
               addLegend(position = "bottomright",
                        pal = pal,
                         values = c(q02, q98),
                         title = paste0("Rendement (", round(q02, 1), "-", round(q98, 1), " kg/ha)")) %>%
                setView(lng = center[1], lat = center[2], zoom = 15)
          } else {
            # Afficher sans palette de couleurs si pas de valeurs valides
            leafletProxy("map") %>%
              clearMarkers() %>%
              clearShapes() %>%
              clearControls() %>%
              addCircleMarkers(data = data,
                              radius = 3,
                              fillColor = "gray",
                              fillOpacity = 0.7,
                              weight = 0.5,
                              color = "black") %>%
              setView(lng = center[1], lat = center[2], zoom = 15)
          }
        } else {
          # Afficher sans palette de couleurs si colonne manquante
          leafletProxy("map") %>%
            clearMarkers() %>%
            clearShapes() %>%
            clearControls() %>%
            addCircleMarkers(data = data,
                            radius = 3,
                            fillColor = "gray",
                            fillOpacity = 0.7,
                            weight = 0.5,
                            color = "black") %>%
            setView(lng = center[1], lat = center[2], zoom = 15)
        }
      }
      
      if (show_notification) {
        showNotification(paste("Champ importe:", field_name), type = "message")
      }
      
      # Lancer le nettoyage automatiquement
      process_data()
      
    }, error = function(e) {
      message(paste("Erreur lors de l'importation:", e$message))
      if (show_notification) {
        showNotification(paste("Erreur:", e$message), type = "error")
      }
    })
  }
  
   # Gestion du switch de vue
   observeEvent(input$view_mode_switch, {
     if (isTRUE(input$view_mode_switch)) {
       rv$view_mode <- "clean"
     } else {
       rv$view_mode <- "raw"
     }
     update_map()
   }, ignoreInit = FALSE)
   
   # Mise a jour de la carte quand le mode d'affichage change
   observeEvent(input$display_mode, {
     if (!is.null(rv$result) && rv$view_mode == "clean") {
       display_clean_map()
     }
   })
   
   # Mise a jour de la carte selon le mode de vue
  update_map <- function() {
    req(rv$result)
    
    if (rv$view_mode == "raw" && !is.null(rv$raw_data)) {
      # Afficher les donnees brutes
      data <- rv$raw_data
      
       # Determiner la colonne de rendement
       # Prioriser Yield_kg_ha (rendement calcule) sur Flow (flux brut)
       yield_col <- NULL
       if ("Yield_kg_ha" %in% names(data) && !all(is.na(data$Yield_kg_ha))) {
         yield_col <- "Yield_kg_ha"
       } else if ("Flow" %in% names(data) && !all(is.na(data$Flow))) {
         yield_col <- "Flow"
       }
      
      if (nrow(data) > 0 && !is.null(yield_col)) {
        # Calculer le centre sans sf (donnees brutes = data frame)
        if ("Longitude" %in% names(data) && "Latitude" %in% names(data)) {
          center <- c(
            mean(data$Longitude, na.rm = TRUE),
            mean(data$Latitude, na.rm = TRUE)
          )
        } else if ("X" %in% names(data) && "Y" %in% names(data)) {
          center <- c(
            mean(data$X, na.rm = TRUE),
            mean(data$Y, na.rm = TRUE)
          )
        } else {
          center <- c(0, 0)
        }
        
        # Verifier que la colonne existe et contient des valeurs valides
        if (yield_col %in% names(data) && !all(is.na(data[[yield_col]]))) {
          valid_values <- data[[yield_col]][!is.na(data[[yield_col]])]
          
          if (length(valid_values) > 0) {
            # Utiliser les quantiles 2% et 98% pour l'echelle
            q02 <- quantile(valid_values, 0.02, na.rm = TRUE)
            q98 <- quantile(valid_values, 0.98, na.rm = TRUE)
            
            # Clamping des valeurs pour la visualisation
            data[[paste0(yield_col, "_clamped")]] <- pmin(pmax(data[[yield_col]], q02), q98)
            
            pal <- colorNumeric(c("#b04a3b", "#e9b44c", "#3d8a6b"), c(q02, q98))
            
            leafletProxy("map") %>%
              clearMarkers() %>%
              clearShapes() %>%
              clearControls() %>%
              addCircleMarkers(data = data,
                              radius = 3,
                              fillColor = ~pal(get(paste0(yield_col, "_clamped"))),
                              fillOpacity = 0.7,
                              weight = 0.5,
                              color = "black") %>%
              addLegend(position = "bottomright",
                       pal = pal,
                       values = c(q02, q98),
                        title = "Rendement brut (kg/ha)") %>%
              setView(lng = center[1], lat = center[2], zoom = 15)
          } else {
            leafletProxy("map") %>%
              clearMarkers() %>%
              clearShapes() %>%
              clearControls() %>%
              addCircleMarkers(data = data,
                              radius = 3,
                              fillColor = "gray",
                              fillOpacity = 0.7,
                              weight = 0.5,
                              color = "black") %>%
              setView(lng = center[1], lat = center[2], zoom = 15)
          }
        } else {
          leafletProxy("map") %>%
            clearMarkers() %>%
            clearShapes() %>%
            clearControls() %>%
            addCircleMarkers(data = data,
                            radius = 3,
                            fillColor = "gray",
                            fillOpacity = 0.7,
                            weight = 0.5,
                            color = "black") %>%
            setView(lng = center[1], lat = center[2], zoom = 15)
        }
      }
    } else {
      # Afficher les donnees nettoyees
      display_clean_map()
    }
  }
  
  # Afficher la carte nettoyee
  display_clean_map <- function() {
    req(rv$result)
    req(rv$result$data_clean)
    
    # Determiner quelle colonne afficher (toujours metrique)
    is_metric <- TRUE
    display_var <- input$display_var
    
    if (display_var == "dry_yield") {
      # Rendement sec
      value_col <- "Yield_kg_ha"
      legend_title <- "Rendement sec (kg/ha)"
      popup_label <- "Rendement sec"
      unit_label <- "kg/ha"
    } else if (display_var == "wet_yield") {
      # Rendement humide
      value_col <- "Yield_kg_ha_wet"
      legend_title <- "Rendement humide (kg/ha)"
      popup_label <- "Rendement humide"
      unit_label <- "kg/ha"
    } else {
      # Humidite - peut etre Moisture ou Moisture_pct selon le format
      legend_title <- "Humidite (%)"
      popup_label <- "Humidite"
      unit_label <- "%"
    }
    
     map_data <- rv$result$data_clean
    
    if (nrow(map_data) == 0) return()
    
    # Mode d'affichage : points ou rectangles
    display_mode <- input$display_mode %||% "points"
    
    # Si on demande points mais les donnees sont des polygones, convertir en centroides
    if (display_mode == "points" && inherits(map_data, "sf")) {
      geom_type <- sf::st_geometry_type(map_data)[1]
      if (grepl("POLYGON", geom_type)) {
        # Convertir polygones en points (centroides)
        map_data <- sf::st_centroid(map_data)
      }
    }
    
    # Determiner la colonne d'humidite apres avoir map_data
    if (display_var == "moisture") {
      if ("Moisture" %in% names(map_data)) {
        value_col <- "Moisture"
      } else if ("Moisture_pct" %in% names(map_data)) {
        value_col <- "Moisture_pct"
      } else {
        value_col <- "Moisture"  # Fallback
      }
    }
    
    # Calculer le centre avec gestion des erreurs de geometrie
    tryCatch({
      # Verifier et reparer les geometries si necessaire
      if (any(!sf::st_is_valid(map_data))) {
        map_data <- sf::st_make_valid(map_data)
      }
      center <- sf::st_coordinates(sf::st_centroid(sf::st_union(map_data)))
    }, error = function(e) {
      # Fallback: utiliser la moyenne des coordonnees
      if ("Longitude" %in% names(map_data)) {
        center <<- c(mean(map_data$Longitude, na.rm = TRUE), 
                     mean(map_data$Latitude, na.rm = TRUE))
      } else {
        # Extraire les coordonnees de la geometrie
        coords <- sf::st_coordinates(map_data)
        center <<- c(mean(coords[, 1], na.rm = TRUE), 
                     mean(coords[, 2], na.rm = TRUE))
      }
    })
    
    # Debug: afficher les colonnes disponibles
    message(paste("Colonnes disponibles:", paste(names(map_data), collapse = ", ")))
    message(paste("Colonne demandee:", value_col))
    
    # Verifier que la colonne existe
    if (!(value_col %in% names(map_data))) {
      message(paste("ERREUR: Colonne", value_col, "non trouvee dans les donnees"))
      # Essayer de trouver une alternative
      if (display_var == "wet_yield") {
        # Si rendement humide n'existe pas, utiliser rendement sec
        value_col <- if (is_metric) "Yield_kg_ha" else "Yield_buacre"
        legend_title <- paste(legend_title, "(donnees non disponibles - affichage rendement sec)")
        message("Utilisation de Yield_kg_ha comme fallback")
      } else if (display_var == "moisture") {
        # Si humidite n'existe pas
        showNotification("Donnees d'humidite non disponibles", type = "warning")
        leafletProxy("map") %>%
          clearMarkers() %>%
          clearShapes() %>%
          clearControls() %>%
          addPolygons(data = map_data,
                      fillColor = "gray",
                      fillOpacity = 0.7,
                      weight = 1,
                      color = "black") %>%
          setView(lng = center[1], lat = center[2], zoom = 15)
        return()
      }
    }
    
     # Verifier que la colonne contient des valeurs valides
    if (!all(is.na(map_data[[value_col]]))) {
      valid_values <- map_data[[value_col]][!is.na(map_data[[value_col]])]
      message(paste("Valeurs valides:", length(valid_values), "/", nrow(map_data)))
      
      if (length(valid_values) > 0) {
        # Utiliser les quantiles 2% et 98% pour l'echelle
        q02 <- quantile(valid_values, 0.02, na.rm = TRUE)
        q98 <- quantile(valid_values, 0.98, na.rm = TRUE)
        
        # Clamping des valeurs pour la visualisation
        map_data[[paste0(value_col, "_clamped")]] <- pmin(pmax(map_data[[value_col]], q02), q98)
        
        pal <- colorNumeric(c("#b04a3b", "#e9b44c", "#3d8a6b"), c(q02, q98))
        
        # Mode d'affichage : points ou rectangles
        display_mode <- input$display_mode %||% "points"
        
        leafletProxy("map") %>%
          clearMarkers() %>%
          clearShapes() %>%
          clearControls()
        
        if (display_mode == "points") {
          # Afficher en tant que points (cercles)
          leafletProxy("map") %>%
            addCircleMarkers(data = map_data,
                            radius = 3,
                            fillColor = ~pal(get(paste0(value_col, "_clamped"))),
                            fillOpacity = 0.7,
                            weight = 0.5,
                            color = "black",
                            popup = ~paste0(popup_label, " : ", round(get(value_col), 1), " ", unit_label))
        } else {
          # Afficher en tant que rectangles (polygones)
          leafletProxy("map") %>%
            addPolygons(data = map_data,
                        fillColor = ~pal(get(paste0(value_col, "_clamped"))),
                        fillOpacity = 0.7,
                        weight = 0,
                        popup = ~paste0(popup_label, " : ", round(get(value_col), 1), " ", unit_label))
        }
        
        leafletProxy("map") %>%
          addLegend(position = "bottomright",
                   pal = pal,
                   values = c(q02, q98),
                   title = legend_title) %>%
          setView(lng = center[1], lat = center[2], zoom = 15)
      } else {
        message("Aucune valeur valide pour la palette de couleurs")
        display_mode <- input$display_mode %||% "points"
        
        leafletProxy("map") %>%
          clearMarkers() %>%
          clearShapes() %>%
          clearControls()
        
        if (display_mode == "points") {
          leafletProxy("map") %>%
            addCircleMarkers(data = map_data,
                            radius = 3,
                            fillColor = "gray",
                            fillOpacity = 0.7,
                            weight = 0.5,
                            color = "black")
        } else {
          leafletProxy("map") %>%
            addPolygons(data = map_data,
                        fillColor = "gray",
                        fillOpacity = 0.7,
                        weight = 0)
        }
        leafletProxy("map") %>%
          setView(lng = center[1], lat = center[2], zoom = 15)
      }
    } else {
      message(paste("Toutes les valeurs de", value_col, "sont NA"))
      display_mode <- input$display_mode %||% "points"
      
      leafletProxy("map") %>%
        clearMarkers() %>%
        clearShapes() %>%
        clearControls()
      
      if (display_mode == "points") {
        leafletProxy("map") %>%
          addCircleMarkers(data = map_data,
                          radius = 3,
                          fillColor = "gray",
                          fillOpacity = 0.7,
                          weight = 0.5,
                          color = "black")
      } else {
        leafletProxy("map") %>%
          addPolygons(data = map_data,
                      fillColor = "gray",
                      fillOpacity = 0.7,
                      weight = 0)
      }
      leafletProxy("map") %>%
        setView(lng = center[1], lat = center[2], zoom = 15)
    }
  }
  
  get_params <- reactive({
    # Valeurs par defaut si les inputs n'existent pas encore (modal ferme)
    delay_min_val <- if (!is.null(input$delay_min)) input$delay_min else -25
    delay_max_val <- if (!is.null(input$delay_max)) input$delay_max else 20
    delay_min <- min(delay_min_val, delay_max_val)
    delay_max <- max(delay_min_val, delay_max_val)

    list(
      delay_range = seq(delay_min, delay_max, by = 1),
      n_iterations = if (!is.null(input$n_iterations)) input$n_iterations else 5,
      noise_level = if (!is.null(input$noise_level)) input$noise_level else 0.05,
      sample_fraction = if (!is.null(input$sample_fraction)) input$sample_fraction else 1,
      yllim = if (!is.null(input$yllim)) input$yllim else 0.10,
      yulim = if (!is.null(input$yulim)) input$yulim else 0.90,
      yscale = if (!is.null(input$yscale)) input$yscale else 1.1,
      v_lim = if (!is.null(input$vlim)) input$vlim else 0.02,
      v_ulim = if (!is.null(input$vulim)) input$vulim else 0.98,
      v_scale = if (!is.null(input$vscale)) input$vscale else 1.5,
      minv_abs = if (!is.null(input$minv)) input$minv else 0.5,
      gbuffer = 100,  # Valeur par defaut
      cellsize_overlap = if (!is.null(input$cellsize_overlap)) input$cellsize_overlap else 0.3,
      overlap_threshold = if (!is.null(input$overlap_threshold)) input$overlap_threshold else 0.4,
      n_swaths = if (!is.null(input$nswaths)) input$nswaths else 5,
      lsd_limit = if (!is.null(input$lsd_limit)) input$lsd_limit else 2.4,
      min_cells = if (!is.null(input$min_cells)) input$min_cells else 3,
      n_std = if (!is.null(input$nstd)) input$nstd else 3,
       # PCDI
       apply_pcdi_flow = if (!is.null(input$apply_pcdi_flow)) input$apply_pcdi_flow else TRUE,
       apply_pcdi_moisture = if (!is.null(input$apply_pcdi_moisture)) input$apply_pcdi_moisture else TRUE,
       # Filtres optionnels - utiliser TRUE/FALSE explicites
       apply_position = if (!is.null(input$apply_position)) input$apply_position else TRUE,
       apply_header = if (!is.null(input$apply_header)) input$apply_header else TRUE,
      apply_gps = if (!is.null(input$apply_gps)) input$apply_gps else TRUE,
      apply_velocity = if (!is.null(input$apply_velocity)) input$apply_velocity else TRUE,
      apply_velocity_jump = if (!is.null(input$apply_velocity_jump)) input$apply_velocity_jump else TRUE,
      apply_heading_anomaly = if (!is.null(input$apply_heading_anomaly)) input$apply_heading_anomaly else TRUE,
      apply_null_yield = if (!is.null(input$apply_null_yield)) input$apply_null_yield else TRUE,
      apply_yield_range = if (!is.null(input$apply_yield_range)) input$apply_yield_range else TRUE,
      apply_moisture = if (!is.null(input$apply_moisture)) input$apply_moisture else TRUE,
      apply_overlap = if (!is.null(input$apply_overlap)) input$apply_overlap else TRUE,
      apply_local_sd = if (!is.null(input$apply_local_sd)) input$apply_local_sd else TRUE,
      # Parametres des filtres - plus stricts par defaut
      max_acceleration = if (!is.null(input$max_acceleration)) input$max_acceleration else 3,
      max_deceleration = if (!is.null(input$max_deceleration)) input$max_deceleration else -5,
      max_heading_change = if (!is.null(input$max_heading_change)) input$max_heading_change else 60
    )
  })

  # Fonction pour reinitialiser les labels des checkboxes aux valeurs par defaut
  resetCheckboxLabels <- function() {
    # Labels par defaut
    default_labels <- list(
      "apply_pcdi_flow" = "PCDI flux",
      "apply_pcdi_moisture" = "PCDI humidite",
      "apply_position" = "Filtre position (hors champ)",
      "apply_header" = "Filtre header",
      "apply_gps" = "Filtre GPS",
      "apply_velocity" = "Filtre vitesse",
      "apply_velocity_jump" = "Filtre changement de vitesse",
      "apply_heading_anomaly" = "Filtre anomalies de direction",
      "apply_null_yield" = "Retirer rendements nuls",
      "apply_yield_range" = "Filtre plage de rendement",
      "apply_moisture" = "Filtre humidite",
      "apply_overlap" = "Filtre chevauchement",
      "apply_local_sd" = "Filtre ecart-type local"
    )
    
    # Reinitialiser chaque checkbox
    for (input_id in names(default_labels)) {
      updateCheckboxInput(session, input_id, label = default_labels[[input_id]])
    }
  }
  
  # Fonction pour mettre a jour les labels des checkboxes avec le nombre de points retires
  updateCheckboxLabels <- function(result) {
    if (is.null(result) || is.null(result$stats) || is.null(result$stats$deletions_by_step)) {
      return()
    }
    
    # Recuperer les suppressions par etape
    deletions <- result$stats$deletions_by_step
    
    # Mapping des noms de filtres avec leurs labels par defaut
    filter_mapping <- list(
      "PCDI flux" = "apply_pcdi_flow",
      "PCDI humidite" = "apply_pcdi_moisture",
      "Filtre position" = "apply_position",
      "Filtre header" = "apply_header",
      "Filtre GPS" = "apply_gps",
      "Filtre vitesse" = "apply_velocity",
      "Filtre changement vitesse" = "apply_velocity_jump",
      "Filtre direction" = "apply_heading_anomaly",
      "Rendement nul" = "apply_null_yield",
      "Filtre plage rendement" = "apply_yield_range",
      "Filtre humidite" = "apply_moisture",
      "Filtre chevauchement" = "apply_overlap",
      "Filtre ET local" = "apply_local_sd"
    )
    
    # Labels par defaut
    default_labels <- list(
      "PCDI flux" = "PCDI flux",
      "PCDI humidite" = "PCDI humidite",
      "Filtre position" = "Filtre position (hors champ)",
      "Filtre header" = "Filtre header",
      "Filtre GPS" = "Filtre GPS",
      "Filtre vitesse" = "Filtre vitesse",
      "Filtre changement vitesse" = "Filtre changement de vitesse",
      "Filtre direction" = "Filtre anomalies de direction",
      "Rendement nul" = "Retirer rendements nuls",
      "Filtre plage rendement" = "Filtre plage de rendement",
      "Filtre humidite" = "Filtre humidite",
      "Filtre chevauchement" = "Filtre chevauchement",
      "Filtre ET local" = "Filtre ecart-type local"
    )
    
    # Mettre a jour chaque checkbox avec le nombre de points retires pour CE filtre
    for (step_name in names(filter_mapping)) {
      input_id <- filter_mapping[[step_name]]
      default_label <- default_labels[[step_name]]
      
      # Verifier si le filtre est active (coché)
      is_active <- tryCatch({
        input[[input_id]]
      }, error = function(e) FALSE)
      
      # Chercher le nombre de suppressions pour cette etape specifique
      n_for_step <- 0
      if (nrow(deletions) > 0) {
        step_deletions <- deletions$n[deletions$step == step_name]
        if (length(step_deletions) > 0) {
          n_for_step <- step_deletions[1]
        }
      }
      
      # Si le filtre est actif et qu'il y a des suppressions pour cette etape, afficher le nombre
      if (isTRUE(is_active) && n_for_step > 0) {
        new_label <- paste0(default_label, " (-", n_for_step, " pts)")
        updateCheckboxInput(session, input_id, label = new_label)
      } else {
        # Sinon, remettre le label par defaut
        updateCheckboxInput(session, input_id, label = default_label)
      }
    }
  }

  # Fonction pour afficher le raster sur la carte
  display_raster_map <- function() {
    req(rv$result)
    req(rv$result$data_clean)
    
    # Verifier que les packages sont disponibles
    if (!requireNamespace("terra", quietly = TRUE) || !requireNamespace("concaveman", quietly = TRUE)) {
      showNotification("Packages 'terra' et 'concaveman' requis pour l'affichage raster", type = "warning")
      return()
    }
    
    tryCatch({
      data <- rv$result$data_clean
      
      # Convertir en sf si necessaire
      if (!inherits(data, "sf")) {
        if (all(c("Longitude", "Latitude") %in% names(data))) {
          data <- sf::st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)
        } else {
          showNotification("Coordonnees non trouvees pour le raster", type = "warning")
          return()
        }
      }
      
      # Determiner la colonne de rendement
      yield_col <- "Yield_kg_ha"
      if (!yield_col %in% names(data)) {
        alt_cols <- c("Yield_kg_ha", "Yield_buacre", "Yield", "yield")
        found_col <- alt_cols[alt_cols %in% names(data)]
        if (length(found_col) > 0) {
          yield_col <- found_col[1]
        } else {
          showNotification("Colonne de rendement non trouvee", type = "warning")
          return()
        }
      }
      
      # Generer le raster
      cell_size <- input$raster_resolution %||% 1
      
      # Creer un fichier temporaire pour le raster
      temp_raster_file <- tempfile(fileext = ".tif")
      
      # Utiliser la meta-fonction export_data() pour creer le raster
      yieldcleanr::export_data(
        data = data,
        file = temp_raster_file,
        format = "raster",
        resolution = cell_size
      )
      
      # Lire le raster genere
      raster_data <- terra::rast(temp_raster_file)
      
      rv$raster_data <- raster_data
      
      # Verifier que le raster contient des donnees
      if (all(is.na(terra::values(raster_data)))) {
        showNotification("Aucune donnee valide pour le raster", type = "warning")
        return()
      }
      
      # Projeter le raster en 3857 pour leaflet
      message("Projection du raster en 3857 pour l'affichage...")
      raster_wgs84 <- terra::project(raster_data, "EPSG:3857")
      
      # Convertir en format pour leaflet
      raster_df <- terra::as.data.frame(raster_wgs84, xy = TRUE)
      names(raster_df)[3] <- "value"
      
      # Filtrer les valeurs NA
      raster_df <- raster_df[!is.na(raster_df$value), ]
      
      if (nrow(raster_df) == 0) {
        showNotification("Aucune donnee valide pour le raster apres projection", type = "warning")
        return()
      }
      
      # Palette de couleurs avec NA transparent
      # Utiliser les quantiles 2% et 98% pour l'echelle
      q02 <- quantile(raster_df$value, 0.02, na.rm = TRUE)
      q98 <- quantile(raster_df$value, 0.98, na.rm = TRUE)
      
      # Clamping des valeurs pour la visualisation
      raster_df$value_clamped <- pmin(pmax(raster_df$value, q02), q98)
      
      pal <- colorNumeric(
        palette = c("#b04a3b", "#e9b44c", "#3d8a6b"),
        domain = c(q02, q98),
        na.color = "transparent"
      )
      
      # Afficher sur la carte
      center <- sf::st_coordinates(sf::st_centroid(sf::st_union(data)))
      
      leafletProxy("map") %>%
        clearMarkers() %>%
        clearShapes() %>%
        clearControls() %>%
        addRasterImage(
          raster_wgs84,
          colors = pal,
          opacity = 0.8,
          project = FALSE,  # Deja en WGS84
          options = tileOptions(opacity = 0.8)
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = c(q02, q98),
          title = "Rendement (kg/ha)",
          na.label = "NA"
        ) %>%
        setView(lng = center[1], lat = center[2], zoom = 15)
      
    }, error = function(e) {
      message(paste("Erreur affichage raster:", e$message))
      showNotification(paste("Erreur affichage raster:", e$message), type = "error")
    })
  }

  # Fonction pour afficher les points supprimes
  display_deleted_points <- function() {
    req(rv$deletions_sf)
    req(input$filter_step)
    
    data <- rv$deletions_sf
    
    # Filtrer par etape si selectionnee
    if (input$filter_step != "Toutes les etapes") {
      data <- data %>% dplyr::filter(step == input$filter_step)
    }
    
    if (nrow(data) == 0) {
      showNotification("Aucun point supprime pour cette etape", type = "message")
      return()
    }
    
    # Palette de couleurs par etape
    steps <- unique(data$step)
    colors <- colorFactor(rainbow(length(steps)), data$step)
    
    center <- sf::st_coordinates(sf::st_centroid(sf::st_union(data)))
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      addCircleMarkers(
        data = data,
        radius = 4,
        fillColor = ~colors(step),
        fillOpacity = 0.8,
        weight = 1,
        color = "black",
        popup = ~paste0("Etape: ", step, "<br>Raison: ", reason)
      ) %>%
      addLegend(
        position = "bottomright",
        pal = colors,
        values = steps,
        title = "Etapes de filtre"
      ) %>%
      setView(lng = center[1], lat = center[2], zoom = 15)
  }

  # Fonction pour verifier si les parametres de pre-traitement ont change
  has_preprocess_params_changed <- function(current_params) {
    if (is.null(rv$preprocessed_data)) return(TRUE)
    if (is.null(rv$preprocess_params)) return(TRUE)
    
    # Parametres qui affectent le pre-traitement
    preprocess_keys <- c("apply_position", "apply_pcdi_flow", "apply_pcdi_moisture",
                         "delay_range", "n_iterations", "noise_level", "sample_fraction")
    
    for (key in preprocess_keys) {
      if (!identical(current_params[[key]], rv$preprocess_params[[key]])) {
        message(paste("Parametre de pre-traitement change:", key))
        return(TRUE)
      }
    }
    return(FALSE)
  }
  
  process_data <- function() {
    req(rv$raw_data)
    
    tryCatch({
      params <- get_params()
      
      output$status <- renderText("Traitement en cours...")
      
       withProgress(message = "Nettoyage des donnees...", value = 0, {
         # Convertir les donnees sf en format compatible
         incProgress(0.05, detail = "Preparation des donnees...")
        
        data_df <- as.data.frame(rv$raw_data)
        if ("geometry" %in% names(data_df)) {
          coords <- sf::st_coordinates(rv$raw_data)
          data_df$Longitude <- coords[, 1]
          data_df$Latitude <- coords[, 2]
          data_df$geometry <- NULL
        }
        
        # Validation des colonnes requises
        required_cols <- c("Longitude", "Latitude", "Flow")
        missing_cols <- setdiff(required_cols, names(data_df))
        if (length(missing_cols) > 0) {
          showNotification(
            paste("Colonnes manquantes:", paste(missing_cols, collapse = ", ")),
            type = "warning"
          )
          for (col in missing_cols) {
            data_df[[col]] <- NA_real_
          }
        }
        
        # Log des donnees pour debug
        message(paste("Donnees brutes:", nrow(data_df), "lignes"))
        
        # === PHASE 1: PRE-TRAITEMENT (si necessaire) ===
        if (has_preprocess_params_changed(params)) {
          incProgress(0.1, detail = "Phase 1: Pre-traitement (UTM, PCDI, position)...")
          message("Parametres de pre-traitement changes - recalcul necessaire")
          
           rv$preprocessed_data <- yieldcleanr::clean_yield_fast(
             data = data_df,
             phase = "preprocess",
             params = params,
             metrique = TRUE
           )
          rv$preprocess_params <- params
          
          message("Pre-traitement termine")
        } else {
          incProgress(0.1, detail = "Phase 1: Pre-traitement (deja calcule)...")
          message("Reutilisation des donnees pre-traitees en cache")
        }
        
         # === PHASE 2: APPLICATION DES FILTRES ===
         incProgress(0.3, detail = "Phase 2: Application des filtres...")
         
          filter_result <- yieldcleanr::clean_yield_fast(
            data = rv$preprocessed_data,
            phase = "filter",
            preprocessed_data = rv$preprocessed_data,
            params = params,
            polygon = TRUE
          )
         
          # Extraire les donnees et les suppressions
          data_clean <- filter_result$data
          deletions_by_step <- filter_result$deletions
          deleted_points <- filter_result$deleted_points
          
          # Creer le resultat au format attendu
          incProgress(0.7, detail = "Mise a jour des resultats...")
          
           # Calculer les suppressions totales
           n_deleted <- nrow(data_df) - nrow(data_clean)
           
           result <- list(
             data_clean = data_clean,
             data_raw = data_df,
             stats = list(
               n_raw = nrow(data_df),
               n_clean = nrow(data_clean),
               n_deleted = n_deleted,
               retention_rate = nrow(data_clean) / nrow(data_df) * 100,
               deletions_by_step = deletions_by_step
             )
           )
         
          rv$result <- result
          rv$processed <- TRUE
          rv$view_mode <- "clean"
          
          message(paste("Resultat:", result$stats$n_clean, "points retenus sur", result$stats$n_raw))
          
          # Creer l'objet sf pour les points supprimes si disponibles
          if (!is.null(deleted_points) && nrow(deleted_points) > 0) {
            # Vérifier si Longitude/Latitude sont disponibles, sinon utiliser X/Y
            if ("Longitude" %in% names(deleted_points) && "Latitude" %in% names(deleted_points)) {
              rv$deletions_sf <- sf::st_as_sf(
                deleted_points,
                coords = c("Longitude", "Latitude"),
                crs = 4326
              )
            } else {
              # Fallback: convertir X,Y (UTM) vers lat/lon
              zone <- floor((mean(deleted_points$X, na.rm = TRUE) + 180) / 6) + 1
              utm_epsg <- 32600 + zone
              deleted_points_sf <- sf::st_as_sf(
                deleted_points,
                coords = c("X", "Y"),
                crs = paste0("EPSG:", utm_epsg),
                remove = FALSE
              ) %>%
                sf::st_transform(crs = 4326)
              coords <- sf::st_coordinates(deleted_points_sf)
              deleted_points$Longitude <- coords[, 1]
              deleted_points$Latitude <- coords[, 2]
              rv$deletions_sf <- sf::st_as_sf(
                deleted_points,
                coords = c("Longitude", "Latitude"),
                crs = 4326
              )
            }
            # Mettre a jour les choix du dropdown filter_step
            step_choices <- c("Toutes les etapes", unique(deleted_points$step))
            updateSelectInput(session, "filter_step", choices = step_choices, selected = "Toutes les etapes")
          } else {
            rv$deletions_sf <- NULL
            updateSelectInput(session, "filter_step", choices = "Toutes les etapes", selected = "Toutes les etapes")
          }
         
         # Mettre a jour les etiquettes des checkboxes avec le nombre de points retires
         updateCheckboxLabels(result)
         
         # Mettre a jour la carte
        incProgress(0.85, detail = "Mise a jour de la carte...")
        update_map()
        
        # Finalisation
        incProgress(1.0, detail = "Termine!")
        rv$progress_step <- ""
        rv$progress_detail <- ""
       })
      
       output$status <- renderText({
         retention_rate <- rv$result$stats$retention_rate
         if (is.null(retention_rate) || is.na(retention_rate)) {
           retention_str <- "N/A"
         } else {
           retention_str <- paste0(round(retention_rate, 1), "%")
         }
         paste("Retenus :", retention_str, "|",
               "Supprimes :", rv$result$stats$n_raw - rv$result$stats$n_clean, "points")
       })
      
    }, error = function(e) {
      message(paste("Erreur lors du traitement:", e$message))
      output$status <- renderText({
        paste("Erreur :", e$message)
      })
      showNotification(paste("Erreur :", e$message), type = "error")
    })
  }
  
  # Observer pour mettre a jour la carte quand on change de variable
  # Sans recalculer le nettoyage
  observeEvent(input$display_var, {
    if (rv$processed && rv$view_mode == "clean") {
      display_clean_map()
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$reprocess, {
    process_data()
  })

  params_trigger <- reactive({
    list(
      params = get_params()
    )
  })

  params_debounced <- debounce(params_trigger, 500)

  observeEvent(params_debounced(), {
    if (!is.null(rv$raw_data)) {
      process_data()
    }
  }, ignoreInit = TRUE)
  
  # Observer pour mettre a jour la carte quand on change de variable
  # Sans recalculer le nettoyage
  observeEvent(input$display_var, {
    if (rv$processed && rv$view_mode == "clean") {
      display_clean_map()
    }
  }, ignoreInit = TRUE)
  
   output$status <- renderText({
     if (rv$processed && !is.null(rv$result)) {
       retention_rate <- rv$result$stats$retention_rate
       if (is.null(retention_rate) || is.na(retention_rate)) {
         retention_str <- "N/A"
       } else {
         retention_str <- paste0(round(retention_rate, 1), "%")
       }
       paste("Retenus :", retention_str, "|",
             "Supprimes :", rv$result$stats$n_deleted, "points")
     } else if (!is.null(rv$raw_data)) {
       "Traitement en cours..."
     } else {
       "Importer un fichier pour commencer"
     }
   })

   output$progress_step <- renderText({
     if (rv$progress_step != "") {
       paste(rv$progress_step, "-", rv$progress_detail)
     } else {
       ""
     }
   })
  
  # Observer pour changer la vue de la carte selon map_type
  observe({
    req(rv$processed)
    req(rv$result)
    req(input$map_type)
    
    if (input$map_type == "raster") {
      display_raster_map()
    } else if (input$map_type == "deleted") {
      display_deleted_points()
    } else if (input$map_type == "yield" && rv$view_mode == "clean") {
      display_clean_map()
    }
  })
  
  # Observer pour mettre a jour la carte quand on change de filter_step
  observeEvent(input$filter_step, {
    if (rv$processed && input$map_type == "deleted") {
      display_deleted_points()
    }
  }, ignoreInit = TRUE)
  
  # Observer pour regenerer le raster quand on change de resolution
  observeEvent(input$raster_resolution, {
    if (rv$processed && input$map_type == "raster") {
      display_raster_map()
    }
  }, ignoreInit = TRUE)
  
  output$summary <- renderTable({
    req(rv$processed)
    req(rv$result)

    yield_col <- "Yield_kg_ha"
    unit_label <- "kg/ha"

    data_clean <- rv$result$data_clean
    if (inherits(data_clean, "sf")) {
      data_clean <- sf::st_drop_geometry(data_clean)
    }
    
    # Helper function to safely round values
    safe_round <- function(x, digits = 1) {
      if (is.null(x) || is.na(x) || !is.numeric(x)) return("N/A")
      round(x, digits)
    }
    
    data.frame(
      Indicateur = c("Observations brutes", "Observations nettoyees", "Points supprimes",
                     "Taux de retention", "Delai de flux (s)", "Rendement moyen"),
      Value = c(
        rv$result$stats$n_raw,
        rv$result$stats$n_clean,
        rv$result$stats$n_deleted,
        paste0(safe_round(rv$result$stats$retention_rate, 1), "%"),
        safe_round(rv$result$stats$flow_delay, 2),
        paste0(safe_round(mean(data_clean[[yield_col]], na.rm = TRUE), 1), " ", unit_label)
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$deletions_table <- DT::renderDataTable({
    req(rv$processed)
    req(rv$result)
    
    DT::datatable(rv$result$stats$deletions_by_step,
                 colnames = c("Etape", "Nombre"),
                  options = list(pageLength = 10, dom = "t"))
  })
  
  output$thresholds_display <- renderPrint({
    req(rv$processed)
    req(rv$result)
    
    thr <- rv$result$stats$thresholds
    yield_factor <- 67.25
    yield_unit <- "kg/ha"
    
    # Helper function to safely round values
    safe_round <- function(x, digits = 1) {
      if (is.null(x) || is.na(x) || !is.numeric(x)) return("N/A")
      round(x, digits)
    }
    
    cat("Plage de rendement :", safe_round(thr$min_yield * yield_factor, 1), "-",
        safe_round(thr$max_yield * yield_factor, 1), yield_unit, "\n")
    cat("Plage de vitesse :", safe_round(thr$min_velocity, 2), "-",
        safe_round(thr$max_velocity, 2), "m/s\n")
    cat("Delai de flux :", rv$result$stats$flow_delay, "secondes\n")
  })
  
  output$yield_distribution <- renderPlot({
    req(rv$processed)
    req(rv$result)
    
    yield_col <- "Yield_kg_ha"
    unit_label <- "kg/ha"
    
    data <- rv$result$data_clean
    if (inherits(data, "sf")) {
      data <- sf::st_drop_geometry(data)
    }
    
    ggplot(data, aes(x = .data[[yield_col]])) +
      geom_histogram(bins = 50, fill = "#2f6f6d", color = "white") +
      labs(title = "Distribution du rendement (donnees nettoyees)",
           x = paste0("Rendement (", unit_label, ")"),
           y = "Frequence") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 16))
  })

  output$diagnostics_ui <- renderUI({
    req(rv$processed)
    req(rv$result)

    diagnostics <- yieldcleanr:::build_filter_diagnostics(
      rv$result$data_raw,
      rv$result$deletions,
      metrique = TRUE
    )

    if (length(diagnostics) == 0) {
      return(tags$div(class = "diag-card", "Aucune suppression a diagnostiquer."))
    }

    plot_ids <- vapply(names(diagnostics), function(step_name) {
      paste0("diag_", gsub("[^A-Za-z0-9]+", "_", tolower(step_name)))
    }, character(1))

    grid_cols <- switch(input$diag_layout,
                       "1" = "1fr",
                       "2" = "repeat(2, 1fr)",
                       "3" = "repeat(3, 1fr)",
                       "repeat(auto-fit, minmax(320px, 1fr))")

    tags$div(
      style = paste0("display: grid; grid-template-columns: ", grid_cols, "; gap: 16px;"),
      lapply(seq_along(diagnostics), function(i) {
        tags$div(
          class = "diag-card",
          tags$h4(names(diagnostics)[i]),
          plotOutput(plot_ids[i], height = paste0(input$diag_plot_height, "px"))
        )
      })
    )
  })

  observe({
    req(rv$processed)
    req(rv$result)

    diagnostics <- yieldcleanr:::build_filter_diagnostics(
      rv$result$data_raw,
      rv$result$deletions,
      metrique = TRUE
    )

    if (length(diagnostics) == 0) {
      return()
    }

    steps <- names(diagnostics)
    for (step_name in steps) {
      plot_id <- paste0("diag_", gsub("[^A-Za-z0-9]+", "_", tolower(step_name)))
      local({
        step_local <- step_name
        plot_id_local <- plot_id
        diag_data <- diagnostics[[step_local]]

        output[[plot_id_local]] <- renderPlot({
          yieldcleanr:::create_diagnostic_plot(
            diag_data = diag_data,
            step_name = step_local,
            base_size = input$diag_base_size
          )
        })
      })
    }
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      format <- input$download_format
      if (format == "raster") {
        paste0("rendement_nettoye_", Sys.Date(), ".tif")
      } else if (format == "geojson") {
        paste0("rendement_nettoye_", Sys.Date(), ".geojson")
      } else {
        paste0("rendement_nettoye_", Sys.Date(), ".csv")
      }
    },
    content = function(file) {
      format <- input$download_format
      
      # Verifier que les donnees nettoyees existent
      if (is.null(rv$result) || is.null(rv$result$data_clean)) {
        showNotification("Aucune donnee nettoyee disponible. Veuillez d'abord nettoyer les donnees.", type = "error")
        return()
      }
      
      data <- rv$result$data_clean
      
      if (format == "raster") {
        # Verifier que les packages necessaires sont disponibles
        if (!requireNamespace("terra", quietly = TRUE)) {
          showNotification("Package 'terra' requis pour l'export raster. Installez-le avec: install.packages('terra')", type = "error")
          return()
        }
        if (!requireNamespace("concaveman", quietly = TRUE)) {
          showNotification("Package 'concaveman' requis pour l'export raster. Installez-le avec: install.packages('concaveman')", type = "error")
          return()
        }
        
        # Verifier que les donnees existent et sont en format sf
        if (is.null(data) || nrow(data) == 0) {
          showNotification("Aucune donnee disponible pour l'export", type = "error")
          return()
        }
        
        # Convertir en sf si necessaire
        if (!inherits(data, "sf")) {
          if (all(c("Longitude", "Latitude") %in% names(data))) {
            data <- sf::st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)
          } else if (all(c("X", "Y") %in% names(data))) {
            # Les donnees sont en UTM, convertir en lat/lon puis en sf
            data <- data %>%
              dplyr::mutate(Longitude = X, Latitude = Y) %>%
              sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
          } else {
            showNotification("Coordonnees non trouvees pour l'export raster", type = "error")
            return()
          }
        }
        
        # Verifier que l'objet sf a bien une colonne de geometrie
        if (is.null(sf::st_geometry(data))) {
          showNotification("Aucune colonne de geometrie presente", type = "error")
          return()
        }
        
        # Debug
        message(paste("Type de geometrie:", paste(sf::st_geometry_type(data), collapse = ", ")))
        message(paste("CRS:", sf::st_crs(data)$epsg))
        
        # Determiner la colonne de rendement (toujours metrique)
        yield_col <- "Yield_kg_ha"
        
        # Verifier que la colonne existe, sinon chercher une alternative
        if (!yield_col %in% names(data)) {
          alt_cols <- c("Yield_kg_ha", "Yield_buacre", "Yield", "yield", "Rendement")
          found_col <- alt_cols[alt_cols %in% names(data)]
          if (length(found_col) > 0) {
            yield_col <- found_col[1]
            message(paste("Colonne", yield_col, "utilisee pour le raster"))
          } else {
            showNotification(paste("Colonne de rendement non trouvee:", yield_col), type = "error")
            return()
          }
        }
        
        tryCatch({
          # Utiliser la meta-fonction export_data() pour l'export raster
          yieldcleanr::export_data(
            data = data,
            file = file,
            format = "raster",
            resolution = 1,
            overwrite = TRUE
          )
          showNotification("Raster exporte avec succes", type = "message")
        }, error = function(e) {
          showNotification(paste("Erreur lors de l'export raster:", e$message), type = "error")
          message(paste("Erreur export raster:", e$message))
        })
      } else if (format == "geojson" && inherits(data, "sf")) {
        sf::st_write(data, file, driver = "GeoJSON", quiet = TRUE)
      } else {
        if (inherits(data, "sf")) {
          data <- sf::st_drop_geometry(data)
        }
        write.csv(data, file, row.names = FALSE)
      }
    }
  )
}

shinyApp(ui = ui, server = server)
