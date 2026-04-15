library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(ggplot2)
library(DT)
library(yieldcleanr)
library(waiter)
library(shinyjs)

# Augmenter la taille maximale de fichier uploadé à 100MB
options(shiny.maxRequestSize = 100 * 1024^2)

ui <- fluidPage(
  useShinyjs(),
  useWaiter(),
  useHostess(),
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Fraunces:wght@500;700&family=Source+Sans+3:wght@400;500;600&display=swap');

      /* IRDA Color Palette */
      :root {
        --primary: #002752;
        --primary-dark: #001D3D;
        --primary-light: #e6eef5;
        --secondary: #AED136;
        --secondary-dark: #8fb82a;
        --secondary-light: #f4f9e6;
        --accent: #D5785A;
        --accent-dark: #b85d42;
        --accent-light: #fbeee9;
        --irda-gold: #D1AD06;
        --irda-green: #75AA41;
        --irda-brown: #833C0B;
        --success: #75AA41;
        --warning: #D1AD06;
        --danger: #D5785A;
        --info: #002752;
        --bg: #f8f9fa;
        --text-dark: #002752;
        --text-medium: #44546A;
        --text-light: #767171;
        --border-light: #E7E6E6;
        --line: #e2d9cf;
        --soft: #f8f4ee;
        --shadow: 0 14px 30px rgba(0,39,82,0.1);
      }

      body {
        font-family: 'Source Sans 3', 'Segoe UI', 'Helvetica Neue', Arial, sans-serif;
        color: var(--text-dark);
        background: radial-gradient(circle at top left, #fdf8f1 0%, #f4f1ec 45%, #efe8dd 100%);
      }

      .app-title {
        font-family: 'Fraunces', 'Times New Roman', serif;
        font-size: 28px;
        font-weight: 700;
        color: var(--primary);
        margin-bottom: 4px;
      }

      .app-subtitle {
        font-size: 14px;
        color: var(--text-light);
      }

      .sidebar-panel {
        background: #ffffff;
        padding: 18px;
        border-radius: 12px;
        border: 1px solid var(--border-light);
        box-shadow: var(--shadow);
        max-height: 90vh;
        overflow-y: auto;
      }

      .section-title {
        color: var(--primary);
        font-weight: 700;
        margin-top: 18px;
        border-bottom: 2px solid var(--secondary);
        padding-bottom: 6px;
        text-transform: uppercase;
        letter-spacing: 0.04em;
        font-size: 12px;
      }

      .section-title:first-child { margin-top: 0; }

      .param-label {
        font-size: 12px;
        margin-bottom: 4px;
        color: var(--text-medium);
      }

      .shiny-slider-input { margin-bottom: 10px; }

      #map {
        border: 1px solid var(--border-light);
        border-radius: 12px;
        overflow: hidden;
        box-shadow: var(--shadow);
      }

      .status-bar {
        background: linear-gradient(120deg, var(--primary-light), var(--secondary-light));
        padding: 12px 14px;
        border-radius: 8px;
        margin-bottom: 16px;
        border: 1px solid var(--secondary);
        font-weight: 600;
        color: var(--primary);
      }

      .btn { 
        border-radius: 8px; 
        font-weight: 600; 
        transition: all 0.2s ease;
      }
      .btn:hover {
        transform: translateY(-1px);
        box-shadow: 0 4px 12px rgba(0,39,82,0.15);
      }
      .btn-success { 
        background: var(--primary); 
        border-color: var(--primary); 
        color: white;
      }
      .btn-success:hover {
        background: var(--primary-dark);
      }
      .btn-info { 
        background: var(--secondary); 
        border-color: var(--secondary); 
        color: var(--primary-dark);
      }
      .btn-info:hover {
        background: var(--secondary-dark);
      }
      .btn-warning { 
        background: var(--accent); 
        border-color: var(--accent); 
        color: white;
      }
      .btn-warning:hover {
        background: var(--accent-dark);
      }
      .btn-primary {
        background: var(--primary);
        border-color: var(--primary);
      }

      .nav-tabs { border-bottom: 1px solid var(--border-light); }
      .nav-tabs > li > a { color: var(--text-medium); font-weight: 600; }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover {
        color: var(--primary);
        border-color: var(--border-light) var(--border-light) transparent;
      }
      .nav-tabs > li > a:hover {
        color: var(--primary);
        background: var(--primary-light);
      }

      .diag-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(320px, 1fr));
        gap: 16px;
      }

      .diag-card {
        background: #ffffff;
        border-radius: 12px;
        border: 1px solid var(--border-light);
        padding: 14px;
        box-shadow: var(--shadow);
        transition: transform 0.2s ease;
      }

      .diag-card:hover {
        transform: translateY(-2px);
        box-shadow: 0 20px 40px rgba(0,39,82,0.12);
      }

      .diag-card h4 {
        font-family: 'Fraunces', 'Times New Roman', serif;
        font-size: 16px;
        margin-top: 0;
        margin-bottom: 10px;
        color: var(--primary);
      }

      .plot-controls {
        background: var(--soft);
        padding: 10px 14px;
        border-radius: 8px;
        margin-bottom: 12px;
        border: 1px solid var(--border-light);
      }

      .plot-controls label {
        font-size: 12px;
        color: var(--text-medium);
        margin-right: 10px;
      }

      /* Metric Cards - Style rapport */
      .metric-card {
        background: #ffffff;
        border-radius: 10px;
        padding: 15px;
        margin-bottom: 12px;
        border: 1px solid var(--border-light);
        box-shadow: var(--shadow);
        display: flex;
        align-items: center;
        gap: 12px;
        transition: transform 0.2s ease;
      }
      
      .metric-card:hover {
        transform: translateY(-2px);
        box-shadow: 0 8px 24px rgba(0,39,82,0.12);
      }
      
      .metric-card.primary {
        background: linear-gradient(135deg, #002752 0%, #004080 100%);
        color: white;
      }
      
      .metric-card.accent {
        background: linear-gradient(135deg, #D5785A 0%, #E08A6C 100%);
        color: white;
      }
      
      .metric-card.info {
        background: linear-gradient(135deg, #3498db 0%, #5dade2 100%);
        color: white;
      }
      
      .metric-icon {
        font-size: 24px;
        opacity: 0.9;
      }
      
      .metric-content {
        flex: 1;
      }
      
      .metric-value {
        font-size: 22px;
        font-weight: 700;
      }
      
      .metric-label {
        font-size: 11px;
        opacity: 0.8;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }

      .info-box {
        background: linear-gradient(135deg, var(--primary-light), var(--secondary-light));
        padding: 12px 16px;
        border-radius: 8px;
        margin-bottom: 16px;
        border: 1px solid var(--border-light);
      }

      .info-box h5 {
        margin: 0 0 8px 0;
        color: var(--primary);
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
        color: var(--primary);
      }
      
       .progress-modal .modal-content {
         background: #ffffff;
         border-radius: 12px;
         border: 1px solid var(--border-light);
         box-shadow: var(--shadow);
         min-width: 500px;
       }
       
       .progress-modal .modal-header {
         border-bottom: 1px solid var(--border-light);
         background: linear-gradient(135deg, var(--primary-light), var(--secondary-light));
         padding: 20px;
       }
       
       .progress-modal .modal-title {
         font-size: 20px !important;
         font-weight: bold;
         color: var(--primary);
       }
       
       .progress-modal .modal-body {
         padding: 30px;
         font-size: 16px;
       }
      
       .progress-bar {
         background-color: var(--primary);
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
          border-radius: 8px;
          margin-bottom: 12px;
          border: 1px solid var(--border-light);
          max-height: 250px;
          overflow-y: auto;
        }
 
        .progress-text {
          color: var(--primary);
          font-size: 18px !important;
          margin-top: 8px;
          font-style: italic;
          min-height: 24px;
          font-weight: 600;
        }
      
      .field-selector label {
        font-weight: 600;
        color: var(--primary);
      }
      
      .field-list {
        margin-top: 8px;
      }
      
      .field-item {
        padding: 8px 10px;
        border: 1px solid var(--border-light);
        border-radius: 8px;
        margin-bottom: 6px;
        cursor: pointer;
        transition: all 0.2s ease;
        background: #ffffff;
      }
      
      .field-item:hover {
        border-color: var(--primary);
        background: var(--primary-light);
      }
      
      .field-item.selected {
        border-color: var(--primary);
        background: var(--primary-light);
      }
      
      .field-item .field-name {
        font-weight: 600;
        color: var(--primary);
        font-size: 13px;
      }
      
      .field-item .field-meta {
        font-size: 11px;
        color: var(--text-light);
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
        background: var(--primary);
        color: white;
      }
      
      .debug-info {
        background: #fff3cd;
        border: 1px solid var(--irda-gold);
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
      
      /* Table styling */
      .table {
        color: var(--text-dark);
      }
      .table-striped > tbody > tr:nth-of-type(odd) {
        background-color: var(--soft);
      }
      .table-hover tbody tr:hover {
        background-color: var(--primary-light);
      }
      
      /* Form controls */
      .form-control {
        border-color: var(--border-light);
        border-radius: 6px;
      }
      .form-control:focus {
        border-color: var(--primary);
        box-shadow: 0 0 0 2px var(--primary-light);
      }
      
      /* Checkbox styling */
      .checkbox label, .radio label {
        color: var(--text-dark);
      }
      
      /* Leaflet controls */
      .leaflet-control-attribution {
        background: rgba(255,255,255,0.8);
        color: var(--text-light);
      }
      
      /* Shiny notifications */
      .shiny-notification {
        border-radius: 8px;
        box-shadow: var(--shadow);
      }
      
      /* Title panel */
      .well {
        background: transparent;
        border: none;
        box-shadow: none;
      }
      
      /* Réduire la saturation du fond satellite Esri */
      .leaflet-layer:nth-child(1) .leaflet-tile {
        filter: saturate(0.3) contrast(0.8) brightness(1.1);
      }
      
      /* Multi-field navigation */
      .field-nav {
        background: var(--primary-light);
        border-radius: 8px;
        padding: 10px;
        margin-bottom: 15px;
      }
      
      .field-nav-header {
        display: flex;
        justify-content: space-between;
        align-items: center;
        margin-bottom: 8px;
      }
      
      .field-nav-title {
        font-weight: 600;
        color: var(--primary);
        font-size: 12px;
      }
      
      .field-nav-count {
        background: var(--secondary);
        color: var(--primary);
        padding: 2px 8px;
        border-radius: 12px;
        font-size: 11px;
        font-weight: 600;
      }
      
      .field-nav-controls {
        display: flex;
        gap: 5px;
        align-items: center;
      }
      
      .field-nav-btn {
        background: var(--primary);
        color: white;
        border: none;
        border-radius: 4px;
        padding: 4px 10px;
        font-size: 12px;
        cursor: pointer;
        transition: background 0.2s;
      }
      
      .field-nav-btn:hover {
        background: var(--primary-dark);
      }
      
      .field-nav-btn:disabled {
        background: var(--border-light);
        cursor: not-allowed;
      }
      
      .field-select {
        flex: 1;
        padding: 4px 8px;
        border: 1px solid var(--border-light);
        border-radius: 4px;
        font-size: 12px;
        max-width: 200px;
      }
      
      .field-badge {
        display: inline-block;
        padding: 2px 6px;
        border-radius: 4px;
        font-size: 10px;
        font-weight: 600;
        margin-left: 5px;
      }
      
      .field-badge.crop-mais { background: #F1C40F; color: black; }
      .field-badge.crop-soya { background: #27AE60; color: white; }
      .field-badge.crop-ble { background: #F9E79F; color: black; }
      .field-badge.crop-other { background: #9B59B6; color: white; }
      
      /* Waiter styling */
      .waiter-overlay-content {
        color: var(--primary);
      }
      
      .hostess-progress-bar {
        background-color: var(--secondary) !important;
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
    "Explorateur de nettoyage des rendements"),
  
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
        
        # Navigation multi-champs (visible quand plusieurs champs sont charges)
        conditionalPanel(
          condition = "output.has_multiple_fields == true",
          div(class = "field-nav",
              div(class = "field-nav-header",
                  span(class = "field-nav-title", icon("layer-group"), " Champs charges"),
                  uiOutput("field_count_badge")
              ),
              div(class = "field-nav-controls",
                  actionButton("prev_field", icon("chevron-left"), class = "field-nav-btn"),
                  uiOutput("field_selector_ui"),
                  actionButton("next_field", icon("chevron-right"), class = "field-nav-btn")
              ),
              div(style = "margin-top: 8px;",
                  uiOutput("current_field_info")
              )
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
         checkboxInput("apply_delay_adjustment_flow", "Delay adjustment flux", value = TRUE),
         checkboxInput("apply_delay_adjustment_moisture", "Delay adjustment humidite", value = TRUE),
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
         downloadButton("download_data", "Telecharger les donnees", class = "btn-success btn-block"),
         
          div(class = "section-title", "7. Export image et rapport"),
           downloadButton("download_map_image", "Telecharger la carte (PNG)", class = "btn-info btn-block"),
           
           # Style du rapport
           selectInput("report_style", "Style du rapport :",
                      choices = c("IRDA" = "irda", "Cedric Bouffard" = "ced"),
                      selected = "irda"),
           
           downloadButton("download_report", "Telecharger le rapport (HTML)", class = "btn-warning btn-block"),
           
           # Rapport multi-champs (visible quand plusieurs champs)
           conditionalPanel(
             condition = "output.has_multiple_fields == true",
             downloadButton("download_batch_report", "Rapport multi-champs (PDF)", 
                           class = "btn-primary btn-block", icon = icon("file-pdf"))
           ),
          
          # Metric Cards - Statistiques
          div(class = "section-title", "8. Statistiques"),
          uiOutput("metric_cards")
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
      delay_adjustment_result = NULL,  # Resultat du delay adjustment (delai optimal)
      delay_adjustment_params = NULL,  # Parametres utilises pour le delay adjustment
      overlap_result = NULL,  # Resultat de l'analyse d'overlap
      overlap_params = NULL,  # Parametres utilises pour l'overlap
      preprocessed_data = NULL,  # Donnees pretraitees (apres UTM, position, delay adjustment)
      
      # Multi-champs support
      fields_store = list(),  # Liste de tous les champs traites {name: {raw_data, result, metadata, ...}}
      current_field = NULL,   # Nom du champ actuellement affiche
      processing = FALSE      # Flag pour indiquer si un traitement est en cours
   )
  
  # Function to store current field data (defined early for use in import)
  store_current_field <- function(field_name) {
    if (is.null(field_name) || field_name == "") return()
    
    # Make TRUE deep copies to avoid reference issues
    field_data <- isolate({
      raw_copy <- if (!is.null(rv$raw_data)) {
        if (inherits(rv$raw_data, "sf")) {
          # For sf objects, use st_sf to force a new copy
          sf::st_sf(sf::st_drop_geometry(rv$raw_data), geometry = sf::st_geometry(rv$raw_data))
        } else {
          # For data frames, use identity to force a copy
          as.data.frame(rv$raw_data)
        }
      } else NULL
      
      result_copy <- if (!is.null(rv$result)) {
        data_clean_copy <- if (!is.null(rv$result$data_clean)) {
          if (inherits(rv$result$data_clean, "sf")) {
            sf::st_sf(sf::st_drop_geometry(rv$result$data_clean), geometry = sf::st_geometry(rv$result$data_clean))
          } else {
            as.data.frame(rv$result$data_clean)
          }
        } else NULL
        
        all_data_copy <- if (!is.null(rv$result$all_data)) {
          if (inherits(rv$result$all_data, "sf")) {
            sf::st_sf(sf::st_drop_geometry(rv$result$all_data), geometry = sf::st_geometry(rv$result$all_data))
          } else {
            as.data.frame(rv$result$all_data)
          }
        } else NULL
        
        list(
          data_clean = data_clean_copy,
          all_data = all_data_copy,
          deletions = rv$result$deletions,
          stats = rv$result$stats
        )
      } else NULL
      
      deletions_copy <- if (!is.null(rv$deletions_sf)) {
        if (inherits(rv$deletions_sf, "sf")) {
          sf::st_sf(sf::st_drop_geometry(rv$deletions_sf), geometry = sf::st_geometry(rv$deletions_sf))
        } else {
          rv$deletions_sf
        }
      } else NULL
      
      preprocessed_copy <- if (!is.null(rv$preprocessed_data)) {
        as.data.frame(rv$preprocessed_data)
      } else NULL
      
      metadata_copy <- attr(rv$raw_data, "jd_metadata")
      
      list(
        raw_data = raw_copy,
        result = result_copy,
        deletions_sf = deletions_copy,
        preprocessed_data = preprocessed_copy,
        preprocess_params = rv$preprocess_params,
        metadata = metadata_copy
      )
    })
    
    rv$fields_store[[field_name]] <- field_data
    
    n_clean <- if (!is.null(field_data$result$data_clean) && inherits(field_data$result$data_clean, "data.frame")) {
      nrow(field_data$result$data_clean)
    } else if (!is.null(field_data$result$data_clean) && inherits(field_data$result$data_clean, "sf")) {
      nrow(field_data$result$data_clean)
    } else {
      "NULL"
    }
    
    message(paste("DEBUG: Stored field '", field_name, "' with", n_clean, "clean points"))
    message(paste("DEBUG: Store now contains:", paste(names(rv$fields_store), collapse = ", ")))
  }
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery, 
                       options = providerTileOptions(opacity = 0.4)) %>%
      # addProviderTiles(providers$CartoDB.Positron) %>%
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
        tabPanel("Delay Adjustment",
          div(class = "param-label", "Plage de delai (secondes)"),
          fluidRow(
            column(6, sliderInput("delay_min", "Min :", -50, 0, -25, 1)),
            column(6, sliderInput("delay_max", "Max :", 0, 50, 20, 1))
          ),
          div(class = "param-label", "Iterations"),
          sliderInput("n_iterations", "Iterations delay adjustment :", 1, 20, 10, 1),
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
        
        # Set current field name (use filename without extension)
        field_name_from_file <- tools::file_path_sans_ext(file_name)
        rv$current_field <- field_name_from_file
        
        # Lancer le nettoyage automatiquement
        process_data()
        
        # Store the field after processing
        store_current_field(field_name_from_file)
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
    
    # Si un seul champ, pas besoin de waiter
    if (length(rv$selected_fields) == 1) {
      import_single_field(rv$selected_fields[1])
    } else {
      # Use waiter for multi-field import
      n_fields <- length(rv$selected_fields)
      
      # Create waiter with hostess progress bar
      hostess <- Hostess$new("import_hostess")
      
      waiter <- Waiter$new(
        html = tagList(
          tags$div(
            style = "text-align: center; color: var(--primary);",
            tags$h3(style = "margin-bottom: 20px;", "Importation des champs"),
            hostess$get_loader(
              preset = "fan",
              text_color = "#002752",
              center_page = TRUE
            ),
            tags$p(id = "import_status", style = "margin-top: 15px; font-weight: 600;", 
                   "Preparation...")
          )
        ),
        color = "rgba(255, 255, 255, 0.9)"
      )
      
      waiter$show()
      hostess$set(0)
      
      # Importer les champs un par un
      for (i in seq_along(rv$selected_fields)) {
        field_name <- rv$selected_fields[i]
        progress_pct <- round((i / n_fields) * 100)
        
        # Update progress
        hostess$set(progress_pct)
        
        message(paste("IMPORT LOOP: Processing field", i, "of", n_fields, ":", field_name))
        
        # Importer le champ
        import_single_field(field_name, show_notification = FALSE)
        
        message(paste("IMPORT LOOP: Finished field", i, ":", field_name))
        
        # Small pause for UI update
        Sys.sleep(0.3)
      }
      
      # Hide waiter
      waiter$hide()
      
      # Debug: show what's stored
      message(paste("AFTER IMPORT: fields_store contains", length(rv$fields_store), "fields"))
      for (fname in names(rv$fields_store)) {
        fd <- rv$fields_store[[fname]]
        if (!is.null(fd) && !is.null(fd$result) && !is.null(fd$result$data_clean)) {
          dc <- fd$result$data_clean
          if (inherits(dc, "data.frame")) {
            message(paste("  -", fname, ":", nrow(dc), "rows"))
          } else if (inherits(dc, "sf")) {
            message(paste("  -", fname, ":", nrow(dc), "rows, sf object, CRS:", sf::st_crs(dc)$epsg))
          }
        } else {
          message(paste("  -", fname, ": NO DATA"))
        }
      }
      
      showNotification(paste(n_fields, "champs importes avec succes"), type = "message")
    }
  })
  
  # Fonction pour importer un seul champ
  import_single_field <- function(field_name, show_notification = TRUE) {
    tryCatch({
      message(paste("import_single_field: START - field_name =", field_name))
      
      # RESET CACHE: Clear preprocessed data to force fresh preprocessing for this field
      rv$preprocessed_data <- NULL
      rv$preprocess_params <- NULL
      
      data <- yieldcleanr::read_yield_from_zip(rv$zip_data, field_name = field_name)
      
      message(paste("import_single_field: Read", nrow(data), "rows for field", field_name))
      message(paste("import_single_field: data object id =", format(object.size(data), units = "auto")))
      
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
             
             pal <- colorNumeric(c("#D5785A", "#D1AD06", "#75AA41"), c(q02, q98))
            
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
      
      # Set current field name
      rv$current_field <- field_name
      
      # Lancer le nettoyage automatiquement
      process_data()
      
      # Store the field data after processing
      store_current_field(field_name)
      
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
            
             pal <- colorNumeric(c("#D5785A", "#D1AD06", "#75AA41"), c(q02, q98))
            
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
        
        # Utiliser la classification discrete si c'est le rendement
        if (display_var %in% c("dry_yield", "wet_yield")) {
          valid_yield <- map_data[[value_col]][!is.na(map_data[[value_col]])]
          yield_min <- min(valid_yield)
          yield_max <- max(valid_yield)
          med <- median(valid_yield, na.rm = TRUE)
          
          # Objectif: ajuster la granularité selon la concentration aux extrémités
          
          # Fonction pour calculer les breaks et obtenir les pourcentages des extrémités
          calc_edge_percentages <- function(bin_size, valid_yield, med, yield_min, yield_max) {
            med_bin_start <- floor(med / bin_size) * bin_size
            n_side <- 3
            n_bins_target <- 7
            
            breaks_custom <- c()
            start_val <- med_bin_start - (n_side * bin_size)
            
            for (i in 0:n_bins_target) {
              breaks_custom <- c(breaks_custom, start_val + (i * bin_size))
            }
            
            # Étendre si nécessaire
            if (yield_min < breaks_custom[1]) breaks_custom[1] <- yield_min - 1
            if (yield_max > breaks_custom[length(breaks_custom)]) breaks_custom[length(breaks_custom)] <- yield_max + 1
            
            # Calculer les pourcentages
            labels <- 1:(length(breaks_custom)-1)
            yield_cut <- cut(valid_yield, breaks = breaks_custom, labels = labels, include.lowest = TRUE)
            bin_counts <- table(yield_cut)
            bin_percentages <- as.numeric(bin_counts) / sum(bin_counts) * 100
            
            # Retourner le pourcentage moyen du premier et dernier bin
            first_pct <- bin_percentages[1]
            last_pct <- bin_percentages[length(bin_percentages)]
            avg_edge <- (first_pct + last_pct) / 2
            
            return(list(avg_edge = avg_edge, breaks = breaks_custom, bin_size = bin_size))
          }
          
          # Tester avec 0.5t d'abord
          result_05 <- calc_edge_percentages(500, valid_yield, med, yield_min, yield_max)
          avg_edge_05 <- result_05$avg_edge
          
          # Progression: augmenter la taille des bins jusqu'à ce que les extrémités < 10%
          if (avg_edge_05 < 2) {
            # Très peu de données aux extrémités → utiliser 0.25t
            bin_size <- 250
          } else if (avg_edge_05 <= 10) {
            # Cas idéal → rester à 0.5t
            bin_size <- 500
          } else {
            # Trop de données aux extrémités → tester avec des bins plus larges
            # Tester 1t
            result_1 <- calc_edge_percentages(1000, valid_yield, med, yield_min, yield_max)
            avg_edge_1 <- result_1$avg_edge
            
            if (avg_edge_1 <= 10) {
              bin_size <- 1000
            } else {
              # Tester 1.5t
              result_15 <- calc_edge_percentages(1500, valid_yield, med, yield_min, yield_max)
              avg_edge_15 <- result_15$avg_edge
              
              if (avg_edge_15 <= 10) {
                bin_size <- 1500
              } else {
                # Tester 2t
                result_2 <- calc_edge_percentages(2000, valid_yield, med, yield_min, yield_max)
                avg_edge_2 <- result_2$avg_edge
                
                if (avg_edge_2 <= 10) {
                  bin_size <- 2000
                } else {
                  # Tester 2.5t
                  result_25 <- calc_edge_percentages(2500, valid_yield, med, yield_min, yield_max)
                  avg_edge_25 <- result_25$avg_edge
                  
                  if (avg_edge_25 <= 10) {
                    bin_size <- 2500
                  } else {
                    # Tester 3t
                    result_3 <- calc_edge_percentages(3000, valid_yield, med, yield_min, yield_max)
                    avg_edge_3 <- result_3$avg_edge
                    
                    if (avg_edge_3 <= 10) {
                      bin_size <- 3000
                    } else {
                      # Tester 3.5t
                      result_35 <- calc_edge_percentages(3500, valid_yield, med, yield_min, yield_max)
                      avg_edge_35 <- result_35$avg_edge
                      
                      if (avg_edge_35 <= 10) {
                        bin_size <- 3500
                      } else {
                        # Tester 4t
                        result_4 <- calc_edge_percentages(4000, valid_yield, med, yield_min, yield_max)
                        avg_edge_4 <- result_4$avg_edge
                        
                        if (avg_edge_4 <= 10) {
                          bin_size <- 4000
                        } else {
                          # Tester 4.5t
                          result_45 <- calc_edge_percentages(4500, valid_yield, med, yield_min, yield_max)
                          avg_edge_45 <- result_45$avg_edge
                          
                          if (avg_edge_45 <= 10) {
                            bin_size <- 4500
                          } else {
                            # Tester avec des bins plus larges pour rendements eleves (cultures maraicheres)
                            result_6 <- calc_edge_percentages(6000, valid_yield, med, yield_min, yield_max)
                            if (result_6$avg_edge <= 10) {
                              bin_size <- 6000
                            } else {
                              result_8 <- calc_edge_percentages(8000, valid_yield, med, yield_min, yield_max)
                              if (result_8$avg_edge <= 10) {
                                bin_size <- 8000
                              } else {
                                result_10 <- calc_edge_percentages(10000, valid_yield, med, yield_min, yield_max)
                                if (result_10$avg_edge <= 10) {
                                  bin_size <- 10000
                                } else {
                                  result_12 <- calc_edge_percentages(12000, valid_yield, med, yield_min, yield_max)
                                  if (result_12$avg_edge <= 10) {
                                    bin_size <- 12000
                                  } else {
                                    result_15 <- calc_edge_percentages(15000, valid_yield, med, yield_min, yield_max)
                                    if (result_15$avg_edge <= 10) {
                                      bin_size <- 15000
                                    } else {
                                      result_20 <- calc_edge_percentages(20000, valid_yield, med, yield_min, yield_max)
                                      if (result_20$avg_edge <= 10) {
                                        bin_size <- 20000
                                      } else {
                                        # Par defaut utiliser 25000 (25 t/ha)
                                        bin_size <- 25000
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
          
          # Créer les breaks avec des bins de taille uniforme (sauf extrémités)
          n_bins_target <- 7
          
          # Calculer la valeur de départ pour avoir exactement 7 bins avec la médiane au centre
          med_rounded <- floor(med / bin_size) * bin_size
          
          # Créer les breaks centrés sur la médiane
          breaks_custom <- c()
          for (i in -3:3) {
            breaks_custom <- c(breaks_custom, med_rounded + (i * bin_size))
          }
          # Ajouter le dernier break pour le 7ème bin
          breaks_custom <- c(breaks_custom, med_rounded + (4 * bin_size))
          
          # Ajuster les extrémités pour couvrir toutes les données
          if (yield_min < breaks_custom[1]) {
            breaks_custom[1] <- floor(yield_min / bin_size) * bin_size - bin_size
          }
          if (yield_max > breaks_custom[length(breaks_custom)]) {
            breaks_custom[length(breaks_custom)] <- ceiling(yield_max / bin_size) * bin_size + bin_size
          }
          
          # Recalculer le nombre de bins après ajustement
          n_bins <- length(breaks_custom) - 1
          
          # Identifier l'index du bin qui contient la médiane
          median_bin_index <- which(breaks_custom[-length(breaks_custom)] <= med & med < breaks_custom[-1])
          if (length(median_bin_index) == 0) median_bin_index <- 4
          
          # Créer les labels
          labels <- c()
          for (i in 1:n_bins) {
            val1 <- breaks_custom[i] / 1000
            val2 <- breaks_custom[i+1] / 1000
            # Nombre de décimales selon la taille des bins
            decimals <- ifelse(bin_size <= 500, 2, ifelse(bin_size <= 1000, 1, 0))
            val1_rounded <- round(val1, decimals)
            val2_rounded <- round(val2, decimals)
            
            if (i == 1) {
              labels <- c(labels, paste0("< ", val2_rounded, " t/ha"))
            } else if (i == n_bins) {
              labels <- c(labels, paste0("> ", val1_rounded, " t/ha"))
            } else {
              labels <- c(labels, paste0(val1_rounded, " - ", val2_rounded, " t/ha"))
            }
          }
          
          # Calculer les pourcentages
          yield_cut <- cut(valid_yield, breaks = breaks_custom, labels = labels, include.lowest = TRUE)
          bin_counts <- table(yield_cut)
          bin_percentages <- as.numeric(bin_counts) / sum(bin_counts) * 100
          
          # Créer la palette de couleurs
          bin_colors <- character(n_bins)
          if (median_bin_index > 1) {
            reds <- colorRampPalette(c("#C0392B", "#E74C3C", "#E67E22"))(median_bin_index - 1)
            bin_colors[1:(median_bin_index-1)] <- reds
          }
          bin_colors[median_bin_index] <- "#F1C40F"
          if (median_bin_index < n_bins) {
            greens <- colorRampPalette(c("#AED136", "#27AE60", "#1E8449"))(n_bins - median_bin_index)
            bin_colors[(median_bin_index+1):n_bins] <- greens
          }
          
          # Assigner les classes
          map_data$yield_classe <- cut(map_data[[value_col]], 
                                       breaks = breaks_custom, 
                                       labels = 1:n_bins, 
                                       include.lowest = TRUE)
          
          pal <- colorFactor(palette = bin_colors, domain = 1:n_bins, na.color = "transparent")
          fill_var <- quote(factor(yield_classe, levels = 1:n_bins))
          legend_pal <- pal
          legend_values <- 1:n_bins
          legend_labels <- labels
        } else {
          # Palette continue pour l'humidite
           pal <- colorNumeric(c("#D5785A", "#D1AD06", "#75AA41"), c(q02, q98))
          fill_var <- substitute(get(paste0(v, "_clamped")), list(v = value_col))
          legend_pal <- pal
          legend_values <- c(q02, q98)
          legend_labels <- NULL
        }
        
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
                            fillColor = ~pal(eval(fill_var)),
                            fillOpacity = 0.8,
                            weight = 0.5,
                            color = "black",
                            popup = ~paste0(popup_label, " : ", round(get(value_col), 1), " ", unit_label))
        } else {
          # Afficher en tant que rectangles (polygones)
          leafletProxy("map") %>%
            addPolygons(data = map_data,
                        fillColor = ~pal(eval(fill_var)),
                        fillOpacity = 0.8,
                        weight = 0.1,
                        color = "black",
                        popup = ~paste0(popup_label, " : ", round(get(value_col), 1), " ", unit_label))
        }
        
        if (is.null(legend_labels)) {
          leafletProxy("map") %>%
            addLegend(position = "bottomright",
                     pal = legend_pal,
                     values = legend_values,
                     title = legend_title)
        } else {
          leafletProxy("map") %>%
            addLegend(position = "bottomright",
                     pal = legend_pal,
                     values = legend_values,
                     labFormat = function(type, cuts, p) { legend_labels },
                     title = legend_title)
        }
        leafletProxy("map") %>%
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
       # Delay Adjustment
       apply_delay_adjustment_flow = if (!is.null(input$apply_delay_adjustment_flow)) input$apply_delay_adjustment_flow else TRUE,
       apply_delay_adjustment_moisture = if (!is.null(input$apply_delay_adjustment_moisture)) input$apply_delay_adjustment_moisture else TRUE,
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
      "apply_delay_adjustment_flow" = "Delay adjustment flux",
      "apply_delay_adjustment_moisture" = "Delay adjustment humidite",
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
      "Delay adjustment flux" = "apply_delay_adjustment_flow",
      "Delay adjustment humidite" = "apply_delay_adjustment_moisture",
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
      "Delay adjustment flux" = "Delay adjustment flux",
      "Delay adjustment humidite" = "Delay adjustment humidite",
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
         palette = c("#D5785A", "#D1AD06", "#75AA41"),
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
    preprocess_keys <- c("apply_position", "apply_delay_adjustment_flow", "apply_delay_adjustment_moisture",
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
          incProgress(0.1, detail = "Phase 1: Pre-traitement (UTM, delay adjustment, position)...")
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
            all_data <- filter_result$all_data
            deletions_by_step <- filter_result$deletions
            deleted_points <- filter_result$deleted_points
            thresholds <- filter_result$thresholds
            flow_delay <- filter_result$flow_delay
            
            # Creer le resultat au format attendu
            incProgress(0.7, detail = "Mise a jour des resultats...")
            
             # Calculer les suppressions totales
             n_deleted <- nrow(data_df) - nrow(data_clean)
             
              result <- list(
                data_clean = data_clean,
                all_data = all_data,
                deletions = deleted_points,
                stats = list(
                  n_raw = nrow(data_df),
                  n_clean = nrow(data_clean),
                  n_deleted = n_deleted,
                  retention_rate = nrow(data_clean) / nrow(data_df) * 100,
                  deletions_by_step = deletions_by_step,
                  thresholds = thresholds,
                  flow_delay = flow_delay
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
      if (is.null(x) || length(x) == 0 || !is.numeric(x) || all(is.na(x))) return("N/A")
      if (any(is.na(x))) x <- x[!is.na(x)]
      if (length(x) == 0) return("N/A")
      round(x[1], digits)
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
  
  # Metric Cards - Affichage des statistiques clés
  output$metric_cards <- renderUI({
    req(rv$processed)
    req(rv$result)
    
    yield_col <- "Yield_kg_ha"
    data_clean <- rv$result$data_clean
    if (inherits(data_clean, "sf")) {
      data_clean <- sf::st_drop_geometry(data_clean)
    }
    
    # Calculer la médiane et l'humidité moyenne
    yield_median <- median(data_clean[[yield_col]], na.rm = TRUE)
    yield_mean <- mean(data_clean[[yield_col]], na.rm = TRUE)
    
    # Chercher colonne humidite
    moisture_col <- NULL
    for (col in names(data_clean)) {
      if (grepl("(?i)moisture|humid|water", col)) {
        moisture_col <- col
        break
      }
    }
    moisture_mean <- if (!is.null(moisture_col)) {
      round(mean(data_clean[[moisture_col]], na.rm = TRUE), 1)
    } else {
      NULL
    }
    
    tagList(
      div(class = "metric-card primary",
          HTML('<i class="fas fa-chart-line metric-icon"></i>'),
          div(class = "metric-content",
              div(class = "metric-value", paste0(round(yield_mean/1000, 1), " t/ha")),
              div(class = "metric-label", "Rendement moyen")
          )
      ),
      div(class = "metric-card accent",
          HTML('<i class="fas fa-balance-scale metric-icon"></i>'),
          div(class = "metric-content",
              div(class = "metric-value", paste0(round(yield_median/1000, 1), " t/ha")),
              div(class = "metric-label", "Rendement médian")
          )
      ),
      if (!is.null(moisture_mean)) {
        div(class = "metric-card info",
            HTML('<i class="fas fa-tint metric-icon"></i>'),
            div(class = "metric-content",
                div(class = "metric-value", paste0(moisture_mean, " %")),
                div(class = "metric-label", "Humidité moyenne")
            )
        )
      }
    )
  })
  
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
      if (is.null(x) || length(x) == 0 || !is.numeric(x) || all(is.na(x))) return("N/A")
      if (any(is.na(x))) x <- x[!is.na(x)]
      if (length(x) == 0) return("N/A")
      round(x[1], digits)
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
      geom_histogram(bins = 50, fill = "#002752", color = "white") +
      labs(title = "Distribution du rendement (donnees nettoyees)",
           x = paste0("Rendement (", unit_label, ")"),
           y = "Frequence") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 16))
  })

  output$diagnostics_ui <- renderUI({
    req(rv$processed)
    req(rv$result)

    # Utiliser all_data qui contient toutes les lignes avec Yield_kg_ha
    diagnostics <- yieldcleanr:::build_filter_diagnostics(
      rv$result$all_data,
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

    # Utiliser all_data qui contient toutes les lignes avec Yield_kg_ha
    diagnostics <- yieldcleanr:::build_filter_diagnostics(
      rv$result$all_data,
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
   
   # Export de l'image de la carte
   output$download_map_image <- downloadHandler(
     filename = function() {
       paste0("carte_rendement_", Sys.Date(), ".png")
     },
     content = function(file) {
       # Verifier que les donnees existent
       if (is.null(rv$result) || is.null(rv$result$data_clean)) {
         showNotification("Aucune donnee disponible pour l'export de l'image. Veuillez d'abord nettoyer les donnees.", type = "error")
         return()
       }
       
       tryCatch({
         # Creer le plot ggplot avec geom_yield_map_polygon
         data <- rv$result$data_clean
         
         # Determiner la colonne de rendement
         yield_col <- "Yield_kg_ha"
         if (!yield_col %in% names(data)) {
           alt_cols <- c("Yield_kg_ha", "Yield_buacre", "Yield", "yield")
           found_col <- alt_cols[alt_cols %in% names(data)]
           if (length(found_col) > 0) {
             yield_col <- found_col[1]
           } else {
             showNotification("Colonne de rendement non trouvee pour l'export", type = "error")
             return()
           }
         }
         
         # Renommer la colonne pour correspondre a l'attendu de geom_yield_map_polygon
         data_plot <- data
         if (yield_col != "yield") {
           data_plot$yield <- data_plot[[yield_col]]
         }
         
         # Creer le plot
         p <- ggplot2::ggplot() +
           yieldcleanr::geom_yield_map_polygon(data_plot) +
           ggplot2::theme_void() +
           ggplot2::theme(
             plot.title = ggplot2::element_text(face = "bold", size = 18, hjust = 0.5),
             plot.subtitle = ggplot2::element_text(color = "#495057", size = 12, hjust = 0.5),
             plot.background = ggplot2::element_rect(fill = "white", color = NA),
             panel.background = ggplot2::element_rect(fill = "#f1f3f5", color = NA),
             legend.position = "bottom",
             legend.title = ggplot2::element_text(face = "bold"),
             plot.margin = ggplot2::margin(20, 20, 20, 20)
           ) +
           ggplot2::labs(
             title = "Carte de rendement",
             subtitle = paste0("Date: ", Sys.Date(), " | Points: ", nrow(data)),
             caption = "Genere avec yieldcleanr"
           )
         
         # Sauvegarder l'image
         ggplot2::ggsave(
           filename = file,
           plot = p,
           width = 10,
           height = 8,
           dpi = 300,
           bg = "white"
         )
         
          showNotification("Image exportee avec succes", type = "message")
        }, error = function(e) {
          showNotification(paste("Erreur lors de l'export de l'image:", e$message), type = "error")
          message(paste("Erreur export image:", e$message))
        })
      }
    )
    
    # Export du rapport HTML
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("rapport_rendement_", Sys.Date(), ".html")
      },
      content = function(file) {
        # Verifier que les donnees nettoyees existent
        if (is.null(rv$result) || is.null(rv$result$data_clean)) {
          showNotification("Aucune donnee nettoyee disponible. Veuillez d'abord nettoyer les donnees.", type = "error")
          return()
        }
        
        # Verifier que pagedown est disponible
        if (!requireNamespace("pagedown", quietly = TRUE)) {
          showNotification("Package 'pagedown' requis pour l'export PDF. Installez-le avec: install.packages('pagedown')", type = "error")
          return()
        }
        
        tryCatch({
          # Verifier si le package chrome est disponible pour la conversion PDF
          chrome_available <- tryCatch({
            pagedown::find_chrome()
            TRUE
          }, error = function(e) FALSE)
          
          if (!chrome_available) {
            showNotification("Chrome/Chromium requis pour la generation PDF. Le rapport sera genere en HTML.", type = "warning")
            # Generer HTML au lieu de PDF
            output_file <- sub("\\.pdf$", ".html", file)
          } else {
            output_file <- file
          }
          
          # Preparer les donnees pour le rapport
          data_clean <- rv$result$data_clean
          data_raw <- rv$raw_data
          stats <- rv$result$stats
          
          # Verifier que stats existe et initialiser avec des valeurs par defaut si necessaire
          if (is.null(stats)) {
            stats <- list(
              n_raw = nrow(data_raw),
              n_clean = nrow(data_clean),
              n_deleted = nrow(data_raw) - nrow(data_clean),
              retention_rate = ifelse(nrow(data_raw) > 0, nrow(data_clean) / nrow(data_raw) * 100, 0)
            )
          }
          
          # Convertir en sf si necessaire
          if (!inherits(data_clean, "sf")) {
            if ("Longitude" %in% names(data_clean) && "Latitude" %in% names(data_clean)) {
              data_clean <- sf::st_as_sf(data_clean, coords = c("Longitude", "Latitude"), crs = 4326)
            }
          }

          # Extraire le metadata pour le rapport
          metadata <- attr(data_clean, "jd_metadata")
          field_name_r <- NA_character_
          season_year_r <- NA_integer_
          crop_name_r <- NA_character_
          farm_name_r <- NA_character_
          harvest_date_r <- NA_character_
          if (!is.null(metadata)) {
            if (!is.null(metadata$field_info)) {
              field_name_r <- metadata$field_info$field %||% NA_character_
              season_year_r <- metadata$field_info$season %||% NA_integer_
              farm_name_r <- metadata$field_info$farm %||% NA_character_
              harvest_date_r <- metadata$field_info$date %||% NA_character_
            }
            if (!is.null(metadata$crop_info)) {
              crop_name_r <- metadata$crop_info$crop_name %||% NA_character_
            }
          }
          
          # Si date du metadata est en format ISO, la parser
          if (!is.na(harvest_date_r) && harvest_date_r != "" && grepl("T", harvest_date_r)) {
            tryCatch({
              parsed_date <- as.Date(as.POSIXct(harvest_date_r, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
              if (!is.na(parsed_date)) {
                harvest_date_r <- as.character(parsed_date)
                message("DEBUG - Date metadata parsee: ", harvest_date_r)
              }
            }, error = function(e) {
              message("DEBUG - Erreur parsing date metadata: ", e$message)
            })
          }
          
          # Si pas de date dans metadata, essayer d'extraire depuis les colonnes de temps
          if (is.na(harvest_date_r) || harvest_date_r == "") {
            tryCatch({
              # Essayer IsoTime d'abord (format ISO 8601: 2025-10-03T20:35:30.275Z)
              if ("IsoTime" %in% names(data_clean)) {
                first_time <- data_clean$IsoTime[1]
                if (!is.na(first_time) && first_time != "") {
                  parsed_date <- as.Date(as.POSIXct(first_time, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
                  if (!is.na(parsed_date)) {
                    harvest_date_r <- as.character(parsed_date)
                    message("DEBUG - Date extraite de IsoTime: ", harvest_date_r)
                  }
                }
              }
              
              # Sinon essayer Time (format: 10/3/2025 8:35:30 PM)
              if ((is.na(harvest_date_r) || harvest_date_r == "") && "Time" %in% names(data_clean)) {
                first_time <- data_clean$Time[1]
                if (!is.na(first_time) && first_time != "") {
                  # Format americain: mois/jour/annee
                  parsed_date <- as.Date(first_time, format = "%m/%d/%Y")
                  if (!is.na(parsed_date)) {
                    harvest_date_r <- as.character(parsed_date)
                    message("DEBUG - Date extraite de Time: ", harvest_date_r)
                  }
                }
              }
              
              # Sinon essayer GPS_Time (timestamp Unix en secondes)
              if ((is.na(harvest_date_r) || harvest_date_r == "") && "GPS_Time" %in% names(data_clean)) {
                gps_time <- data_clean$GPS_Time[1]
                if (!is.na(gps_time) && is.numeric(gps_time) && gps_time > 0) {
                  parsed_date <- as.Date(as.POSIXct(gps_time, origin = "1970-01-01", tz = "UTC"))
                  if (!is.na(parsed_date) && parsed_date > as.Date("2000-01-01")) {
                    harvest_date_r <- as.character(parsed_date)
                    message("DEBUG - Date extraite de GPS_Time: ", harvest_date_r)
                  }
                }
              }
            }, error = function(e) {
              message("DEBUG - Erreur extraction date: ", e$message)
            })
          }
          
          # Traduire le nom de la culture
          if (!is.na(crop_name_r) && crop_name_r != "") {
            crop_lower <- tolower(crop_name_r)
            translations <- list(
              "corn" = "Maïs",
              "maize" = "Maïs",
              "soybean" = "Soya",
              "soybeans" = "Soya",
              "soy" = "Soya",
              "wheat" = "Blé",
              "barley" = "Orge",
              "oats" = "Avoine",
              "canola" = "Canola",
              "rapeseed" = "Canola",
              "alfalfa" = "Luzerne"
            )
            if (crop_lower %in% names(translations)) {
              crop_name_r <- translations[[crop_lower]]
            }
          }
          
          # DEBUG: Afficher les valeurs pour les badges
          message("DEBUG BADGES - crop_name_r (traduit): ", crop_name_r)
          message("DEBUG BADGES - harvest_date_r: ", harvest_date_r)
          message("DEBUG BADGES - season_year_r: ", season_year_r)
          
          # Creer un fichier temporaire pour le rapport
          temp_dir <- tempfile("yield_report_")
          dir.create(temp_dir, recursive = TRUE)
          
          # Copier le template - utiliser la version sans images
          template_path <- system.file("rapport", "yield_report_template.Rmd", package = "yieldcleanr")
          if (template_path == "" || !file.exists(template_path)) {
            template_path <- file.path(getwd(), "rapport", "yield_report_template.Rmd")
          }
          
          if (!file.exists(template_path)) {
            showNotification("Template de rapport non trouve.", type = "error")
            return()
          }
          
          report_file <- file.path(temp_dir, "report.Rmd")
          file.copy(template_path, report_file, overwrite = TRUE)
          
          # Determiner le repertoire du style selectionne
          selected_style <- input$report_style
          template_dir <- dirname(template_path)
          
          if (selected_style == "ced") {
            style_dir <- system.file("rapport", "ced", package = "yieldcleanr")
            if (style_dir == "" || !dir.exists(style_dir)) {
              style_dir <- file.path(dirname(template_path), "ced")
            }
            if (!dir.exists(style_dir)) {
              showNotification("Style 'Cedric Bouffard' non trouve, utilisation du style IRDA", type = "warning")
              style_dir <- template_dir
              selected_style <- "irda"
            }
          } else {
            style_dir <- template_dir
          }
          
          message("DEBUG - Style selectionne: ", selected_style)
          message("DEBUG - Style directory: ", style_dir)
          
          # Nom de l'organisation selon le style
          org_name <- if (selected_style == "ced") "Cedric Bouffard" else "IRDA"
          
          # Copier les fichiers CSS et HTML depuis le repertoire du style
          css_files <- list.files(style_dir, pattern = "\\.css$", full.names = TRUE)
          html_files <- list.files(style_dir, pattern = "\\.html$", full.names = TRUE)
          
          for (f in c(css_files, html_files)) {
            if (file.exists(f)) {
              file.copy(f, file.path(temp_dir, basename(f)), overwrite = TRUE)
            }
          }
          
          # Fonction pour convertir une image en base64
          image_to_base64 <- function(image_path) {
            if (!file.exists(image_path)) {
              return(NULL)
            }
            img_data <- readBin(image_path, "raw", file.size(image_path))
            base64_data <- base64enc::base64encode(img_data)
            # Supprimer les newlines qui pourraient casser le JS/HTML
            base64_data <- gsub("\n", "", base64_data)
            base64_data <- gsub("\r", "", base64_data)
            ext <- tolower(tools::file_ext(image_path))
            mime_type <- switch(ext, "png" = "image/png", "jpg" = "image/jpeg", "jpeg" = "image/jpeg", "gif" = "image/gif", "image/png")
            paste0("data:", mime_type, ";base64,", base64_data)
          }
          
          # Copier et modifier le CSS pour intégrer les images en base64 selon le style
          css_source <- file.path(style_dir, "brochure.css")
          css_dest <- file.path(temp_dir, "brochure.css")
          if (file.exists(css_source)) {
            css_content <- readLines(css_source, warn = FALSE)
            
            # Pour le style IRDA, integrer bandeaugauche.png
            if (selected_style == "irda") {
              bandeau_path <- file.path(style_dir, "bandeaugauche.png")
              if (file.exists(bandeau_path)) {
                bandeau_base64 <- image_to_base64(bandeau_path)
                if (!is.null(bandeau_base64)) {
                  css_content <- gsub("url\\('bandeaugauche.png'\\)", paste0("url('", bandeau_base64, "')"), css_content)
                  message("DEBUG - Embedded bandeaugauche.png as base64 in CSS")
                }
              }
            }
            
            # Pour le style CED, integrer background.png
            if (selected_style == "ced") {
              background_path <- file.path(style_dir, "background.png")
              if (file.exists(background_path)) {
                background_base64 <- image_to_base64(background_path)
                if (!is.null(background_base64)) {
                  css_content <- gsub("url\\('background.png'\\)", paste0("url('", background_base64, "')"), css_content)
                  message("DEBUG - Embedded background.png as base64 in CSS")
                }
              }
            }
            
            writeLines(css_content, css_dest)
          }
          
          # Copier et modifier le HTML pour intégrer les images en base64
          html_source <- file.path(style_dir, "header_overrides.html")
          html_dest <- file.path(temp_dir, "header_overrides.html")
          if (file.exists(html_source)) {
            html_content <- readLines(html_source, warn = FALSE)
            
            # Intégrer logo.png depuis le repertoire du style
            logo_path <- file.path(style_dir, "logo.png")
            if (file.exists(logo_path)) {
              logo_base64 <- image_to_base64(logo_path)
              if (!is.null(logo_base64)) {
                html_content <- gsub("logo\\.src = 'logo\\.png'", paste0("logo.src = '", logo_base64, "'"), html_content)
                message("DEBUG - Embedded logo.png as base64")
              }
            }
            
            # Intégrer image de couverture selon le style
            if (selected_style == "irda") {
              couverture_path <- file.path(style_dir, "image_couverture.png")
              if (file.exists(couverture_path)) {
                couverture_base64 <- image_to_base64(couverture_path)
                if (!is.null(couverture_base64)) {
                  html_content <- gsub("imgCouverture\\.src = 'image_couverture\\.png'", paste0("imgCouverture.src = '", couverture_base64, "'"), html_content)
                  message("DEBUG - Embedded image_couverture.png as base64")
                }
              }
            } else {
              # Pour le style CED, utiliser background.png comme image de couverture
              background_path <- file.path(style_dir, "background.png")
              if (file.exists(background_path)) {
                background_base64 <- image_to_base64(background_path)
                if (!is.null(background_base64)) {
                  html_content <- gsub("imgCouverture\\.src = 'image_couverture\\.png'", paste0("imgCouverture.src = '", background_base64, "'"), html_content)
                  message("DEBUG - Embedded background.png as base64 for cover")
                }
              }
            }
            
            writeLines(html_content, html_dest)
          }
          
          # Creer le fichier GeoJSON pour le rapport
          geojson_file <- file.path(temp_dir, "rendement_nettoye.geojson")
          sf::st_write(data_clean, geojson_file, driver = "GeoJSON", quiet = TRUE, delete_dsn = TRUE)
          
          # Preparer les statistiques
          yield_col <- "Yield_kg_ha"
          if (inherits(data_clean, "sf")) {
            data_df <- sf::st_drop_geometry(data_clean)
          } else {
            data_df <- data_clean
          }
          
          # Fonction helper pour gerer les valeurs NA - retourne toujours une valeur numerique
          safe_stat <- function(x, fun, default = 0) {
            if (is.null(x) || length(x) == 0 || all(is.na(x))) return(default)
            result <- suppressWarnings(fun(x, na.rm = TRUE))
            if (is.null(result) || length(result) == 0) return(default)
            if (is.na(result) || is.nan(result) || is.infinite(result)) return(default)
            return(round(result, 1))
          }
          
          if (yield_col %in% names(data_df)) {
            yield_values <- data_df[[yield_col]]
            yield_mean <- safe_stat(yield_values, mean)
            yield_sd <- safe_stat(yield_values, sd)
            yield_min <- safe_stat(yield_values, min)
            yield_max <- safe_stat(yield_values, max)
          } else {
            yield_mean <- 0
            yield_sd <- 0
            yield_min <- 0
            yield_max <- 0
          }
          
          # Stats de nettoyage avec gestion des NA
          safe_clean_stat <- function(x, default = 0) {
            if (is.null(x)) return(default)
            if (length(x) == 0) return(default)
            val <- suppressWarnings(as.numeric(x[1]))
            if (is.na(val) || is.nan(val) || is.infinite(val)) return(default)
            return(val)
          }
          
          n_raw_val <- safe_clean_stat(stats$n_raw)
          n_clean_val <- safe_clean_stat(stats$n_clean)
          n_deleted_val <- n_raw_val - n_clean_val
          retention_rate_val <- safe_clean_stat(stats$retention_rate)
          
          current_date <- format(Sys.Date(), "%B %Y")
          
          # Lire et modifier le template
          template_content <- readLines(report_file)
          
          # Remplacer les donnees dans le template
          # YAML
          template_content <- gsub("title: \".*\"", paste0("title: \"Rapport de nettoyage des rendements\""), template_content)
          template_content <- gsub("subtitle: \".*\"", "subtitle: \"Analyse des donnees de rendement\"", template_content)
          template_content <- gsub("author: \".*\"", "author: \"YieldCleanr - Application de nettoyage\"", template_content)
          template_content <- gsub("date: \".*\"", paste0("date: \"Date: ", current_date, "\""), template_content)
          
          # Modifier les chemins CSS pour utiliser les fichiers locaux
          template_content <- gsub("- \"brochure.css\"", "- \"./brochure.css\"", template_content)
          template_content <- gsub("in_header: header_overrides.html", "in_header: ./header_overrides.html", template_content)
          
          # Debug: afficher les valeurs
          message(paste("DEBUG - n_raw:", n_raw_val, "n_clean:", n_clean_val, "n_deleted:", n_deleted_val))
          message(paste("DEBUG - retention:", retention_rate_val))
          message(paste("DEBUG - yield mean:", yield_mean, "sd:", yield_sd))
          
          # S'assurer que toutes les valeurs sont numeriques et non NA
          ensure_numeric <- function(x, default = 0) {
            if (is.null(x) || is.na(x) || is.nan(x) || is.infinite(x)) return(default)
            val <- suppressWarnings(as.numeric(x))
            if (is.na(val) || is.nan(val) || is.infinite(val)) return(default)
            return(val)
          }
          
          n_raw_val <- ensure_numeric(n_raw_val, 0)
          n_clean_val <- ensure_numeric(n_clean_val, 0)
          n_deleted_val <- ensure_numeric(n_deleted_val, 0)
          retention_rate_val <- ensure_numeric(retention_rate_val, 0)
          yield_mean <- ensure_numeric(yield_mean, 0)
          yield_sd <- ensure_numeric(yield_sd, 0)
          yield_min <- ensure_numeric(yield_min, 0)
          yield_max <- ensure_numeric(yield_max, 0)
          
          # Echapper les backslashes pour Windows
          geojson_file_escaped <- gsub("\\\\", "/", geojson_file)
          
          # Inserer les definitions de variables dans le setup chunk
          # Trouver le setup chunk existant
          setup_start <- grep("^```\\{r setup", template_content)[1]
          
          # Trouver la fin du setup chunk (ligne avec juste ```)
          chunk_ends <- grep("^```\\s*$", template_content)
          setup_end <- chunk_ends[chunk_ends > setup_start][1]
          
          # Code a inserer avec les valeurs calculees (remplace tout le setup)
          # Calculer la mediane et humidite
          yield_median_val <- 0
          moisture_mean_val <- 0
          if (yield_col %in% names(data_df)) {
            yield_values <- data_df[[yield_col]]
            yield_median_val <- safe_stat(yield_values, median)
          }
          # Chercher colonne humidite
          moisture_col <- NULL
          for (col in names(data_df)) {
            if (grepl("(?i)moisture|humid|water", col)) {
              moisture_col <- col
              break
            }
          }
          if (!is.null(moisture_col)) {
            moisture_mean_val <- safe_stat(data_df[[moisture_col]], mean)
          }
          yield_median_val <- ensure_numeric(yield_median_val, 0)
          moisture_mean_val <- ensure_numeric(moisture_mean_val, 0)
          
          new_setup <- c(
            "```{r setup, include=FALSE}",
            "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, fig.width = 10, fig.height = 7, out.width = '100%')",
            "library(ggplot2)",
            "library(sf)",
            "library(dplyr)",
            "library(knitr)",
            "library(ggspatial)",
            "library(ggbasemap)",
            "",
            "# Data loaded from cleaned yield data",
            paste0("gdf <- st_read('", geojson_file_escaped, "', quiet = TRUE)"),
            "",
            "# Field metadata",
            paste0("field_name <- ", ifelse(is.na(field_name_r), "NA_character_", paste0("'", gsub("'", "\\\\'", field_name_r, fixed = TRUE), "'"))),
            paste0("season_year <- ", ifelse(is.na(season_year_r), "NA_integer_", as.integer(season_year_r))),
            paste0("crop_name <- ", ifelse(is.na(crop_name_r), "NA_character_", paste0("'", gsub("'", "\\\\'", crop_name_r, fixed = TRUE), "'"))),
            paste0("farm_name <- ", ifelse(is.na(farm_name_r), "NA_character_", paste0("'", gsub("'", "\\\\'", farm_name_r, fixed = TRUE), "'"))),
            paste0("harvest_date <- ", ifelse(is.na(harvest_date_r), "NA_character_", paste0("'", harvest_date_r, "'"))),
            "# Preparer les libelles pour l'affichage (pre-calcules dans l'app)",
            "field_label <- if (!is.na(field_name) && field_name != '') paste0('Champ ', field_name) else 'Champ'",
            {
              # Calculer year_label (annee seulement pour la section)
              year_label_val <- if (!is.na(harvest_date_r) && harvest_date_r != "") {
                tryCatch(format(as.Date(harvest_date_r), "%Y"), error = function(e) {
                  if (!is.na(season_year_r)) as.character(season_year_r) else ""
                })
              } else if (!is.na(season_year_r)) {
                as.character(season_year_r)
              } else ""
              message("DEBUG BADGES - year_label genere: ", year_label_val)
              paste0("year_label <- '", year_label_val, "'")
            },
            {
              # Calculer date_label (date complete pour le badge)
              date_label_val <- if (!is.na(harvest_date_r) && harvest_date_r != "") {
                tryCatch(format(as.Date(harvest_date_r), "%d %b %Y"), error = function(e) {
                  if (!is.na(season_year_r)) as.character(season_year_r) else ""
                })
              } else if (!is.na(season_year_r)) {
                as.character(season_year_r)
              } else ""
              message("DEBUG BADGES - date_label genere: ", date_label_val)
              paste0("date_label <- '", date_label_val, "'")
            },
            {
              # Calculer crop_label  
              crop_label_val <- if (!is.na(crop_name_r) && crop_name_r != "") {
                gsub("'", "\\'", crop_name_r, fixed = TRUE)
              } else ""
              message("DEBUG BADGES - crop_label genere: ", crop_label_val)
              paste0("crop_label <- '", crop_label_val, "'")
            },
            "# Statistics for the report",
            paste0("n_raw <- ", n_raw_val),
            paste0("n_clean <- ", n_clean_val),
            paste0("n_deleted <- ", n_deleted_val),
            paste0("retention_rate <- ", retention_rate_val),
            paste0("yield_mean <- ", yield_mean),
            paste0("yield_sd <- ", yield_sd),
            paste0("yield_min <- ", yield_min),
            paste0("yield_max <- ", yield_max),
            paste0("yield_median <- ", yield_median_val),
            paste0("moisture_mean <- ", moisture_mean_val),
            "",
            "# Safe default function",
            "safe_val <- function(x, default = 0) {",
            "  if (is.null(x)) return(default)",
            "  if (length(x) == 0) return(default)",
            "  val <- suppressWarnings(as.numeric(x))",
            "  if (is.na(val) || is.nan(val) || is.infinite(val)) return(default)",
            "  return(val)",
            "}",
            "```"
          )
          
          # Remplacer le setup chunk existant
          template_content <- c(
            template_content[1:(setup_start-1)],
            new_setup,
            template_content[(setup_end+1):length(template_content)]
          )
          
          # Ecrire le template modifie
          writeLines(template_content, report_file)
          
          # Rendre le rapport en PDF
          output_file <- file
          temp_html <- file.path(temp_dir, "report.html")
          
          showNotification("Generation du rapport PDF en cours...", type = "message")
          
          message("DEBUG - Final output file:", output_file)
          message("DEBUG - Report file:", report_file)
          message("DEBUG - Temp dir:", temp_dir)
          
          # Generer le HTML autonome (avec ressources embarquees) dans le temp_dir
          tryCatch({
            rmarkdown::render(
              input = report_file,
              output_file = temp_html,
              quiet = TRUE,
              encoding = "UTF-8"
            )
            message("DEBUG - HTML rendering completed:", temp_html)
          }, error = function(e) {
            message(paste("DEBUG - Error during render:", e$message))
            stop(e)
          })
          
          # Verifier que le HTML existe
          if (!file.exists(temp_html)) {
            stop("Le fichier HTML n'a pas ete genere")
          }
          
          # Copier le HTML vers la destination finale
          file.copy(temp_html, output_file, overwrite = TRUE)
          message("DEBUG - HTML report saved:", output_file)
          
          # Cleanup temp directory after successful render
          unlink(temp_dir, recursive = TRUE)
          
          showNotification(paste("Rapport exporte:", basename(output_file)), type = "message")
          
        }, error = function(e) {
          showNotification(paste("Erreur lors de la generation du rapport:", e$message), type = "error")
          message(paste("Erreur generation rapport:", e$message))
        })
      }
    )
    
    # ============================================================
    # MULTI-FIELD SUPPORT
    # ============================================================
    
    # Reactive: check if multiple fields are loaded
    output$has_multiple_fields <- reactive({
      length(rv$fields_store) > 1
    })
    outputOptions(output, "has_multiple_fields", suspendWhenHidden = FALSE)
    
    # UI: Field count badge
    output$field_count_badge <- renderUI({
      n_fields <- length(rv$fields_store)
      if (n_fields > 0) {
        span(class = "field-nav-count", paste0(n_fields, " champs"))
      }
    })
    
    # UI: Field selector dropdown
    output$field_selector_ui <- renderUI({
      req(length(rv$fields_store) > 0)
      
      field_names <- names(rv$fields_store)
      current <- rv$current_field
      
      selectInput("field_selector", NULL,
                  choices = field_names,
                  selected = current,
                  width = "150px")
    })
    
    # UI: Current field info
    output$current_field_info <- renderUI({
      req(rv$current_field)
      req(rv$fields_store[[rv$current_field]])
      
      field_data <- rv$fields_store[[rv$current_field]]
      
      # Extract metadata if available
      metadata <- field_data$metadata
      crop_name <- ""
      harvest_date <- ""
      
      if (!is.null(metadata)) {
        if (!is.null(metadata$crop_info)) {
          crop_name <- metadata$crop_info$crop_name %||% ""
        }
        if (!is.null(metadata$field_info)) {
          harvest_date <- metadata$field_info$date %||% ""
        }
      }
      
      # Translate crop name
      if (crop_name != "") {
        crop_lower <- tolower(crop_name)
        translations <- list(
          "corn" = "Maïs", "maize" = "Maïs",
          "soybean" = "Soya", "soybeans" = "Soya", "soy" = "Soya",
          "wheat" = "Blé", "barley" = "Orge", "oats" = "Avoine"
        )
        if (crop_lower %in% names(translations)) {
          crop_name <- translations[[crop_lower]]
        }
      }
      
      # Build badges
      badges <- tagList()
      if (crop_name != "") {
        crop_class <- switch(tolower(crop_name),
                            "maïs" = "crop-mais",
                            "soya" = "crop-soya", 
                            "blé" = "crop-ble",
                            "crop-other")
        badges <- tagList(badges, span(class = paste("field-badge", crop_class), crop_name))
      }
      if (harvest_date != "" && !is.na(harvest_date)) {
        date_str <- tryCatch({
          format(as.Date(harvest_date), "%Y")
        }, error = function(e) harvest_date)
        badges <- tagList(badges, span(class = "field-badge", style = "background: var(--primary); color: white;", date_str))
      }
      
      div(
        style = "font-size: 11px; color: var(--text-medium);",
        badges
      )
    })
    
    # Observer: Previous field navigation
    observeEvent(input$prev_field, {
      req(length(rv$fields_store) > 1)
      req(rv$current_field)
      
      field_names <- names(rv$fields_store)
      current_idx <- which(field_names == rv$current_field)
      
      if (current_idx > 1) {
        new_field <- field_names[current_idx - 1]
        switch_to_field(new_field)
      }
    })
    
    # Observer: Next field navigation
    observeEvent(input$next_field, {
      req(length(rv$fields_store) > 1)
      req(rv$current_field)
      
      field_names <- names(rv$fields_store)
      current_idx <- which(field_names == rv$current_field)
      
      if (current_idx < length(field_names)) {
        new_field <- field_names[current_idx + 1]
        switch_to_field(new_field)
      }
    })
    
    # Observer: Field selector dropdown change
    observeEvent(input$field_selector, {
      req(input$field_selector)
      req(input$field_selector != rv$current_field)
      
      switch_to_field(input$field_selector)
    }, ignoreInit = TRUE)
    
    # Function to switch to a different field
    switch_to_field <- function(field_name) {
      req(field_name %in% names(rv$fields_store))
      
      message(paste("DEBUG switch_to_field: Switching to field:", field_name))
      
      field_data <- rv$fields_store[[field_name]]
      
      if (is.null(field_data)) {
        message(paste("ERROR: field_data is NULL for", field_name))
        return()
      }
      
      message(paste("DEBUG: Available fields in store:", paste(names(rv$fields_store), collapse = ", ")))
      
      # Check data_clean
      if (!is.null(field_data$result) && !is.null(field_data$result$data_clean)) {
        dc <- field_data$result$data_clean
        if (inherits(dc, "sf")) {
          message(paste("DEBUG: data_clean is sf with", nrow(dc), "rows, CRS:", sf::st_crs(dc)$epsg))
          message(paste("DEBUG: data_clean bbox:", paste(sf::st_bbox(dc), collapse = ", ")))
        } else {
          message(paste("DEBUG: data_clean is df with", nrow(dc), "rows"))
        }
      } else {
        message(paste("DEBUG: data_clean is NULL"))
      }
      
      # Update current field
      rv$current_field <- field_name
      
      # Restore the field's data
      rv$raw_data <- field_data$raw_data
      rv$result <- field_data$result
      rv$deletions_sf <- field_data$deletions_sf
      rv$preprocessed_data <- field_data$preprocessed_data
      rv$preprocess_params <- field_data$preprocess_params
      
      message(paste("DEBUG: Restored rv$result$data_clean, checking..."))
      if (!is.null(rv$result$data_clean)) {
        dc <- rv$result$data_clean
        if (inherits(dc, "sf")) {
          message(paste("DEBUG: rv$result$data_clean now has", nrow(dc), "rows"))
        }
      }
      
      # Update processed flag
      rv$processed <- !is.null(field_data$result)
      
      # Update view mode
      if (!is.null(field_data$result)) {
        rv$view_mode <- "clean"
      } else {
        rv$view_mode <- "raw"
      }
      
      # Update map and zoom to the field
      if (rv$processed && !is.null(rv$result$data_clean)) {
        # Get the cleaned data
        map_data <- rv$result$data_clean
        
        message(paste("DEBUG: map_data class:", class(map_data)))
        
        if (!is.null(map_data)) {
          message(paste("DEBUG: map_data nrow:", nrow(map_data)))
          
          tryCatch({
            # Ensure data is in WGS84 (EPSG:4326) for Leaflet
            if (inherits(map_data, "sf")) {
              # Check CRS and transform if needed
              current_crs <- sf::st_crs(map_data)
              message(paste("DEBUG: current_crs:", current_crs$epsg))
              
              if (!is.na(current_crs$epsg) && current_crs$epsg != 4326) {
                map_data <- sf::st_transform(map_data, crs = 4326)
              }
              
              # Get bounding box
              bbox <- sf::st_bbox(map_data)
              message(paste("DEBUG: bbox:", paste(bbox, collapse = ", ")))
              
              # Validate bbox values
              if (all(is.finite(c(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"])))) {
                # Update the map display first
                update_map()
                
                # Store bbox values for use in delayed zoom
                bbox_xmin <- as.numeric(bbox["xmin"])
                bbox_ymin <- as.numeric(bbox["ymin"])
                bbox_xmax <- as.numeric(bbox["xmax"])
                bbox_ymax <- as.numeric(bbox["ymax"])
                
                # Use delay to ensure map is updated before zooming
                shinyjs::delay(200, {
                  leafletProxy("map") %>%
                    fitBounds(
                      lng1 = bbox_xmin,
                      lat1 = bbox_ymin,
                      lng2 = bbox_xmax,
                      lat2 = bbox_ymax,
                      options = list(padding = c(50, 50))
                    )
                })
              } else {
                message("DEBUG: bbox values not finite, just updating map")
                update_map()
              }
            } else {
              # Not an sf object, update map without zooming
              message("DEBUG: map_data is not sf object")
              update_map()
            }
          }, error = function(e) {
            message(paste("Error zooming to field:", e$message))
            update_map()
          })
        }
      } else if (!is.null(rv$raw_data)) {
        # For raw data, zoom to center
        if ("Longitude" %in% names(rv$raw_data) && "Latitude" %in% names(rv$raw_data)) {
          center_lng <- mean(rv$raw_data$Longitude, na.rm = TRUE)
          center_lat <- mean(rv$raw_data$Latitude, na.rm = TRUE)
          
          if (is.finite(center_lng) && is.finite(center_lat)) {
            leafletProxy("map") %>%
              setView(lng = center_lng, lat = center_lat, zoom = 15)
          }
        }
      }
      
      # Update checkbox labels
      if (!is.null(rv$result)) {
        updateCheckboxLabels(rv$result)
      }
      
      showNotification(paste("Champ actif:", field_name), type = "message", duration = 2)
    }
    
    # Download handler for batch report (multi-field HTML)
    output$download_batch_report <- downloadHandler(
      filename = function() {
        paste0("rapport_multi_champs_", Sys.Date(), ".html")
      },
      content = function(file) {
        req(length(rv$fields_store) > 0)

        # Show progress modal
        showModal(modalDialog(
          title = "Generation du rapport PDF",
          size = "m",
          easyClose = FALSE,
          footer = NULL,
          div(
            style = "text-align: center; padding: 20px;",
            hostess_loader("report_hostess", preset = "fan", text_color = "#002752"),
            tags$p(id = "report_status", style = "margin-top: 15px; color: #002752; font-weight: 600;", 
                   "Preparation des donnees...")
          )
        ))
        
        # Create Hostess instance
        report_hostess <- Hostess$new("report_hostess")
        report_hostess$set(0)
        
        tryCatch({
          showNotification("Generation du rapport multi-champs en cours...", type = "message")
          
          # Create temporary directory for GeoJSON files
          temp_dir <- tempfile("batch_report_")
          dir.create(temp_dir, recursive = TRUE)
          
          # Save all cleaned field data as GeoJSON
          temp_files <- c()
          field_names <- names(rv$fields_store)
          total_fields <- length(field_names)
          processed_fields <- 0
          
          for (field_name in field_names) {
            field_data <- rv$fields_store[[field_name]]
            
            if (!is.null(field_data$result) && !is.null(field_data$result$data_clean)) {
              # Update status with field name
              processed_fields <- processed_fields + 1
              progress_pct <- round((processed_fields / total_fields) * 30)
              report_hostess$set(progress_pct)
              
              # Update status text to show current field
              status_text <- paste0("Export: ", field_name, " (", processed_fields, "/", total_fields, ")")
              shinyjs::runjs(paste0("document.getElementById('report_status').innerText = '", status_text, "';"))
              
              # Create a clean filename
              clean_name <- gsub("[^a-zA-Z0-9_-]", "_", field_name)
              geojson_file <- file.path(temp_dir, paste0(clean_name, ".geojson"))
              
              # Write GeoJSON
              sf::st_write(field_data$result$data_clean, geojson_file, 
                          driver = "GeoJSON", quiet = TRUE, delete_dsn = TRUE)
              
              # Save metadata as JSON file for batch report to read
              metadata_file <- file.path(temp_dir, paste0(clean_name, "_metadata.json"))
              if (!is.null(field_data$metadata)) {
                jsonlite::write_json(field_data$metadata, metadata_file, pretty = TRUE)
              }
              
              temp_files <- c(temp_files, geojson_file)
            }
          }
          
          if (length(temp_files) == 0) {
            removeModal()
            showNotification("Aucun champ nettoye disponible pour le rapport", type = "warning")
            return()
          }
          
          # Update status for report generation
          report_hostess$set(50)
          shinyjs::runjs("document.getElementById('report_status').innerText = 'Generation du PDF...';")
          
          # Generate batch report using the package function
          # This uses the same template as single-field reports
          # Get the selected style from the UI
          selected_style <- input$report_style
          
          result_file <- yieldcleanr::generate_batch_report(
            file_paths = temp_files,
            output_file = file,
            title = "Rapport multi-champs",
            output_format = "html",
            style = selected_style
          )
          
          # Update to complete
          report_hostess$set(100)
          Sys.sleep(0.5)
          
          # Cleanup and close modal
          removeModal()
          unlink(temp_dir, recursive = TRUE)
          
          # Check if PDF or HTML was returned
          if (grepl("\\.html$", result_file, ignore.case = TRUE)) {
            showNotification("Rapport HTML genere (PDF indisponible)", type = "warning")
          } else {
            showNotification("Rapport PDF exporte avec succes", type = "message")
          }
          
        }, error = function(e) {
          # Close modal on error
          removeModal()
          showNotification(paste("Erreur:", e$message), type = "error")
          message(paste("Erreur batch report:", e$message))
        })
      }
    )
  }

shinyApp(ui = ui, server = server)
