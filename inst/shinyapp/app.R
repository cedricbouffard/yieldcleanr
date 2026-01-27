library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(ggplot2)
library(DT)
library(yieldcleanr)

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
        font-family: "Source Sans 3", "Trebuchet MS", sans-serif;
        color: var(--ink);
        background: radial-gradient(circle at top left, #fdf8f1 0%, #f4f1ec 45%, #efe8dd 100%);
      }

      .app-title {
        font-family: "Fraunces", "Times New Roman", serif;
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
        grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
        gap: 16px;
      }

      .diag-card {
        background: var(--panel);
        border-radius: 16px;
        border: 1px solid var(--line);
        padding: 14px;
        box-shadow: var(--shadow);
      }

      .diag-card h4 {
        font-family: "Fraunces", "Times New Roman", serif;
        font-size: 16px;
        margin-top: 0;
        margin-bottom: 10px;
        color: var(--ink);
      }
    "))
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
        
        h4("1. Importer les donnees"),
        fileInput("file", "Choisir un fichier de rendement",
                 accept = c(".txt", ".csv")),
        
        h4("2. Parametres de sortie"),
        radioButtons("units", "Unites de sortie :",
                    c("Metrique (kg/ha)" = TRUE,
                      "Imperial (bu/acre)" = FALSE)),
        checkboxInput("polygon", "Creer des polygones", value = TRUE),
        
        div(class = "section-title", "3. Parametres PCDI"),
        div(class = "param-label", "Plage de delai (secondes)"),
        fluidRow(
          column(6, sliderInput("delay_min", "Min :", -50, 0, -25, 1)),
          column(6, sliderInput("delay_max", "Max :", 0, 50, 10, 1))
        ),
        div(class = "param-label", "Iterations"),
        sliderInput("n_iterations", "Iterations PCDI :", 1, 20, 5, 1),
        div(class = "param-label", "Niveau de bruit"),
        sliderInput("noise_level", "Bruit :", 0, 0.2, 0.05, 0.01),
        
        div(class = "section-title", "4. Parametres de seuil"),
        div(class = "param-label", "Quantiles de rendement"),
        fluidRow(
          column(6, sliderInput("yllim", "Bas :", 0, 0.3, 0.10, 0.01)),
          column(6, sliderInput("yulim", "Haut :", 0.7, 1.0, 0.90, 0.01))
        ),
        div(class = "param-label", "Multiplicateur IQR rendement"),
        sliderInput("yscale", "IQR rendement :", 0.5, 2.0, 1.1, 0.1),
        div(class = "param-label", "Quantiles de vitesse"),
        fluidRow(
          column(6, sliderInput("vlim", "Bas :", 0, 0.2, 0.05, 0.01)),
          column(6, sliderInput("vulim", "Haut :", 0.8, 1.0, 0.95, 0.01))
        ),
        div(class = "param-label", "Multiplicateur IQR vitesse"),
        sliderInput("vspace", "IQR vitesse :", 0.5, 2.0, 1.1, 0.1),
        div(class = "param-label", "Vitesse minimale (m/s)"),
        sliderInput("minv", "Vitesse min :", 0, 2, 0.5, 0.1),
        div(class = "param-label", "Marge de position (m)"),
        sliderInput("gbuffer", "Marge :", 0, 500, 100, 10),
        
        div(class = "section-title", "5. Parametres de filtres"),
        div(class = "param-label", "Filtre de chevauchement"),
        fluidRow(
          column(6, sliderInput("cellsize", "Cellule (m) :", 0.1, 1.0, 0.3, 0.05)),
          column(6, sliderInput("overlap_thresh", "Seuil max :", 0.1, 1.0, 0.5, 0.1))
        ),
        div(class = "param-label", "Filtre ecart-type local"),
        fluidRow(
          column(6, sliderInput("nswaths", "Passages :", 1, 20, 5, 1)),
          column(6, sliderInput("lsd_limit", "Limite (ET) :", 1.0, 5.0, 2.4, 0.1))
        ),
        div(class = "param-label", "Cellules minimales par grille"),
        sliderInput("min_cells", "Cellules min :", 1, 10, 3, 1),
        div(class = "param-label", "Ecart-type auto"),
        sliderInput("nstd", "Ecarts-types :", 1, 5, 3, 0.5),
        
        div(class = "section-title", "6. Carte et telechargement"),
        selectInput("map_type", "Vue :",
                   choices = c("Carte des rendements" = "yield",
                             "Points supprimes" = "deleted",
                             "Comparaison" = "comparison")),
        
        conditionalPanel(
          condition = "input.map_type == 'deleted' || input.map_type == 'comparison'",
          selectInput("filter_step", "Etape de filtre :",
                     choices = "Toutes les etapes", selected = "Toutes les etapes")
        ),
        
        radioButtons("download_format", "Format :",
                    c("GeoJSON" = "geojson",
                      "CSV" = "csv")),
        downloadButton("download_data", "Telecharger les donnees", class = "btn-success btn-block"),
        downloadButton("download_log", "Telecharger le journal", class = "btn-info btn-block"),
        
        hr(),
        actionButton("reprocess", "Appliquer les parametres", class = "btn-warning btn-block")
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
                 uiOutput("diagnostics_ui"))
      )
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    result = NULL,
    processed = FALSE
  )
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 0, lat = 0, zoom = 2)
  })
  
  get_params <- reactive({
    delay_min <- min(input$delay_min, input$delay_max)
    delay_max <- max(input$delay_min, input$delay_max)
    list(
      delay_range = seq(delay_min, delay_max, by = 1),
      n_iterations = input$n_iterations,
      noise_level = input$noise_level,
      yllim = input$yllim,
      yulim = input$yulim,
      yscale = input$yscale,
      v_lim = input$vlim,
      v_ulim = input$vulim,
      v_scale = input$vspace,
      minv_abs = input$minv,
      gbuffer = input$gbuffer,
      cellsize_overlap = input$cellsize,
      overlap_threshold = input$overlap_thresh,
      n_swaths = input$nswaths,
      lsd_limit = input$lsd_limit,
      min_cells = input$min_cells,
      n_std = input$nstd
    )
  })
  
  process_data <- function() {
    req(input$file)
    
    tryCatch({
      params <- get_params()
      
      output$status <- renderText("Traitement en cours...")
      
      withProgress(message = "Nettoyage des donnees...", value = 0.3, {
        result <- yieldcleanr::clean_yield_with_tracking(
          file_path = input$file$datapath,
          metrique = as.logical(input$units),
          polygon = input$polygon,
          params = params
        )
        
        rv$result <- result
        rv$processed <- TRUE
        
        raw_sf <- result$data_raw %>%
          dplyr::select(Longitude, Latitude) %>%
          sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
        
        if (nrow(raw_sf) > 0) {
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
        
        steps <- c("Toutes les etapes", unique(result$deletions$step))
        updateSelectInput(session, "filter_step", choices = steps)
      })
      
      output$status <- renderText({
        paste("Retenus :", round(rv$result$stats$retention_rate * 100, 1), "% |",
              "Supprimes :", rv$result$stats$n_deleted, "points")
      })
      
    }, error = function(e) {
      output$status <- renderText({
        paste("Erreur :", e$message)
      })
      showNotification(paste("Erreur :", e$message), type = "error")
    })
  }
  
  observeEvent(input$file, {
    process_data()
  })
  
  observeEvent(input$reprocess, {
    process_data()
  })

  params_trigger <- reactive({
    list(
      params = get_params(),
      units = input$units,
      polygon = input$polygon
    )
  })

  params_debounced <- debounce(params_trigger, 500)

  observeEvent(params_debounced(), {
    process_data()
  }, ignoreInit = TRUE)
  
  output$status <- renderText({
    if (rv$processed && !is.null(rv$result)) {
      paste("Retenus :", round(rv$result$stats$retention_rate * 100, 1), "% |",
            "Supprimes :", rv$result$stats$n_deleted, "points")
    } else if (!is.null(input$file)) {
      "Traitement en cours..."
    } else {
      "Importer un fichier pour commencer"
    }
  })
  
  observe({
    req(rv$processed)
    req(rv$result)
    req(input$map_type)
    
    raw_sf <- rv$result$data_raw %>%
      dplyr::select(Longitude, Latitude) %>%
      sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
    
    if (nrow(raw_sf) == 0) return()
    
    center_lng <- mean(sf::st_coordinates(raw_sf)[, 1])
    center_lat <- mean(sf::st_coordinates(raw_sf)[, 2])
    
    if (input$map_type == "yield") {
      yield_col <- if (as.logical(input$units)) "Yield_kg_ha" else "Yield_buacre"
      legend_title <- if (as.logical(input$units)) "Rendement (kg/ha)" else "Rendement (bu/acre)"
      map_data <- if (input$polygon) {
        rv$result$data_clean
      } else {
        sf::st_as_sf(rv$result$data_clean,
                     coords = c("Longitude", "Latitude"),
                     crs = 4326)
      }
      
      pal <- colorNumeric(c("#b04a3b", "#e9b44c", "#3d8a6b"), map_data[[yield_col]])

      leafletProxy("map") %>%
        clearMarkers() %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(data = map_data,
                    fillColor = ~pal(get(yield_col)),
                    fillOpacity = 0.7,
                    weight = 0,
                    popup = ~paste0("Rendement : ", round(get(yield_col), 1), " ", ifelse(as.logical(input$units), "kg/ha", "bu/acre"))) %>%
        addLegend(position = "bottomright",
                 pal = pal,
                 values = map_data[[yield_col]],
                 title = legend_title) %>%
        setView(lng = center_lng, lat = center_lat, zoom = 15)
      
    } else {
      filter_step <- input$filter_step
      
      leafletProxy("map") %>%
        clearMarkers() %>%
        clearShapes() %>%
        clearControls()
      
      if (filter_step != "Toutes les etapes") {
        deleted_points <- rv$result$data_raw %>%
          dplyr::inner_join(
            rv$result$deletions %>% dplyr::filter(step == filter_step),
            by = "orig_row_id"
          ) %>%
          dplyr::select(Longitude, Latitude, step, reason) %>%
          sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
        
        if (nrow(deleted_points) > 0) {
          leafletProxy("map") %>%
            addCircleMarkers(data = deleted_points,
                            radius = 4,
                            fillColor = "red",
                            fillOpacity = 0.8,
                            weight = 1,
                            color = "darkred",
                            popup = ~paste0("<b>Etape :</b> ", step, "<br><b>Raison :</b> ", reason))
        }
      } else if (nrow(rv$result$deletions) > 0) {
        all_deleted <- rv$result$data_raw %>%
          dplyr::inner_join(rv$result$deletions, by = "orig_row_id") %>%
          dplyr::select(Longitude, Latitude, step) %>%
          sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
        
        step_colors <- colorFactor("Set2", all_deleted$step)
        
        leafletProxy("map") %>%
          addCircleMarkers(data = all_deleted,
                          radius = 3,
                          fillColor = ~step_colors(step),
                          fillOpacity = 0.7,
                          weight = 1,
                          color = "black",
                          popup = ~paste0("<b>Etape :</b> ", step)) %>%
          addLegend(position = "bottomright",
                   pal = step_colors,
                   values = all_deleted$step,
                   title = "Etape de suppression")
      }
      
      if (input$map_type == "comparison") {
        clean_data <- if (input$polygon) {
          rv$result$data_clean
        } else {
          sf::st_as_sf(rv$result$data_clean,
                       coords = c("Longitude", "Latitude"),
                       crs = 4326)
        }
        
        leafletProxy("map") %>%
          addPolygons(data = clean_data,
                      fillColor = "#3d8a6b",
                      fillOpacity = 0.3,
                      weight = 1,
                      color = "#1f5a43",
                      popup = "Nettoye")
      }
      
      leafletProxy("map") %>%
        setView(lng = center_lng, lat = center_lat, zoom = 15)
    }
  })
  
  output$summary <- renderTable({
    req(rv$processed)
    req(rv$result)

    yield_col <- if (as.logical(input$units)) "Yield_kg_ha" else "Yield_buacre"
    unit_label <- if (as.logical(input$units)) "kg/ha" else "bu/acre"

    data_clean <- rv$result$data_clean
    if (inherits(data_clean, "sf")) {
      data_clean <- sf::st_drop_geometry(data_clean)
    }
    
    data.frame(
      Indicateur = c("Observations brutes", "Observations nettoyees", "Points supprimes",
                     "Taux de retention", "Delai de flux (s)", "Rendement moyen"),
      Value = c(
        rv$result$stats$n_raw,
        rv$result$stats$n_clean,
        rv$result$stats$n_deleted,
        paste0(round(rv$result$stats$retention_rate * 100, 1), "%"),
        round(rv$result$stats$flow_delay, 2),
        paste0(round(mean(data_clean[[yield_col]], na.rm = TRUE), 1), " ", unit_label)
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
    yield_factor <- if (as.logical(input$units)) 67.25 else 1
    yield_unit <- if (as.logical(input$units)) "kg/ha" else "bu/acre"
    cat("Plage de rendement :", round(thr$min_yield * yield_factor, 1), "-",
        round(thr$max_yield * yield_factor, 1), yield_unit, "\n")
    cat("Plage de vitesse :", round(thr$min_velocity, 2), "-",
        round(thr$max_velocity, 2), "m/s\n")
    cat("Delai de flux :", rv$result$stats$flow_delay, "secondes\n")
  })
  
  output$yield_distribution <- renderPlot({
    req(rv$processed)
    req(rv$result)
    
    yield_col <- if (as.logical(input$units)) "Yield_kg_ha" else "Yield_buacre"
    unit_label <- if (as.logical(input$units)) "kg/ha" else "bu/acre"
    
    data <- rv$result$data_clean
    if (inherits(data, "sf")) {
      data <- sf::st_drop_geometry(data)
    }
    
    ggplot(data, aes_string(x = yield_col)) +
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
      metrique = as.logical(input$units)
    )

    if (length(diagnostics) == 0) {
      return(tags$div(class = "diag-card", "Aucune suppression a diagnostiquer."))
    }

    plot_ids <- vapply(names(diagnostics), function(step_name) {
      paste0("diag_", gsub("[^A-Za-z0-9]+", "_", tolower(step_name)))
    }, character(1))

    tags$div(
      class = "diag-grid",
      lapply(seq_along(diagnostics), function(i) {
        tags$div(
          class = "diag-card",
          tags$h4(names(diagnostics)[i]),
          plotOutput(plot_ids[i], height = "240px")
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
      metrique = as.logical(input$units)
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
          plot_data <- diag_data |>
            dplyr::filter(is.finite(valeur)) |>
            dplyr::mutate(
              statut = factor(statut, levels = c("Conserve", "Supprime"))
            )

          removed_count <- sum(plot_data$statut == "Supprime", na.rm = TRUE)
          kept_count <- sum(plot_data$statut == "Conserve", na.rm = TRUE)
          unit_label <- unique(plot_data$unite)

          ggplot(plot_data, aes(x = valeur, fill = statut)) +
            geom_histogram(alpha = 0.7, bins = 35, position = "identity") +
            scale_fill_manual(values = c("Conserve" = "#5b6470", "Supprime" = "#b04a3b")) +
            labs(
              title = "",
              subtitle = paste0("Supprimes : ", removed_count, " | Conserves : ", kept_count),
              x = paste0("Flux (", unit_label, ")"),
              y = "Points"
            ) +
            theme_minimal(base_size = 11) +
            theme(
              legend.position = "top",
              legend.title = element_blank(),
              plot.subtitle = element_text(color = "#5b6470")
            )
        })
      })
    }
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      if (input$download_format == "geojson") {
        paste0("rendement_nettoye_", Sys.Date(), ".geojson")
      } else {
        paste0("rendement_nettoye_", Sys.Date(), ".csv")
      }
    },
    content = function(file) {
      data <- rv$result$data_clean
      if (input$download_format == "geojson" && inherits(data, "sf")) {
        sf::st_write(data, file, driver = "GeoJSON", quiet = TRUE)
      } else {
        if (inherits(data, "sf")) {
          data <- sf::st_drop_geometry(data)
        }
        write.csv(data, file, row.names = FALSE)
      }
    }
  )
  
  output$download_log <- downloadHandler(
    filename = function() {
      paste0("journal_nettoyage_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(rv$result$stats$deletions_by_step, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
