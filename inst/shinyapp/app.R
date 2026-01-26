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
      .sidebar-panel { background-color: #f8f9fa; padding: 15px; border-radius: 5px; }
      .section-title { color: #337ab7; font-weight: bold; margin-top: 15px; border-bottom: 2px solid #337ab7; padding-bottom: 5px; }
      .section-title:first-child { margin-top: 0; }
      .param-label { font-size: 12px; margin-bottom: 2px; }
      .shiny-slider-input { margin-bottom: 8px; }
      #map { border: 1px solid #ddd; border-radius: 4px; }
      .status-bar { background: #e7f3ff; padding: 10px; border-radius: 4px; margin-bottom: 15px; }
    "))
  ),
  
  titlePanel("Yield Data Cleaning Explorer"),
  
  fluidRow(
    column(3,
      div(class = "sidebar-panel",
        div(class = "status-bar",
          textOutput("status")
        ),
        
        h4("1. Upload Data"),
        fileInput("file", "Choose yield data file",
                 accept = c(".txt", ".csv")),
        
        h4("2. Output Settings"),
        radioButtons("units", "Output Units:",
                    c("Metric (kg/ha)" = TRUE,
                      "Imperial (bu/acre)" = FALSE)),
        checkboxInput("polygon", "Create polygons", value = TRUE),
        
        div(class = "section-title", "3. PCDI Parameters"),
        div(class = "param-label", "Delay Range (seconds)"),
        fluidRow(
          column(6, sliderInput("delay_min", "Min:", -50, 0, -25, 1)),
          column(6, sliderInput("delay_max", "Max:", 0, 50, 10, 1))
        ),
        div(class = "param-label", "Iterations"),
        sliderInput("n_iterations", "PCDI Iterations:", 1, 20, 5, 1),
        div(class = "param-label", "Noise Level"),
        sliderInput("noise_level", "Noise:", 0, 0.2, 0.05, 0.01),
        
        div(class = "section-title", "4. Threshold Parameters"),
        div(class = "param-label", "Yield Quantiles"),
        fluidRow(
          column(6, sliderInput("yllim", "Lower:", 0, 0.3, 0.10, 0.01)),
          column(6, sliderInput("yulim", "Upper:", 0.7, 1.0, 0.90, 0.01))
        ),
        div(class = "param-label", "Yield IQR Multiplier"),
        sliderInput("yscale", "Yield IQRx:", 0.5, 2.0, 1.1, 0.1),
        div(class = "param-label", "Velocity Quantiles"),
        fluidRow(
          column(6, sliderInput("vlim", "Lower:", 0, 0.2, 0.05, 0.01)),
          column(6, sliderInput("vulim", "Upper:", 0.8, 1.0, 0.95, 0.01))
        ),
        div(class = "param-label", "Velocity IQR Multiplier"),
        sliderInput("vspace", "Velocity IQRx:", 0.5, 2.0, 1.1, 0.1),
        div(class = "param-label", "Min Velocity (m/s)"),
        sliderInput("minv", "Min Vel:", 0, 2, 0.5, 0.1),
        div(class = "param-label", "Position Buffer (m)"),
        sliderInput("gbuffer", "Buffer:", 0, 500, 100, 10),
        
        div(class = "section-title", "5. Filter Parameters"),
        div(class = "param-label", "Overlap Filter"),
        fluidRow(
          column(6, sliderInput("cellsize", "Cell (m):", 0.1, 1.0, 0.3, 0.05)),
          column(6, sliderInput("overlap_thresh", "Max:", 0.1, 1.0, 0.5, 0.1))
        ),
        div(class = "param-label", "Local SD Filter"),
        fluidRow(
          column(6, sliderInput("nswaths", "Swaths:", 1, 20, 5, 1)),
          column(6, sliderInput("lsd_limit", "Limit (sd):", 1.0, 5.0, 2.4, 0.1))
        ),
        div(class = "param-label", "Min Cells per Grid"),
        sliderInput("min_cells", "Min Cells:", 1, 10, 3, 1),
        div(class = "param-label", "Auto-Detect sd"),
        sliderInput("nstd", "Std Devs:", 1, 5, 3, 0.5),
        
        div(class = "section-title", "6. Map & Download"),
        selectInput("map_type", "Map View:",
                   choices = c("Yield Map" = "yield",
                             "Deleted Points" = "deleted",
                             "Comparison" = "comparison")),
        
        conditionalPanel(
          condition = "input.map_type == 'deleted' || input.map_type == 'comparison'",
          selectInput("filter_step", "Filter Step:",
                     choices = "All Steps", selected = "All Steps")
        ),
        
        radioButtons("download_format", "Download Format:",
                    c("GeoJSON" = "geojson",
                      "CSV" = "csv")),
        downloadButton("download_data", "Download Data", class = "btn-success btn-block"),
        downloadButton("download_log", "Download Log", class = "btn-info btn-block"),
        
        hr(),
        actionButton("reprocess", "Apply Parameters", class = "btn-warning btn-block")
      )
    ),
    
    column(9,
      tabsetPanel(
        tabPanel("Map",
                 leafletOutput("map", height = "650px")),
        tabPanel("Statistics",
                 fluidRow(
                   column(6,
                          h4("Cleaning Summary"),
                          tableOutput("summary")),
                   column(6,
                          h4("Deletions by Step"),
                          DT::dataTableOutput("deletions_table"))
                 ),
                 hr(),
                 fluidRow(
                   column(12,
                          h4("Calculated Thresholds"),
                          verbatimTextOutput("thresholds_display"))
                 )),
        tabPanel("Distribution",
                 plotOutput("yield_distribution", height = "500px"))
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
    list(
      delay_range = input$delay_min:input$delay_max,
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
      
      output$status <- renderText("Processing...")
      
      withProgress(message = "Cleaning data...", value = 0.3, {
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
        
        steps <- c("All Steps", unique(result$deletions$step))
        updateSelectInput(session, "filter_step", choices = steps)
      })
      
      output$status <- renderText({
        paste("Retained:", round(rv$result$stats$retention_rate * 100, 1), "% |",
              "Deleted:", rv$result$stats$n_deleted, "points")
      })
      
    }, error = function(e) {
      output$status <- renderText({
        paste("Error:", e$message)
      })
      showNotification(paste("Error:", e$message), type = "error")
    })
  }
  
  observeEvent(input$file, {
    process_data()
  })
  
  observeEvent(input$reprocess, {
    process_data()
  })
  
  output$status <- renderText({
    if (rv$processed && !is.null(rv$result)) {
      paste("Retained:", round(rv$result$stats$retention_rate * 100, 1), "% |",
            "Deleted:", rv$result$stats$n_deleted, "points")
    } else if (!is.null(input$file)) {
      "Processing..."
    } else {
      "Upload data to begin"
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
      map_data <- if (input$polygon) {
        rv$result$data_clean
      } else {
        sf::st_as_sf(rv$result$data_clean,
                     coords = c("Longitude", "Latitude"),
                     crs = 4326)
      }
      
      leafletProxy("map") %>%
        clearMarkers() %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(data = map_data,
                    fillColor = ~colorNumeric(c("red", "yellow", "green"), Yield_kg_ha)(Yield_kg_ha),
                    fillOpacity = 0.7,
                    weight = 0,
                    popup = ~paste0("Yield: ", round(Yield_kg_ha, 1), " kg/ha")) %>%
        addLegend(position = "bottomright",
                 pal = colorNumeric(c("red", "yellow", "green"), map_data$Yield_kg_ha),
                 values = map_data$Yield_kg_ha,
                 title = "Yield (kg/ha)") %>%
        setView(lng = center_lng, lat = center_lat, zoom = 15)
      
    } else {
      filter_step <- input$filter_step
      
      leafletProxy("map") %>%
        clearMarkers() %>%
        clearShapes() %>%
        clearControls()
      
      if (filter_step != "All Steps") {
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
                            popup = ~paste0("<b>Step:</b> ", step, "<br><b>Reason:</b> ", reason))
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
                          popup = ~paste0("<b>Step:</b> ", step)) %>%
          addLegend(position = "bottomright",
                   pal = step_colors,
                   values = all_deleted$step,
                   title = "Deletion Step")
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
                      fillColor = "green",
                      fillOpacity = 0.3,
                      weight = 1,
                      color = "darkgreen",
                      popup = "Cleaned")
      }
      
      leafletProxy("map") %>%
        setView(lng = center_lng, lat = center_lat, zoom = 15)
    }
  })
  
  output$summary <- renderTable({
    req(rv$processed)
    req(rv$result)
    
    data.frame(
      Metric = c("Raw Observations", "Cleaned Observations", "Deleted Points",
                 "Retention Rate", "Flow Delay (s)", "Mean Yield"),
      Value = c(
        rv$result$stats$n_raw,
        rv$result$stats$n_clean,
        rv$result$stats$n_deleted,
        paste0(round(rv$result$stats$retention_rate * 100, 1), "%"),
        round(rv$result$stats$flow_delay, 2),
        round(mean(rv$result$data_clean$Yield_kg_ha, na.rm = TRUE), 1)
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$deletions_table <- DT::renderDataTable({
    req(rv$processed)
    req(rv$result)
    
    DT::datatable(rv$result$stats$deletions_by_step,
                 colnames = c("Step", "Count"),
                 options = list(pageLength = 10, dom = "t"))
  })
  
  output$thresholds_display <- renderPrint({
    req(rv$processed)
    req(rv$result)
    
    thr <- rv$result$stats$thresholds
    cat("Yield Range:", round(thr$min_yield, 1), "-", round(thr$max_yield, 1), "bu/acre\n")
    cat("Velocity Range:", round(thr$min_velocity, 2), "-", round(thr$max_velocity, 2), "m/s\n")
    cat("Flow Delay:", rv$result$stats$flow_delay, "seconds\n")
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
      geom_histogram(bins = 50, fill = "steelblue", color = "white") +
      labs(title = "Yield Distribution (Cleaned Data)",
           x = paste0("Yield (", unit_label, ")"),
           y = "Frequency") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 16))
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      if (input$download_format == "geojson") {
        paste0("cleaned_yield_", Sys.Date(), ".geojson")
      } else {
        paste0("cleaned_yield_", Sys.Date(), ".csv")
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
      paste0("cleaning_log_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(rv$result$stats$deletions_by_step, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
