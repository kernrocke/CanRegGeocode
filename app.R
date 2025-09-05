
library(shiny)
library(tidygeocoder)
library(dplyr)
library(readr)
library(leaflet)
library(stringr)
library(sf)

ui <- fluidPage(
  titlePanel("Geocode and Map Addresses in Barbados with OSM and Shapefile"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File (Max 5000 addresses)",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      fileInput("shapefile", "Upload Barbados Road Network Shapefile (.shp)",
                accept = c(".shp"), multiple = TRUE),
      textInput("address_col", "Address Column Name", value = "address"),
      actionButton("process", "Geocode Addresses"),
      br(), br(),
      downloadButton("download", "Download Processed CSV"),
      br(), br(),
      textOutput("status"),
      br(),
      h4("Failed Addresses"),
      tableOutput("failed_preview")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview",
                 h4("Preview of Processed Data (First 6 Rows)"),
                 tableOutput("preview")),
        tabPanel("Map",
                 h4("Map of Geocoded Addresses"),
                 leafletOutput("map", height = 600))
      ),
      h4("Geocoding Progress"),
      uiOutput("progress")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive value to store the uploaded data
  uploaded_data <- reactiveVal(NULL)
  
  # Reactive value to store shapefile
  shapefile_data <- reactiveVal(NULL)
  
  # Reactive value to store progress percentage
  progress_value <- reactiveVal(0)
  
  # Reactive value to store failed addresses
  failed_data <- reactiveVal(NULL)
  
  # Function to clean and extract components from address
  extract_components <- function(address) {
    # Extract potential house number: ^(\d+[A-Za-z]?|# \d+|NO \d+|LOT \d+)
    house_number <- str_extract(address, "^(\\d+[A-Za-z]?|#\\d+|NO \\d+|LOT \\d+)")
    house_number <- if (is.na(house_number) || str_detect(house_number, "9999|8888")) NA_character_ else trimws(house_number)
    
    # Clean address: remove house number prefix and ambiguous "99"
    cleaned <- str_remove(address, "^(\\d+[A-Za-z]?|#\\d+|NO \\d+|LOT \\d+)\\s*")
    cleaned <- str_remove(cleaned, "\\s+99\\s+")
    cleaned <- trimws(cleaned)
    
    # Extract parish
    parish <- str_extract(cleaned, "(St\\.\\s*[A-Za-z]+|Christ Church)")
    parish <- if (!is.na(parish)) paste0(parish, ", Barbados") else NA_character_
    
    # Extract street: the rest before parish or end
    street <- str_extract(cleaned, "^[^,]+?(?=\\s*(,|St\\.\\s*[A-Za-z]+|Christ Church|$))")
    street <- trimws(street)
    
    # Full address for geocoding: house_number + street + parish if available
    full_address <- if (!is.na(house_number)) paste(house_number, street) else street
    full_address <- paste0(full_address, if (!is.na(parish)) paste0(", ", parish) else ", Barbados")
    
    # Street for midpoint: street + parish
    street_midpoint <- if (!is.na(street)) paste0(street, if (!is.na(parish)) paste0(", ", parish) else ", Barbados") else NA_character_
    
    list(full_address = full_address, street_midpoint = street_midpoint, parish = parish, house_number = house_number)
  }
  
  # Function to interpolate along street geometry
  interpolate_along_street <- function(street_name, house_number, parish, roads_sf) {
    # Normalize street name for matching
    street_name <- toupper(trimws(street_name))
    roads_sf <- roads_sf %>% mutate(street_name = toupper(street_name))
    
    # Filter roads by street name and optionally parish
    matched_roads <- roads_sf %>% filter(street_name == !!street_name)
    if (!is.na(parish)) {
      parish_clean <- str_remove(parish, ", Barbados")
      matched_roads <- matched_roads %>% filter(parish == parish_clean | is.na(parish))
    }
    
    if (nrow(matched_roads) == 0) {
      message("No matching street found in shapefile: ", street_name)
      return(data.frame(latitude = NA, longitude = NA, source = "failed"))
    }
    
    # Get the first matching street geometry (LINESTRING)
    road_geometry <- matched_roads$geometry[1]
    
    # Assume house_number is numeric for interpolation (strip letters)
    house_num <- as.numeric(gsub("[A-Za-z]", "", house_number))
    if (is.na(house_num)) {
      # If no valid house number, return midpoint
      coords <- st_coordinates(st_centroid(road_geometry))
      return(data.frame(latitude = coords[2], longitude = coords[1], source = "shapefile_midpoint"))
    }
    
    # Assume house number range (e.g., 1 to 100) if available
    # Replace 'from_number' and 'to_number' with your shapefile's column names
    if ("from_number" %in% names(matched_roads) && "to_number" %in% names(matched_roads)) {
      from_num <- matched_roads$from_number[1]
      to_num <- matched_roads$to_number[1]
      if (is.na(from_num) || is.na(to_num)) {
        coords <- st_coordinates(st_centroid(road_geometry))
        return(data.frame(latitude = coords[2], longitude = coords[1], source = "shapefile_midpoint"))
      }
      
      # Interpolate position along street
      fraction <- (house_num - from_num) / (to_num - from_num)
      fraction <- pmax(0, pmin(1, fraction))  # Clamp to [0,1]
      point <- st_line_sample(road_geometry, sample = fraction)
      coords <- st_coordinates(point)
      return(data.frame(latitude = coords[2], longitude = coords[1], source = "shapefile_interpolated"))
    } else {
      # Fallback to midpoint if no range data
      coords <- st_coordinates(st_centroid(road_geometry))
      return(data.frame(latitude = coords[2], longitude = coords[1], source = "shapefile_midpoint"))
    }
  }
  
  # Read the uploaded CSV
  observeEvent(input$file, {
    req(input$file)
    df <- read_csv(input$file$datapath, show_col_types = FALSE)
    
    if (nrow(df) > 5000) {
      showNotification("File exceeds 5000 addresses. Please upload a smaller file.", type = "error")
      return(NULL)
    }
    
    uploaded_data(df)
    progress_value(0)
    failed_data(NULL)
  })
  
  # Read the uploaded shapefile
  observeEvent(input$shapefile, {
    req(input$shapefile)
    tryCatch({
      shp_path <- input$shapefile$datapath[grepl("\\.shp$", input$shapefile$name)]
      roads_sf <- st_read(shp_path, quiet = TRUE)
      shapefile_data(roads_sf)
      showNotification("Shapefile loaded successfully.", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading shapefile:", e$message), type = "error")
      shapefile_data(NULL)
    })
  })
  
  # Render progress bar
  output$progress <- renderUI({
    value <- progress_value()
    tagList(
      div(style = "margin-top: 20px;",
          div(style = "width: 100%; background-color: #f0f0f0; border-radius: 5px; overflow: hidden;",
              div(style = sprintf("width: %s%%; height: 20px; background-color: #4caf50; text-align: center; color: white;", value),
                  sprintf("%.1f%%", value)
              )
          ),
          p(sprintf("Geocoding addresses... (%.1f%% complete)", value))
      )
    )
  })
  
  # Reactive for geocoded data
  geocoded_data <- eventReactive(input$process, {
    req(uploaded_data())
    df <- uploaded_data()
    address_col <- input$address_col
    
    if (!(address_col %in% names(df))) {
      showNotification("Specified address column not found in CSV.", type = "error")
      return(NULL)
    }
    
    roads_sf <- shapefile_data()
    if (is.null(roads_sf)) {
      showNotification("No shapefile loaded. Falling back to Nominatim only.", type = "warning")
    }
    
    progress_value(0)
    batch_size <- 50
    geocoded_list <- list()
    failed_addresses <- list()
    total_rows <- nrow(df)
    
    for (i in seq(1, total_rows, by = batch_size)) {
      end_idx <- min(i + batch_size - 1, total_rows)
      batch <- df[i:end_idx, ]
      
      new_progress <- ((i - 1) / total_rows) * 100
      progress_value(new_progress)
      message(sprintf("Updating UI progress to %.1f%% for batch %s-%s (%s addresses)", 
                      new_progress, i, end_idx, end_idx - i + 1))
      
      cleaned_batch <- batch %>%
        mutate(full_address = sapply(.data[[address_col]], function(x) extract_components(x)$full_address),
               street_midpoint = sapply(.data[[address_col]], function(x) extract_components(x)$street_midpoint),
               parish = sapply(.data[[address_col]], function(x) extract_components(x)$parish),
               house_number = sapply(.data[[address_col]], function(x) extract_components(x)$house_number))
      
      message("Batch ", i, "-", end_idx, " components:")
      print(cleaned_batch %>% select(.data[[address_col]], full_address, street_midpoint, parish, house_number))
      
      # Geocode using Nominatim (full_address)
      result <- tryCatch({
        geo(
          address = cleaned_batch$full_address,
          method = "osm",
          lat = "latitude",
          long = "longitude",
          limit = 1
        ) %>% 
          mutate(address = cleaned_batch[[address_col]], 
                 source = "full_address",
                 house_number = cleaned_batch$house_number,
                 street_midpoint = cleaned_batch$street_midpoint,
                 parish = cleaned_batch$parish)
      }, error = function(e) {
        showNotification(paste("Error in batch", i, "-", end_idx, ":", e$message), type = "error")
        return(NULL)
      })
      
      if (!is.null(result)) {
        # Identify failed or parish-center geocodes
        failed <- result %>%
          filter(is.na(latitude) | is.na(longitude) | source == "parish_center") %>%
          select(address, street_midpoint, parish, house_number)
        
        if (nrow(failed) > 0) {
          failed_addresses[[length(failed_addresses) + 1]] <- failed
          
          # Attempt street-level geocoding with Nominatim
          failed_streets <- failed %>%
            filter(!is.na(street_midpoint))
          
          if (nrow(failed_streets) > 0) {
            message("Attempting street geocoding for: ", paste(failed_streets$street_midpoint, collapse = ", "))
            street_result <- tryCatch({
              geo(
                address = failed_streets$street_midpoint,
                method = "osm",
                lat = "latitude",
                long = "longitude",
                limit = 1
              ) %>% 
                mutate(address = failed_streets$address, 
                       source = "street_midpoint",
                       house_number = failed_streets$house_number,
                       street_midpoint = failed_streets$street_midpoint,
                       parish = failed_streets$parish)
            }, error = function(e) {
              showNotification(paste("Error geocoding streets for batch", i, "-", end_idx, ":", e$message), type = "error")
              return(NULL)
            })
            
            if (!is.null(street_result)) {
              result <- result %>%
                filter(!is.na(latitude) & !is.na(longitude) & source != "parish_center") %>%
                bind_rows(street_result)
            }
          }
          
          # Attempt shapefile interpolation for remaining failures or parish centers
          if (!is.null(roads_sf)) {
            failed_for_shapefile <- result %>%
              filter(is.na(latitude) | is.na(longitude) | source == "parish_center") %>%
              select(address, street_midpoint, parish, house_number)
            
            if (nrow(failed_for_shapefile) > 0) {
              message("Attempting shapefile interpolation for: ", paste(failed_for_shapefile$street_midpoint, collapse = ", "))
              shapefile_result <- lapply(1:nrow(failed_for_shapefile), function(j) {
                street <- str_remove(failed_for_shapefile$street_midpoint[j], ",.*$")  # Remove parish suffix
                house_num <- failed_for_shapefile$house_number[j]
                parish <- failed_for_shapefile$parish[j]
                interp_result <- interpolate_along_street(street, house_num, parish, roads_sf)
                interp_result$address <- failed_for_shapefile$address[j]
                interp_result$house_number <- house_num
                interp_result$street_midpoint <- failed_for_shapefile$street_midpoint[j]
                interp_result$parish <- parish
                interp_result
              }) %>% bind_rows()
              
              result <- result %>%
                filter(!is.na(latitude) & !is.na(longitude) & source != "parish_center") %>%
                bind_rows(shapefile_result)
            }
          }
          
          # Attempt parish-level geocoding for remaining failures
          failed_parishes <- result %>%
            filter(is.na(latitude) | is.na(longitude)) %>%
            select(address, street_midpoint, parish, house_number) %>%
            filter(!is.na(parish))
          
          if (nrow(failed_parishes) > 0) {
            message("Attempting parish geocoding for: ", paste(failed_parishes$parish, collapse = ", "))
            parish_result <- tryCatch({
              geo(
                address = failed_parishes$parish,
                method = "osm",
                lat = "latitude",
                long = "longitude",
                limit = 1
              ) %>% 
                mutate(address = failed_parishes$address, 
                       source = "parish_center",
                       house_number = failed_parishes$house_number,
                       street_midpoint = failed_parishes$street_midpoint,
                       parish = failed_parishes$parish)
            }, error = function(e) {
              showNotification(paste("Error geocoding parishes for batch", i, "-", end_idx, ":", e$message), type = "error")
              return(NULL)
            })
            
            if (!is.null(parish_result)) {
              result <- result %>%
                filter(!is.na(latitude) & !is.na(longitude)) %>%
                bind_rows(parish_result)
            }
          }
        }
        
        geocoded_list[[length(geocoded_list) + 1]] <- result
      }
      
      Sys.sleep(1)  # Respect Nominatim's rate limit
    }
    
    progress_value(100)
    message("Geocoding complete. UI progress set to 100%.")
    
    geocoded <- bind_rows(geocoded_list)
    
    final_data <- df %>%
      left_join(geocoded, by = setNames("address", address_col)) %>%
      select(pid, address, latitude, longitude, source) %>%
      mutate(source = ifelse(is.na(source), "failed", source))
    
    failed_df <- final_data %>% filter(source == "failed")
    failed_data(failed_df)
    
    if (nrow(failed_df) > 0) {
      message("Failed to geocode ", nrow(failed_df), " addresses (after shapefile and parish attempts):")
      failed_details <- failed_df %>%
        mutate(full_address = sapply(address, function(x) extract_components(x)$full_address),
               street_attempt = sapply(address, function(x) extract_components(x)$street_midpoint),
               parish_attempt = sapply(address, function(x) extract_components(x)$parish))
      print(failed_details %>% select(address, full_address, street_attempt, parish_attempt))
      showNotification(paste(nrow(failed_df), "addresses failed all geocoding attempts. See console and Failed Addresses table."), type = "warning")
    }
    
    final_data
  })
  
  output$status <- renderText({
    req(geocoded_data())
    paste("Geocoding complete. Processed", nrow(geocoded_data()), "addresses.", 
          sum(geocoded_data()$source == "failed"), "failed all geocoding attempts.")
  })
  
  output$preview <- renderTable({
    req(geocoded_data())
    head(geocoded_data())
  })
  
  output$failed_preview <- renderTable({
    req(failed_data())
    head(failed_data() %>% select(address))
  })
  
  output$map <- renderLeaflet({
    req(geocoded_data())
    data <- geocoded_data() %>%
      filter(!is.na(latitude) & !is.na(longitude))
    
    if (nrow(data) == 0) {
      showNotification("No valid coordinates to display on the map.", type = "warning")
      return(leaflet() %>% addTiles())
    }
    
    red_flag_icon <- makeIcon(
      iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png",
      iconWidth = 25,
      iconHeight = 41,
      iconAnchorX = 12,
      iconAnchorY = 41
    )
    
    leaflet(data) %>%
      addTiles() %>%
      setView(lng = -59.55, lat = 13.15, zoom = 10) %>%
      addMarkers(
        lng = ~longitude,
        lat = ~latitude,
        popup = ~paste("PID:", pid, "<br>Address:", address, "<br>Source:", 
                       case_when(
                         source == "full_address" ~ "Full Address",
                         source == "street_midpoint" ~ "Street Midpoint",
                         source == "shapefile_interpolated" ~ "Shapefile Interpolated",
                         source == "shapefile_midpoint" ~ "Shapefile Midpoint",
                         source == "parish_center" ~ "Parish Center"
                       )),
        icon = red_flag_icon
      )
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("geocoded_", tools::file_path_sans_ext(input$file$name), ".csv", sep = "")
    },
    content = function(file) {
      req(geocoded_data())
      write_csv(geocoded_data(), file)
    }
  )
}

shinyApp(ui = ui, server = server)
