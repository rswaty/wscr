


library(sf)
library(leaflet)
library(RColorBrewer)
library(jsonlite)

# --- BPS palette (categorical) ---
base_palette <- RColorBrewer::brewer.pal(12, "Paired")
bps_names <- sort(unique(hexs$bps_name))
bps_colors <- colorRampPalette(base_palette)(length(bps_names))
pal_bps <- colorFactor(palette = bps_colors, domain = bps_names)

# --- FRI palette (continuous YlGn) with robust domain handling (Option B: inline handling of -9999) ---
no_data_color <- "#4D4D4D"

fri_vals  <- hexs$fri_allfir
fri_valid <- fri_vals[fri_vals != -9999 & !is.na(fri_vals)]

# Safe domain (handles empty, flat, or infinite cases)
if (length(fri_valid) == 0) {
  fri_domain <- c(0, 1)
} else {
  r <- range(fri_valid, na.rm = TRUE)
  if (!is.finite(r[1]) || !is.finite(r[2])) {
    fri_domain <- c(0, 1)
  } else if (r[1] == r[2]) {
    eps <- max(1, abs(r[1]) * 0.01)
    fri_domain <- c(r[1] - eps, r[2] + eps)
  } else {
    fri_domain <- r
  }
}

pal_fri <- colorNumeric(
  palette  = "YlGn",        # Yellow→Green
  domain   = fri_domain,
  na.color = no_data_color
)

# --- Fit to data extent ---
bbox <- st_bbox(hexs)
bbox_list <- as.list(bbox)
json_output <- toJSON(bbox_list, keep_vec_names = TRUE)  # if you need this elsewhere

# --- Map with two overlay groups + layer control ---
leaflet(hexs) %>%
  addTiles() %>%
  fitBounds(
    lng1 = bbox_list$xmin, lat1 = bbox_list$ymin,
    lng2 = bbox_list$xmax, lat2 = bbox_list$ymax
  ) %>%
  
  # Layer 1: BPS polygons (categorical)
  addPolygons(
    fillColor = ~pal_bps(bps_name),
    color = "#BDBDC3",
    weight = 1,
    opacity = 1,
    fillOpacity = 1.0,
    highlightOptions = highlightOptions(
      weight = 2, color = "#666", fillOpacity = 1.0, bringToFront = TRUE
    ),
    label = ~bps_name,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px", direction = "auto"
    ),
    group = "BPS"
  ) %>%
  
  # Layer 2: FRI polygons (continuous YlGn; -9999/NA -> dark gray) 
  addPolygons(
    fillColor = ~ifelse(fri_allfir == -9999 | is.na(fri_allfir),
                        no_data_color, pal_fri(fri_allfir)),
    color = "#BDBDC3",
    weight = 1,
    opacity = 1,
    fillOpacity = 1.0,
    highlightOptions = highlightOptions(
      weight = 2, color = "#666", fillOpacity = 1.0, bringToFront = TRUE
    ),
    label = ~paste0(
      "FRI (years): ",
      ifelse(fri_allfir == -9999 | is.na(fri_allfir), "No Data", fri_allfir)
    ),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px", direction = "auto"
    ),
    group = "FRI"
  ) %>%
  
  # Layer control (checkboxes to toggle BPS/FRI)
  addLayersControl(
    overlayGroups = c("BPS", "FRI"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  # Start with FRI hidden (optional)
  hideGroup("FRI") 



%>%
  
  # Legends (FRI shows 'No Data' for -9999 via na.label)
  addLegend(
    position = "bottomright",
    pal =    pal = pal_fri,
    values = ifelse(fri_vals == -9999, NA, fri_vals),
    title = "FRI (years)",
    na.label = "No Data",
    opacity = 0.8
  ) %>%
  addLegend(
    position = "bottomleft",
    pal = pal_bps,
    values = bps_names,
    title = "BPS",
    opacity = 0.8
    