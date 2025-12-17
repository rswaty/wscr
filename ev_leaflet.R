

evc_ramp <- read_csv('inputs/LF22_EVC_230_acc.csv')
evh_ramp <- read_csv('inputs/LF22_EVH_230_acc.csv')



# Libraries
library(sf)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(jsonlite)

# -------------------------
# 1) Palettes & lookups
# -------------------------

# No-data color (override ramp's -9999 if you prefer dark gray)
no_data_color <- "#4D4D4D"

# EVT palette (categorical, like your BPS approach)
base_palette <- RColorBrewer::brewer.pal(12, "Paired")
evt_names    <- sort(unique(hexs$evt_name))
evt_colors   <- colorRampPalette(base_palette)(length(evt_names))
pal_evt      <- colorFactor(palette = evt_colors, domain = evt_names)

# Helper: convert a ramp table (evc_ramp / evh_ramp) to a clean lookup
# Uses 0–255 R,G,B columns to make hex; replaces -9999 with your no_data_color

to_hex_lut <- function(ramp_tbl, no_data_hex = no_data_color) {
  ramp_tbl %>%
    dplyr::mutate(
      Value = as.integer(Value),
      hex   = rgb(R, G, B, maxColorValue = 255),
      hex   = ifelse(Value == -9999, no_data_hex, hex)
    ) %>%
    dplyr::select(Value, CLASSNAMES, hex)
}

evc_lut <- to_hex_lut(evc_ramp)
evh_lut <- to_hex_lut(evh_ramp)


# -------------------------
# 2) Attach hex colors by value to the sf
# -------------------------
hexs2 <- hexs %>%
  mutate(
    evc_value = as.integer(evc_value),
    evh_value = as.integer(evh_value)
  ) %>%
  # join brings in the hex for each code
  left_join(
    evc_lut %>% rename(evc_value = Value, evc_hex = hex, evc_class = CLASSNAMES),
    by = "evc_value"
  ) %>%
  left_join(
    evh_lut %>% rename(evh_value = Value, evh_hex = hex, evh_class = CLASSNAMES),
    by = "evh_value"
  )

# Optional sanity checks (helpful during development)
evc_missing <- setdiff(unique(hexs2$evc_value), evc_lut$Value)
evh_missing <- setdiff(unique(hexs2$evh_value), evh_lut$Value)
if (length(evc_missing) > 0) message("EVC values without colors: ", paste(evc_missing, collapse = ", "))
if (length(evh_missing) > 0) message("EVH values without colors: ", paste(evh_missing, collapse = ", "))

# -------------------------
# 3) Fit to extent
# -------------------------
bbox <- sf::st_bbox(hexs2)
bbox_list <- as.list(bbox)
# json_output <- toJSON(bbox_list, keep_vec_names = TRUE)  # if you need elsewhere

# -------------------------
# 4) Leaflet map with 3 overlay groups
# -------------------------
leaflet(hexs2) %>%
  addTiles() %>%
  fitBounds(
    lng1 = bbox_list$xmin, lat1 = bbox_list$ymin,
    lng2 = bbox_list$xmax, lat2 = bbox_list$ymax
  ) %>%
  
  # Layer A: EVT (Existing Vegetation Type) - categorical palette
  addPolygons(
    fillColor = ~pal_evt(evt_name),
    color = "#BDBDC3",
    weight = 1,
    opacity = 1,
    fillOpacity = 1.0,
    highlightOptions = highlightOptions(
      weight = 2, color = "#666", fillOpacity = 1.0, bringToFront = TRUE
    ),
    label = ~evt_name,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px", direction = "auto"
    ),
    group = "Existing Vegetation Type"
  ) %>%
  
  # Layer B: EVC (Existing Vegetation Cover) - colors from evc_ramp via evc_value
  addPolygons(
    fillColor = ~ifelse(is.na(evc_value) | evc_value == -9999, no_data_color, evc_hex),
    color = "#BDBDC3",
    weight = 1,
    opacity = 1,
    fillOpacity = 1.0,
    highlightOptions = highlightOptions(
      weight = 2, color = "#666", fillOpacity = 1.0, bringToFront = TRUE
    ),
    label = ~paste0(
      "Cover: ", evc_labels
    ),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px", direction = "auto"
    ),
    group = "Existing Vegetation Cover"
  ) %>%
  
  # Layer C: EVH (Existing Vegetation Height) - colors from evh_ramp via evh_value
  addPolygons(
    fillColor = ~ifelse(is.na(evh_value) | evh_value == -9999, no_data_color, evh_hex),
    color = "#BDBDC3",
    weight = 1,
    opacity = 1,
    fillOpacity = 1.0,
    highlightOptions = highlightOptions(
      weight = 2, color = "#666", fillOpacity = 1.0, bringToFront = TRUE
    ),
    label = ~paste0(
      "Height: ", evh_labels
    ),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px", direction = "auto"
    ),
    group = "Existing Vegetation Height"
  ) %>%
  
  # Layer control
  addLayersControl(
    overlayGroups = c("Existing Vegetation Type",
                      "Existing Vegetation Cover",
                      "Existing Vegetation Height"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  # Optionally start with EVC/EVH hidden

  # Start with EVC/EVH hidden
  hideGroup("Existing Vegetation Cover") %>%
  hideGroup("Existing Vegetation Height") %>%
  
  # ✅ Only an EVT legend, tied to the EVT group
  addLegend(
    position = "bottomright",
    pal      = pal_evt,
    values   = evt_names,        # categorical legend entries
    title    = "Existing Vegetation Type (EVT)",
    opacity  = 1,
    group    = "Existing Vegetation Type"  # legend shows only when this group is on
  )
  