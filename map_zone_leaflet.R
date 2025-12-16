



# Install and load the necessary packages

library(leaflet)
library(sf)

# Read your shapefile
# Replace 'path_to_shapefile' with the actual path to your shapefile
map_zones <- st_read("data/conus_mapzones_102611.shp")

# Transform the coordinate system to WGS84
map_zones <- st_transform(map_zones, crs = 4326)

# Create the leaflet map
leaflet(map_zones) %>%
  addTiles() %>%
  addPolygons(
    fillColor = "transparent",  # No fill
    weight = 2,
    opacity = 1,
    color = "black",  # Solid black border
    label = ~ZONE_NUM,  # Labels for ZONE_NUM attribute
    labelOptions = labelOptions(
      noHide = TRUE,  # Always show labels
      direction = "auto",
      textOnly = TRUE,
      style = list(
        "color" = "black",
        "font-weight" = "bold",
        "font-size" = "12px"
      )
    ),
    popup = ~paste("Map Zone:", ZONE_NUM)  # Popups for ZONE_NUM attribute
  )




# Replace 'path_to_map_zones_shapefile' and 'path_to_additional_shapefile' with the actual paths to your shapefiles
map_zones <- st_read("data/conus_mapzones_102611.shp")
additional_shape <- st_read("data/ely_blm.shp")


map_zones <- st_transform(map_zones, crs = 4326)
additional_shape <- st_transform(additional_shape, crs = 4326)


# Create the tmap


tm_shape(map_zones) +
  tm_borders(col = "black", lwd = 2) +
  tm_text("ZONE_NUM", size = 1, col = "black", fontface = "bold") +
  tm_shape(additional_shape) +
  tm_borders(col = "green", lwd = 2) +
  tm_fill(fill = "green", fill_alpha = 0.5) +
  tm_view(bbox = st_bbox(additional_shape)) +
  tm_layout(legend.show = FALSE)














