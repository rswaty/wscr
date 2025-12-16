

# Notes ----
## LANDFIRE data GIS Processing
## downloads data; makes hex shapefile for leaflet map
## run this first
## code by Randy Swaty
## December 15, 2025

# Set up ----

## load packages

library(exactextractr)
library(janitor)
library(raster)
library(rlandfire)
library(sf)
library(terra)
library(tidyverse)


# load landscape name and shape

landscape_name <- "Western Shawnee/Cache River"

shp <- st_read("inputs/wscr_tnc.shp") %>% 
  st_transform(crs = 5070) %>%
  st_union() %>%
  st_sf()

vect(shp)
plot(shp)

# load conus-wide LANDFIRE attribute tables for joining later

bps_url <- "https://landfire.gov/sites/default/files/CSV/LF2016/LF16_BPS.csv" # will likely get warning, but it's OK
bps_conus_atts <- read.csv(bps_url)

evt_url <- "https://landfire.gov/sites/default/files/CSV/2024/LF2024_EVT.csv" # will likely get warning, but it's OK
evt_conus_atts <- read.csv(evt_url)

scl_url <- "https://landfire.gov/sites/default/files/CSV/2024/LF2024_SClass.csv"
scl_conus_atts <- read.csv(scl_url)


# non-accessible versions from LANDFIRE-note use versions from Sarah Hagen for maps and charts

evc_url <- "https://landfire.gov/sites/default/files/CSV/2024/LF2024_EVC.csv"
evc_conus_atts <- read.csv(evc_url)

evh_url <- "https://landfire.gov/sites/default/files/CSV/2024/LF2024_EVH.csv"
evh_conus_atts <- read.csv(evh_url)


# Download LANDFIRE data for Area of Interest (AoI), manage that data ----

aoi <- getAOI(shp)

products <-  c("200BPS", "250SCLASS", "250EVC", "250EVH", "250EVT")  
projection <- 5070  
resolution <- 30
email <- "rswaty@tnc.org" # REPLACE WITH YOUR E-MAIL ADDRESS PLEASE! 

# R specific arguments
save_file <- tempfile(fileext = ".zip")

# call API
ncal <- landfireAPIv2(
  products, 
  aoi, 
  projection, 
  resolution, 
  path = save_file,
  email = email)


# define the destination path
dest_file <- file.path("inputs", "landfire_data.zip")

# move and rename the file
file.rename(save_file, dest_file)

# create a temporary directory for unzipping
temp_dir <- tempfile()
dir.create(temp_dir)

# unzip the file into the temporary directory
unzip(dest_file, exdir = temp_dir)

# get the list of unzipped files
unzipped_files <- list.files(temp_dir, full.names = TRUE)

# rename each unzipped file to "landfire_data" with its full original extension
for (file in unzipped_files) {
  file_name <- basename(file)
  file_extension <- sub("^[^.]*", "", file_name)  # Extract the full extension
  new_file_path <- file.path("inputs", paste0("landfire_data", file_extension))
  file.rename(file, new_file_path)
}

# clean up the temporary directory
unlink(temp_dir, recursive = TRUE)


# Process datasets ----

# load in downloaded LANDFIRE tif
stacked_rasters <- rast("inputs/landfire_data.tif")

# "split" downloaded raster into separate layers
for(lyr in names(stacked_rasters)) assign(lyr, stacked_rasters[[lyr]])


## Create hexgrids for BpS, EVT, EVC and EVH ----


# --- parameters ---
acres_target <- 10000
m2_per_acre <- 4046.8564224

# compute hex side length from area
A_m2 <- acres_target * m2_per_acre             # target area in m^2
hex_side <- sqrt((2 * A_m2) / (3 * sqrt(3)))   # side length s (meters)

# spacing for flat-topped hex grid (st_make_grid default for square = FALSE)
cellsize_x <- 1.5 * hex_side        # center-to-center in X
cellsize_y <- sqrt(3) * hex_side    # center-to-center in Y

# build hex grid covering the bbox of shp 
hex_grid <- st_make_grid(
  shp,
  cellsize = c(cellsize_x, cellsize_y),
  square = FALSE,        # hexagons
  what = "polygons") %>%
  st_sf(crs = st_crs(shp))  # preserve CRS

# clip to shp and add an index 
hex_grid_clipped <- st_intersection(hex_grid, shp) %>%
  mutate(index = dplyr::row_number(),
         acres_est = as.numeric(st_area(.)) / m2_per_acre)

# quick check: mean area in acres
mean_area_acres <- mean(hex_grid_clipped$acres_est, na.rm = TRUE)
cat(sprintf("Mean clipped hex area: %.1f acres\n", mean_area_acres))

# plot 
plot(st_geometry(hex_grid_clipped), col = NA, border = "gray40")
plot(st_geometry(shp), add = TRUE, border = "red", lwd = 2)

## Extract values and add them to hexgrid ----

# bps
bps_majority_hex <- exact_extract(US_200BPS, hex_grid_clipped, 'majority', append_cols = "index") %>%
  left_join(dplyr::select(bps_conus_atts,
                   VALUE,
                   BPS_MODEL,
                   BPS_NAME,
                   FRI_ALLFIR),
            by = c('majority' = 'VALUE')) %>%
  rename(bps_value = majority) |>
  clean_names()

# evt
evt_majority_hex <- exact_extract(US_250EVT, hex_grid_clipped, 'majority', append_cols = "index") %>%
  left_join(dplyr::select(evt_conus_atts,
                   VALUE,
                   EVT_NAME,
                   EVT_PHYS),
            by = c('majority' = 'VALUE')) |>
  rename(evt_value = majority) |>
  clean_names()

# evc
evc_majority_hex <- exact_extract(US_250EVC, hex_grid_clipped, 'majority', append_cols = "index") %>%
  left_join(dplyr::select(evc_conus_atts,
                          VALUE,
                          CLASSNAMES),
            by = c('majority' = 'VALUE')) |>
  rename(evc_value = majority,
         evc_labels = CLASSNAMES) |>
  clean_names()

# evh
evh_majority_hex <- exact_extract(US_250EVH, hex_grid_clipped, 'majority', append_cols = "index") %>%
  left_join(dplyr::select(evh_conus_atts,
                          VALUE,
                          CLASSNAMES),
            by = c('majority' = 'VALUE')) |>
  rename(evh_value = majority,
         evh_labels = CLASSNAMES) |>
  clean_names()



# Join both BpS and EVT attributes to hex shapefile
hexs_bps_evt_evc_evh <- hex_grid_clipped %>%
  left_join(bps_majority_hex, by = 'index') %>%
  left_join(evt_majority_hex, by = 'index') %>%
  left_join(evc_majority_hex, by = 'index') %>%
  left_join(evh_majority_hex, by = 'index') |>
  clean_names()


# save the shapefile for mapping in non-R applications or to read back into R

st_write(hexs_bps_evt_evc_evh, "outputs/bps_evt_evc_evh_hexs.shp")















