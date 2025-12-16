

# Notes ----
## LANDFIRE data GIS Processing
## takes downloaded data, clips, makes attribute tables, write GIS files
## run this second
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

# load attributes for VDEP calculations
ref_con <- read_csv("inputs/ref_con_long.csv")
bps_names <- read_csv("inputs/bps_model_number_name.csv")

## Clip and Make Attribute Tables ----

## BpS ----

bps_aoi <- US_200BPS %>%
  crop(shp) %>%
  mask(shp)

# plot(bps_aoi)

levels(bps_aoi)[[1]] <- bps_conus_atts
activeCat(bps_aoi) <- "VALUE"

bps_aoi_atts <- values(bps_aoi, dataframe = T, na.rm = T) %>%
  table(dnn = "VALUE") %>%
  as.data.frame() %>%
  mutate_all(as.character) %>%
  mutate_all(as.integer) %>%
  left_join(cats(bps_aoi)[[1]], by = "VALUE") %>%
  filter(Freq != 0) %>%
  mutate(ACRES = round((Freq * 900 / 4046.86), 0),
         REL_PERCENT = round((Freq / sum(Freq)), 3) * 100) %>%
  arrange(desc(REL_PERCENT))

# write
writeRaster(bps_aoi, "outputs/bps_aoi.tif",
            gdal = c("COMPRESS=NONE", "TFW=YES"),
            datatype = "INT2S",
            overwrite = T)

write.dbf(bps_aoi_atts, "outputs/bps_aoi.tif.vat.dbf")

write.csv(bps_aoi_atts, "outputs/bps_aoi_attributes.csv")

## color file for charts and map legend 

summary_bps_name <- bps_aoi_atts %>%
  subset(BPS_NAME != "Open Water" & BPS_NAME != "Barren-Rock/Sand/Clay") %>%
  group_by(BPS_NAME) %>%
  summarise(bps_name_totals = sum(REL_PERCENT)) %>%
  ungroup()

top_groups <- summary_bps_name %>%
  filter(bps_name_totals >= 1)

filtered_bps_name_groups <- bps_aoi_atts %>%
  filter(BPS_NAME %in% top_groups$BPS_NAME)

bps_color_file <- filtered_bps_name_groups %>%
  add_column(z = 255) %>%
  dplyr::select(
    VALUE,
    R,
    G,
    B,
    z,
    BPS_NAME)  %>%
  arrange(BPS_NAME) 

## way to remove geographies to shorten names

# geographies <- c(
#   "Boreal ",
#   "Central Interior and Appalachian ",
#   "Great Lakes ",
#   "Laurentian ",
#   "Laurentian-Acadian ",
#   "North-Central Interior ")

# bps_color_file$BPS_NAME <- gsub(paste(geographies, collapse = "|"), "", BpSColorFile$BPS_NAME)

# write for use in QGIS
write.table(bps_color_file, file = "outputs/bps_color_file.txt", sep = ",",
            row.names = FALSE, col.names = FALSE, quote = FALSE)


## EVT ----

evt_aoi <- US_250EVT %>%
  crop(shp) %>%
  mask(shp)

levels(evt_aoi)[[1]] <- evt_conus_atts
activeCat(evt_aoi) <- "VALUE"

evt_aoi_atts <- values(evt_aoi, dataframe = T, na.rm = T) %>%
  table(dnn = "VALUE") %>%
  as.data.frame() %>%
  mutate_all(as.character) %>%
  mutate_all(as.integer) %>%
  left_join(cats(evt_aoi)[[1]], by = "VALUE") %>%
  filter(Freq != 0) %>%
  mutate(ACRES = round((Freq * 900 / 4046.86), 0),
         REL_PERCENT = round((Freq / sum(Freq)), 3) * 100) 

# write outputs
writeRaster(evt_aoi, "outputs/evt_aoi.tif",
            gdal = c("COMPRESS=NONE", "TFW=YES"),
            datatype = "INT2S",
            overwrite = TRUE)
write.dbf(evt_aoi_atts, "outputs/evt_aoi.tif.vat.dbf")

write.csv(evt_aoi_atts, "outputs/evt_aoi_attributes.csv")


##  color file for use in QGIS    
evt_color_file <- evt_aoi_atts %>%
  subset(EVT_NAME != "Open Water" & EVT_NAME != "Barren-Rock/Sand/Clay") %>%
  top_n(n = 10, wt = REL_PERCENT) %>%
  add_column(z = 255) %>%
  dplyr::select(VALUE,
                R,
                G,
                B,
                z,
                EVT_NAME)


write.table(evt_color_file , file = "outputs/evt_color_file .txt", sep = ",",
            row.names = FALSE, col.names = FALSE, quote = FALSE)   

## EVC ----

evc_aoi <- US_250EVC  %>%
  crop(shp) %>%
  mask(shp)

levels(evc_aoi)[[1]] <- evc_conus_atts
activeCat(evc_aoi) <- "VALUE"

evc_aoi_atts <- values(evc_aoi, dataframe = T, na.rm = T) %>%
  table(dnn = "VALUE") %>%
  as.data.frame() %>%
  mutate_all(as.character) %>%
  mutate_all(as.integer) %>%
  left_join(cats(evc_aoi)[[1]], by = "VALUE") %>%
  filter(Freq != 0) %>%
  mutate(ACRES = round((Freq * 900 / 4046.86), 0),
         REL_PERCENT = round((Freq / sum(Freq)), 3) * 100) 


writeRaster(evc_aoi, "outputs/evc_aoi.tif",
            gdal = c("COMPRESS=NONE", "TFW=YES"),
            datatype = "INT2S",
            overwrite = T)

write.dbf(evc_aoi_atts, "outputs/evc_aoi.tif.vat.dbf")

write.csv(evc_aoi_atts, "outputs/evc_aoi_attributes.csv")

## EVC color file for use in QGIS
evc_color_file <- evc_aoi_atts    %>%
  add_column(z = 255)     %>%
  dplyr::select(VALUE,
                R,
                G,
                B,
                z,
                CLASSNAMES) 


write.table(evc_color_file, file = "outputs/evc_color_file.txt", sep = ",",
            row.names = FALSE, col.names = FALSE, quote = FALSE)

## EVH ----

evh_aoi <- US_250EVH  %>%
  crop(shp) %>%
  mask(shp)

levels(evh_aoi)[[1]] <- evh_conus_atts
activeCat(evh_aoi) <- "VALUE"

evh_aoi_atts <- values(evh_aoi, dataframe = T, na.rm = T) %>%
  table(dnn = "VALUE") %>%
  as.data.frame() %>%
  mutate_all(as.character) %>%
  mutate_all(as.integer) %>%
  left_join(cats(evh_aoi)[[1]], by = "VALUE") %>%
  filter(Freq != 0) %>%
  mutate(ACRES = round((Freq * 900 / 4046.86), 0),
         REL_PERCENT = round((Freq / sum(Freq)), 3) * 100) 


writeRaster(evh_aoi, "outputs/evh_aoi.tif",
            gdal = c("COMPRESS=NONE", "TFW=YES"),
            datatype = "INT2S",
            overwrite = T)

write.dbf(evh_aoi_atts, "outputs/evh_aoi.tif.vat.dbf")

write.csv(evh_aoi_atts, "outputs/evh_aoi_attributes.csv")

## evh color file for use in QGIS
evh_color_file <- evh_aoi_atts    %>%
  add_column(z = 255)     %>%
  dplyr::select(VALUE,
                R,
                G,
                B,
                z,
                CLASSNAMES) 


write.table(evh_color_file, file = "outputs/evh_color_file.txt", sep = ",",
            row.names = FALSE, col.names = FALSE, quote = FALSE)


## SCLS ----

scls_aoi <-  US_250SCLASS %>%
  crop(shp) %>%
  mask(shp)

levels(scls_aoi)[[1]] <- scl_conus_atts
activeCat(scls_aoi) <- "VALUE"

scls_aoi_atts <- values(scls_aoi, dataframe = T, na.rm = T) %>%
  table(dnn = "VALUE") %>%
  as.data.frame() %>%
  mutate_all(as.character) %>%
  mutate_all(as.integer) %>%
  left_join(cats(scls_aoi)[[1]], by = "VALUE") %>%
  filter(Freq != 0) %>%
  mutate(ACRES = round((Freq * 900 / 4046.86), 0),
         REL_PERCENT = round((Freq / sum(Freq)), 3) * 100) 

# write outputs  
writeRaster(scls_aoi, "outputs/scls_aoi.tif",
            gdal = c("COMPRESS=NONE", "TFW=YES"),
            datatype = "INT2S",
            overwrite = T)
write.dbf(scls_aoi_atts, "outputs/scls_aoi.tif.vat.dbf")

write.csv(scls_aoi_atts, "outputs/scls_aoi_attributes.csv")


# Combine BpS and S_class for VDEP ----

# subset reference conditions to BpS models present in AOI 
aoi_bps_models <- bps_aoi_atts$BPS_MODEL

aoi_ref_con <- ref_con %>%
  dplyr::filter(model_code %in% aoi_bps_models)

# count combinations using terra::crosstab ---



combine_raw <- terra::crosstab(c(bps_aoi, scls_aoi), long = TRUE, useNA = FALSE) |>
  rename(scl_value = VALUE.1,
         bps_value = VALUE)

# join S-Class labels 
combine  <- combine_raw  %>%
  dplyr::left_join(
    scls_aoi_atts %>% dplyr::select(VALUE, LABEL),
    by = c("scl_value" = "VALUE")
  )

test <- bps_conus_atts |>
  distinct(BPS_MODEL, BPS_NAME)

combine <- combine %>%
  dplyr::left_join(
    bps_conus_atts %>% 
      dplyr::select(VALUE, BPS_MODEL, BPS_NAME ), 
    by = c("bps_value" = "VALUE")) 


# join BpS attributes-to count number of duplicates-may remove?
combine_test <- combine %>%
  group_by(BPS_MODEL, LABEL) |>
  mutate(duplicate = n()) 

combine_test <- combine_test |>
  filter(duplicate > 1)

|>
  
  
  
  
  ungroup() |>
  filter(duplicate > 1)


test_bps_model <- combine |>
  group_by(BPS_MODEL, BPS_NAME, LABEL) |>
  summarize(sum_count = sum(n))

###  CHECK ABOVE AND WILL NEED TO ADD IN ANY MISSING COLUMNS

# compute current S-Class percent within each BpS model
combine <- combine %>%
  # 1) Compute the true total per model (sum of n over all s-class rows in that model)
  dplyr::group_by(BPS_MODEL) %>%
  dplyr::mutate(total_bps_count = sum(n, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  # 2) Recompute currentPercent using the uniform total_count
  dplyr::mutate(current_percent = as.integer((n / total_bps_count) * 100)) %>%
  # 3) Build model_label for downstream joins
  tidyr::unite(col = model_label, c("BPS_MODEL", "LABEL"), sep = "_", remove = FALSE)

# joins in reference conditions 
combine <- combine |>
  dplyr::left_join(ref_con,  
                   by = "model_label")


# clean vdep
clean_vdep <- aoi_ref_con |>
  dplyr::left_join(combine |>
                     dplyr::select(model_label, current_percent),
                   by =  "model_label") |>
  mutate(current_percent = replace_na(current_percent, 0))









