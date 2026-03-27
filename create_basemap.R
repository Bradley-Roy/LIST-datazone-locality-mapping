

library(tidyverse)
library(leaflet)
library(sf)
library(htmltools)
library(readxl)
library(purrr)
library(glue)
library(rmapshaper)
library(glue)

# HSCP input for testing
# hscp_name <- "North Lanarkshire"

# Source useful functions (DZ Lookup) --------------------------------------

data_folder <- "/conf/LIST_analytics/West Hub/Geospatial Cross Team/2022 Datazone to HSCP Locality Lookups/Returned Lookup Files/"
out_dir <- glue("{data_folder}Output/")

source("/conf/LIST_analytics/West Hub/Geospatial Cross Team/Functions/DZ Lookup Function.R")

base_map_geo <- geography_area_lookup("datazone2022") |> 
  select(datazone2022, geometry)

#NL data
shp_data <- st_read(glue("{data_folder}/inputs/HSCP-2011 Aligned/HSCP_North_Lanarkshire.shp")) |> 
  rename(hscp_locality = hscp_local)

# Read in new datazone files ----------------------------------------------

# Folder containing the Excel files
path <- "/conf/LIST_analytics/West Hub/Geospatial Cross Team/2022 Datazone to HSCP Locality Lookups/Returned Lookup Files"

# Get all .xlsx files in the folder
files <- list.files(path, pattern = "\\.xlsx$", full.names = TRUE)

files1 <- files[files != "/conf/LIST_analytics/West Hub/Geospatial Cross Team/2022 Datazone to HSCP Locality Lookups/Returned Lookup Files/~$North Lanarkshire HSCP - 2022 Datazone to HSCP Locality Lookup.xlsx"]

# Read all files into a named list
new_locality_files <- files1 |> 
  map(read_excel, sheet = "2022 Datazone to HSCP Locality") |> 
  bind_rows(.id = "source_file") 

geo_datazone <- base_map_geo |> 
  left_join(new_locality_files, by = "datazone2022") |> 
  filter(!is.na(source_file)) |> 
  select(source_file, datazone2022, geometry, datazone2022name, hscp2019, hscp2019name, hscp_locality)
  # mutate(hscp_oi = (hscp2019name == hscp_name))
  
# turns off the automatic offset - allowing the map to laid out on a flat surface. 
sf::sf_use_s2(FALSE)

check_localities <- geo_datazone |> 
  distinct(hscp_locality) |> 
  count() |> 
  pull()

check_hscp <- geo_datazone |> 
  distinct(hscp2019name) |> 
  count() |> 
  pull()



# Testing -----------------------------------------------------------------


locality_sf <- geo_datazone |>
  filter(hscp2019name == "Fife") |> 
  count(hscp_locality) 

duplicate_dz <- geo_datazone %>%
  group_by(datazone2022) %>%
  filter(n() > 1) %>%
  arrange(datazone2022)

# Set up for Map ---------------------------------------------------------
# 
# pal <- colorFactor(
#   palette = c(
#     '#006d77',
#     '#1d568e',	
#     '#F3BA2D',
#     '#3e7730',
#     '#ff3b75'),
#   domain = sort(unique(locality_sf$hscp_locality))
# )


  

# areas <- sort(unique(geo_datazone$hscp2019name))

areas <- c("North Lanarkshire")


# Create map for each HSCP ------------------------------------------------

# Loop over all areas to create multiple dashboards, 1 for each HSCP
purrr::walk(areas, function(a) {

  rmarkdown::render(
    input       = "dz-locality-hscp-map.Rmd",
    params      = list(area = a),
    output_file = file.path(out_dir, paste0("Data Zone, Locality, HSCP Map - ", a, ".html")),
    envir       = new.env(parent = globalenv()),
    quiet       = TRUE
  )
  
})

