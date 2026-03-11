

library(tidyverse)
library(leaflet)
library(sf)
# library(leaflet.extras)
library(htmltools)
library(readxl)
library(purrr)
library(glue)

# set colours -------------------------------------------------------------


# pal <- colorFactor(
#   palette = c('#f0c28d', '#f88de6', '#2596be', 'darkred', '#6ea728', "red", "#6f7e28"),
#   domain = All_Services_Locations$service_type
# )
# 
# logos <- awesomeIconList(
#   "Care Home" = makeAwesomeIcon(
#     icon = "glyphicon-bed",
#     markerColor = "beige"),
#   "Community hospital"= makeAwesomeIcon(
#     icon = "glyphicon-heart",
#     markerColor = "pink")
# )


# Source useful functions (DZ Lookup) --------------------------------------

source("/conf/LIST_analytics/West Hub/Geospatial Cross Team/Functions/DZ Lookup Function.R")

base_map_geo <- geography_area_lookup("datazone2022") |> 
  select(datazone2022, geometry)

# Read in new datazone files ----------------------------------------------

# Folder containing the Excel files
path <- "/conf/LIST_analytics/West Hub/Geospatial Cross Team/2022 Datazone to HSCP Locality Lookups/Returned Lookup Files"

# Get all .xlsx files in the folder
files <- list.files(path, pattern = "\\.xlsx$", full.names = TRUE)

# Read all files into a named list
new_locality_files <- files |> 
  # set_names(basename(.)) |>
  map(read_excel, sheet = "2022 Datazone to HSCP Locality") |> 
  bind_rows(.id = "source_file") 

geo_datazone <- base_map_geo |> 
  left_join(new_locality_files, by = "datazone2022") |> 
  filter(!is.na(source_file))


# Create transport links baseline map
base_map <- geo_datazone %>% 
  # Load map
  leaflet()  %>%
  addProviderTiles(
    "OpenStreetMap.Mapnik",
    options = providerTileOptions(opacity = 0.5)
  ) |> 
  addPolygons(data = geo_datazone, fillColor = "#5dade2", weight = 1, color = "#2e86c1",
              fillOpacity = 0.4, popup = ~glue("<b>DataZone Code: {datazone2022}</b>
                                               <br>DataZone Name: {hscp2019}
                                               <br>DataZone Name: {hscp2019name}
                                               <br>DataZone Name: {hscp_locality}")) #|> 
  

  



