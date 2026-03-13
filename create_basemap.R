

library(tidyverse)
library(leaflet)
library(sf)
# library(leaflet.extras)
library(htmltools)
library(readxl)
library(purrr)
library(glue)


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
  filter(!is.na(source_file)) |> 
  select(source_file, datazone2022, geometry, datazone2022name, hscp2019, hscp2019name, hscp_locality)


check_localities <- geo_datazone |> 
  distinct(hscp_locality) |> 
  count()

check_hscp <- geo_datazone |> 
  distinct(hscp_locality) |> 
  count()

# Create mapping tile files -----------------------------------------------

# sf::sf_use_s2(FALSE)

test_geo <- data.frame(st_is_valid(geo_datazone)) |> 
  count(st_is_valid.geo_datazone.)

locality_sf <- geo_datazone |>
  group_by(hscp_locality) |>
  summarise(do_union = TRUE) |>  
  st_make_valid() |> 
  ungroup()

test_locality_sf <- locality_sf |> 
  count(hscp_locality)

hscp_sf <- geo_datazone |>
  group_by(hscp2019, hscp2019name) |>
  summarise(do_union = TRUE) |>   
  st_make_valid()  |> 
  ungroup()

test_locality_sf <- hscp_sf |> 
  count(hscp2019name)


# Set Colours -------------------------------------------------------------


pal <- colorFactor(
  palette = c('#f0c28d', '#f88de6', '#2596be', 'darkred', '#6ea728',
              'red', '#6f7e28', 'lightyellow', 'darkblue'),
  domain = sort(unique(hscp_sf$hscp2019))
)


# Create Map --------------------------------------------------------------

# Create transport links baseline map
base_map <- geo_datazone %>%
  # Load map
  leaflet()  %>%
  addProviderTiles(
    "OpenStreetMap.Mapnik",
    options = providerTileOptions(opacity = 0.5)
  ) |>
  addPolygons(data = geo_datazone, weight = 1, color = "#2e86c1", fillOpacity = 0) |> 
  addPolygons(data = locality_sf, weight = 1,  fillColor = "#48E5D5", color = "#148F83",  fillOpacity = 0) |> 
              # dashArray = "3, 3") |>
  addPolygons(data = hscp_sf, weight = 1,  fillColor = ~pal(hscp2019),#color = "#000000",  
              fillOpacity = 0.4) |>
  addPolygons(data = geo_datazone, fillColor = "#5dade2", color = "#2e86c1",
              weight = 0,
              fillOpacity = 0, popup = ~glue("<br>HSCP Name: {hscp2019name} (HSCP Code: {hscp2019})
                                              <br>HSCP Locality: {hscp_locality}
                                              <b>DataZone Code: {datazone2022}"),
              highlightOptions = highlightOptions(color = 'white', weight = 1, bringToFront = TRUE))
  

rmarkdown::render("dz-locality-hscp-map.Rmd",
                  output_file = "/conf/LIST_analytics/West Hub/Geospatial Cross Team/2022 Datazone to HSCP Locality Lookups/Returned Lookup Files/Output/Returned Lookups Map.html")

# leaflet(geo_datazone) |>
#   # 1) Base tiles (lowest by default)
#   addProviderTiles("OpenStreetMap.Mapnik",
#                    options = providerTileOptions(opacity = 0.5)) |>
#   
#   # 2) Define drawing order explicitly (lowest zIndex draws first)
#   addMapPane("dz-pane",       zIndex = 410) |>   # Data Zones (bottom)
#   addMapPane("hscp-pane",     zIndex = 420) |>   # HSCP (middle)
#   addMapPane("locality-pane", zIndex = 430) |>   # Localities (top)
#   # You can add even higher panes for labels/markers if needed (e.g., 500+)
#   
#   # 3) Add layers into their panes
#   addPolygons(
#     data = hscp_sf,
#     options = pathOptions(pane = "hscp-pane"),
#     color = "#000000", weight = 1, fillOpacity = 0
#   ) |>
#   addPolygons(
#     data = locality_sf,
#     options = pathOptions(pane = "locality-pane"),
#     color = "#332FE0", weight = 1, fillOpacity = 0
#   ) |> 
#   addPolygons(
#     data = geo_datazone,
#     options = pathOptions(pane = "dz-pane"),
#     color = "#9E95F0", weight = 1, fillOpacity = 0,
#     popup = ~glue("<b>DataZone Code: {datazone2022}</b>
#                    <br>HSCP Code: {hscp2019}
#                    <br>HSCP Name: {hscp2019name}
#                    <br>HSCP Locality: {hscp_locality}"),
#     highlightOptions = highlightOptions(color = "white", weight = 1, bringToFront = TRUE),
#     smoothFactor = 0   # <= keep edges crisp so they align with HSCP/Locality
#   ) 



