

library(tidyverse)
library(leaflet)
library(sf)
library(htmltools)
library(readxl)
library(purrr)
library(glue)
library(rmapshaper)


hscp_name <- "North Lanarkshire"
# Source useful functions (DZ Lookup) --------------------------------------

source("/conf/LIST_analytics/West Hub/Geospatial Cross Team/Functions/DZ Lookup Function.R")

base_map_geo <- geography_area_lookup("datazone2022") |> 
  select(datazone2022, geometry)

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

  
# geo_datazone_filtered <- geo_datazone |> 
#   filter(hscp2019name == hscp_name)


# Create mapping tile files -----------------------------------------------

# test_geo <- data.frame(st_is_valid(geo_datazone)) |> 
#   count(st_is_valid.geo_datazone.)
# 
locality_sf <- geo_datazone |>
  group_by(hscp_locality) |>
  summarise(do_union = TRUE) |>
  st_make_valid() |>
  ungroup()

# test_locality_sf <- locality_sf |>
#   count(hscp_locality)
# 
# hscp_sf <- geo_datazone |>
#   group_by(hscp2019, hscp2019name) |>
#   summarise(do_union = TRUE) |>   
#   st_make_valid()  |> 
#   ungroup()
# 
# test_locality_sf <- hscp_sf |> 
#   count(hscp2019name)


# Set Colours -------------------------------------------------------------


# pal <- colorFactor(
#   # palette = c('#f0c28d', '#f88de6', '#2596be', 'darkred', '#6ea728',
#   #             'red', '#6f7e28', 'lightyellow', 'darkblue'),
#   palette = c(#'#f0c28d',
#               'yellow',
#               '#f88de6', 
#               # '#2596be', 
#               'darkred', 
#               # '#6ea728',
#               "green",
#               'red', 
#               # 'lightyellow', 
#               'darkblue'),
#   domain = sort(unique(locality_sf$hscp_locality))
# )




# Reduce map size ---------------------------------------------------------

# geo_datazone_simp <- ms_simplify(geo_datazone, keep = 0.05, keep_shapes = TRUE)
# locality_simp     <- ms_simplify(locality_sf, keep = 0.05, keep_shapes = TRUE)
# hscp_simp         <- ms_simplify(hscp_sf, keep = 0.05, keep_shapes = TRUE)


# locality_centroids <- locality_sf %>%
#   st_centroid()

pal <- colorFactor(
  # palette = c('#f0c28d', '#f88de6', '#2596be', 'darkred', '#6ea728',
  #             'red', '#6f7e28', 'lightyellow', 'darkblue'),
  palette = c(#'#ffc101',
    '#006d77',
    '#1d568e',	
    '#ac1e28',	
    # '#ddd4b2',	
    '#ff3b75',
    '#3e7730'	),
  domain = sort(unique(locality_sf$hscp_locality))
)

# Create Map --------------------------------------------------------------

# Create transport links baseline map
# base_map <- geo_datazone %>%
#   # Load map
#   leaflet()  %>%
#   addProviderTiles(
#     "OpenStreetMap.Mapnik",
#     options = providerTileOptions(opacity = 0.5)
#   ) |>
#   # addPolygons(data = geo_datazone, weight = 1, color = "#2e86c1", fillOpacity = 0) |> 
#   addPolygons(data = locality_sf, weight = 2.5, color = "#000000",  fillOpacity = 0
#   ) |> 
#   addPolygons(data = hscp_sf, weight = 0,  fillColor = ~pal(hscp2019), color = ~pal(hscp2019), fillOpacity = 0.3) |>
#   addPolygons(data = geo_datazone, weight = 0, color = "#2e86c1", 
#               fillOpacity = 0, popup = ~glue("<br><b>HSCP</b>: {hscp2019name} (HSCP Code: {hscp2019})
#                                               <br>
#                                               <br><b>HSCP Locality</b>: {hscp_locality}
#                                               <br> 
#                                               <br><b>DataZone Code</b>: {datazone2022}"),
#               highlightOptions = highlightOptions(color = 'white', weight = 1, bringToFront = TRUE)) 
#     


# map v2 ------------------------------------------------------------------

# base_map <- geo_datazone %>%
#   leaflet()  %>%
#   addProviderTiles(
#     #   "OpenStreetMap.Mapnik",
#     # providers$CartoDB.Positron,
#     providers$Esri.WorldTopoMap,
#     options = providerTileOptions(opacity = 0.5)
#   ) |> 
#   # addPolygons(data = geo_datazone, weight = 1, color = "#2e86c1", fillOpacity = 0) |> 
#   addPolygons(data = locality_sf, 
#               weight = 2.5, 
#               fillColor = ~pal(hscp_locality), 
#               color = ~pal(hscp_locality), 
#               fillOpacity = 0.3
#   ) |> 
#   # addPolygons(data = hscp_sf, weight = 0,  fillColor = ~pal(hscp2019), color = ~pal(hscp2019), fillOpacity = 0.3) |>
#   addPolygons(data = geo_datazone, weight = 0.5, color = "black", 
#               fillOpacity = 0, popup = ~glue("<br><b>HSCP</b>: {hscp2019name} (HSCP Code: {hscp2019})
#                                               <br>
#                                               <br><b>HSCP Locality</b>: {hscp_locality}
#                                               <br> 
#                                               <br><b>DataZone Code</b>: {datazone2022}"),
#               highlightOptions = highlightOptions(color = 'white', weight = 3.5, bringToFront = TRUE)) 


  

areas <- sort(unique(geo_datazone$hscp2019name))

# areas <- c("South Lanarkshire")

out_dir <- "/conf/LIST_analytics/West Hub/Geospatial Cross Team/2022 Datazone to HSCP Locality Lookups/Returned Lookup Files/Output/"


# Loop over all areas to create multiple dashboards, 1 for each HSCP
purrr::walk(areas, function(a) {
  

  rmarkdown::render(
    input       = "dz-locality-hscp-map.Rmd",
    params      = list(area = a),
    output_file = file.path(out_dir, paste0("dashboard_", a, ".html")),
    envir       = new.env(parent = globalenv()),
    quiet       = TRUE
  )
})


# rmarkdown::render("dz-locality-hscp-map.Rmd",
#                   output_file = glue("/conf/LIST_analytics/West Hub/Geospatial Cross Team/2022 Datazone to HSCP Locality Lookups/Returned Lookup Files/Output/Returned Lookups Map {hscp_name}.html"))

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



