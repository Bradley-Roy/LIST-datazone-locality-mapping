
# Load Packages and Set Options ----

library(tidyverse)
library(leaflet)
library(sf)
library(htmltools)
library(readxl)
library(purrr)
library(glue)
library(rmapshaper)
library(glue)

# turns off the automatic offset - allowing the map to laid out on a flat surface. 
sf::sf_use_s2(FALSE)

# 0. Source useful functions (DZ Lookup) --------------------------------------

data_folder <- "/conf/LIST_analytics/West Hub/Geospatial Cross Team/2022 Datazone to HSCP Locality Lookups/Returned Lookup Files/"
out_dir <- glue("{data_folder}Output/")

source("/conf/LIST_analytics/West Hub/Geospatial Cross Team/Functions/DZ Lookup Function.R")

# 1. Load Data ----

## 1.1. Read In 2022 Datazones + HSCP 2019 ----

datazone2022_sf <- geography_area_lookup("datazone2022") 

hscp2019_sf <- geography_area_lookup("hscp2019") 

# hscplocality2011_sf <- geography_area_lookup("hscplocality2011") 

## 1.2. Read In True Locality Boundaries (Not Best-Fit) ---- 

true_locality_boundaries <- st_read(glue("{data_folder}/inputs/HSCP-2011 Aligned/HSCP_North_Lanarkshire.shp")) |> 
  rename(hscp_locality = hscp_local,hscp2019name=hscp2019na) %>%
  st_transform(4326)


## 1.3. Read In Datazone To 2022 Best-Fit Returned Lookups ----

# Get all .xlsx files in the folder
files <- list.files(data_folder, pattern = "\\.xlsx$", full.names = TRUE) 

# Remove Opened xlsx files 
files <- files[!grepl("[~$]",files)]

# Read all files into a named list
proposed_locality_lookup <- files |> 
  map(read_excel, sheet = "2022 Datazone to HSCP Locality") |> 
  bind_rows(.id = "source_file") |> 
  dplyr::select(hscp2019,datazone2022,proposed_locality=hscp_locality)


# 2. Add new best fit-localities to all 2022 Datazones ---- 

datazone2022_sf <- datazone2022_sf |> 
  left_join(proposed_locality_lookup, by = c("hscp2019","datazone2022")) 
  
# 3. Create Proposed Locality Lookup Using Datazone Shapefiles ----

proposed_locality_sf <- datazone2022_sf |>
  group_by(hscp2019,hscp2019name,proposed_locality) |>
  summarise(pop2022=sum(pop2022),do_union = TRUE) |>  
  st_make_valid() |> 
  ungroup()


# 4. Attach Proposed Locality ----

datazone2022_sf <- datazone2022_sf |>
  left_join(dplyr::rename(st_drop_geometry(proposed_locality_sf),pop2022_proposed_locality=pop2022),
            by=c("hscp2019","hscp2019name","proposed_locality")) |>
  mutate(pop2022 = format(pop2022,big.mark=","),
         pop2022_proposed_locality = format(pop2022_proposed_locality,big.mark=","))


# Sidebar. Check Numbers ----

check_localities <- datazone2022_sf |> 
  filter(!is.na(proposed_locality)) |>
  distinct(proposed_locality) |> 
  count() |> 
  pull()

check_hscp <- datazone2022_sf |> 
  filter(!is.na(proposed_locality)) |>
  distinct(hscp2019name) |> 
  count() |> 
  pull()

# 4. Create Base Map ----

base_map <- leaflet() |>
  addProviderTiles(
    providers$Esri.WorldTopoMap,
    options = providerTileOptions(opacity = 0.5)
  )


# 4. Run Dashboard For All HSCPs ---- 

areas <- datazone2022_sf |> 
  filter(!is.na(proposed_locality)) |> 
  pull(hscp2019name) |> 
  unique() |> 
  sort()

# Create map for each HSCP 
# Loop over all areas to create multiple dashboards, 1 for each HSCP
purrr::walk(areas, function(a) {

  print(a)
  
  rmarkdown::render(
    input       = "dz-locality-hscp-map.Rmd",
    params      = list(area = a),
    output_file = file.path(out_dir, paste0("Data Zone, Locality, HSCP Map - ", a, ".html")),
    envir       = new.env(parent = globalenv()),
    quiet       = TRUE
  )
  
})

