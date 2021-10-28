# Function to check and install packages
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Checks and installs packages
packages <- c("ggplot2", "ggspatial", "here", "raster", "sf", "whitebox", # Practical-1
              "data.table", "dplyr", "forcats", "MASS", "units") # Practical-2 
check.packages(packages)

# Sets file path for DEM
dem <- here("data", "practical_2", "mersey_dem_fill.tif")

# Calculates D8 pointer
wbt_d8_pointer(dem, 
               here("output", "practical_2", "mersey_dem_D8_pointer.tif"))

# Calculates accumulation file
wbt_d8_flow_accumulation(here("output", "practical_2", "mersey_dem_D8_pointer.tif"), 
                         here("output", "practical_2", "mersey_dem_flow_accumulation.tif"), 
                         out_type = "specific contributing area",
                         log = TRUE, 
                         pntr = TRUE)



# Loads flow accumulation raster using the raster and here packages
mersey_accumulation <- raster(here("output", "practical_2", "mersey_dem_flow_accumulation.tif"))

# Loads EA monitoring sites using the st_read function
seed_points <- st_read(here("data", "practical_2", "mersey_EA_sites.shp"))

# Plots using ggplot
p <- ggplot() +
  layer_spatial(mersey_accumulation, aes(fill = stat(band1))) + # Adds raster layer
  geom_sf(data = seed_points, shape = 21, fill = "#FFFFFF", colour = "black", size = 3) + # Adds vector layer, modifying point size, colour and fill
  theme_classic() + 
  labs(fill = "Log-transformed flow accumulation value", x = "Easting", y = "Northing") +
  scale_fill_continuous(type = "viridis",  na.value = NA) +
  theme(legend.position = "top")
p 

# Crops the accumulation raster based on extent (row and col indexes) 
mersey_accumulation_crop <- crop(mersey_accumulation, extent(mersey_accumulation,  632, 645, 540, 578))

# Plots using ggplot
p <- ggplot() +
  layer_spatial(mersey_accumulation_crop, aes(fill = stat(band1))) + # Adds raster layer
  annotation_spatial(data = seed_points, shape = 21, fill = "#FFFFFF", colour = "black", size = 3) + # Adds vector layer
  theme_classic() + 
  labs(fill = "Log-transformed flow accumulation value", x = "Easting", y = "Northing") +
  scale_fill_continuous(type = "viridis",  na.value = NA) +
  theme(legend.position = "top")
p 

#-------------------- [4] Number of upslope elements ---------------------

# Calculates accumulation file
wbt_d8_flow_accumulation(dem, 
                         here("output", "practical_2", "mersey_dem_D8_flow_accumulation_NUE.tif"), 
                         out_type = "cells",
                         log = FALSE)


#-------------------- [5] Streams ---------------------

# Extracts streams, accumulation threshold of 500
wbt_extract_streams(here("output", "practical_2", "mersey_dem_D8_flow_accumulation_NUE.tif"),
                    here("output", "practical_2", "mersey_dem_streams_act200.tif"), 
                    threshold = 200, 
                    zero_background = TRUE)

# Loads flow accumulation raster using the raster and here packages
mersey_streams <- raster(here("output", "practical_2", "mersey_dem_streams_act200.tif"))

p <- ggplot() +
  layer_spatial(mersey_streams, aes(fill = stat(band1))) + # Adds raster layer
  theme_classic() + 
  labs(fill = "Log-transformed flow accumulation value", x = "Easting", y = "Northing") +
  scale_fill_continuous(type = "viridis",  na.value = NA) +
  theme(legend.position = "top")
p 


#-------------------- [6] Snapping ---------------------

# Snaps pour points to stream network, snap distance in map units (150 = 150 m)
wbt_jenson_snap_pour_points(here("data", "practical_2", "mersey_EA_sites.shp"),
                            here("output", "practical_2", "mersey_dem_streams_act200.tif"), 
                            here("output", "practical_2", "mersey_EA_sites_snapped.shp"),
                            snap_dist = 500)

# Loads streams raster using the raster and here packages
mersey_streams <- raster(here("output", "practical_2", "mersey_dem_streams_act200.tif"))

# Crops the accumulation raster based on a defined extent (row and col indexes) 
mersey_streams_crop <- crop(mersey_streams, extent(mersey_streams,  632, 645, 540, 578))

# Loads original and Jenson-snapped EA monitoring sites using the st_read function
seed_points <- st_read(here("data", "practical_2", "mersey_EA_sites.shp"))
snapped_seed_points <- st_read(here("output", "practical_2", "mersey_EA_sites_snapped.shp"))

# Plots using ggplot
p <- ggplot() +
  layer_spatial(mersey_accumulation_crop, aes(fill = stat(band1))) + # Adds raster layer
  annotation_spatial(data = seed_points, shape = 21, fill = "#FFFFFF", colour = "black", size = 3) + # Adds vector layer
  annotation_spatial(data = snapped_seed_points, shape = 21, fill = "#FB5858", colour = "black", size = 3) + # Adds vector layer
  theme_classic() + 
  labs(fill = "Log-transformed flow accumulation value", x = "Easting", y = "Northing") +
  scale_fill_continuous(type = "viridis",  na.value = NA) +
  theme(legend.position = "top")
p 


#-------------------- [7] Watershed creation ---------------------

# Watershed from pour points
wbt_watershed(here("output", "practical_2", "mersey_dem_D8_pointer.tif"), 
              here("output", "practical_2", "mersey_EA_sites_snapped.shp"), 
              here("output", "practical_2", "mersey_watersheds.tif"))


# Converts watershed to vector format (polygon)
wbt_raster_to_vector_polygons(here("output", "practical_2", "mersey_watersheds.tif"),
                              here("output", "practical_2", "mersey_watersheds.shp"))

# Loads streams raster using the raster and here packages
mersey_watersheds <- raster(here("output", "practical_2", "mersey_watersheds.tif"))

# Plots using ggplot
p <- ggplot() +
  layer_spatial(mersey_watersheds, aes(fill = stat(band1))) + # Adds raster layer
  annotation_spatial(data = snapped_seed_points, shape = 21, fill = "#FB5858", colour = "black", size = 3) + # Adds snapped seeds layer
  theme_classic() + 
  labs(fill = "Stream network", x = "Easting", y = "Northing") +
  scale_fill_continuous(type = "viridis",  na.value = NA) +
  theme(legend.position = "top")
p 

