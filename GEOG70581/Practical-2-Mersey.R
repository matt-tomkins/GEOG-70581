# Function to check and install packages
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Checks and installs packages
packages <- c("ggplot2", "ggspatial", "here", "raster", "sf", "whitebox", # Practical-1
              "data.table", "dplyr", "forcats", "MASS", "units", "patchwork") # Practical-2 
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

#-------------------- [8] Land cover raster reclassification ---------------------

# Loads land cover raster
land_cover <- as.factor(raster(here("data", "practical_2", "mersey_LC.tif")))

# Prints the unique values in the raster
unique(land_cover)

# Categories of interest
categories <- as.data.frame(c(41, 42, 43, 91, 101, 102, 51, 52, 61, 71, 81, 171, 172, 111, 121))
colnames(categories) <- "ID"

head(categories)

# Collapse categories into broad groups
categories$name <- fct_collapse(as.factor(categories$ID),
                                "Arable" = c("41", "42", "43"),
                                "Heath" = c("91", "101", "102"),
                                "Grassland" = c("51", "52", "61", "71", "81"),
                                "Urban" = c("171", "172"), 
                                "Wetland" = c("111", "121"))

categories

# Substitute raster values for new categories
land_cover_classified <- subs(land_cover, categories)

# Write to new raster
writeRaster(land_cover_classified, here("output", "practical_2", "mersey_LC_reclass.tif"))


# Loads streams raster using the raster and here packages
mersey_land_cover <- raster(here("output", "practical_2", "mersey_LC_reclass.tif"))


# Plots using ggplot
p <- ggplot() +
  layer_spatial(mersey_land_cover, aes(fill = stat(band1))) + # Adds raster layer
  #annotation_spatial(data = snapped_seed_points, shape = 21, fill = "#FB5858", colour = "black", size = 3) + # Adds snapped seeds layer
  theme_classic() + 
  labs(fill = "Land cover class", x = "Easting", y = "Northing") +
  #scale_fill_continuous(type = "viridis",  na.value = NA) +
  scale_fill_distiller(palette = "RdYlBu", na.value = NA) +
  theme(legend.position = "top")
p 


#-------------------- [11] Calculating surface derivatives ---------------------

# Slope and aspect
wbt_slope(dem, here("output", "practical_2", "mersey_dem_slope.tif")) 
wbt_aspect(dem, here("output", "practical_2", "mersey_dem_aspect.tif")) 

# Loads slope and aspect rasters
mersey_slope <- raster(here("output", "practical_2", "mersey_dem_slope.tif"))
mersey_aspect <- raster(here("output", "practical_2", "mersey_dem_aspect.tif"))

slope <- ggplot() +
  layer_spatial(mersey_slope, aes(fill = stat(band1))) + # Adds raster layer
  #annotation_spatial(data = snapped_seed_points, shape = 21, fill = "#FB5858", colour = "black", size = 3) + # Adds snapped seeds layer
  theme_classic() + 
  labs(fill = "Slope angle", x = "Easting", y = "Northing") +
  scale_fill_continuous(type = "viridis",  na.value = NA) +
  #scale_fill_distiller(palette = "RdYlBu", na.value = NA) +
  theme(legend.position = "top")
slope 

aspect <- ggplot() +
  layer_spatial(mersey_aspect, aes(fill = stat(band1))) + # Adds raster layer
  #annotation_spatial(data = snapped_seed_points, shape = 21, fill = "#FB5858", colour = "black", size = 3) + # Adds snapped seeds layer
  theme_classic() + 
  labs(fill = "Aspect", x = "Easting", y = "Northing") +
  #scale_fill_continuous(type = "viridis",  na.value = NA) +
  scale_fill_distiller(palette = "RdYlBu", na.value = NA) +
  theme(legend.position = "top")
aspect 

slope+aspect


#-------------------- [12] Merging watersheds with WQ data ---------------------

# Load watersheds (vector, sf, VALUE column is important) and renames
watersheds <- st_read(here("output", "practical_2", "mersey_watersheds.shp"))
names(watersheds)[names(watersheds) == 'VALUE'] <- 'Seed_Point_ID'

# Load Environment Agency data
ea_data <- read.csv(here("data", "practical_2", "mersey_EA_chemistry.csv"))

# Merge based on ID
watersheds_ea <- merge(watersheds, ea_data, by = "Seed_Point_ID") # correct



# Calculates geometry as sf object, converts to km^2 using the units package
watersheds_ea$area <- set_units(st_area(watersheds_ea), km^2)

# Load elevation raster
mersey_dem <- raster(here("data", "practical_2", "mersey_dem_fill.tif"))

# Calculates the number of raster cells per watershed 
watersheds_ea$count <- extract(dem, watersheds_ea, fun=function(x, ...) length(x)) 

# Removes object(s) from memory to avoid confusion
rm(watersheds)


#-------------------- [13] Extracting continuous surface derivatives ---------------------

# Load raster data
rainfall <- raster(here("data", "practical_2", "mersey_rainfall.tif"))
slope <- raster(here("output", "practical_2", "mersey_dem_slope.tif"))
aspect <- raster(here("output", "practical_2", "mersey_dem_aspect.tif"))

# Extract derivatives, calculate mean, and store in attribute table
watersheds_ea$average_elevation <- extract(mersey_dem, watersheds_ea, fun=mean, na.rm=TRUE)
watersheds_ea$average_rainfall  <- extract(rainfall, watersheds_ea, fun=mean, na.rm=TRUE)
watersheds_ea$average_slope <- extract(slope, watersheds_ea, fun=mean, na.rm=TRUE)
watersheds_ea$average_aspect  <- extract(aspect, watersheds_ea, fun=mean, na.rm=TRUE)

head(watersheds_ea)

# Removes object(s) from memory
rm(dem, rainfall, slope, aspect)


#-------------------- [14] Extracting categorical surface derivatives ---------------------

# Loads categorical rasters into R
land_cover <- raster(here("output", "practical_2", "mersey_LC_reclass.tif"))
#soils <- raster(here("output", "practical_2", "mersey_HOST_reclass.tif"))
#bedrock <- raster(here("output", "practical_2", "mersey_bedrock_reclass.tif"))

# Extract land cover counts (5 classes, 1:5)
land_cover_classes <- extract(land_cover, watersheds_ea, fun=function(i,...) table(factor(i, levels = 1:5)))
colnames(land_cover_classes) <- c("Arable", "Heath", "Grassland", "Urban", "Wetland")

# Extract soils counts (4 classes, 1:4)
soils_classes <- extract(soils, watersheds_ea, fun=function(i,...) table(factor(i, levels = 1:4)))
colnames(soils_classes) <- c("Permeable", "Impermeable", "Gleyed", "Peats")

# Extract bedrock counts (3 classes, 1:3)
bedrock_classes <- extract(bedrock, watersheds_ea, fun=function(i,...) table(factor(i, levels = 1:3)))
colnames(bedrock_classes) <- c("Sands_and_Muds", "Limestone", "Coal")

# Combines counts with watersheds data
watersheds_ea <- cbind(watersheds_ea, land_cover_classes, soils_classes, bedrock_classes)

#-------------------- [15] Normalising categorical surface derivatives ---------------------

# Creates list of categorical variables
categorical_names <- c("Arable", "Heath", "Grassland", "Urban", "Wetland", "Permeable", "Impermeable", "Gleyed", "Peats", "Sands_and_Muds", "Limestone", "Coal")

# Loops through list
for (i in categorical_names){
  # Defines new column name
  col <- paste(i, "percent", sep="_")
  # Updates with percentage
  watersheds_ea[col] <- as.numeric(watersheds_ea[[i]]/watersheds_ea$count*100)
}


# Drop geometry from sf object
watersheds_df <- st_drop_geometry(watersheds_ea)

# Writes completed file to csv
write.csv(x = watersheds_df, here("output", "practical_2", "mersey_watersheds_EA_compiled.csv"), row.names=FALSE)


#-------------------- [16] Multi-linear regression ---------------------

# Reads completed file from csv
watersheds_df <- read.csv(here("output", "practical_2", "mersey_watersheds_ea.csv"))


# Runs a linear model (mu)
model <- lm(formula = NO2 ~ average_elevation + average_rainfall + Urban_percent, data = watersheds_df)

# Column names of interest
factors <- colnames(watersheds_df %>% dplyr::select(contains(c("average", "percent"))))

# Creates data frame
variables <- watersheds_df[factors]

# Column bind the NO2 column from watersheds_df with the data frame containing all the independent variables 
model_df <- cbind(NO2 = watersheds_df$NO2, variables)


# Fits a linear model
no2_model <- lm(formula = NO2 ~ ., data = model_df)

summary(no2_model)

step.model <- stepAIC(no2_model, direction = "backward", 
                      trace = FALSE, k = 1)
summary(step.model)
