# Code to install whitebox from R-forge and initialise
# install.packages("whitebox", repos="http://R-Forge.R-project.org")
# whitebox::wbt_init()

# Function to check and install packages
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Checks and installs packages
packages <- c("ggplot2", "here", "patchwork", "gridExtra", "raster", "sf", "whitebox", "ggspatial", "RColorBrewer")
check.packages(packages)

# Sets file path for DEM
dem <- here("data", "practical_1", "dem_10m.tif")

# Breach and fills depressions
wbt_fill_depressions(dem, here("output", "practical_1", "dem_10m_fill.tif"))
wbt_breach_depressions(dem, here("output", "practical_1", "dem_10m_breach.tif"))

# Subtract function; the difference between the processed DEM (filled) and the original DEM
wbt_subtract(here("output", "practical_1", "dem_10m_fill.tif"), # Input file 1
             dem, # Input file 2
             here("output", "practical_1", "dem_10m_fill_difference.tif")) # Output file

# Loads raster using the raster and here packages
eskdale_dem <- raster(here("output", "practical_1", "dem_10m_fill_difference.tif"))

# Find minimum and maximum values
minimum_value <- min(values(eskdale_dem)[which(values(eskdale_dem) > 0)])
maximum_value <- max(values(eskdale_dem))

# Plots using ggplot
g <- ggplot() +
  layer_spatial(eskdale_dem, aes(fill = stat(band1))) +
  theme_classic() + 
  labs(fill = "Elevation difference (m)", x = "Easting", y = "Northing") +
  scale_fill_continuous(type = "viridis",  na.value = NA, 
                        limits = c(minimum_value, maximum_value))
g

# Create slope raster
wbt_slope(dem, here("output", "practical_1", "dem_10m_slope.tif"))

# Loads raster using the raster and here packages
eskdale_slope <- raster(here("output", "practical_1", "dem_10m_slope.tif"))

# Plots using ggplot
s <- ggplot() +
  layer_spatial(eskdale_slope, aes(fill = stat(band1))) +
  theme_classic() + 
  labs(fill = "Slope angle", x = "Easting", y = "Northing") +
  #scale_fill_continuous(type = "viridis",  na.value = NA) +
  scale_fill_viridis(discrete = TRUE)
s

#install.packages("gridExtra")               # Install gridExtra package
#library("gridExtra")

# # Using rainbow
# r <- ggplot() +
#   layer_spatial(eskdale_slope, aes(fill = stat(band1))) +
#   theme_classic() + 
#   labs(fill = "Slope angle", x = "Easting", y = "Northing") +
#   scale_fill_gradientn(colours = rainbow(5), na.value = NA) +
#   theme(legend.position="top")
# r
# # Using monochrome
# m <- ggplot() +
#   layer_spatial(eskdale_slope, aes(fill = stat(band1))) +
#   theme_classic() + 
#   labs(fill = "Slope angle", x = "Easting", y = "Northing") +
#   scale_fill_gradient2(low = "white", high = 'black', na.value = NA) +
#   theme(legend.position="top")
# m
# 
# d <- grid.arrange(r, m, ncol = 2)
# ggsave(plot = d, here("images", "practical_1", "two_plot.png"), dpi = 150)

# Calculates D8 pointer
wbt_d8_pointer(here("output", "practical_1", "dem_10m_fill.tif"), 
               here("output", "practical_1", "dem_10m_D8_pointer.tif"))


# Loads raster using the raster and here packages
eskdale_pointer <- raster(here("output", "practical_1", "dem_10m_D8_pointer.tif"))


# Manually defined colours based on "RdYlBu" colour palette
colours <- c("1" = "#D73027",
             "2" = "#F46D43",
             "4" = "#FDAE61",
             "8" = "#FEE090",
             "16" = "#E0F3F8",
             "32" = "#ABD9E9",
             "64" = "#74ADD1",
             "128" = "#4575B4")

# Plotting
p <- ggplot() +
  layer_spatial(eskdale_pointer, aes(fill = factor(stat(band1)))) +
  theme_classic() + 
  labs(fill = "Slope angle", x = "Easting", y = "Northing") +
  #scale_fill_brewer(palette = "RdYlBu") +
  scale_fill_manual(values = colours, na.value = NA)
p



# Calculates accumulation file (D8)
wbt_d8_flow_accumulation(here("output", "practical_1", "dem_10m_fill.tif"), 
                         here("output", "practical_1", "dem_10m_flow_accumulation.tif"), 
                         out_type = "specific contributing area", 
                         log	= "TRUE")

# Loads raster using the raster and here packages
eskdale_d8 <- raster(here("output", "practical_1", "dem_10m_flow_accumulation.tif"))

# Plots using ggplot
p <- ggplot() +
  layer_spatial(eskdale_d8, aes(fill = stat(band1))) +
  theme_classic() + 
  labs(fill = "Flow accumulation value", x = "Easting", y = "Northing", 
       title="D8") +
  scale_fill_continuous(type = "viridis",  na.value = NA) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5)) +
  coord_sf(ylim=c(504063.9,506727.6), xlim = c(320705.6, 322255.7))

p 

# Calculates accumulation file (FD8)
wbt_fd8_flow_accumulation(here("output", "practical_1", "dem_10m_fill.tif"), 
                         here("output", "practical_1", "dem_10m_flow_accumulation_fd8.tif"), 
                         out_type = "specific contributing area", 
                         log	= "TRUE", 
                         exponent = 1.1)

# Loads raster using the raster and here packages
eskdale_fd8 <- raster(here("output", "practical_1", "dem_10m_flow_accumulation_fd8.tif"))

# Plots using ggplot
g <- ggplot() +
  layer_spatial(eskdale_fd8, aes(fill = stat(band1))) +
  theme_classic() + 
  labs(fill = "Flow accumulation value", x = "Easting", y = "Northing", 
       title="FD8") +
  scale_fill_continuous(type = "viridis",  na.value = NA) +
  theme(legend.position = "bottom") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5))
g 

# Calculates accumulation file (D-Infinity)
wbt_d_inf_flow_accumulation(here("output", "practical_1", "dem_10m_fill.tif"), 
                          here("output", "practical_1", "dem_10m_flow_accumulation_d_inf.tif"), 
                          out_type = "specific contributing area", 
                          log	= "TRUE")

# Loads raster using the raster and here packages
eskdale_d_inf <- raster(here("output", "practical_1", "dem_10m_flow_accumulation_d_inf.tif"))

# Plots using ggplot
a <- ggplot() +
  layer_spatial(eskdale_d_inf, aes(fill = stat(band1))) +
  theme_classic() + 
  labs(fill = "Flow accumulation value", x = "Easting", y = "Northing",
       title="D-Infinity") +
  scale_fill_continuous(type = "viridis",  na.value = NA) +
  theme(legend.position = "top") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none", 
        plot.title = element_text(hjust = 0.5))
a 



patchwork <- p + g + a
patchwork

ggsave(plot = patchwork, here("images", "practical_1", "three-methods.png"), dpi = 300)


# The code below is used to extract streams and calculate the watershed

# Extracts streams, accumulation threshold of 500
wbt_extract_streams(here("output", "practical_1", "dem_10m_flow_accumulation.tif"),
                    here("output", "practical_1", "dem_10m_streams_act500.tif"), 
                    500)

# Snaps pour points to stream network, snap distance in map units (50 = 50 m)
wbt_jenson_snap_pour_points(here("data", "practical_1", "pour_point.shp"),
                            here("output", "practical_1", "dem_10m_streams_act500.tif"), 
                            here("output", "practical_1", "dem_10m_pour_point_snapped.shp"),
                            snap_dist = 50)

# Watershed from pour points
wbt_watershed(here("output", "practical_1", "dem_10m_D8_pointer.tif"), 
              here("output", "practical_1", "dem_10m_pour_point_snapped.shp"), 
              here("output", "practical_1", "dem_10m_watersheds.tif"))

# Converts watershed to vector format (polygon)
wbt_raster_to_vector_polygons(here("output", "practical_1", "dem_10m_watersheds.tif"),
                              here("output", "practical_1", "dem_10m_watersheds.shp"))

# Converts streams to vector format (lines)
wbt_raster_to_vector_lines(here("output", "practical_1", "dem_10m_streams_act500.tif"), 
                           here("output", "practical_1", "dem_10m_streams_act500.shp"))