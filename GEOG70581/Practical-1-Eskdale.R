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
packages <- c("ggplot2", "here", "raster", "sf", "whitebox")
check.packages(packages)

