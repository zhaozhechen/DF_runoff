# Author: Zhaozhe Chen
# Date: 2025.7.8

# This code is to explore DF data

# ------- Global -------
library(here)
library(sf)
library(dplyr)

# Source functions for plotting
source(here("Functions","Plotting_functions.R"))
# Path to all Data
Data_path <- here("00_Data")
# Site info
Site_info <- read.csv(paste0(Data_path,"/USGS raw/EOF_Site_Table.csv"))
# Event data
Storm_events <- read.csv(paste0(Data_path,"/USGS raw/All_EOF_StormEventLoadsFormatted.csv"))
# DF metadata
DF_meta_Site <- read.csv(paste0(Data_path,"/Metadata/DF EOF Site & Year Metadata (2004-2023)-Site.csv"))
DF_meta_Year <- read.csv(paste0(Data_path,"/Metadata/DF EOF Site & Year Metadata (2004-2023)-Year.csv"))
# County-level shape file for plotting
US_bd <- st_read(paste0(Data_path,"/Msc/cb_2018_us_county_20m/cb_2018_us_county_20m.shp"))
# Only keep WI county
WI_bd <- US_bd[6][US_bd$STATEFP == 55,]
# Get the outer boundary of WI
WI_outer_bd <- st_union(WI_bd)
# Colors for plotting
my_color <- brewer.pal(n=8,name = "Set2")
# Output path
Output_path <- here("Results","DF Metadata")

# -------- Main ------------
# Make map of all DF sites
# For Land cover
Site_plot("LandCover")
# For Monitoring
Site_plot("Monitoring")
# For FarmEnterprise
Site_plot("FarmEnterprise")
# For CropRotation
Site_plot("CropRotation")
# For tillage
Site_plot("Tillage")
# For Manure
Site_plot("Manure")
# For Tile
Site_plot("Tile")

# Make bar plots for all DF field-year
Year_plot("Type")
Year_plot("Tillage")
Year_plot("Irrigation")
Year_plot("Manure")
Year_plot("Crop")
Year_plot("PreviousCrop")






