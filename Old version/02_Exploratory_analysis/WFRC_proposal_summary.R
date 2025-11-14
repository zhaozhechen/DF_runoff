# This code is just to get some summary statistics for the WFRC proposal
# Date: 2025.11.6

# -------- Global -----------
library(dplyr)
library(lfstat) # assign dates into USGS water years
library(sf)
library(lubridate)
library(readxl)

# Data paths ======
# USGS raw EOF Storm event data
usgs_eof <- read.csv("00_Data/USGS raw/All_EOF_StormEventLoadsFormatted.csv")
# DF Site info
DF_site_info <- read.csv("00_Data/Metadata/DF EOF Site & Year Metadata (2004-2023)-Site_Update.csv")
# DF site updated coordinates
DF_site_location <- read_xlsx('00_Data/Metadata/DiscoveryFarms_SiteLocations.xlsx')
# Only keep Start data in this one
DF_site_time <- read.csv("00_Data/USGS raw/EOF_Site_Table.csv") %>%
  select(Field_Name,Approximate_Start_Date)

# Plotting related =========
# Source functions for plotting
source("Functions/Plotting_functions.R")
source("Functions/Data_processing_functions.R")
# Colors for plotting
my_color <- brewer.pal(n=8,name = "Set2")

# Output path for figures
Output_path <- "Results/DF Metadata_cleaned/"

# ------- Main ---------
# USGS EOF storm event data processing =================
# Filter out sites that should not be included
usgs_eof <- usgs_eof %>%
  # Only keep required DF sites
  filter(project == "DiscoveryFarms") %>%
  filter(Field_Name !="JF1", Field_Name !="JF3", Field_Name != "JF6", # Jersey Valley CRP and urban sites, site with basin delineation issues
         Field_Name != "K1", Field_Name != "K2", Field_Name !="K3",Field_Name != "K4", # Saxon project sites with data quality concerns
         Field_Name != "KP1", Field_Name != "KP2", # tile sites with data quality concerns
         Field_Name != "RC1", Field_Name != "RC2", Field_Name != "RC3", Field_Name != "RC4", # USGS labels as "DiscoveryFarms", but I have no idea what these sites are, so excluding
         Field_Name != "WF3", # Dry run CRP site and site with abnormal soil hydrology
         Field_Name != "AO2") # Basin size issue 

# Revise some notations
# estimated: 1=data are estimated and 0=concentrations were measured in the lab
# frozen: 1=the ground was frozen during the event and 0=the ground was not frozen
usgs_eof[usgs_eof$estimated=="1" ,"estimated"]<-"Estimated"
usgs_eof[usgs_eof$estimated=="0" ,"estimated"]<-"Measured"
usgs_eof[usgs_eof$frozen=="1" ,"frozen"]<-"Frozen"
usgs_eof[usgs_eof$frozen=="0" ,"frozen"]<-"Non-Frozen"



# Get summary of variables in frozen vs non-frozen
usgs_eof %>%
  group_by(frozen) %>%
  summarize(mean_TN = mean(total_nitrogen_load_pounds,na.rm=TRUE),
            mean_NH4 = mean(ammonia_plus_ammonium_load_pounds,na.rm=TRUE),
            mean_SS = mean(suspended_sediment_load_pounds,na.rm=TRUE),
            mean_DS = mean(total_dissolved_solids_load_pounds,na.rm=TRUE),
            mean_TS = mean(total_solids_load_pounds,na.rm=TRUE),
            mean_TSS = mean(total_suspended_solids_load_pounds,na.rm=TRUE),
            mean_OP = mean(orthophosphate_yield_pounds_per_acre,na.rm=TRUE),
            mean_VSS = mean(total_volatile_suspended_solids_load_pounds,na.rm=TRUE))
  
  
  
