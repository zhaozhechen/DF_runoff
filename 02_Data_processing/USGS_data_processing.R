# Author: Zhaozhe Chen
# Update Date: 2025.7.15

# This data processing code is adapted from Ellen Albright (personal communication)

# -------- Global -----------
library(dplyr)
library(lfstat) # assign dates into USGS water years
library(here)

# USGS raw EOF Storm event data
usgs_eof <- read.csv(here("00_Data","USGS raw","All_EOF_StormEventLoadsFormatted.csv"))
# USGS site info
usgs_site_info <- read.csv(here("00_Data","USGS raw","EOF_Site_Table.csv"))

# ------- Main ---------
# USGS EOF storm event data processing
usgs_eof <- usgs_eof %>%
  # Only keep DF sites
  filter(project == "DiscoveryFarms") %>%
  filter(Field_Name !="JF1", Field_Name !="JF3", Field_Name != "JF6", # Jersey Valley CRP and urban sites, site with basin delineation issues
         Field_Name != "K1", Field_Name != "K2", Field_Name !="K3", # Saxon project sites with data quality concerns
         Field_Name != "KP1", Field_Name != "KP2", Field_Name !="K4", # tile sites with data quality concerns
         Field_Name != "RC1", Field_Name != "RC2", Field_Name != "RC3", Field_Name != "RC4", # USGS labels as "DiscoveryFarms", but I have no idea what these sites are, so excluding
         Field_Name != "WF2", Field_Name != "WF3") # Dry run CRP site and site with abnormal soil hydrology

# estimated: 1=data are estimated and 0=concentrations were measured in the lab
# frozen: 1=the ground was frozen during the event and 0=the ground was not frozen
usgs_eof[usgs_eof$estimated=="1" ,"estimated"]<-"Estimated"
usgs_eof[usgs_eof$estimated=="0" ,"estimated"]<-"Measured"
usgs_eof[usgs_eof$frozen=="1" ,"frozen"]<-"Frozen"
usgs_eof[usgs_eof$frozen=="0" ,"frozen"]<-"Non-Frozen"

# USGS site info processing
# Only keep sites after filtering
usgs_site_info <- usgs_site_info %>%
  filter(Field_Name %in% usgs_eof$Field_Name)

# Combine site info with eof data
eof_all <- left_join(usgs_eof,usgs_site_info,by="Field_Name")

# Total number of events
n_total <- nrow(usgs_eof)
# Number of events that are not classified as "storm": the flow is not associated with rainfall or snowmelt
n_nonstorm <- sum(usgs_eof$storm==0)




