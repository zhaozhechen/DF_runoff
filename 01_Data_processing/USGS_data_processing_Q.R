# Author: Zhaozhe Chen
# Date: 2025.10.7

# This code processes raw USGS EOF dataset
# Filtering out sites that should not be included
# Extract variables related to Q
# This data processing code is adapted from Ellen Albright (personal communication)

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

# Plotting related =========
# Source functions for plotting
source("Functions/Plotting_functions.R")
# Colors for plotting
my_color <- brewer.pal(n=8,name = "Set2")

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

# Only keep Runoff (Q) related variables
usgs_eof <- usgs_eof %>%
  select(
    USGS_Station_Number,
    Field_Name,
    estimated,
    frozen,
    storm,
    unique_storm_number,
    storm_start,
    storm_end,
    runoff_volume,
    peak_discharge
  )

# Get a summary of event # at each site
event_n <- usgs_eof %>%
  count(Field_Name) %>%
  rename(event_n = n)

# DF site info processing =====================
# Only keep sites after filtering
DF_site_info <- DF_site_info %>%
  filter(Field_Name %in% usgs_eof$Field_Name) %>%
  left_join(event_n,by="Field_Name") %>%
  mutate(MeanSlope_per = as.numeric(MeanSlope_per))

# K6,P4,P5 are tile drainage
DF_site_info$Monitoring[DF_site_info$Field_Name %in% c("K6","P4","P5")] <- "Tile"
# Their paired sites are surface
DF_site_info$Monitoring[DF_site_info$Field_Name %in% c("K5","P1","P3")] <- "Surface"

# Extract required variables from the DF_site_location dataset
# And match their names
DF_site_location <- DF_site_location %>%
  select(Field_Name = `Site ID`,
         MajorWatershed_HUC8 = `Major Watershed HUC8`,
         Watershed_HUC10 = `Watershed HUC10`,
         Watershed_HUC12 = `Watershed HUC12`,
         HUC12 = HUC12,
         BasinArea_ac = `Drainage Area (ac)`,
         MeanSlope_per = `Average Slope (%)`,
         SoilType = `Primary Soil Type`,
         HydrologicGroup = `Hydrologic Group`,
         DrainageClass = `Drainage Class`,
         Tile = `Tiled`,
         LAT_approx = `GPS Lat`,
         LONG_approx = `GPS Lon`) %>%
  mutate(MeanSlope_per = MeanSlope_per * 100)

# Update DF site info if exist in DF_site_location
# Add new columns from DF_site_location
new_cols <- setdiff(names(DF_site_location),names(DF_site_info))
DF_site_info <- DF_site_info %>%
  left_join(DF_site_location %>%
              select(all_of(c("Field_Name",new_cols))))

# Update overlapping columns for matching sites
overlap_cols <- intersect(names(DF_site_location),names(DF_site_info))
DF_site_info <- rows_update(
  DF_site_info,
  DF_site_location %>% select(all_of(c("Field_Name",overlap_cols))),
  by = "Field_Name",
  unmatched = "ignore"
)

# Combine DF_site_info with eof data
eof_all <- usgs_eof %>%
  left_join(DF_site_info,by="Field_Name")

# Additional data cleaning/calculation =================

# Join in basin area information and calculate runoff in inches based on runoff volume and basin area
eof_all <- eof_all %>%
  # convert area from acre to sqrt ft
  mutate(area_ft2 = BasinArea_ac*43560) %>%
  # runoff volume unit: cubit ft for now
  mutate(runoff_in = runoff_volume/area_ft2 * 12)

# Assign month and USGS water year to each event
# Some storm event start and end time are 00:00, so that they are not present, need to fill
# Standardize the time
# Matches pattern that has M/D/YYYY or MM/DD/YYYY
eof_all$storm_start <- ifelse(grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", eof_all$storm_start),
                              paste(eof_all$storm_start,"00:00"),
                              eof_all$storm_start)
eof_all$storm_end <- ifelse(grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", eof_all$storm_end),
                            paste(eof_all$storm_end,"00:00"),
                            eof_all$storm_end)

eof_all$storm_start <- mdy_hm(eof_all$storm_start)
eof_all$storm_end <- mdy_hm(eof_all$storm_end)

eof_all$water_year <- water_year(eof_all$storm_start,origin="usgs")
eof_all$month <- month(eof_all$storm_start)

# Rename storm
eof_all <- eof_all %>%
  mutate(storm = ifelse(storm == 1,"Storm","Non-storm"))

# Output the processed df
write.csv(eof_all,"00_Data/Processed_data/Cleaned_data/DF_EOF_cleaned.csv")
write.csv(DF_site_info,"00_Data/Processed_data/Cleaned_data/DF_site_info_cleaned.csv")

g_map <- ggplot()+
  geom_sf(data=WI_bd,fill=my_color[3],alpha=0.3,color="grey")+
  geom_sf(data=WI_outer_bd,fill=NA,color="black")+
  geom_point(data=DF_site_info,
             aes(x=LONG_approx,y=LAT_approx,
                 size = event_n),
             shape = 21,
             color="black",
             fill="Orange",
             alpha=0.7)+
  #scale_fill_brewer(palette = "Set3")+
  my_theme+
  geom_label_repel(data=DF_site_info,
                   aes(x=LONG_approx,y=LAT_approx,label=Field_Name),
                   point.padding = 0,
                   label.padding = 0.25,
                   box.padding = 0.25,
                   min.segment.length = 0,
                   max.overlaps = 30,
                   segment.color="black")+
  guides(fill = guide_legend(override.aes = list(size = 6,shape=21)),
         size=guide_legend(override.aes = list(shape=21)))

