# Author: Zhaozhe Chen
# Date: 2025.11.13

# This code processes raw USGS EOF dataset
# Filtering out sites that should not be included
# Extract variables related to P and Q
# This data processing code is adapted from Ellen Albright (personal communication)

# -------- Global -----------
library(dplyr)
library(lfstat) # assign dates into USGS water years
library(sf)
library(lubridate)
library(readxl)

# Data paths ======
# USGS raw EOF Storm event data
# This is from USGS data release (https://www.sciencebase.gov/catalog/item/6696bef8d34ecb78f609f651)
usgs_eof <- read.csv("00_Data/USGS raw/All_EOF_StormEventLoadsRainCalculated.csv")
# DF Site info
DF_site_info <- read.csv("00_Data/Metadata/DF EOF Site & Year Metadata (2004-2023)-Site_Update.csv")
# DF site updated coordinates
DF_site_location <- read_xlsx('00_Data/Metadata/DiscoveryFarms_SiteLocations.xlsx')
# USGS raw P data
usgs_p <- read.csv("00_Data/USGS raw/All_EOF_RainEvents.csv")
# Only keep Start and End time in this one
DF_site_time <- read.csv("00_Data/USGS raw/EOF_Site_Table.csv") %>%
  select(Field_Name,Approximate_Start_Date,Approximate_End_Date)

# PRISM Temperature data
PRISM_T <- read.csv("00_Data/Processed_data/DF_PRISM_tmean.csv")

# This is output path for data
Output_path <- "00_Data/Processed_data_v2/"

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
#usgs_eof[usgs_eof$estimated=="1" ,"estimated"]<-"Estimated"
#usgs_eof[usgs_eof$estimated=="0" ,"estimated"]<-"Measured"
usgs_eof[usgs_eof$frozen=="1" ,"frozen"]<-"Frozen"
usgs_eof[usgs_eof$frozen=="0" ,"frozen"]<-"Non-Frozen"

# Only keep P and Q related variables
usgs_eof <- usgs_eof %>%
  select(Field_Name,
         frozen,
         # Storm = 0: The flow in this event is associated with baseflow, groundwater flow, melting snow, 
         # or melting frozen ground and not a monitored precipitation event
         storm,
         unique_storm_number,
         Q_start = storm_start,
         Q_end = storm_end,
         runoff_volume,
         peak_discharge,
         # Note: below the notes indicate Q event, which is correct. Not P event. Because multiple P events are combined within this Q event.
         rain_in = rain,   # Total rain during this Q event (Unit: in)
         duration,   # Duration of all P events overlapping with this Q event.  (Unit: hour)
         Ievent,    # Mean Intensity during this Q event (Unit: in/hour)
         I5,        # Maximum 5-min intensity during this Q event (Unit: in/hour)
         I10,
         I30,
         I60,
         # Antecedent rainfall (ARF) for each event was calculated by taking the sum of the total amount of rain 
         # for a period of days (not events) before the beginning of the event associated with the flow event and reported in inches.
         ARFdays1,
         ARFdays2,
         ARFdays7,
         ARFdays14)

  # Combine unique storms (these Q events are associated with the same P event)
  #group_by(Field_Name,unique_storm_number) %>%
  #summarize(USGS_Station_Number = first(USGS_Station_Number),
  #          frozen = first(frozen),
  #          storm = first(storm),
  #          storm_start = first(storm_start),
  #          storm_end = last(storm_end),
  #          runoff_volume = sum(runoff_volume,na.rm=TRUE),
  #          peak_discharge = max(peak_discharge,na.rm=TRUE),
  #          .groups = "drop") %>%
  #select(-unique_storm_number)

# DF site info processing =====================
# Only keep sites after filtering
DF_site_info <- DF_site_info %>%
  filter(Field_Name %in% usgs_eof$Field_Name) %>%
  mutate(MeanSlope_per = as.numeric(MeanSlope_per))

# K6,P4,P5 are tile drainage
DF_site_info$Monitoring[DF_site_info$Field_Name %in% c("K6","P4","P5")] <- "Tile"
# Their paired sites are surface
DF_site_info$Monitoring[DF_site_info$Field_Name %in% c("K5","P1","P3")] <- "Surface"

# Update coordinates
DF_site_location <- DF_site_location %>%
  select(Field_Name = `Site ID`,
         LAT_approx = `GPS Lat`,
         LONG_approx = `GPS Lon`)

DF_site_info <- DF_site_info %>%
  left_join(DF_site_location,by="Field_Name",suffix = c("","_New")) %>%
  # Update coordinates using values in DF_site_location, if non-NA
  mutate(LAT_approx = coalesce(LAT_approx_New,LAT_approx),
         LONG_approx = coalesce(LONG_approx_New,LONG_approx)) %>%
  select(-LAT_approx_New,-LONG_approx_New) %>%
  left_join(DF_site_time,by="Field_Name")

# Output this DF site info
write.csv(DF_site_info,paste0(Output_path,"DF_site_info.csv"))

# Additional calculation for EOF =======================
usgs_eof <- usgs_eof %>%
  left_join(DF_site_info %>%
              select(Field_Name,BasinArea_ac),
            by="Field_Name") %>%
  # convert area from acre to sqrt ft
  mutate(area_ft2 = BasinArea_ac*43560) %>%
  # runoff volume unit: cubit ft to in
  mutate(runoff_in = runoff_volume/area_ft2 * 12) %>%
  mutate(storm = ifelse(storm == 1,"Storm","Non-storm")) 

# Output this eof_df
write.csv(usgs_eof,paste0(Output_path,"eof_P_Q_df.csv"))

# Process USGS P data ===============
# Process P data for each site individually  
arrayid <- 1
Site_ID <- DF_site_info$Field_Name[arrayid]
# Match using Site ID because some sites share the same USGS Station gauge

# Keep the corresponding P data for this site
p_site_df <- usgs_p %>%
  filter(USGS_Station_Number == USGS_ID)




# This is P data from all relevant USGS P gauges, but because each gauge may be related to more than 1 DF field, 
# with different Q events, and different monitoring periods
# So, P events at each field should be delineated separately




usgs_p <- usgs_p %>%
  # Only keep sites in EOF sites
  filter(USGS_Station_Number_for_Precipitation %in% usgs_eof$USGS_Station_Number) %>%
  # Filter out P <= 0.01 inch
  filter(rain > 0.01) %>%
  select(-project,-All_Field_Names) %>%
  rename(USGS_Station_Number = USGS_Station_Number_for_Precipitation,
         P_start = StartDate,
         P_end = EndDate)








# For each site, Only keep the P events during the monitoring period

# If a P event is associated with a Q event, Associated_Q is Yes, otherwise no


# Frozen or not.






# Plotting =================
# Map of sites

# Number of Q and P events at each site, as well as # of available years









