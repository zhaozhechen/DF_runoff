# Author: Zhaozhe Chen
# Update Date: 2025.11.15

# This code processes raw USGS EOF dataset
# Filter out sites that should not be included
# Extract variables related to P and Q
# This data processing code is adapted from Ellen Albright (personal communication)

# For EOF Q events, only Q associated with storm events are kept

# This code also processes USGS P data
# For each site, only P events happened during the monitoring period of that site is kept
# P <= 0.01 in are filtered out

# -------- Global -----------
library(dplyr)
library(lfstat) # assign dates into USGS water years
library(sf)
library(lubridate)
library(readxl)
library(tidyr)

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
# Step 1. USGS EOF storm event data processing =================
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
         ARFdays14) %>%
  # Reformat time
  mutate(Q_start = mdy_hm(ifelse(grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", Q_start),
                                 paste(Q_start,"00:00"),
                                 Q_start)),
         Q_end = mdy_hm(ifelse(grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", Q_end),
                               paste(Q_end,"00:00"),
                               Q_end))) %>%
  mutate(storm = ifelse(storm == 1,"Storm","Non-storm")) %>%
  # Filter out Q events that are not associated with storm
  filter(storm == "Storm")

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

# Step 2. DF site info processing =====================
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
  left_join(DF_site_time,by="Field_Name") %>%
  mutate(Approximate_Start_Date = mdy(Approximate_Start_Date),
         Approximate_End_Date = mdy(Approximate_End_Date))

# Output this DF site info
write.csv(DF_site_info,paste0(Output_path,"DF_site_info.csv"))

# Step 3. Process USGS P data ===============
# Preprocessing of usgs_p
usgs_p <- usgs_p %>%
  # Only keep sites in EOF sites
  filter(project == "DiscoveryFarms") %>%
  # Filter out P <= 0.01 inch
  filter(rain > 0.01) %>%
  select(-project) %>%
  rename(USGS_Station_Number = USGS_Station_Number_for_Precipitation,
         P_start = StartDate,
         P_end = EndDate) %>%
  # Split All_Field_Names into multiple rows, into long data
  separate_rows(All_Field_Names,sep="\\|") %>%
  # Reformat time
  mutate(P_start = mdy_hm(ifelse(grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", P_start),
                           paste(P_start,"00:00"),
                           P_start)),
         P_end = mdy_hm(ifelse(grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", P_end),
                                 paste(P_end,"00:00"),
                                 P_end)))

# Process P data for each site individually 

# This is wrapper function to process P at each site
# Including keeping only P events during the monitoring period of site
# Label whether a P event is associated with a P event or not
# Label whether a P event is frozen or not
process_site_P <- function(Site_ID){
  # Extract P events for this target site
  # Match using Site ID not USGS Station Number because some sites share the same USGS Station gauge
  usgs_p_site <- usgs_p %>%
    filter(All_Field_Names == Site_ID)
  
  # Get this site's monitoring period
  site_dates <- DF_site_info %>%
    filter(Field_Name == Site_ID) %>%
    select(Approximate_Start_Date, Approximate_End_Date)
  
  site_start <- site_dates$Approximate_Start_Date
  site_end <- site_dates$Approximate_End_Date
  
  # For each site, Only keep the P events during the monitoring period
  # Filter P records to the study period
  if (is.na(site_end)) {
    # No end date: keep everything from start onward
    usgs_p_site <- usgs_p_site %>%
      filter(P_start >= site_start)
  } else {
    # Both start and end defined → keep only within window
    usgs_p_site <- usgs_p_site %>%
      filter(P_start >= site_start,
             P_start <= site_end)
  }
  
  # If a P event is associated with a Q event, Associated_Q is TRUE, otherwise FALSE
  # Get Q for this site fist
  eof_site <- usgs_eof %>%
    filter(Field_Name == Site_ID)
  
  usgs_p_site$Associated_Q <- sapply(seq_len(nrow(usgs_p_site)), function(i) {
    any(
      eof_site$Q_end   >= usgs_p_site$P_start[i] &
        eof_site$Q_start <= usgs_p_site$P_end[i]
    )
  })
  
  # Get the PRISM T for this site
  T_site <- data.frame(
    Date = PRISM_T$Date,
    Tmp = PRISM_T[[Site_ID]]) %>%
    # Convert character to Date
    mutate(Date = ymd(Date))
  
  # Label if the P event is frozen or not
  usgs_p_site <- usgs_p_site %>%
    # Use the calendar day for P_start
    mutate(Date = as.Date(P_start)) %>%
    # Join in daily temperature for this site
    left_join(T_site,by="Date") %>%
    # Label frozen vs non-frozen
    mutate(P_frozen = ifelse(Tmp <=0,TRUE,FALSE))
  
  return(usgs_p_site)
}

# Combine processed P for all sites
Site_ID_ls <- DF_site_info$Field_Name
usgs_p_all_sites <- lapply(Site_ID_ls,process_site_P) %>%
  bind_rows() %>%
  rename(Field_Name = All_Field_Names)

# Step 4. Additional calculation for EOF =======================
usgs_eof <- usgs_eof %>%
  left_join(DF_site_info %>%
              select(Field_Name,BasinArea_ac),
            by="Field_Name") %>%
  # convert area from acre to sqrt ft
  mutate(area_ft2 = BasinArea_ac*43560) %>%
  # runoff volume unit: cubit ft to in
  mutate(runoff_in = runoff_volume/area_ft2 * 12)

# Calculate duration from P_start to Q_start
usgs_eof <- usgs_eof %>%
  rowwise() %>%
  mutate(
    first_P_start = {
      # P event at this site that overlap this Q event
      site_p <- usgs_p_all_sites %>%
        filter(Field_Name == Field_Name,
               P_end >= Q_start,
               P_start <= Q_start)
      
      if(nrow(site_p) ==0){
        as.POSIXct(NA)
      }else{
        min(site_p$P_start,na.rm=TRUE)  
      }
    }
  ) %>%
  ungroup() %>%
  mutate(
    Q_response_time_hr = as.numeric(difftime(Q_start,first_P_start,units = "hours"))
  ) %>%
  # Force time output to keep 00:00:00
  mutate(
    Q_start = format(Q_start,"%Y-%m-%d %H:%M:%S"),
    Q_end = format(Q_end,"%Y-%m-%d %H:%M:%S")
  )

# Output this eof_df
write.csv(usgs_eof,paste0(Output_path,"All_Q_events_df.csv"))

# Convert time to character of usgs_p_all_sites for easier processing later
usgs_p_all_sites <- usgs_p_all_sites %>%
  mutate(
    P_start = format(P_start,"%Y-%m-%d %H:%M:%S"),
    P_end = format(P_end,"%Y-%m-%d %H:%M:%S")
)
# Output this P data frame
write.csv(usgs_p_all_sites,paste0(Output_path,"All_P_events_df.csv"))




