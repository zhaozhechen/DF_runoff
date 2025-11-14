# Description:Discovery Farms Figures Updates (2004-2023)
#             Data compilation and cleaning - EOF event data, soil test phosphorus data
# Last modified: 17 February 2025, Ellen Albright

library(tidyverse)
library(lfstat) # assign dates into USGS water years


# DATA -------------------------------------------------------------------------------------------------------------
# From USGS Data Release (https://www.sciencebase.gov/catalog/item/5fcaa045d34e30b91238941f)
#   1. All_EOF_StormEventLoadsFormatted_USGS.csv - USGS EOF data, includes WI DF 2004-2021
#   2. EOF_Site_Table_USGS.csv - site information (including basin size) for WI DF sites 2004-2021, USGS
# From more recent Discovery Farms data
#   3. Antigo_events_2022.csv - manually compiled 2022 data from Antigo sites AO1, AO2, AO3 (finalized).
#   4. Antigo_events_2023.csv - manually compiled 2023 data from Antigo sites AO1, AO2, AO3 (finalized).
#   3. Kewaunee_events_2022.csv - manually compiled 2022 data from Kewaunee County sites KD1, KD2 (finalized).
#   4. Kewaunee_events_2023.csv - manually compiled 2023 data from Kewaunee County sites KD1, KD2 (finalized).
#   5. Marathon_events_2022.csv - manually compiled 2022 data from Marathon County sites AO1, AO2, AO3 (finalized).
#   6. Marathon_events_2023.csv - manually compiled 2023 data from Marathon County sites AO1, AO2, AO3 (finalized).
#   7. Redstone_events_2022.csv - manually compiled 2022 data from Lake Redstone sites RS1, RS2 (finalized).
#   8. Redstone_events_2023.csv - manually compiled 2023 data from Lake Redstone sites RS1, RS2 (finalized).
#   9. DF_soils.csv - all MN, WI DF soil data through water year 2022 (didn't sample for water year 2023)


### PREPARE 2004-2021 EOF DATA, SURFACE AND TILE ################################################################################################################################
#### Read in USGS EOF data, select only Discovery Farms data and columns of interest -----

# Tile Sites: Field_Name !="DK1A", Field_Name !="MA1A", Field_Name != "SH1A", Field_Name !="KP1", Field_Name != "KP2", Field_Name !="P4", Field_Name != "P5",Field_Name != "K6",Field_Name != "K4"
usgs_eof<-read.csv("./data/All_EOF_StormEventLoadsFormatted_USGS.csv") %>% 
  filter(project=="DiscoveryFarms") %>% 
  filter(Field_Name !="JF1", Field_Name !="JF3", Field_Name != "JF6", # Jersey Valley CRP and urban sites, site with basin delineation issues
         Field_Name != "K1", Field_Name != "K2", Field_Name !="K3", # Saxon project sites with data quality concerns
         Field_Name != "KP1", Field_Name != "KP2", Field_Name !="K4", # tile sites with data quality concerns
         Field_Name != "RC1", Field_Name != "RC2", Field_Name != "RC3", Field_Name != "RC4", # USGS labels as "DiscoveryFarms", but I have no idea what these sites are, so excluding
         Field_Name != "WF2", Field_Name != "WF3") %>% # Dry run CRP site and site with abnormal soil hydrology
  select(-USGS_Station_Number, -project, -unique_storm_number, -discrete, -storm,
         -dissolved_organic_carbon_load_pounds, -total_organic_carbon_load_pounds,
         -total_Kjeldahl_nitrogen_filtered_load_pounds, -total_phosphorus_filtered_load_pounds, -total_dissolved_solids_load_pounds,
         -total_solids_load_pounds, -total_suspended_solids_load_pounds, -total_volatile_suspended_solids_load_pounds,
         -dissolved_organic_carbon_conc_mgL, -total_organic_carbon_conc_mgL, -total_Kjeldahl_nitrogen_filtered_conc_mgL, 
         -total_phosphorus_filtered_conc_mgL, -total_dissolved_solids_conc_mgL, -total_solids_conc_mgL, -total_suspended_solids_conc_mgL,
         -total_volatile_suspended_solids_conc_mgL, -remark_suspended_sediment, -remark_chloride, -remark_nitrate_plus_nitrite, -remark_ammonia_plus_ammonium,
         -remark_total_Kjeldahl_nitrogen_unfiltered, -remark_orthophosphate, -remark_total_phosphorus_unfiltered, -remark_total_nitrogen,
         -remark_organic_nitrogen, -remark_dissolved_organic_carbon, -remark_total_organic_carbon, -remark_total_Kjeldahl_nitrogen_filtered,
         -remark_total_phosphorus_filtered, -remark_total_dissolved_solids, -remark_total_solids, -remark_total_suspended_solids, -remark_total_volatile_suspended_solids,
         -dissolved_organic_carbon_yield_pounds_per_acre, -total_organic_carbon_yield_pounds_per_acre, -total_Kjeldahl_nitrogen_filtered_yield_pounds_per_acre,
         -total_phosphorus_filtered_yield_pounds_per_acre, -total_dissolved_solids_yield_pounds_per_acre, -total_solids_yield_pounds_per_acre,
         -total_suspended_solids_yield_pounds_per_acre, -total_volatile_suspended_solids_yield_pounds_per_acre)

# estimated: 1=data are estimated and 0=concentrations were measured in the lab
# frozen: 1=the ground was frozen during the event and 0=the ground was not frozen
usgs_eof[usgs_eof$estimated=="1" ,"estimated"]<-"Estimated"
usgs_eof[usgs_eof$estimated=="0" ,"estimated"]<-"Measured"
usgs_eof[usgs_eof$frozen=="1" ,"frozen"]<-"Frozen"
usgs_eof[usgs_eof$frozen=="0" ,"frozen"]<-"Non-Frozen"


#### 2022 data -------------------------------------------------------------------------------------------------------------------
# read in hand-entered versions of 2022 data (matching format)
ao_2022<-read.csv("./data/Antigo_events_2022.csv") 
kd_2022<-read.csv("./data/Kewaunee_events_2022.csv") 
mt_2022<-read.csv("./data/Marathon_events_2022.csv") 
rs_2022<-read.csv("./data/Redstone_events_2022.csv") %>% select(-event_info)

# matches annual summary values in FY22 farmer reports
ao_2022_summary<-ao_2022 %>% 
  group_by(Field_Name) %>% 
  mutate(annual_soil_lbac = sum(suspended_sediment_yield_pounds_per_acre),
         annual_TP_lbac = sum(total_phosphorus_unfiltered_yield_pounds_per_acre),
         annual_TN_lbac = sum(total_nitrogen_yield_pounds_per_acre)) %>% 
  slice(1L) %>% select(Field_Name, annual_soil_lbac, annual_TP_lbac, annual_TN_lbac)

mt_2022_summary<-mt_2022 %>% 
  group_by(Field_Name) %>% 
  mutate(annual_soil_lbac = sum(suspended_sediment_yield_pounds_per_acre),
         annual_TP_lbac = sum(total_phosphorus_unfiltered_yield_pounds_per_acre),
         annual_TN_lbac = sum(total_nitrogen_yield_pounds_per_acre)) %>% 
  slice(1L) %>% select(Field_Name, annual_soil_lbac, annual_TP_lbac, annual_TN_lbac)

kd_2022_summary<-kd_2022 %>% 
  group_by(Field_Name) %>% 
  mutate(annual_soil_lbac = sum(suspended_sediment_yield_pounds_per_acre),
         annual_TP_lbac = sum(total_phosphorus_unfiltered_yield_pounds_per_acre),
         annual_TN_lbac = sum(total_nitrogen_yield_pounds_per_acre)) %>% 
  slice(1L) %>% select(Field_Name, annual_soil_lbac, annual_TP_lbac, annual_TN_lbac)

rs_2022_summary<-rs_2022 %>% 
  group_by(Field_Name) %>% 
  mutate(annual_soil_lbac = sum(suspended_sediment_yield_pounds_per_acre),
         annual_TP_lbac = sum(total_phosphorus_unfiltered_yield_pounds_per_acre),
         annual_TN_lbac = sum(total_nitrogen_yield_pounds_per_acre)) %>% 
  slice(1L) %>% select(Field_Name, annual_soil_lbac, annual_TP_lbac, annual_TN_lbac)

#### 2023 data -------------------------------------------------------------------------------------------------------------------
# read in hand-entered version of 2023 data (matching format)
ao_2023<-read.csv("./data/Antigo_events_2023.csv") 
kd_2023<-read.csv("./data/Kewaunee_events_2023.csv") 
mt_2023<-read.csv("./data/Marathon_events_2023.csv") 
rs_2023<-read.csv("./data/Redstone_events_2023.csv") %>% select(-event_info)

# matches annual summary values in FY23 annual farmer reports
ao_2023_summary<-ao_2023 %>% 
  group_by(Field_Name) %>% 
  mutate(annual_soil_lbac = sum(suspended_sediment_yield_pounds_per_acre),
         annual_TP_lbac = sum(total_phosphorus_unfiltered_yield_pounds_per_acre),
         annual_TN_lbac = sum(total_nitrogen_yield_pounds_per_acre)) %>% 
  slice(1L) %>% select(Field_Name, annual_soil_lbac, annual_TP_lbac, annual_TN_lbac)

mt_2023_summary<-mt_2023 %>% 
  group_by(Field_Name) %>% 
  mutate(annual_soil_lbac = sum(suspended_sediment_yield_pounds_per_acre),
         annual_TP_lbac = sum(total_phosphorus_unfiltered_yield_pounds_per_acre),
         annual_TN_lbac = sum(total_nitrogen_yield_pounds_per_acre)) %>% 
  slice(1L) %>% select(Field_Name, annual_soil_lbac, annual_TP_lbac, annual_TN_lbac)

kd_2023_summary<-kd_2023 %>% 
  group_by(Field_Name) %>% 
  mutate(annual_soil_lbac = sum(suspended_sediment_yield_pounds_per_acre),
         annual_TP_lbac = sum(total_phosphorus_unfiltered_yield_pounds_per_acre),
         annual_TN_lbac = sum(total_nitrogen_yield_pounds_per_acre)) %>% 
  slice(1L) %>% select(Field_Name, annual_soil_lbac, annual_TP_lbac, annual_TN_lbac)

rs_20223_summary<-rs_2023 %>% 
  group_by(Field_Name) %>% 
  mutate(annual_soil_lbac = sum(suspended_sediment_yield_pounds_per_acre),
         annual_TP_lbac = sum(total_phosphorus_unfiltered_yield_pounds_per_acre),
         annual_TN_lbac = sum(total_nitrogen_yield_pounds_per_acre)) %>% 
  slice(1L) %>% select(Field_Name, annual_soil_lbac, annual_TP_lbac, annual_TN_lbac)

#### bind it all together -------------------------------------------------------------------------------------------------------------------
eof_all<-rbind(usgs_eof, ao_2022, kd_2022, mt_2022, rs_2022, ao_2023, kd_2023, mt_2023, rs_2023) %>% 
  rename(runoff_volume_cubicft=runoff_volume, peak_discharge_cfs = peak_discharge, 
         sediment_load_lbs=suspended_sediment_load_pounds, cl_load_lbs = chloride_load_pounds,
         no3_load_lbs=nitrate_plus_nitrite_load_pounds, nh4_load_lbs=ammonia_plus_ammonium_load_pounds,
         tkn_load_lbs=total_Kjeldahl_nitrogen_unfiltered_load_pounds, tn_load_lbs=total_nitrogen_load_pounds, orgn_load_lbs=organic_nitrogen_load_pounds,
         srp_load_lbs=orthophosphate_load_pounds, tp_load_pounds=total_phosphorus_unfiltered_load_pounds,
         
         sediment_conc_mgL=suspended_sediment_conc_mgL, cl_conc_mgL=chloride_conc_mgL, 
         no3_conc_mgL=nitrate_plus_nitrite_conc_mgL, nh4_conc_mgL=ammonia_plus_ammonium_conc_mgL, tkn_conc_mgL=total_Kjeldahl_nitrogen_unfiltered_conc_mgL,
         tn_conc_mgL=total_nitrogen_conc_mgL, orgn_conc_mgL=organic_nitrogen_conc_mgL,
         srp_conc_mgL=orthophosphate_conc_mgL, tp_conc_mgL=total_phosphorus_unfiltered_conc_mgL,
         
         sediment_yield_lbsac=suspended_sediment_yield_pounds_per_acre, cl_yield_lbsac=chloride_yield_pounds_per_acre,
         no3_yield_lbsac=nitrate_plus_nitrite_yield_pounds_per_acre, nh4_yield_lbsac=ammonia_plus_ammonium_yield_pounds_per_acre, tkn_yield_lbsac=total_Kjeldahl_nitrogen_unfiltered_yield_pounds_per_acre,
         tn_yield_lbsac=total_nitrogen_yield_pounds_per_acre, orgn_yield_lbsac=organic_nitrogen_yield_pounds_per_acre,
         srp_yield_lbsac=orthophosphate_yield_pounds_per_acre, tp_yield_lbsac=total_phosphorus_unfiltered_yield_pounds_per_acre)


#### Read in USGS site information data, select only Discovery Farms sites. Exclude sites not included in DF averages. Select columns of interest----
usgs_sites<-read.csv("./data/EOF_Site_Table_USGS.csv") %>% 
  filter(Project=="DiscoveryFarms")  %>% 
  filter(Field_Name !="JF1", Field_Name !="JF3", Field_Name != "JF6", # Jersey Valley CRP and urban sites, site with basin delineation issues
         Field_Name != "K1", Field_Name != "K2", Field_Name !="K3", # Saxon project sites with data quality concerns
         Field_Name != "KP1", Field_Name != "KP2", Field_Name !="K4", # tile sites with data quality concerns
         Field_Name != "RC1", Field_Name != "RC2", Field_Name != "RC3", Field_Name != "RC4", # USGS labels as "DiscoveryFarms", but I have no idea what these sites are, so excluding
         Field_Name != "WF2", Field_Name != "WF3") %>% # Dry run CRP site and site with abnormal soil hydrology
  select(Field_Name, Area, Site_Type) %>% 
  rename(Area_ac = Area)

# manually add MT1, MT2
MT1_basin<-data.frame("MT1","7.7", "Surface")
names(MT1_basin)<-c("Field_Name","Area_ac","Site_Type")
MT2_basin<-data.frame("MT2","5.2", "Surface")
names(MT2_basin)<-c("Field_Name","Area_ac","Site_Type")
eof_sites<-rbind(usgs_sites, MT1_basin, MT2_basin)
eof_sites$Area_ac<-as.numeric(eof_sites$Area_ac)

### CALCULATIONS ----------------------------------------------------------------------------------------------------------------
# join in basin area information and calculate runoff in inches based on runoff volume and basin area
eof_join<-left_join(eof_all, eof_sites, by="Field_Name")
eof_calc<-eof_join %>% 
  mutate(area_ft2=Area_ac*43560) %>% 
  mutate(runoff_in=runoff_volume_cubicft/area_ft2 *12)

# assign month and USGS water year to each event
# format storm start and end dates as dates (currently characters)
eof_calc$storm_start<-mdy_hm(eof_calc$storm_start) 
eof_calc$storm_end<-mdy_hm(eof_calc$storm_end) 

eof_calc$field_year<-water_year(eof_calc$storm_start, origin = "usgs")
eof_calc$month<-month(eof_calc$storm_start)

eof_calc[eof_calc$month=="1" ,"month_name"]<-"Jan"
eof_calc[eof_calc$month=="2" ,"month_name"]<-"Feb"
eof_calc[eof_calc$month=="3" ,"month_name"]<-"Mar"
eof_calc[eof_calc$month=="4" ,"month_name"]<-"Apr"
eof_calc[eof_calc$month=="5" ,"month_name"]<-"May"
eof_calc[eof_calc$month=="6" ,"month_name"]<-"Jun"
eof_calc[eof_calc$month=="7" ,"month_name"]<-"Jul"
eof_calc[eof_calc$month=="8" ,"month_name"]<-"Aug"
eof_calc[eof_calc$month=="9" ,"month_name"]<-"Sep"
eof_calc[eof_calc$month=="10" ,"month_name"]<-"Oct"
eof_calc[eof_calc$month=="11" ,"month_name"]<-"Nov"
eof_calc[eof_calc$month=="12" ,"month_name"]<-"Dec"
eof_calc$month_name<-factor(eof_calc$month_name, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))


eof_final<-eof_calc %>% 
  select(field_year,Field_Name,Site_Type, estimated,frozen,storm_start,storm_end,month_name,
         runoff_in, runoff_volume_cubicft, peak_discharge_cfs,
         sediment_yield_lbsac,cl_yield_lbsac,no3_yield_lbsac,nh4_yield_lbsac,orgn_yield_lbsac,tn_yield_lbsac,srp_yield_lbsac,tp_yield_lbsac,
         sediment_conc_mgL,cl_conc_mgL,no3_conc_mgL,nh4_conc_mgL,orgn_conc_mgL,tn_conc_mgL,srp_conc_mgL,tp_conc_mgL,
         sediment_load_lbs,cl_load_lbs,no3_load_lbs,nh4_load_lbs,orgn_load_lbs,tn_load_lbs,srp_load_lbs,tp_load_pounds)
write.csv(eof_final, "./data/DF_EOFsurfacetile_events_2004to2023_tidy.csv",row.names = F)



### PREPARE SOIL DATA ################################################################################################################################
# read in all DF soil data
soil_all<-read.csv("./data/DF_soils.csv")%>%  
  select(Site, Field.ID, WaterYear, Soil.test.date, Start.depth, End.depth, Soil.Test.P.ppm, Soil.Test.P.test.type) %>% 
  rename(STP_ppm = Soil.Test.P.ppm, STP_type = Soil.Test.P.test.type, startdepth_in = Start.depth, enddepth_in = End.depth)

soil_all$Soil.test.date<-mdy(soil_all$Soil.test.date) 
soil_all$WaterYear<-water_year(soil_all$Soil.test.date, origin = "usgs")

sites<-eof_final %>%  group_by(Field_Name) %>% slice(1L) %>% select(Field_Name)

soil_wi<-inner_join(soil_all, sites, by=c("Site"="Field_Name"))
surface_soil<-soil_wi %>% filter(enddepth_in=="1"|enddepth_in=="2")

write.csv(surface_soil, "./data/WI_DF_surface_STP.csv",row.names = F)


