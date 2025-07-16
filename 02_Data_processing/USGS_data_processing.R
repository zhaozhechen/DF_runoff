# Author: Zhaozhe Chen
# Update Date: 2025.7.15

# This data processing code is adapted from Ellen Albright (personal communication)

# -------- Global -----------
library(dplyr)
library(lfstat) # assign dates into USGS water years
library(here)
library(sf)

# USGS raw EOF Storm event data
usgs_eof <- read.csv(here("00_Data","USGS raw","All_EOF_StormEventLoadsFormatted.csv"))
# USGS site info
usgs_site_info <- read.csv(here("00_Data","USGS raw","EOF_Site_Table.csv"))
# DF site info
DF_site_info <- read.csv(here("00_Data","Metadata","DF EOF Site & Year Metadata (2004-2023)-Site_Update.csv"))
# County-level shape file for plotting
US_bd <- st_read(here("00_Data","Msc","cb_2018_us_county_20m/cb_2018_us_county_20m.shp"))
# Only keep WI county
WI_bd <- US_bd[6][US_bd$STATEFP == 55,]
# Get the outer boundary of WI
WI_outer_bd <- st_union(WI_bd)
# Colors for plotting
my_color <- brewer.pal(n=8,name = "Set2")
# Output path
Output_path <- here("Results","DF Exploratory")
# Source functions for plotting
source(here("Functions","Plotting_functions.R"))

# ------- Main ---------
# USGS EOF storm event data processing =================
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

# Get a summary of events # at each site
event_n <- usgs_eof %>%
  count(Field_Name)

# USGS site info processing =======================
# Only keep sites after filtering
usgs_site_info <- usgs_site_info %>%
  filter(Field_Name %in% usgs_eof$Field_Name)

# Combine site info with eof data
eof_all <- left_join(usgs_eof,usgs_site_info,by="Field_Name")

# Total number of events
n_total <- nrow(usgs_eof)
# Number of events that are not classified as "storm": the flow is not associated with rainfall or snowmelt
n_nonstorm <- sum(usgs_eof$storm==0)

# DF site info processing =====================
# Only keep sites after filtering
DF_site_info <- DF_site_info %>%
  filter(Field_Name %in% usgs_eof$Field_Name) %>%
  left_join(event_n,by="Field_Name")

# Make a map
g_map <- ggplot()+
  geom_sf(data=WI_bd,fill=my_color[3],alpha=0.3,color="grey")+
  geom_sf(data=WI_outer_bd,fill=NA,color="black")+
  geom_point(data=DF_site_info,
             aes(x=LONG_approx,y=LAT_approx,
                 size = n),
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
# Make a histogram of event number distribution
g_hist <- ggplot(data=event_n,aes(x=n))+
  geom_histogram(binwidth=20,color="black",fill="lightblue")+
  theme(#axis.line=element_line(color="black"),
    panel.background = element_blank(),
    panel.border = element_rect(colour="black",fill=NA),
    legend.key = element_blank(),
    #legend.key.size = unit(6,"cm"),
    #aspect.ratio = 1/1,
    #legend.key.size = unit(0.3,'cm'),
    legend.text = element_text(size=18),
    plot.title = element_text(size=18))+
  labs(x="Event number at each site")
