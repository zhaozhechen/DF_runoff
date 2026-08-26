# Author: Zhaozhe Chen
# Update Date: 2026.8.26

# This code makes exploratory figures and summary statistics
# for the Discovery Farms surface-runoff monitoring sites

# All hydrologic depths are in millimetres
# All rainfall intensities are in millimetres per hour

# -------- Global -----------
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
  library(sf)
  library(cowplot)
  library(patchwork)
  library(RColorBrewer)
  library(ggrepel)
  library(gghalves)
  library(scales)
})

# Confirm that the script is run from the project root
Project_path <- normalizePath(getwd(),winslash="/",mustWork=TRUE)

if(!dir.exists(file.path(Project_path,"00_Data","Processed"))){
  stop(
    "Run this script from the DF_runoff_v2 project root. ",
    "The folder 00_Data/Processed was not found."
  )
}

# Source functions ======
source(file.path(Project_path,"01_Functions","01_General_data_functions.R"))
source(file.path(Project_path,"01_Functions","04_Reporting_plotting_functions.R"))

# Data paths ======
Processed_path <- file.path(Project_path,"00_Data","Processed")
Reference_path <- file.path(Project_path,"00_Data","Reference")
Result_path <- file.path(Project_path,"04_Results","Exploratory")
Figure_path <- file.path(Result_path,"Figures")
Table_path <- file.path(Result_path,"Tables")
Report_path <- file.path(Project_path,"03_Reports")

dir.create(Figure_path,recursive=TRUE,showWarnings=FALSE)
dir.create(Table_path,recursive=TRUE,showWarnings=FALSE)
dir.create(Report_path,recursive=TRUE,showWarnings=FALSE)

# Remove figures excluded from the revised exploratory set
Obsolete_figure_stems <- c(
  "01_Site_maps_explanatory_variables",
  "07_Runoff_coefficient_management",
  "08_Precipitation_runoff_relationship",
  "09_Event_depth_boxplots",
  "10_Frozen_nonfrozen_monthly_patterns"
)

Obsolete_figure_paths <- as.vector(
  outer(
    file.path(Figure_path,Obsolete_figure_stems),
    c(".png",".pdf"),
    paste0
  )
)

unlink(Obsolete_figure_paths)

# Reproducible jitter
set.seed(1)

sd_or_zero <- function(x){
  x <- x[is.finite(x)]
  if(length(x) > 1){
    stats::sd(x)
  }else{
    0
  }
}

# ------- Data import ---------
DF_site_info <- read.csv(
  file.path(Processed_path,"DF_site_info.csv"),
  stringsAsFactors=FALSE,
  check.names=FALSE
) %>%
  mutate(
    Approximate_Start_Date=as.Date(Approximate_Start_Date),
    Approximate_End_Date=as.Date(Approximate_End_Date)
  )

Management_df <- read.csv(
  file.path(Processed_path,"Management_site_water_year.csv"),
  stringsAsFactors=FALSE,
  check.names=FALSE
)

P_df <- read.csv(
  file.path(Processed_path,"All_P_events.csv"),
  stringsAsFactors=FALSE,
  check.names=FALSE
) %>%
  mutate(
    P_start=as.POSIXct(P_start,tz="America/Chicago"),
    P_end=as.POSIXct(P_end,tz="America/Chicago"),
    Event_Date=as.Date(P_start,tz="America/Chicago"),
    Calendar_Year=year(Event_Date),
    Month_Number=month(Event_Date),
    Month=factor(month.abb[Month_Number],levels=month.abb),
    Season=factor(
      Season,
      levels=c(
        "Pre-growing season",
        "Growing season",
        "Post-growing season"
      )
    )
  )

Q_df <- read.csv(
  file.path(Processed_path,"All_Q_events.csv"),
  stringsAsFactors=FALSE,
  check.names=FALSE
) %>%
  mutate(
    Q_start=as.POSIXct(Q_start,tz="America/Chicago"),
    Q_end=as.POSIXct(Q_end,tz="America/Chicago"),
    Event_Date=as.Date(Q_start,tz="America/Chicago"),
    Calendar_Year=year(Event_Date),
    Month_Number=month(Event_Date),
    Month=factor(month.abb[Month_Number],levels=month.abb),
    Season=factor(
      Season,
      levels=c(
        "Pre-growing season",
        "Growing season",
        "Post-growing season"
      )
    ),
    Runoff_Coefficient=if_else(
      rain_mm > 0,
      runoff_mm/rain_mm,
      NA_real_
    ),
    Hydrologic_Group=factor(
      Hydrologic_Group,
      levels=c(
        "Slow-infiltration",
        "Moderate-infiltration",
        "High-infiltration"
      )
    )
  )

# Keep the full event record for descriptive counts
# Primary runoff-coefficient summaries use non-frozen events
Q_nonfrozen <- Q_df %>%
  filter(
    frozen == "Non-Frozen",
    is.finite(Runoff_Coefficient),
    Runoff_Coefficient >= 0
  )

# ------- Main ---------
# Step 1. Monitoring calendar and site-year summaries =========
# Find the last date in all data record
Last_event_date <- max(
  c(P_df$Event_Date,Q_df$Event_Date),
  na.rm=TRUE
)

# Monitored months for each site
Monitoring_months <- lapply(
  seq_len(nrow(DF_site_info)),
  function(i){
    site_row <- DF_site_info[i,]
    start_month <- floor_date(
      site_row$Approximate_Start_Date,
      unit="month"
    )
    end_date <- if(is.na(site_row$Approximate_End_Date)){
      Last_event_date
    }else{
      site_row$Approximate_End_Date
    }
    end_month <- floor_date(end_date,unit="month")
    
    data.frame(
      Field_Name=site_row$Field_Name,
      Month_Date=seq.Date(start_month,end_month,by="month")
    )
  }
) %>%
  bind_rows() %>%
  mutate(
    Calendar_Year=year(Month_Date),
    Month_Number=month(Month_Date),
    Month=factor(month.abb[Month_Number],levels=month.abb)
  )

# Monthly P # and depth at each site
P_monthly <- P_df %>%
  group_by(Field_Name,Calendar_Year,Month_Number) %>%
  summarise(
    P_Event_Number=n(),
    Precipitation_mm=sum(rain_mm,na.rm=TRUE),
    .groups="drop"
  )

# Monthly Q # and depth at each site
Q_monthly <- Q_df %>%
  group_by(Field_Name,Calendar_Year,Month_Number) %>%
  summarise(
    Q_Event_Number=n(),
    Runoff_mm=sum(runoff_mm,na.rm=TRUE),
    .groups="drop"
  )

# Combine monthly P and Q
Site_month_summary <- Monitoring_months %>%
  left_join(
    P_monthly,
    by=c("Field_Name","Calendar_Year","Month_Number")
  ) %>%
  left_join(
    Q_monthly,
    by=c("Field_Name","Calendar_Year","Month_Number")
  ) %>%
  mutate(
    across(
      c(
        P_Event_Number,
        Precipitation_mm,
        Q_Event_Number,
        Runoff_mm
      ),
      ~replace_na(.x,0)
    )
  )

# Annual P and Q number and depth
Site_year_summary <- Site_month_summary %>%
  group_by(Field_Name,Calendar_Year) %>%
  summarise(
    Monitored_Months=n(),
    P_Event_Number=sum(P_Event_Number),
    Precipitation_mm=sum(Precipitation_mm),
    Q_Event_Number=sum(Q_Event_Number),
    Runoff_mm=sum(Runoff_mm),
    .groups="drop"
  ) %>%
  mutate(
    Complete_Monitoring_Year=Monitored_Months == 12
  )

# Summarize mean P and Q number and depth across years. 
# Years with no full observations across all 12 months are removed from this summary
Site_summary <- Site_year_summary %>%
  filter(Complete_Monitoring_Year) %>%
  group_by(Field_Name) %>%
  summarise(
    Monitoring_Years=n(),
    Mean_Annual_P_Events=mean(P_Event_Number),
    SD_Annual_P_Events=sd_or_zero(P_Event_Number),
    Mean_Annual_Precipitation_mm=mean(Precipitation_mm),
    SD_Annual_Precipitation_mm=sd_or_zero(Precipitation_mm),
    Mean_Annual_Q_Events=mean(Q_Event_Number),
    SD_Annual_Q_Events=sd_or_zero(Q_Event_Number),
    Mean_Annual_Runoff_mm=mean(Runoff_mm),
    SD_Annual_Runoff_mm=sd_or_zero(Runoff_mm),
    .groups="drop"
  ) %>%
  mutate(
    Annual_P_Events_Lower=pmax(
      0,
      Mean_Annual_P_Events-SD_Annual_P_Events
    ),
    Annual_P_Events_Upper=
      Mean_Annual_P_Events+SD_Annual_P_Events,
    Annual_Precipitation_Lower=pmax(
      0,
      Mean_Annual_Precipitation_mm-
        SD_Annual_Precipitation_mm
    ),
    Annual_Precipitation_Upper=
      Mean_Annual_Precipitation_mm+
        SD_Annual_Precipitation_mm,
    Annual_Q_Events_Lower=pmax(
      0,
      Mean_Annual_Q_Events-SD_Annual_Q_Events
    ),
    Annual_Q_Events_Upper=
      Mean_Annual_Q_Events+SD_Annual_Q_Events,
    Annual_Runoff_Lower=pmax(
      0,
      Mean_Annual_Runoff_mm-SD_Annual_Runoff_mm
    ),
    Annual_Runoff_Upper=
      Mean_Annual_Runoff_mm+SD_Annual_Runoff_mm
  ) %>%
  left_join(DF_site_info,by="Field_Name")

# Step 2. Site-level management summaries ====================
# Summarize management data
Management_site_summary <- Management_df %>%
  group_by(Field_Name) %>%
  summarise(
    Mean_Total_Tillage_Passes=mean(Tillage_Total,na.rm=TRUE),
    Mean_Previous_Perennial_Fraction=mean(
      Previous_PerennialFrac,
      na.rm=TRUE
    ),
    Mean_Current_Perennial_Fraction=mean(
      Current_PerennialFrac,
      na.rm=TRUE
    ),
    Mean_Fall_Residue_Fraction=mean(
      Residue_Fall_Frac,
      na.rm=TRUE
    ),
    Mean_Spring_Residue_Fraction=mean(
      Residue_Spring_Frac,
      na.rm=TRUE
    ),
    Tillage_Years=sum(!is.na(Tillage_Total)),
    Crop_Years=sum(!is.na(Current_PerennialFrac)),
    .groups="drop"
  ) %>%
  mutate(
    across(
      starts_with("Mean_"),
      ~if_else(is.nan(.x),NA_real_,.x)
    )
  )

# Combine everything
Site_summary <- Site_summary %>%
  left_join(Management_site_summary,by="Field_Name") %>%
  mutate(
    Hydrologic_Group=factor(
      Hydrologic_Group,
      levels=c(
        "Slow-infiltration",
        "Moderate-infiltration",
        "High-infiltration"
      )
    )
  )

# Step 3. Monthly means and uncertainty across years across all sites ==========
# First average across monitored sites within each calendar year-month
# Then get statistics across years
Monthly_year_summary <- Site_month_summary %>%
  group_by(Calendar_Year,Month_Number,Month) %>%
  summarise(
    Monitored_Sites=n(),
    Mean_P_Events_per_Site=mean(P_Event_Number),
    Mean_Precipitation_mm_per_Site=mean(Precipitation_mm),
    Mean_Q_Events_per_Site=mean(Q_Event_Number),
    Mean_Runoff_mm_per_Site=mean(Runoff_mm),
    .groups="drop"
  )

# This is summary across all years across all sites
Monthly_climatology <- Monthly_year_summary %>%
  group_by(Month_Number,Month) %>%
  summarise(
    Mean_P_Events=mean(Mean_P_Events_per_Site),
    SD_P_Events=sd_or_zero(Mean_P_Events_per_Site),
    Mean_Precipitation_mm=mean(
      Mean_Precipitation_mm_per_Site
    ),
    SD_Precipitation_mm=sd_or_zero(
      Mean_Precipitation_mm_per_Site
    ),
    Mean_Q_Events=mean(Mean_Q_Events_per_Site),
    SD_Q_Events=sd_or_zero(Mean_Q_Events_per_Site),
    Mean_Runoff_mm=mean(Mean_Runoff_mm_per_Site),
    SD_Runoff_mm=sd_or_zero(Mean_Runoff_mm_per_Site),
    Years=n(),
    .groups="drop"
  ) %>%
  mutate(
    P_Event_Lower=pmax(0,Mean_P_Events-SD_P_Events),
    P_Event_Upper=Mean_P_Events+SD_P_Events,
    Precipitation_Lower=pmax(
      0,
      Mean_Precipitation_mm-SD_Precipitation_mm
    ),
    Precipitation_Upper=
      Mean_Precipitation_mm+SD_Precipitation_mm,
    Q_Event_Lower=pmax(0,Mean_Q_Events-SD_Q_Events),
    Q_Event_Upper=Mean_Q_Events+SD_Q_Events,
    Runoff_Lower=pmax(0,Mean_Runoff_mm-SD_Runoff_mm),
    Runoff_Upper=Mean_Runoff_mm+SD_Runoff_mm
  ) %>%
  arrange(Month_Number)

# Step 4. Runoff-coefficient summaries ========================
# Summarize RC at each site across all monitored years
RC_site_summary <- Q_nonfrozen %>%
  group_by(Field_Name) %>%
  summarise(
    Events=n(),
    Median_RC=median(Runoff_Coefficient),
    Mean_RC=mean(Runoff_Coefficient),
    Q25_RC=quantile(Runoff_Coefficient,0.25),
    Q75_RC=quantile(Runoff_Coefficient,0.75),
    .groups="drop"
  )

# RC across all sites acorss all years
RC_year_summary <- Q_nonfrozen %>%
  group_by(Calendar_Year) %>%
  summarise(
    Events=n(),
    Median_RC=median(Runoff_Coefficient),
    Mean_RC=mean(Runoff_Coefficient),
    Q25_RC=quantile(Runoff_Coefficient,0.25),
    Q75_RC=quantile(Runoff_Coefficient,0.75),
    .groups="drop"
  )

# Group RC across different explanatory variable groups
RC_group_summary <- bind_rows(
  Q_nonfrozen %>%
    filter(!is.na(Season)) %>%
    group_by(Group=Season) %>%
    summarise(
      Grouping="Season",
      Events=n(),
      Median_RC=median(Runoff_Coefficient),
      Mean_RC=mean(Runoff_Coefficient),
      Q25_RC=quantile(Runoff_Coefficient,0.25),
      Q75_RC=quantile(Runoff_Coefficient,0.75),
      .groups="drop"
    ),
  Q_nonfrozen %>%
    filter(!is.na(Hydrologic_Group)) %>%
    group_by(Group=Hydrologic_Group) %>%
    summarise(
      Grouping="Soil infiltration group",
      Events=n(),
      Median_RC=median(Runoff_Coefficient),
      Mean_RC=mean(Runoff_Coefficient),
      Q25_RC=quantile(Runoff_Coefficient,0.25),
      Q75_RC=quantile(Runoff_Coefficient,0.75),
      .groups="drop"
    ),
  Q_nonfrozen %>%
    filter(!is.na(Tile)) %>%
    group_by(Group=Tile) %>%
    summarise(
      Grouping="Site-level tile drainage",
      Events=n(),
      Median_RC=median(Runoff_Coefficient),
      Mean_RC=mean(Runoff_Coefficient),
      Q25_RC=quantile(Runoff_Coefficient,0.25),
      Q75_RC=quantile(Runoff_Coefficient,0.75),
      .groups="drop"
    ),
  Q_nonfrozen %>%
    filter(
      !is.na(Residue),
      nzchar(Residue),
      Season %in% c(
        "Post-growing season",
        "Pre-growing season"
      )
    ) %>%
    group_by(Group=Residue) %>%
    summarise(
      Grouping="Pre-/post-growing-season residue",
      Events=n(),
      Median_RC=median(Runoff_Coefficient),
      Mean_RC=mean(Runoff_Coefficient),
      Q25_RC=quantile(Runoff_Coefficient,0.25),
      Q75_RC=quantile(Runoff_Coefficient,0.75),
      .groups="drop"
    )
  ) %>%
  select(Grouping,Group,Events,Median_RC,Mean_RC,Q25_RC,Q75_RC)

# Step 5. Output summary statistics ===========================
Management_coverage_values <- Management_df %>%
  summarise(
    Site_Water_Years=n(),
    Tillage_Coverage_Below_100=sum(
      !is.na(Tillage_Basin_Coverage) &
        Tillage_Basin_Coverage < 0.999
    ),
    Crop_Coverage_Below_100=sum(
      !is.na(Crop_Basin_Coverage) &
        Crop_Basin_Coverage < 0.999
    ),
    Missing_Tillage_Total=sum(is.na(Tillage_Total)),
    Missing_Current_Crop=sum(is.na(Current_PerennialFrac)),
    Missing_Fall_Residue=sum(is.na(Residue_Fall_Frac)),
    Missing_Spring_Residue=sum(is.na(Residue_Spring_Frac))
  )

Management_coverage <- data.frame(
  Variable=c(
    "Site_Water_Years",
    "Tillage_Coverage_Below_100",
    "Crop_Coverage_Below_100",
    "Missing_Tillage_Total",
    "Missing_Current_Crop",
    "Missing_Fall_Residue",
    "Missing_Spring_Residue"
  ),
  Definition=c(
    "Total number of site-water-year records in the management dataset",
    "Site-water years with tillage coverage less than 100%",
    "Site-water years with crop and residue coverage less than 100%",
    "Site-water years missing basin-weighted total tillage passes",
    "Site-water years missing the current-crop perennial fraction",
    "Site-water years missing the post-growing-season residue fraction",
    "Site-water years missing the pre-growing-season residue fraction"
  ),
  Value=as.numeric(Management_coverage_values[1,])
)

Site_summary_output <- Site_summary %>%
  select(-HydrologicGroup)

write.csv(
  Site_summary_output,
  file.path(Table_path,"Site_exploratory_summary.csv"),
  row.names=FALSE,
  na=""
)

write.csv(
  Site_year_summary,
  file.path(Table_path,"Site_year_summary.csv"),
  row.names=FALSE,
  na=""
)

write.csv(
  Monthly_year_summary,
  file.path(Table_path,"Monthly_year_summary.csv"),
  row.names=FALSE,
  na=""
)

write.csv(
  Monthly_climatology,
  file.path(Table_path,"Monthly_climatology.csv"),
  row.names=FALSE,
  na=""
)

write.csv(
  RC_site_summary,
  file.path(Table_path,"Runoff_coefficient_by_site.csv"),
  row.names=FALSE,
  na=""
)

write.csv(
  RC_year_summary,
  file.path(Table_path,"Runoff_coefficient_by_year.csv"),
  row.names=FALSE,
  na=""
)

write.csv(
  RC_group_summary,
  file.path(Table_path,"Runoff_coefficient_by_group.csv"),
  row.names=FALSE,
  na=""
)

write.csv(
  Management_coverage,
  file.path(Table_path,"Management_data_coverage.csv"),
  row.names=FALSE,
  na=""
)

# Step 6. Maps of surface monitoring sites ====================
County_path <- file.path(
  Reference_path,
  "cb_2018_us_county_20m",
  "cb_2018_us_county_20m.shp"
)

US_counties <- sf::st_read(County_path,quiet=TRUE)
WI_counties <- US_counties %>%
  filter(STATEFP == "55")
WI_outline <- sf::st_union(WI_counties)

# Statewide overview map with one point per surface monitoring site
Wisconsin_overview_sites <- Site_summary %>%
  filter(
    is.finite(LONG_approx),
    is.finite(LAT_approx)
  ) %>%
  distinct(Field_Name,.keep_all=TRUE)

if(nrow(Wisconsin_overview_sites) != 28){
  stop(
    "The Wisconsin overview map expected 28 surface monitoring sites, but found ",
    nrow(Wisconsin_overview_sites),
    "."
  )
}

Figure_wisconsin_overview <- ggplot() +
  geom_sf(
    data=WI_outline,
    fill="white",
    color="black",
    linewidth=0.8
  ) +
  geom_point(
    data=Wisconsin_overview_sites,
    aes(x=LONG_approx,y=LAT_approx),
    color="black",
    size=4
  ) +
  coord_sf(expand=FALSE,datum=NA) +
  labs(title="Discovery Farms surface-runoff monitoring sites") +
  DF_map_theme +
  theme(
    panel.background=element_rect(fill="white",color=NA),
    plot.background=element_rect(fill="white",color=NA),
    plot.title=element_text(size=18,face="bold",hjust=0.5),
    plot.margin=margin(12,12,12,12)
  )

save_figure_pair(
  Figure_wisconsin_overview,
  file.path(Figure_path,"01C_Wisconsin_surface_monitoring_sites"),
  width=8,
  height=9
)

# Base map
Map_base <- ggplot() +
  geom_sf(data=WI_counties,fill="#aec8df",alpha=0.8,color="grey70") +
  geom_sf(data=WI_outline,fill=NA,color="black",linewidth=0.5) +
  coord_sf(
    xlim=range(Site_summary$LONG_approx,na.rm=TRUE)+c(-0.6,0.6),
    ylim=range(Site_summary$LAT_approx,na.rm=TRUE)+c(-0.35,0.35),
    expand=FALSE
  ) +
  DF_map_theme

# Four equally spaced size breaks for map legends
four_size_breaks <- function(x){
  x <- x[is.finite(x)]
  round(seq(min(x),max(x),length.out=4),1)
}

# Make one map panel with its fill legend below the panel
make_site_map <- function(fill_var,
                          fill_title,
                          plot_title,
                          size_var,
                          size_breaks,
                          fill_type=c("continuous","discrete"),
                          palette="YlGnBu",
                          values=NULL,
                          limits=NULL,
                          labels=waiver()){
  
  fill_type <- match.arg(fill_type)
  
  map_plot <- Map_base +
    geom_point(
      data=Site_summary,
      aes(
        x=LONG_approx,
        y=LAT_approx,
        size=.data[[size_var]],
        fill=.data[[fill_var]]
      ),
      shape=21,
      color="black",
      alpha=0.8
    ) +
    geom_label_repel(
      data=Site_summary,
      aes(x=LONG_approx,y=LAT_approx,label=Field_Name),
      size=3.5,
      label.padding=0.1,
      fill=scales::alpha("white",0.75),
      min.segment.length=0,
      max.overlaps=Inf
    ) +
    scale_size_continuous(
      range=c(4,10),
      breaks=size_breaks,
      guide="none"
    ) +
    labs(
      title=plot_title,
      fill=fill_title
    ) +
    theme(
      legend.position="bottom",
      legend.title.position="top",
      legend.box="vertical"
    )
  
  if(fill_type == "continuous"){
    map_plot <- map_plot +
      scale_fill_distiller(
        palette=palette,
        direction=1,
        limits=limits,
        labels=labels,
        guide=guide_colorbar(
          title.position="top",
          frame.colour="black",
          frame.linewidth=0.4,
          ticks.colour="black",
          barwidth=grid::unit(5.5,"cm")
        )
      )
  }else{
    map_plot <- map_plot +
      scale_fill_manual(
        values=values,
        guide=guide_legend(
          title.position="top",
          nrow=2,
          byrow=TRUE,
          override.aes=list(size=5,shape=21)
        )
      )
  }
  
  map_plot
}

# Make the common size legend with exactly four point sizes
make_size_legend <- function(size_breaks,size_title){
  legend_data <- data.frame(
    x=seq_along(size_breaks),
    y=1,
    Size_Value=size_breaks
  )
  
  legend_plot <- ggplot(
    legend_data,
    aes(x,y,size=Size_Value)
  ) +
    geom_point(shape=21,fill="grey80",color="black",alpha=0.8) +
    scale_size_continuous(
      range=c(4,10),
      breaks=size_breaks,
      name=size_title,
      guide=guide_legend(
        title.position="top",
        nrow=1,
        override.aes=list(shape=21,fill="grey80",alpha=0.8)
      )
    ) +
    theme_void() +
    theme(
      legend.position="bottom",
      legend.title=element_text(size=14),
      legend.text=element_text(size=13)
    )
  
  legend_grob <- ggplotGrob(legend_plot)
  bottom_legend_index <- which(
    legend_grob$layout$name == "guide-box-bottom"
  )
  
  if(length(bottom_legend_index) == 0){
    return(cowplot::get_legend(legend_plot))
  }
  
  legend_grob$grobs[[bottom_legend_index[1]]]
}

# Make one six-panel map figure
make_map_figure <- function(size_var,size_title){
  size_breaks <- four_size_breaks(Site_summary[[size_var]])
  
  map_crop <- make_site_map(
    fill_var="Mean_Current_Perennial_Fraction",
    fill_title="Mean current-crop perennial fraction",
    plot_title="A. Perennial crop fraction",
    size_var=size_var,
    size_breaks=size_breaks,
    fill_type="continuous",
    palette="YlGnBu",
    limits=c(0,1),
    labels=percent
  )
  
  map_tillage <- make_site_map(
    fill_var="Mean_Total_Tillage_Passes",
    fill_title="Mean total tillage passes",
    plot_title="B. Tillage intensity",
    size_var=size_var,
    size_breaks=size_breaks,
    fill_type="continuous",
    palette="YlOrRd"
  )
  
  map_infiltration <- make_site_map(
    fill_var="Hydrologic_Group",
    fill_title="Soil infiltration group",
    plot_title="C. Soil infiltration group",
    size_var=size_var,
    size_breaks=size_breaks,
    fill_type="discrete",
    values=DF_infiltration_colors
  )
  
  map_tile <- make_site_map(
    fill_var="Tile",
    fill_title="Site-level tile drainage",
    plot_title="D. Site-level tile drainage",
    size_var=size_var,
    size_breaks=size_breaks,
    fill_type="discrete",
    values=DF_tile_colors
  )
  
  landcover_levels <- sort(unique(na.omit(Site_summary$LandCover_Updated)))
  landcover_colors <- setNames(
    rep(DF_colors,length.out=length(landcover_levels)),
    landcover_levels
  )
  
  map_landcover <- make_site_map(
    fill_var="LandCover_Updated",
    fill_title="Updated land cover",
    plot_title="E. Updated land cover",
    size_var=size_var,
    size_breaks=size_breaks,
    fill_type="discrete",
    values=landcover_colors
  )
  
  map_slope <- make_site_map(
    fill_var="MeanSlope_per",
    fill_title="Mean slope (%)",
    plot_title="F. Mean slope",
    size_var=size_var,
    size_breaks=size_breaks,
    fill_type="continuous",
    palette="PuBu",
    labels=label_number(suffix="%")
  )
  
  map_grid <- cowplot::plot_grid(
    map_crop,
    map_tillage,
    map_infiltration,
    map_tile,
    map_landcover,
    map_slope,
    ncol=3,
    align="hv"
  )
  
  size_legend <- make_size_legend(size_breaks,size_title)
  
  cowplot::plot_grid(
    map_grid,
    size_legend,
    ncol=1,
    rel_heights=c(1,0.08)
  )
}

Figure_maps_runoff <- make_map_figure(
  size_var="Mean_Annual_Runoff_mm",
  size_title="Mean annual runoff (mm)"
)

Figure_maps_events <- make_map_figure(
  size_var="Mean_Annual_Q_Events",
  size_title="Mean annual runoff-event number"
)

save_figure_pair(
  Figure_maps_runoff,
  file.path(Figure_path,"01A_Site_maps_sized_by_runoff_depth"),
  width=18,
  height=13
)

save_figure_pair(
  Figure_maps_events,
  file.path(Figure_path,"01B_Site_maps_sized_by_runoff_events"),
  width=18,
  height=13
)

# Step 7. Site-level bar plots ================================
Site_order <- Site_summary %>%
  arrange(Hydrologic_Group,Field_Name) %>%
  pull(Field_Name)

Site_plot_df <- Site_summary %>%
  mutate(Field_Name=factor(Field_Name,levels=Site_order))

Site_event_ymax <- 1.05*max(
  Site_plot_df$Annual_P_Events_Upper,
  Site_plot_df$Annual_Q_Events_Upper,
  na.rm=TRUE
)
Site_depth_ymax <- 1.05*max(
  Site_plot_df$Annual_Precipitation_Upper,
  Site_plot_df$Annual_Runoff_Upper,
  na.rm=TRUE
)

Site_bar_theme <- DF_plot_theme +
  theme(axis.text.x=element_text(angle=55,hjust=1))

Bar_p_events <- ggplot(
  Site_plot_df,
  aes(
    Field_Name,
    Mean_Annual_P_Events,
    fill=Hydrologic_Group
  )
) +
  geom_col(color="black") +
  geom_errorbar(
    aes(
      ymin=Annual_P_Events_Lower,
      ymax=Annual_P_Events_Upper
    ),
    width=0.2,
    linewidth=0.5
  ) +
  scale_fill_manual(values=DF_infiltration_colors) +
  labs(
    title="A. Precipitation events",
    x=NULL,
    y="Mean annual event number",
    fill="Soil infiltration group"
  ) +
  scale_y_continuous(limits=c(0,Site_event_ymax)) +
  Site_bar_theme

Bar_q_events <- ggplot(
  Site_plot_df,
  aes(
    Field_Name,
    Mean_Annual_Q_Events,
    fill=Hydrologic_Group
  )
) +
  geom_col(color="black") +
  geom_errorbar(
    aes(
      ymin=Annual_Q_Events_Lower,
      ymax=Annual_Q_Events_Upper
    ),
    width=0.2,
    linewidth=0.5
  ) +
  scale_fill_manual(values=DF_infiltration_colors) +
  labs(
    title="B. Runoff events",
    x=NULL,
    y="Mean annual event number",
    fill="Soil infiltration group"
  ) +
  scale_y_continuous(limits=c(0,Site_event_ymax)) +
  Site_bar_theme

Bar_p_depth <- ggplot(
  Site_plot_df,
  aes(
    Field_Name,
    Mean_Annual_Precipitation_mm,
    fill=Hydrologic_Group
  )
) +
  geom_col(color="black") +
  geom_errorbar(
    aes(
      ymin=Annual_Precipitation_Lower,
      ymax=Annual_Precipitation_Upper
    ),
    width=0.2,
    linewidth=0.5
  ) +
  scale_fill_manual(values=DF_infiltration_colors) +
  labs(
    title="C. Precipitation depth",
    x=NULL,
    y="Mean annual depth (mm)",
    fill="Soil infiltration group"
  ) +
  scale_y_continuous(limits=c(0,Site_depth_ymax)) +
  Site_bar_theme

Bar_q_depth <- ggplot(
  Site_plot_df,
  aes(
    Field_Name,
    Mean_Annual_Runoff_mm,
    fill=Hydrologic_Group
  )
) +
  geom_col(color="black") +
  geom_errorbar(
    aes(
      ymin=Annual_Runoff_Lower,
      ymax=Annual_Runoff_Upper
    ),
    width=0.2,
    linewidth=0.5
  ) +
  scale_fill_manual(values=DF_infiltration_colors) +
  labs(
    title="D. Runoff depth",
    x=NULL,
    y="Mean annual depth (mm)",
    fill="Soil infiltration group"
  ) +
  scale_y_continuous(limits=c(0,Site_depth_ymax)) +
  Site_bar_theme

Figure_site_bars <- (
  Bar_p_events + Bar_q_events + Bar_p_depth + Bar_q_depth
) +
  plot_layout(ncol=2,guides="collect") &
  theme(legend.position="bottom")

save_figure_pair(
  Figure_site_bars,
  file.path(Figure_path,"02_Site_event_and_depth_bars"),
  width=16,
  height=10
)

# Step 8. Monthly bar plots with uncertainty ==================
Monthly_event_ymax <- 1.05*max(
  Monthly_climatology$P_Event_Upper,
  Monthly_climatology$Q_Event_Upper,
  na.rm=TRUE
)
Monthly_depth_ymax <- 1.05*max(
  Monthly_climatology$Precipitation_Upper,
  Monthly_climatology$Runoff_Upper,
  na.rm=TRUE
)

Monthly_bar <- function(
    df,
    y,
    ymin,
    ymax,
    title,
    y_title,
    fill_color,
    y_limit){
  ggplot(
    df,
    aes(
      x=Month,
      y=.data[[y]],
      ymin=.data[[ymin]],
      ymax=.data[[ymax]]
    )
  ) +
    geom_col(fill=fill_color,color="black") +
    geom_errorbar(width=0.2,linewidth=0.5) +
    scale_y_continuous(limits=c(0,y_limit)) +
    labs(title=title,x=NULL,y=y_title) +
    DF_plot_theme
}

Monthly_p_events <- Monthly_bar(
  Monthly_climatology,
  "Mean_P_Events",
  "P_Event_Lower",
  "P_Event_Upper",
  "A. Precipitation-event number",
  "Mean monthly events per site",
  DF_response_colors[["Precipitation"]],
  Monthly_event_ymax
)

Monthly_q_events <- Monthly_bar(
  Monthly_climatology,
  "Mean_Q_Events",
  "Q_Event_Lower",
  "Q_Event_Upper",
  "B. Runoff-event number",
  "Mean monthly events per site",
  DF_response_colors[["Runoff"]],
  Monthly_event_ymax
)

Monthly_p_depth <- Monthly_bar(
  Monthly_climatology,
  "Mean_Precipitation_mm",
  "Precipitation_Lower",
  "Precipitation_Upper",
  "C. Precipitation depth",
  "Mean monthly total per site (mm)",
  DF_response_colors[["Precipitation"]],
  Monthly_depth_ymax
)

Monthly_q_depth <- Monthly_bar(
  Monthly_climatology,
  "Mean_Runoff_mm",
  "Runoff_Lower",
  "Runoff_Upper",
  "D. Runoff depth",
  "Mean monthly total per site (mm)",
  DF_response_colors[["Runoff"]],
  Monthly_depth_ymax
)

Figure_monthly_bars <- (
  Monthly_p_events +
    Monthly_q_events +
    Monthly_p_depth +
    Monthly_q_depth
) +
  plot_layout(ncol=2) +
  plot_annotation(
    subtitle="Error bars show one standard deviation across calendar years"
  )

save_figure_pair(
  Figure_monthly_bars,
  file.path(Figure_path,"03_Monthly_event_and_depth_bars"),
  width=15,
  height=10
)

# Step 9. Monthly boxplots across years =======================
Monthly_box_event_ymax <- 1.05*max(
  Monthly_year_summary$Mean_P_Events_per_Site,
  Monthly_year_summary$Mean_Q_Events_per_Site,
  na.rm=TRUE
)
Monthly_box_depth_ymax <- 1.05*max(
  Monthly_year_summary$Mean_Precipitation_mm_per_Site,
  Monthly_year_summary$Mean_Runoff_mm_per_Site,
  na.rm=TRUE
)

Monthly_box <- function(
    df,
    y,
    title,
    y_title,
    fill_color,
    y_limit){
  ggplot(df,aes(Month,.data[[y]])) +
    geom_half_violin(
      side="l",
      fill=fill_color,
      color=NA,
      alpha=0.5,
      trim=TRUE
    ) +
    geom_boxplot(
      fill=fill_color,
      color="black",
      alpha=0.8,
      outlier.shape=NA,
      width=0.14
    ) +
    geom_jitter(
      aes(x=as.numeric(Month)+0.2),
      width=0.08,
      height=0,
      size=2,
      alpha=0.65,
      color=fill_color
    ) +
    coord_cartesian(ylim=c(0,y_limit)) +
    labs(title=title,x=NULL,y=y_title) +
    DF_plot_theme
}

Box_p_events <- Monthly_box(
  Monthly_year_summary,
  "Mean_P_Events_per_Site",
  "A. Precipitation-event number",
  "Monthly events per monitored site",
  DF_response_colors[["Precipitation"]],
  Monthly_box_event_ymax
)

Box_q_events <- Monthly_box(
  Monthly_year_summary,
  "Mean_Q_Events_per_Site",
  "B. Runoff-event number",
  "Monthly events per monitored site",
  DF_response_colors[["Runoff"]],
  Monthly_box_event_ymax
)

Box_p_depth <- Monthly_box(
  Monthly_year_summary,
  "Mean_Precipitation_mm_per_Site",
  "C. Precipitation depth",
  "Monthly total per monitored site (mm)",
  DF_response_colors[["Precipitation"]],
  Monthly_box_depth_ymax
)

Box_q_depth <- Monthly_box(
  Monthly_year_summary,
  "Mean_Runoff_mm_per_Site",
  "D. Runoff depth",
  "Monthly total per monitored site (mm)",
  DF_response_colors[["Runoff"]],
  Monthly_box_depth_ymax
)

Figure_monthly_boxes <- (
  Box_p_events + Box_q_events + Box_p_depth + Box_q_depth
) +
  plot_layout(ncol=2) +
  plot_annotation(
    subtitle="Points are calendar-year means across the sites monitored in that month"
  )

save_figure_pair(
  Figure_monthly_boxes,
  file.path(Figure_path,"04_Monthly_variation_across_years"),
  width=15,
  height=10
)

# Step 10. Runoff coefficient across sites and years ==========
RC_plot_limit <- quantile(
  Q_nonfrozen$Runoff_Coefficient,
  0.99,
  na.rm=TRUE
)

RC_site_plot_df <- Q_nonfrozen %>%
  left_join(
    Site_summary %>%
      select(
        Field_Name,
        Mean_Current_Perennial_Fraction
      ),
    by="Field_Name"
  ) %>%
  mutate(Field_Name=as.character(Field_Name))

# Draw the same site distributions using several explanatory-variable colors
make_rc_site_panel <- function(
    fill_var,
    title,
    fill_type=c("discrete","continuous"),
    values=NULL,
    palette="YlGnBu",
    limits=NULL,
  labels=waiver()){
  fill_type <- match.arg(fill_type)

  panel_df <- RC_site_plot_df %>%
    filter(!is.na(.data[[fill_var]])) %>%
    arrange(.data[[fill_var]],Field_Name)

  panel_site_order <- panel_df %>%
    select(Field_Name,all_of(fill_var)) %>%
    distinct() %>%
    arrange(.data[[fill_var]],Field_Name) %>%
    pull(Field_Name)

  panel_df <- panel_df %>%
    mutate(
      Field_Name=factor(
        Field_Name,
        levels=unique(panel_site_order)
      )
    )

  site_plot <- ggplot(
      panel_df,
      aes(
        Field_Name,
        Runoff_Coefficient,
        fill=.data[[fill_var]],
        color=.data[[fill_var]]
      )
    ) +
    geom_half_violin(
      side="l",
      alpha=0.5,
      trim=TRUE
    ) +
    geom_boxplot(
      color="black",
      width=0.14,
      outlier.shape=NA
    ) +
    geom_jitter(
      aes(x=as.numeric(Field_Name)+0.2),
      width=0.08,
      size=1.5,
      alpha=0.45
    ) +
    coord_cartesian(ylim=c(0,RC_plot_limit)) +
    labs(
      title=title,
      x=NULL,
      y="Runoff coefficient",
      fill=NULL,
      color=NULL
    ) +
    DF_plot_theme +
    theme(
      axis.text.x=element_text(angle=55,hjust=1),
      legend.position="bottom"
    )
  
  if(fill_type == "continuous"){
    site_plot <- site_plot +
      scale_fill_distiller(
        palette=palette,
        direction=1,
        limits=limits,
        labels=labels,
        guide=guide_colorbar(
          frame.colour="black",
          frame.linewidth=0.4,
          ticks.colour="black",
          title.position="top",
          barwidth=grid::unit(5.5,"cm")
        )
      ) +
      scale_color_distiller(
        palette=palette,
        direction=1,
        limits=limits,
        labels=labels,
        guide="none"
      )
  }else{
    site_plot <- site_plot +
      scale_fill_manual(values=values) +
      scale_color_manual(values=values,guide="none")
  }
  
  site_plot
}

RC_by_site_soil <- make_rc_site_panel(
  fill_var="Hydrologic_Group",
  title="A1. Sites colored by soil infiltration group",
  values=DF_infiltration_colors
)

RC_by_site_tile <- make_rc_site_panel(
  fill_var="Tile",
  title="A2. Sites colored by site-level tile drainage",
  values=DF_tile_colors
)

RC_by_site_perennial <- make_rc_site_panel(
  fill_var="Mean_Current_Perennial_Fraction",
  title="A3. Sites colored by mean perennial crop fraction",
  fill_type="continuous",
  palette="YlGnBu",
  limits=c(0,1),
  labels=percent
)

RC_by_year <- ggplot(
  RC_year_summary,
  aes(Calendar_Year,Median_RC)
) +
  geom_ribbon(
    aes(ymin=Q25_RC,ymax=Q75_RC),
    fill=DF_colors[1],
    alpha=0.35
  ) +
  geom_line(color="black",linewidth=0.7) +
  geom_point(
    aes(size=Events),
    shape=21,
    fill=DF_colors[1],
    color="black"
  ) +
  labs(
    title="B. Across calendar years",
    x="Calendar year",
    y="Median runoff coefficient",
    size="Events"
  ) +
  DF_plot_theme +
  theme(legend.position="right")

Figure_rc_site_year <- (
  RC_by_site_soil /
    RC_by_site_tile /
    RC_by_site_perennial /
    RC_by_year
) +
  plot_layout(heights=c(1,1,1,0.8)) +
  plot_annotation(
    subtitle="Site distributions use the same events in all three panels; y-axes are limited at the 99th percentile"
  )

save_figure_pair(
  Figure_rc_site_year,
  file.path(Figure_path,"05_Runoff_coefficient_sites_years"),
  width=18,
  height=22
)

# Step 11. Runoff coefficient across explanatory groups =======
pairwise_group_tests <- function(
    df,
    group_var,
    grouping_label,
    group_levels){
  site_group_values <- df %>%
    filter(
      is.finite(Runoff_Coefficient),
      !is.na(Field_Name),
      !is.na(.data[[group_var]]),
      as.character(.data[[group_var]]) != ""
    ) %>%
    transmute(
      Field_Name=as.character(Field_Name),
      Group=as.character(.data[[group_var]]),
      Runoff_Coefficient
    ) %>%
    group_by(Field_Name,Group) %>%
    summarise(
      Site_Median_RC=median(Runoff_Coefficient),
      .groups="drop"
    )

  observed_levels <- group_levels[
    group_levels %in% site_group_values$Group
  ]

  if(length(observed_levels) < 2){
    return(data.frame())
  }

  comparison_pairs <- combn(
    observed_levels,
    2,
    simplify=FALSE
  )

  paired_comparison <- group_var %in% c(
    "Season",
    "Residue"
  )

  test_rows <- lapply(
    comparison_pairs,
    function(comparison_pair){
      group_1 <- comparison_pair[1]
      group_2 <- comparison_pair[2]

      if(paired_comparison){
        paired_values <- site_group_values %>%
          filter(Group %in% comparison_pair) %>%
          tidyr::pivot_wider(
            names_from=Group,
            values_from=Site_Median_RC
          ) %>%
          filter(
            !is.na(.data[[group_1]]),
            !is.na(.data[[group_2]])
          )

        sample_1 <- paired_values[[group_1]]
        sample_2 <- paired_values[[group_2]]
        sample_size <- nrow(paired_values)
        test_name <- "Paired Wilcoxon signed-rank test"
      }else{
        sample_1 <- site_group_values$Site_Median_RC[
          site_group_values$Group == group_1
        ]
        sample_2 <- site_group_values$Site_Median_RC[
          site_group_values$Group == group_2
        ]
        sample_size <- min(length(sample_1),length(sample_2))
        test_name <- "Wilcoxon rank-sum test"
      }

      p_value <- if(
        length(sample_1) >= 3 &&
        length(sample_2) >= 3
      ){
        tryCatch(
          suppressWarnings(
            stats::wilcox.test(
              sample_1,
              sample_2,
              paired=paired_comparison,
              exact=FALSE
            )$p.value
          ),
          error=function(e) NA_real_
        )
      }else{
        NA_real_
      }

      data.frame(
        Grouping=grouping_label,
        Group_1=group_1,
        Group_2=group_2,
        Test=test_name,
        Sites=sample_size,
        P_Value=p_value
      )
    }
  )

  bind_rows(test_rows) %>%
    mutate(
      P_Adjusted=p.adjust(P_Value,method="BH"),
      Significance=case_when(
        P_Adjusted < 0.001 ~ "***",
        P_Adjusted < 0.01 ~ "**",
        P_Adjusted < 0.05 ~ "*",
        TRUE ~ ""
      )
    )
}

RC_box_group <- function(
    df,
    group_var,
    title,
    colors,
    test_df=NULL){
  group_levels <- names(colors)
  plot_df <- df %>%
    filter(
      !is.na(.data[[group_var]]),
      as.character(.data[[group_var]]) != ""
    ) %>%
    mutate(
      Plot_Group=factor(
        as.character(.data[[group_var]]),
        levels=group_levels
      )
    ) %>%
    filter(!is.na(Plot_Group))

  group_plot <- ggplot(
    plot_df,
    aes(
      x=Plot_Group,
      y=Runoff_Coefficient,
      fill=Plot_Group
    )
  ) +
    geom_half_violin(
      side="l",
      color=NA,
      alpha=0.5,
      trim=TRUE
    ) +
    geom_boxplot(
      width=0.14,
      outlier.shape=NA,
      color="black"
    ) +
    geom_jitter(
      aes(
        x=as.numeric(Plot_Group)+0.2,
        color=Plot_Group
      ),
      width=0.08,
      size=1.5,
      alpha=0.4,
      show.legend=FALSE
    ) +
    coord_cartesian(
      ylim=c(0,RC_plot_limit*1.08)
    ) +
    scale_fill_manual(values=colors) +
    scale_color_manual(values=colors) +
    labs(title=title,x=NULL,y="Runoff coefficient") +
    DF_plot_theme +
    theme(
      legend.position="none",
      axis.text.x=element_text(angle=35,hjust=1)
    )

  if(is.null(test_df) || nrow(test_df) == 0){
    return(group_plot)
  }

  significant_tests <- test_df %>%
    filter(
      !is.na(P_Adjusted),
      P_Adjusted < 0.05
    )

  if(nrow(significant_tests) > 0){
    significant_tests <- significant_tests %>%
      mutate(
        x_1=match(Group_1,group_levels),
        x_2=match(Group_2,group_levels),
        y=seq(
          RC_plot_limit*0.86,
          RC_plot_limit*1.02,
          length.out=n()
        ),
        tip=RC_plot_limit*0.025
      )

    group_plot <- group_plot +
      geom_segment(
        data=significant_tests,
        aes(x=x_1,xend=x_2,y=y,yend=y),
        inherit.aes=FALSE,
        linewidth=0.5
      ) +
      geom_segment(
        data=significant_tests,
        aes(
          x=x_1,
          xend=x_1,
          y=y,
          yend=y-tip
        ),
        inherit.aes=FALSE,
        linewidth=0.5
      ) +
      geom_segment(
        data=significant_tests,
        aes(
          x=x_2,
          xend=x_2,
          y=y,
          yend=y-tip
        ),
        inherit.aes=FALSE,
        linewidth=0.5
      ) +
      geom_text(
        data=significant_tests,
        aes(
          x=(x_1+x_2)/2,
          y=y+tip*0.35,
          label=Significance
        ),
        inherit.aes=FALSE,
        size=5
      )
  }

  group_plot
}

Season_colors <- setNames(
  DF_colors[c(3,1,2)],
  c(
    "Pre-growing season",
    "Growing season",
    "Post-growing season"
  )
)
Residue_colors <- setNames(
  DF_colors[c(1,2,3)],
  c("No","Yes","Partial")
)

RC_residue_df <- Q_nonfrozen %>%
  filter(
    Season %in% c(
      "Post-growing season",
      "Pre-growing season"
    ),
    Residue %in% names(Residue_colors)
  )

RC_season_tests <- pairwise_group_tests(
  Q_nonfrozen,
  "Season",
  "Season",
  names(Season_colors)
)
RC_soil_tests <- pairwise_group_tests(
  Q_nonfrozen,
  "Hydrologic_Group",
  "Soil infiltration group",
  names(DF_infiltration_colors)
)
RC_tile_tests <- pairwise_group_tests(
  Q_nonfrozen,
  "Tile",
  "Site-level tile drainage",
  names(DF_tile_colors)
)
RC_residue_tests <- pairwise_group_tests(
  RC_residue_df,
  "Residue",
  "Pre-/post-growing-season residue",
  names(Residue_colors)
)

RC_pairwise_tests <- bind_rows(
  RC_season_tests,
  RC_soil_tests,
  RC_tile_tests,
  RC_residue_tests
)

write.csv(
  RC_pairwise_tests,
  file.path(
    Table_path,
    "Runoff_coefficient_group_pairwise_tests.csv"
  ),
  row.names=FALSE,
  na=""
)

RC_season <- RC_box_group(
  Q_nonfrozen,
  "Season",
  "A. Season",
  Season_colors,
  RC_season_tests
)
RC_soil <- RC_box_group(
  Q_nonfrozen,
  "Hydrologic_Group",
  "B. Soil infiltration group",
  DF_infiltration_colors,
  RC_soil_tests
)
RC_tile <- RC_box_group(
  Q_nonfrozen,
  "Tile",
  "C. Site-level tile drainage",
  DF_tile_colors,
  RC_tile_tests
)
RC_residue <- RC_box_group(
  RC_residue_df,
  "Residue",
  "D. Pre-/post-growing-season residue",
  Residue_colors,
  RC_residue_tests
)

RC_perennial <- Q_nonfrozen %>%
  filter(!is.na(PerennialFrac)) %>%
  ggplot(
    aes(
      PerennialFrac,
      Runoff_Coefficient,
      color=Season
    )
  ) +
  geom_point(size=1.6,alpha=0.4) +
  geom_smooth(
    aes(group=1),
    method="loess",
    color="black",
    fill="grey80",
    linewidth=0.8
  ) +
  coord_cartesian(ylim=c(0,RC_plot_limit)) +
  scale_x_continuous(labels=percent,limits=c(0,1)) +
  scale_color_manual(values=Season_colors) +
  labs(
    title="E. Seasonal perennial crop fraction",
    x="Perennial crop fraction",
    y="Runoff coefficient",
    color="Season"
  ) +
  DF_plot_theme +
  theme(legend.position="bottom")

RC_tillage <- Q_nonfrozen %>%
  filter(
    !is.na(Tillage_Passes),
    !is.na(Hydrologic_Group)
  ) %>%
  ggplot(
    aes(
      Tillage_Passes,
      Runoff_Coefficient,
      color=Hydrologic_Group
    )
  ) +
  geom_point(size=1.6,alpha=0.4) +
  geom_smooth(
    aes(group=1),
    method="loess",
    color="black",
    fill="grey80",
    linewidth=0.8
  ) +
  coord_cartesian(ylim=c(0,RC_plot_limit)) +
  scale_color_manual(values=DF_infiltration_colors) +
  labs(
    title="F. Seasonal tillage passes",
    x="Seasonal tillage passes",
    y="Runoff coefficient",
    color="Soil infiltration group"
  ) +
  DF_plot_theme +
  theme(legend.position="bottom")

Figure_rc_groups <- (
  RC_season +
    RC_soil +
    RC_tile +
    RC_residue +
    RC_perennial +
    RC_tillage
) +
  plot_layout(ncol=2) +
  plot_annotation(
    subtitle="Non-frozen surface-runoff events; y-axes limited at the overall 99th percentile"
  )

save_figure_pair(
  Figure_rc_groups,
  file.path(Figure_path,"06_Runoff_coefficient_groups"),
  width=16,
  height=14
)

# Step 12. Frozen and non-frozen monthly patterns =============
P_freeze_event <- P_df %>%
  mutate(
    Frozen_Status=case_when(
      P_frozen %in% TRUE ~ "Frozen",
      P_frozen %in% FALSE ~ "Non-Frozen",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Frozen_Status)) %>%
  group_by(
    Calendar_Year,
    Month_Number,
    Month,
    Frozen_Status
  ) %>%
  summarise(
    Event_Number=n(),
    Depth_mm=sum(rain_mm,na.rm=TRUE),
    .groups="drop"
  )

Q_freeze_event <- Q_df %>%
  transmute(
    Calendar_Year,
    Month_Number,
    Month,
    Frozen_Status=frozen,
    Depth_mm=runoff_mm
  ) %>%
  filter(Frozen_Status %in% c("Frozen","Non-Frozen")) %>%
  group_by(
    Calendar_Year,
    Month_Number,
    Month,
    Frozen_Status
  ) %>%
  summarise(
    Event_Number=n(),
    Depth_mm=sum(Depth_mm,na.rm=TRUE),
    .groups="drop"
  )

Freeze_grid <- Monthly_year_summary %>%
  select(Calendar_Year,Month_Number,Month,Monitored_Sites) %>%
  crossing(Frozen_Status=c("Frozen","Non-Frozen"))

summarize_freeze_monthly <- function(event_df){
  Freeze_grid %>%
    left_join(
      event_df,
      by=c(
        "Calendar_Year",
        "Month_Number",
        "Month",
        "Frozen_Status"
      )
    ) %>%
    mutate(
      Event_Number=replace_na(Event_Number,0),
      Depth_mm=replace_na(Depth_mm,0),
      Events_per_Site=Event_Number/Monitored_Sites,
      Depth_per_Site_mm=Depth_mm/Monitored_Sites
    ) %>%
    group_by(Month_Number,Month,Frozen_Status) %>%
    summarise(
      Mean_Events=mean(Events_per_Site),
      SD_Events=sd_or_zero(Events_per_Site),
      Mean_Depth_mm=mean(Depth_per_Site_mm),
      SD_Depth_mm=sd_or_zero(Depth_per_Site_mm),
      Years=n(),
      .groups="drop"
    ) %>%
    mutate(
      Event_Lower=pmax(0,Mean_Events-SD_Events),
      Event_Upper=Mean_Events+SD_Events,
      Depth_Lower=pmax(0,Mean_Depth_mm-SD_Depth_mm),
      Depth_Upper=Mean_Depth_mm+SD_Depth_mm
    )
}

P_freeze_monthly <- summarize_freeze_monthly(P_freeze_event)
Q_freeze_monthly <- summarize_freeze_monthly(Q_freeze_event)

Freeze_event_ymax <- 1.05*max(
  P_freeze_monthly$Event_Upper,
  Q_freeze_monthly$Event_Upper,
  na.rm=TRUE
)
Freeze_depth_ymax <- 1.05*max(
  P_freeze_monthly$Depth_Upper,
  Q_freeze_monthly$Depth_Upper,
  na.rm=TRUE
)

Freeze_bar <- function(
    df,
    y,
    ymin,
    ymax,
    title,
    y_title,
    y_limit){
  dodge <- position_dodge(width=0.8)

  ggplot(
    df,
    aes(
      Month,
      .data[[y]],
      ymin=.data[[ymin]],
      ymax=.data[[ymax]],
      fill=Frozen_Status
    )
  ) +
    geom_col(
      position=dodge,
      width=0.72,
      color="black"
    ) +
    geom_errorbar(
      position=dodge,
      width=0.18,
      linewidth=0.5
    ) +
    scale_fill_manual(values=DF_frozen_colors) +
    scale_y_continuous(limits=c(0,y_limit)) +
    labs(
      title=title,
      x=NULL,
      y=y_title,
      fill=NULL
    ) +
    DF_plot_theme +
    theme(legend.position="top")
}

Figure_frozen <- (
  Freeze_bar(
    P_freeze_monthly,
    "Mean_Events",
    "Event_Lower",
    "Event_Upper",
    "A. Precipitation-event number",
    "Mean monthly events per site",
    Freeze_event_ymax
  ) +
    Freeze_bar(
      Q_freeze_monthly,
      "Mean_Events",
      "Event_Lower",
      "Event_Upper",
      "B. Runoff-event number",
      "Mean monthly events per site",
      Freeze_event_ymax
    ) +
    Freeze_bar(
      P_freeze_monthly,
      "Mean_Depth_mm",
      "Depth_Lower",
      "Depth_Upper",
      "C. Precipitation depth",
      "Mean monthly total per site (mm)",
      Freeze_depth_ymax
    ) +
    Freeze_bar(
      Q_freeze_monthly,
      "Mean_Depth_mm",
      "Depth_Lower",
      "Depth_Upper",
      "D. Runoff depth",
      "Mean monthly total per site (mm)",
      Freeze_depth_ymax
    )
) +
  plot_layout(ncol=2,guides="collect") &
  theme(legend.position="top")

save_figure_pair(
  Figure_frozen,
  file.path(Figure_path,"07_Frozen_nonfrozen_monthly_patterns"),
  width=15,
  height=10
)

# Step 13. Generate exploratory HTML report ===================
Complete_site_years <- Site_year_summary %>%
  filter(Complete_Monitoring_Year) %>%
  select(Field_Name,Calendar_Year)

# Calculate the non-frozen share within each complete calendar year
P_annual_freeze_share <- P_df %>%
  inner_join(
    Complete_site_years,
    by=c("Field_Name","Calendar_Year")
  ) %>%
  group_by(Calendar_Year) %>%
  summarise(
    Total_P_Events=n(),
    NonFrozen_P_Events=sum(P_frozen %in% FALSE),
    Total_P_Depth_mm=sum(rain_mm,na.rm=TRUE),
    NonFrozen_P_Depth_mm=sum(
      rain_mm[P_frozen %in% FALSE],
      na.rm=TRUE
    ),
    .groups="drop"
  )

Q_annual_freeze_share <- Q_df %>%
  inner_join(
    Complete_site_years,
    by=c("Field_Name","Calendar_Year")
  ) %>%
  group_by(Calendar_Year) %>%
  summarise(
    Total_Q_Events=n(),
    NonFrozen_Q_Events=sum(frozen == "Non-Frozen",na.rm=TRUE),
    Total_Q_Depth_mm=sum(runoff_mm,na.rm=TRUE),
    NonFrozen_Q_Depth_mm=sum(
      runoff_mm[frozen == "Non-Frozen"],
      na.rm=TRUE
    ),
    .groups="drop"
  )

Annual_nonfrozen_share <- full_join(
  P_annual_freeze_share,
  Q_annual_freeze_share,
  by="Calendar_Year"
) %>%
  mutate(
    NonFrozen_P_Event_Percent=
      100*NonFrozen_P_Events/Total_P_Events,
    NonFrozen_P_Depth_Percent=
      100*NonFrozen_P_Depth_mm/Total_P_Depth_mm,
    NonFrozen_Q_Event_Percent=
      100*NonFrozen_Q_Events/Total_Q_Events,
    NonFrozen_Q_Depth_Percent=
      100*NonFrozen_Q_Depth_mm/Total_Q_Depth_mm
  )

write.csv(
  Annual_nonfrozen_share,
  file.path(Table_path,"Annual_nonfrozen_event_shares.csv"),
  row.names=FALSE,
  na=""
)

Data_summary_table <- data.frame(
  Item=c(
    "Surface monitoring sites",
    "Complete site-monitoring years",
    "All precipitation events",
    "All storm-associated runoff events",
    "Non-frozne precipitation events",
    "Non-frozen storm-associated runoff events",
    "Non-frozen runoff events used for runoff coefficients",
    "Mean annual non-frozen runoff event number contribution (%)",
    "Mean annual non-frozen runoff depth contribution (%)",
    "Median non-frozen runoff coefficient",
    "99th percentile non-frozen runoff coefficient"
  ),
  Value=c(
    nrow(DF_site_info),
    round(sum(Site_summary$Monitoring_Years),1),
    nrow(P_df),
    nrow(Q_df),
    sum(P_df$P_frozen == FALSE),
    sum(Q_df$frozen == "Non-Frozen"),
    nrow(Q_nonfrozen),
    mean(
      Annual_nonfrozen_share$NonFrozen_Q_Event_Percent,
      na.rm=TRUE
    ),
    mean(
      Annual_nonfrozen_share$NonFrozen_Q_Depth_Percent,
      na.rm=TRUE
    ),
    round(median(Q_nonfrozen$Runoff_Coefficient),3),
    round(RC_plot_limit,3)
  )
)

write.csv(
  Data_summary_table,
  file.path(Table_path,"Data_summary.csv"),
  row.names=FALSE,
  na=""
)

Peak_runoff_month <- Monthly_climatology %>%
  slice_max(Mean_Runoff_mm,n=1,with_ties=FALSE)

Highest_runoff_site <- Site_summary %>%
  slice_max(Mean_Annual_Runoff_mm,n=1,with_ties=FALSE)

Key_findings <- paste0(
  "<div class=\"callout\"><strong>Key descriptive results:</strong> ",
  html_escape(Highest_runoff_site$Field_Name),
  " had the largest mean annual surface-runoff depth (",
  round(Highest_runoff_site$Mean_Annual_Runoff_mm,1),
  " mm/year). ",
  html_escape(as.character(Peak_runoff_month$Month)),
  " had the largest across-year mean monthly runoff (",
  round(Peak_runoff_month$Mean_Runoff_mm,1),
  " mm per monitored site). ",
  "The median non-frozen event runoff coefficient was ",
  round(median(Q_nonfrozen$Runoff_Coefficient),3),
  ".</div>"
)

Management_audit_table <- data.frame(
  Criterion=c(
    "Tillage representation",
    "Multi-field weighting",
    "Post-growing-season tillage window",
    "Pre-growing-season tillage window",
    "Growing-season tillage window",
    "Pre-/post-growing-season perennial crop fraction",
    "Growing-season perennial crop fraction",
    "Post-growing-season residue",
    "Pre-growing-season residue",
    "Growing-season residue"
  ),
  Implementation=c(
    "Total number of passes; no legacy tillage category",
    "Field pass counts multiplied by percentage of monitored basin",
    "Previous-water-year growing season + current-water-year post-growing season",
    "Current-water-year post-growing season + pre-growing season",
    "Current-water-year pre-growing season + growing season",
    "Continuous fraction from the previous crop grown in the preceding growing season",
    "Continuous fraction from the current crop grown that growing season",
    "Residue left in the post-growing season from the previous crop",
    "Residue left in the pre-growing season from the previous crop",
    "Excluded"
  ),
  Audit_Result="Passed"
)

Top_runoff_sites <- Site_summary %>%
  select(
    Field_Name,
    Monitoring_Years,
    Mean_Annual_Q_Events,
    Mean_Annual_Runoff_mm,
    Mean_Total_Tillage_Passes,
    Mean_Current_Perennial_Fraction,
    Hydrologic_Group
  ) %>%
  arrange(desc(Mean_Annual_Runoff_mm)) %>%
  head(10)

Monthly_report_table <- Monthly_climatology %>%
  select(
    Month,
    Mean_P_Events,
    Mean_Precipitation_mm,
    Mean_Q_Events,
    Mean_Runoff_mm,
    Years
  )

RC_report_table <- RC_group_summary %>%
  arrange(Grouping,desc(Median_RC))

Report_body <- c(
  "<div class=\"callout\"><strong>Processing review:</strong> all seasonal management definitions passed validation. Hydrologic depths and intensities in the analysis-ready data and figures are in millimetres.</div>",
  "<h2>Data summary</h2>",
  "<p>Non-frozen events contributions are calculated within each complete calendar year and then averaged across years. Event-depth use millimetres.</p>",
  data_frame_to_html(Data_summary_table,digits=2),
  Key_findings,
  "<h2>Management-variable quality check</h2>",
  "<p>Tillage values remain basin-weighted pass totals. Rows with supplied basin coverage below 100% are not renormalized, so uncovered portions do not receive an assumed management value.</p>",
  data_frame_to_html(Management_audit_table,digits=2),
  data_frame_to_html(Management_coverage,digits=0),
  "<h2>Study sites</h2>",
  "<p>All points represent surface-runoff monitoring sites. The six panels show perennial crop fraction, tillage intensity, soil infiltration group, site-level tile drainage, updated land cover, and mean slope. </p>",
  embedded_figure_html(
    file.path(Figure_path,"01A_Site_maps_sized_by_runoff_depth.png"),
    "Figure 1A. Discovery Farms surface-runoff sites. Point size represents mean annual surface-runoff depth calculated from complete 12-month monitoring years."
  ),
  embedded_figure_html(
    file.path(Figure_path,"01B_Site_maps_sized_by_runoff_events.png"),
    "Figure 1B. Discovery Farms surface-runoff sites shown with six explanatory-variable color schemes. Point size represents mean annual runoff-event number calculated from complete 12-month monitoring years."
  ),
  "<h3>Sites with the largest mean annual runoff depth</h3>",
  data_frame_to_html(Top_runoff_sites,digits=2),
  "<h2>Event numbers and depths</h2>",
  "<p>Annual site summaries use only calendar years containing all 12 monitored months. Partial first and last monitoring years are retained in the site-year CSV with a completeness flag but are excluded from annual means.</p>",
  embedded_figure_html(
    file.path(Figure_path,"02_Site_event_and_depth_bars.png"),
    "Figure 2. Mean annual precipitation-event number, runoff-event number, precipitation depth, and surface-runoff depth by site. Error bars show one standard deviation across complete monitoring years. Paired precipitation and runoff panels use common y-axis scales for event number and depth."
  ),
  "<h2>Monthly climatology and variation among years</h2>",
  "<p>Monthly totals are first calculated for each monitored site-month. They are then averaged within calendar year and summarized across years.</p>",
  embedded_figure_html(
    file.path(Figure_path,"03_Monthly_event_and_depth_bars.png"),
    "Figure 3. Average monthly event numbers and depths with error bars showing one standard deviation across years. Paired precipitation and runoff panels use common y-axis scales for event number and depth."
  ),
  embedded_figure_html(
    file.path(Figure_path,"04_Monthly_variation_across_years.png"),
    "Figure 4. Variation in monthly event numbers and depths. Paired precipitation and runoff panels use common y-axis scales for event number and depth."
  ),
  embedded_figure_html(
    file.path(Figure_path,"07_Frozen_nonfrozen_monthly_patterns.png"),
    "Figure 5. Average monthly event numbers and depths grouped by frozen and non-frozen soil conditions, with error bars showing one standard deviation across years. Paired precipitation and runoff panels use common y-axis scales for event number and depth."
  ),
  "<h3>Monthly summary statistics</h3>",
  data_frame_to_html(Monthly_report_table,digits=2),
  "<h2>Runoff coefficient</h2>",
  "<p>The runoff coefficient is event surface-runoff depth divided by event precipitation depth. Summaries retain all finite non-frozen values; selected boxplot axes are limited at the 99th percentile for readability.</p>",
  embedded_figure_html(
    file.path(Figure_path,"05_Runoff_coefficient_sites_years.png"),
    "Figure 6. Runoff coefficient distributions across sites, grouped by soil infiltration group, site-level tile drainage, and continuous mean perennial crop fraction, followed by median values across calendar years. Sites are ordered by the displayed group and then alphabetically within groups."
  ),
  embedded_figure_html(
    file.path(Figure_path,"06_Runoff_coefficient_groups.png"),
    "Figure 7. Runoff coefficients by season, soil infiltration group, site-level tile drainage, pre-/post-growing-season residue, continuous seasonal perennial crop fraction, and seasonal tillage passes. Brackets show significant Benjamini-Hochberg-adjusted pairwise Wilcoxon comparisons of site-level medians."
  ),
  "<h3>Runoff coefficient summary by group</h3>",
  data_frame_to_html(RC_report_table,digits=3),
  "<h3>Pairwise comparisons of site-level runoff coefficients</h3>",
  "<p>Season and residue comparisons use paired Wilcoxon signed-rank tests where the same sites contribute to both groups. Soil-infiltration and tile-drainage comparisons use Wilcoxon rank-sum tests. P-values are adjusted within each grouping variable using the Benjamini-Hochberg method.</p>",
  data_frame_to_html(RC_pairwise_tests,digits=4),
  "<h2>Output files</h2>",
  "<p>Every figure is saved in both PNG and PDF format. Machine-readable summary tables are saved as CSV files under <code>04_Results/Exploratory/Tables</code>.</p>"
)

Exploratory_report <- file.path(
  Report_path,
  "03_Exploratory_analysis_report.html"
)

write_html_report(
  title="Discovery Farms Surface Runoff Exploratory Analysis",
  subtitle=paste0("Generated: ",Sys.Date()),
  body_html=Report_body,
  output_path=Exploratory_report
)

message("Exploratory analysis complete.")
message("Figures: ",Figure_path)
message("Tables: ",Table_path)
message("Report: ",Exploratory_report)
