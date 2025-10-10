# Author: Zhaozhe Chen
# Date: 2025.10.9

# This code is to compare daily P from USGS sites vs PRISM
# Note: USGS ppt date, PRISM data are all UTC time
# Output ppt unit: mm

# -------- Global -----------
library(dplyr)
library(lubridate)
library(dataRetrieval) # Used to extract USGS ppt data
# https://doi-usgs.github.io/dataRetrieval/index.html
# https://cran.r-project.org/web/packages/dataRetrieval/vignettes/dataRetrieval.html
library(stringr)
library(tidyr)

# Data paths =====
# Cleaned EOF dataframe
eof_df <- read.csv("00_Data/Processed_data/Cleaned_data/DF_EOF_cleaned.csv")
# PRISM ppt
PRISM_ppt_df <- read.csv("00_Data/Processed_data/DF_PRISM_ppt.csv")
# Source functions
source("Functions/Data_processing_functions.R")
source("Functions/Plotting_functions.R")

# Output path for subdaily USGS P
P_output_path <- "D:/OneDrive - UW-Madison/Research/Discovery Farms/DF Runoff Generation/Results/USGS_subdaily_P/"
# Output path for plots
Output_path <- "D:/OneDrive - UW-Madison/Research/Discovery Farms/DF Runoff Generation/Results/USGS_vs_PRISM_daily_P/"

# Global parameters =======
# Code for precipitation. Unit: in 
P_code <- "00045"

# ------------ Main ----------
# Get Site info for USGS P extraction
Site_ls <- eof_df %>%
  distinct(Field_Name,USGS_Station_Number,StudyPeriod) %>%
  arrange(Field_Name) %>%
  rename(Site_ID = Field_Name,
         USGS_ID = USGS_Station_Number) %>%
  mutate(USGS_ID = str_remove(USGS_ID,"USGS-")) %>%
  # Extract start and end date
  mutate(
    WY_Start = as.integer(str_extract(StudyPeriod,"((?<=WY)\\d{4})")),
    WY_End = as.integer(str_extract(StudyPeriod,"((?<=-WY)\\d{4})")),
    Start = paste0(WY_Start - 1,"-10-01"),
    End = paste0(WY_End,"-09-30")
  )

for(arrayid in 6:nrow(Site_ls)){
  # Site info =====================
  # Site to process
  Site_ID <- Site_ls$Site_ID[arrayid]
  # The corresponding USGS_ID
  USGS_ID <- Site_ls$USGS_ID[arrayid]
  # Start and end date of study period at this site
  start <- Site_ls$Start[arrayid]
  end <- Site_ls$End[arrayid]
  
  # USGS P processing =====================
  # Extract sub daily USGS P, Unit: mm
  USGS_subd_P <- tryCatch(
    {USGS_ppt(USGS_ID, start, end)},
    error = function(e) {
      message("Skipping ", Site_ID, " due to error: ", e$message)
      return(NULL)
    }
  )
  
  # If extraction failed, skip to next site
  if (is.null(USGS_subd_P)) next
  
  # Output this sub daily USGS df
  write.csv(USGS_subd_P,paste0(P_output_path,"USGS_subdaily_P_",Site_ID,".csv"))
  
  # Aggregate to daily P
  USGS_d_P <- USGS_subd_P %>%
    mutate(Date = as.Date(dateTime,tz="UTC")) %>%
    group_by(Date) %>%
    summarize(USGS_P_mm = sum(Precip_Inst_mm,na.rm=TRUE))
  
  # Remove last row (XXXX-10-01)
  USGS_d_P <- USGS_d_P[-nrow(USGS_d_P),]
  
  # PRISM P processing =======================
  # Extract daily P from PRISM for the same site
  PRISM_d_P <- data.frame(Date = PRISM_ppt_df$Date,
                          PRISM_P_mm = PRISM_ppt_df[[Site_ID]])
  # Filter date to be the same as USGS P
  PRISM_d_P <- PRISM_d_P %>%
    mutate(Date = ymd(Date)) %>%
    filter(Date >= as.Date(start),
           Date <= as.Date(end))
  
  # Join the two df for comparison
  ppt_df <- USGS_d_P %>%
    left_join(PRISM_d_P,by="Date")
  
  # Make plots for comparison =============================
  # Make scatter plots for comparison
  g_scatter <- ggplot(data=ppt_df,aes(x = USGS_P_mm,y=PRISM_P_mm))+
    geom_pointdensity(alpha=0.8)+
    scale_color_viridis_c(option = "D", name = "Density") +
    geom_smooth(method = "lm", color = "black", se = FALSE, linetype = "dashed") +
    stat_cor(
      method = "pearson",
      cor.coef.name = "rho",
      label.x.npc = "left",      
      label.y.npc = "top",        
      label.sep = "\n",   
      size = 6
    )  +
    labs(
      x = "USGS Daily P (mm)",
      y = "PRISM Daily P (mm)",
      title = Site_ID
    )+
    theme(aspect.ratio = 1:1)+
    my_theme2
  
  # Make TS plots
  ppt_df_long <- ppt_df %>%
    pivot_longer(
      cols = c(USGS_P_mm,PRISM_P_mm),
      names_to = "Source",
      values_to = "Daily_P_mm"
    )
  
  g_TS <- ggplot(data=ppt_df_long)+
    geom_segment(aes(x=Date,xend = Date,y=0,yend = Daily_P_mm,color=Source),
                 alpha=0.8)+
    my_theme2+
    scale_color_manual(
      values = c("PRISM_P_mm" = "darkorange",
                 "USGS_P_mm" = "steelblue"),
      labels = c("PRISM_P_mm" = "PRISM",
                 "USGS_P_mm" = "USGS")
    )+
    labs(x="",y="Daily P (mm)",color="")+
    theme(legend.position = c(0.8,0.8),
          legend.background = element_blank())
  
  # Combine two plots
  g_Site <- plot_grid(g_scatter,g_TS,nrow=1,
                      align = "h",axis="tb",labels = "auto")

  # Output this figure
  print_g(g_Site,paste0("USGS_vs_PRISM_daily_P_",Site_ID),
          10,4)
  message(arrayid)
}




