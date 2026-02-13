# Author: Zhaozhe Chen
# Date: 2026.2.13

# This code is to explore Q runoff coefficient (RC)

# Seasons to consider
# All
# Growing season (GS): May - Sep 
# Spring shoulder (SS): Jan - April 
# Fall shoulder (FS): Oct - Dec

# ---------- Global -----------
library(stringr)
library(dplyr)
library(lubridate)
library(cowplot)
library(lme4)
library(GGally)
library(ggeffects)
library(performance)

# Data path =======
# Joint non-frozen Q events
Q_df_all <- read.csv("00_Data/Processed_data_v2/Non-Frozen_Q_joint_df.csv")

# Source functions
source("01_Codes/Plotting_functions.R")
source("01_Codes/Analyses_functions.R")

# Colors for plotting 
my_color <- RColorBrewer::brewer.pal(7,"Set2")

# Decide if only focus on Surface monitoring sites and filter out Tile sites
Tile_Y <- "NOTile"

# Output path
Output_path <- "D:/OneDrive - UW-Madison/Research/Discovery Farms/DF Runoff Generation/Results/RC_modeling/"

# ----- Main -------
season <- "SS"

# Preprocessing Q dataset =================
Q_df <- Q_df_all

# If Tile_Y is TRUE, keep Tile sites, otherwise, filter them out
if(Tile_Y == "NOTile"){
  Q_df <- Q_df %>%
    filter(Monitoring == "Surface")
}

# Trunk to the target season
if(season == "GS"){
  # Growing season: May - Sep
  Q_df <- Q_df %>%
    filter(month(Q_df$Q_start) < 10 & month(Q_df$Q_start) > 4)
}else if(season == "SS"){
  # Spring shoulder season: Jan - April
  Q_df <- Q_df %>%
    filter(month(Q_df$Q_start) <  5)
}else if(season == "FS"){
  # Fall shoulder season: Oct - Dec
  Q_df <- Q_df %>%
    filter(month(Q_df$Q_start) > 9)
}

Q_df <- Q_df %>%
  # Runoff coefficient is the response
  mutate(RC = runoff_in/rain_in) %>%
  # Filter out tiny rain events
  filter(rain_in > 0) %>%
  # Filter out extreme RC values
  filter(RC < 5) %>%
  # Convert factors and transform skewed predictors
  mutate(Field_Name = factor(Field_Name),
         Tile = factor(Tile),
         Tillage = factor(Tillage),
         Annual_Tillage = factor(Annual_Tillage,levels=c("Conventional","Reduced","No-Till","Pasture")),
         SoilType = factor(SoilType),
         Crop = factor(Crop),
         HydrologicGroup = factor(HydrologicGroup),
         log_P = log(rain_in),
         log_Dur = log(duration),
         log_I30 = log(I30),
         log_ARFdays7 = log(ARFdays7+0.1))

# Explore the dataset ================
# List of target explanatory variables
x_varname_ls <- c("I30","rain_in","duration","ARFdays7")
x_title_ls <- c("I30","P depth","P duration","ARFdays7")

i <- 1
x_varname <- x_varname_ls[i]
x_title <- x_title_ls[i]
Q_df_tmp <- Q_df
# Distribution of the continuous explanatory variable
g_hist_x <- Dist_bar(Q_df_tmp,var=x_varname,xtitle = x_title)
# Distribution of RC
g_hist_RC <- Dist_bar(Q_df_tmp,"RC",xtitle = "RC")
# RC vs x variable, not grouping
g_scatter_all <- plot_scatter_lm(Q_df_tmp,x=x_varname,y="RC",se=TRUE,my_colors=my_color[1],
                                 label_x = -0.2,label_y=1.5)
# RC vs x variable, grouped by Tile
g_scatter_Tile <- plot_scatter_lm(Q_df_tmp,x=x_varname,y="RC",group="Tile",se=TRUE,my_colors=my_color[c(1,2)],
                                 label_x = -0.2,label_y=1.5)
# RC vs x variable, grouped by Tillage
g_scatter_Tillage <- plot_scatter_lm(Q_df_tmp,x=x_varname,y="RC",group = "Tillage",se=TRUE,my_colors=my_color[c(1,2)],
                                     label_x = -0.2,label_y=1.5)
# RC vs x variable, grouped by Annual Tillage
g_scatter_Annual_Tillage <- plot_scatter_lm(Q_df_tmp,x=x_varname,y="RC",group = "Annual_Tillage",se=TRUE,my_colors=my_color[c(1,2,3,4)],
                                            label_x = -0.2,label_y=1.5)+
  labs(color = "")

# Combine these plots together
g_all <- plot_grid(g_hist_x,g_hist_RC,g_scatter_all,g_scatter_Tile,g_scatter_Tillage,g_scatter_Annual_Tillage,
                   nrow=2,ncol=3,aglin = "hv")
print_g(g_all,paste0("RC_",x_varname,"_",Tile_Y,"_",season),11,10)







