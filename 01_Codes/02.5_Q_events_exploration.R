# Author: Zhaozhe Chen
# Date: 2026.2.17

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
Output_path <- "D:/OneDrive - UW-Madison/Research/Discovery Farms/DF Runoff Generation/Results/RC_modeling_log/"

# ----- Main -------
for (season in c("SS","GS","FS")){
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
    mutate(RC = runoff_in/rain_in,
           log_RC = log(RC)) %>%
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
  
  # RC across Drainage classes
  g_Drainage <- plot_box(Q_df,x_varname = "DrainageClass",y_varname ="log_RC",fill_name ="DrainageClass",
                         x_title = "Drainage Class",y_title = "log RC",my_cols = my_color[c(1,2,3,4)])+
    theme(legend.position = "none")
  print_g(g_Drainage,paste0("RC_Drainage_",Tile_Y,"_",season),4,5)
  
  # List of target explanatory variables
  x_varname_ls <- c("log_I30","log_P","log_Dur","log_ARFdays7")
  x_title_ls <- c("log I30","log P depth","log P duration","log ARFdays7")
  
  for(i in 1:length(x_varname_ls)){
    x_varname <- x_varname_ls[i]
    x_title <- x_title_ls[i]
    Q_df_tmp <- Q_df
    # Distribution of the continuous explanatory variable
    g_hist_x <- Dist_bar(Q_df_tmp,var=x_varname,xtitle = x_title)
    # Distribution of RC
    g_hist_RC <- Dist_bar(Q_df_tmp,"log_RC",xtitle = "log RC")
    # RC vs x variable, not grouping
    g_scatter_all <- plot_scatter_lm(Q_df_tmp,x=x_varname,y="log_RC",se=TRUE,my_colors=my_color[1],
                                     label_x = -0.2,label_y=1.5)+
      labs(x = x_title,y="log RC")
    
    # RC vs x variable, grouped by Tile --------------
    g_scatter_Tile <- plot_scatter_lm(Q_df_tmp,x=x_varname,y="log_RC",group="Tile",se=TRUE,my_colors=my_color[c(1,2)],
                                      label_x = -0.2,label_y=1.5)+
      labs(x = x_title,y="log RC")+
      theme(legend.position = "none")
    # RC density plot across Tile
    g_density_Tile <- Density_group(Q_df_tmp,varname = "log_RC",group = "Tile",xtitle = "log RC",my_colors = my_color[c(1,2)])+
      theme(legend.position = "none")
    #RC boxplot across Tile
    g_box_Tile <- plot_box(Q_df_tmp,x_varname = "Tile",y_varname = "log_RC",fill_name="Tile",
                           x_title = "Tile",y_title = "log RC","",my_cols = my_color[c(1,2)])+
      theme(legend.position = "none")
    
    # RC vs x variable, grouped by Tillage --------------
    g_scatter_Tillage <- plot_scatter_lm(Q_df_tmp,x=x_varname,y="log_RC",group = "Tillage",se=TRUE,my_colors=my_color[c(1,2)],
                                         label_x = -0.2,label_y=1.5)+
      labs(x = x_title,y="log RC")+
      theme(legend.position = "none")
    # RC density across Tillage
    g_density_Tillage <- Density_group(Q_df_tmp,varname = "log_RC",group = "Tillage",xtitle = "log RC",my_colors = my_color[c(1,2)])+
      theme(legend.position = "none")
    # RC boxplot across Tillage
    g_box_Tillage <- plot_box(Q_df_tmp,x_varname = "Tillage",y_varname = "log_RC",fill_name = "Tillage",
                              x_title = "Tillage",y_title = "log RC",my_cols = my_color[c(1,2)])+
      theme(legend.position = "none")
    
    # RC vs x variable, grouped by Annual Tillage -----------
    g_scatter_Annual_Tillage <- plot_scatter_lm(Q_df_tmp,x=x_varname,y="log_RC",group = "Annual_Tillage",se=TRUE,my_colors=my_color[c(1,2,3,4)],
                                                label_x = -0.2,label_y=1.5)+
      labs(color = "",fill="")+
      labs(x = x_title,y="log RC")+
      theme(legend.position = "none")
    # RC density across Annual Tillage
    g_density_Annual_Tillage <- Density_group(Q_df_tmp,varname = "log_RC",group = "Annual_Tillage",xtitle = "log RC",my_colors = my_color[c(1,2,3,4)])+
      theme(legend.position = "none")
    # RC boxplot across Annual Tillage
    g_box_Annual_Tillage <- plot_box(Q_df_tmp,x_varname = "Annual_Tillage",y_varname ="log_RC",fill_name ="Annual_Tillage",
                                     x_title = "Annual Tillage",y_title = "log RC",my_cols = my_color[c(1,2,3,4)])+
      theme(legend.position = "none")
    
    # Combine these plots together ---------------
    g_all <- plot_grid(g_hist_x,g_hist_RC,g_scatter_all,
                       g_scatter_Tile,g_scatter_Tillage,g_scatter_Annual_Tillage,
                       g_box_Tile,g_box_Tillage,g_box_Annual_Tillage,
                       ncol=3)
    print_g(g_all,paste0("RC_",x_varname,"_",Tile_Y,"_",season),11,10)  
    
    message(i)
  }
  
  # Correlations, and scatter plots among continuous variables, to see if there is any obvious relationship ----
  df_CM <- Q_df %>%
    select(log_RC,log_I30,log_Dur,log_P,log_ARFdays7,DSP)
  g_CM <- ggpairs(df_CM)
  # Output this correlation matrix
  print_g(g_CM,paste0("CM_",Tile_Y,"_",season),8,8)    
  message("Complete",season)
}





