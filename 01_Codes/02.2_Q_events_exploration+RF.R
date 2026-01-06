# Author: Zhaozhe Chen
# Update Date: 2026.1.6

# This code is to explore non-frozen Q events

# ---------- Global -----------
library(stringr)
library(dplyr)
library(lubridate)

# Data path =======
# Joint non-frozen Q events
Q_df <- read.csv("00_Data/Processed_data_v2/Non-Frozen_Q_joint_df.csv")
# Source functions
source("01_Codes/Plotting_functions.R")

# Colors for plotting 
my_color <- RColorBrewer::brewer.pal(7,"Set2")

# ------- Main ---------
# Processing Q event df =========
Q_df <- Q_df %>%
  filter(rain_in > 0) %>%
  # Calculate Runoff coefficient (RC)
  mutate(RC = runoff_in/rain_in)

# Explore RC across groups ==========
# Across Sites
RC_Site <- plot_box(df = Q_df,x_varname = "Field_Name",y_varname = "RC",fill_name = "Monitoring",
         x_title = "",y_title = "Runoff Coefficient",fill_title = "",box_width = 0.4,
         jitter_offset = 0.4,label_y = 0.8,y_limits = c(0,2),
         my_cols = c("Surface" = my_color[1],"Tile" = my_color[4]))
# Across Soil types
RC_Soil <- plot_box(df = Q_df,x_varname = "SoilType",y_varname = "RC",fill_name = "storm",
                    x_title = "",y_title = "Runoff Coefficient",box_width = 0.4,fill_title = NULL,
                    jitter_offset = 0.4,label_y = 0.8,y_limits = c(0,2))
# Across Drainage class
RC_Drainage <- plot_box(df = Q_df,x_varname = "DrainageClass",y_varname = "RC",fill_name = "storm",
                    x_title = "",y_title = "Runoff Coefficient",box_width = 0.4,fill_title = NULL,
                    jitter_offset = 0.4,label_y = 0.8,y_limits = c(0,2))






