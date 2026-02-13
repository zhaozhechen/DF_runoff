# Author: Zhaozhe Chen
# Date: 2026.2.12

# This code is to model Q event runoff coefficient (RC)

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
library(pROC)

# Data path =======
# Joint non-frozen Q events
Q_df <- read.csv("00_Data/Processed_data_v2/Non-Frozen_Q_joint_df.csv")

# Source functions
source("01_Codes/Plotting_functions.R")
source("01_Codes/Analyses_functions.R")

# Colors for plotting 
my_color <- RColorBrewer::brewer.pal(7,"Set2")


# -------- Main --------
# Preprocessing Q dataset ============
Q_df <- Q_df %>%
  # Remove funny tails (tiny rain events)
  filter()