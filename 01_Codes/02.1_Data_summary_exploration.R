# Author: Zhaozhe Chen
# Update Date: 2025.11.15

# This code is to explore and summarize the dataset

# ------ Global --------
library(dplyr)
library(tidyr)
library(lubridate)
library(RColorBrewer)

# Data paths ========
# All EOF Q events
Q_df <- read.csv("00_Data/Processed_data_v2/All_Q_events_df.csv")
# All USGS P events
P_df <- read.csv("00_Data/Processed_data_v2/All_P_events_df.csv")
# Site info
DF_site_info <- read.csv("00_Data/Processed_data_v2/DF_site_info.csv")

# Source functions ========
source("01_Codes/Plotting_functions.R")

# Others
# Colors for plotting 
my_color <- RColorBrewer::brewer.pal(7,"Set2")

# ------- Main ---------
# Preprocessing of data =================
# Format time
Q_df <- Q_df %>%
  mutate(
    Q_start = ymd_hms(Q_start),
    Q_end = ymd_hms(Q_end)
  )
P_df <- P_df %>%
  mutate(
    P_start = ymd_hms(P_start),
    P_end = ymd_hms(P_end)
  )

# This is to define orders of site names
site_order <- sort(DF_site_info$Field_Name)

# Summarize Q events (frozen vs non-frozen) ========
# Group by sites
Q_n_site <- Q_df %>%
  group_by(Field_Name,frozen) %>%
  summarise(n_Q_events = n()) %>%
  mutate(Field_Name = factor(Field_Name,levels = site_order))

# Across all sites
Q_n_all <- Q_df %>%
  group_by(frozen) %>%
  summarise(n_Q_events = n()) %>%
  mutate(Field_Name = "Total") %>%
  select(Field_Name,frozen,n_Q_events) %>%
  bind_rows(Q_n_site)

# Calculate proportion of non-frozen vs total events, at each site
Q_p_all <- Q_n_all %>%
  pivot_wider(names_from = frozen,
              values_from = n_Q_events) %>%
  # Calculate proportion: Non-Frozen/Total
  mutate(p_nonfrozen = `Non-Frozen`/(`Non-Frozen` + Frozen) * 100,
         Field_Name = factor(Field_Name, levels = Field_Name)) %>%
  mutate(Field_Name = factor(Field_Name,levels = c(site_order,"Total")))

# Make bar plots of Q# across sites
g_nQ <- plot_bar(df = Q_n_site,x_varname = "Field_Name",y_varname = "n_Q_events",fill_name = "frozen",
         x_title = "",y_title = "# of Q Events",fill_title = "",
         label_x = 0.1,label_y = 0.9,
         my_cols = c("Frozen" = my_color[3],"Non-Frozen" = my_color[2]))

# Make bar plots of proportion of non-frozen/total across all sites
g <- pfrozen <- plot_bar(df = Q_p_all,x_varname = "Field_Name",y_varname = "p_nonfrozen",
         x_title = "",y_title = "% of Non-Frozen Q",total_color = my_color[1])

# # of years





