# Author: Zhaozhe Chen
# Update Date: 2025.11.17

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

Output_path <- "02_Results/"

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
  ) %>%
  rename(frozen = P_frozen) %>%
  mutate(frozen = ifelse(frozen == TRUE,"Frozen","Non-Frozen"))
  
DF_site_info <- DF_site_info %>%
  mutate(Approximate_Start_Date = ymd(Approximate_Start_Date),
         Approximate_End_Date = ymd(Approximate_End_Date))

# This is to define orders of site names
site_order <- sort(DF_site_info$Field_Name)

# Summarize Q events numbers and percentage (frozen vs non-frozen) ========
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
         Field_Name = factor(Field_Name,levels = c(site_order,"Total")))

# Add # of non-frozen Q to DF_sites_info
DF_site_info <- DF_site_info %>%
  left_join(Q_n_site %>% filter(frozen == "Non-Frozen") %>% select(Field_Name,n_Q_events),
            by="Field_Name") %>%
  mutate(Field_Name = factor(Field_Name,levels = site_order)) %>%
  rename(n_nonfrozen_Q = n_Q_events)

# Q numbers grouped by month across all sites
Q_n_month <- Q_df %>%
  #filter(frozen == "Non-Frozen") %>%
  mutate(Month = month(Q_start)) %>%
  group_by(Month,frozen) %>%
  summarise(n_Q = n()) %>%
  mutate(Month = factor(month.abb[Month],levels=month.abb))

# Make bar plots of Q# across sites
g_nQ <- plot_bar(df = Q_n_site,x_varname = "Field_Name",y_varname = "n_Q_events",fill_name = "frozen",
         x_title = "",y_title = "# of Q Events",fill_title = "",
         label_x = 0.1,label_y = 0.9,
         my_cols = c("Frozen" = my_color[3],"Non-Frozen" = my_color[2]))

# Make bar plots of proportion of non-frozen/total across all sites
g_pfrozen <- plot_bar(df = Q_p_all,x_varname = "Field_Name",y_varname = "p_nonfrozen",
         x_title = "",y_title = "% of Non-Frozen Q",total_color = my_color[1])

# Bar plots of # of years across sites
g_nyear <- plot_bar(df = DF_site_info,x_varname = "Field_Name",y_varname = "UseableYears",
                    x_title = "",y_title = "# of Years")

# Bar plots of # of non-frozen Q events during each month
g_nQ_month <- plot_bar(df=Q_n_month,x_varname = "Month",y_varname = "n_Q",fill_name = "frozen",
                       x_title = "",y_title = "# of Q",
                       my_cols = c("Frozen" = my_color[3],"Non-Frozen" = my_color[2]),
                       label_x = 0.8,label_y = 0.8)

# Output above plots
g_bars <- plot_grid(g_nQ,g_pfrozen,g_nyear,g_nQ_month,
                    ncol=1,
                    align = "hv")
print_g(g_bars,"Q_n_bars",10,14)

# Site property maps =====================
# Map color coded by Monitoring
DF_monitoring <- DF_map(DF_site_info,var_size = "n_nonfrozen_Q",var_fill = "Monitoring",var_label = "Field_Name",
                        my_color,size_name = "# of Non-Frozen Q",fill_name = "Monitoring")

# Map color coded by Tillage
DF_tillage <- DF_map(DF_site_info,var_size = "n_nonfrozen_Q",var_fill = "Tillage",var_label = "Field_Name",
                        my_color,size_name = "# of Non-Frozen Q",fill_name = "Tillage")

# Map color coded by Drainage Class
DF_Drainage <- DF_map(DF_site_info,var_size = "n_nonfrozen_Q",var_fill = "DrainageClass",var_label = "Field_Name",
                        my_color,size_name = "# of non-frozen Q",fill_name = "DrainageClass")

# Map color coded by Land Cover
DF_LC <- DF_map(DF_site_info,var_size = "n_nonfrozen_Q",var_fill = "LandCover",var_label = "Field_Name",
                     my_color,size_name = "# of Non-Frozen Q",fill_name = "Land Cover")

g_maps <- plot_grid(DF_monitoring,DF_Drainage,DF_tillage,DF_LC,ncol=2,align="hv")
print_g(g_maps,"DF_maps",16,12)

# Summarize Q volume =================
# Annual mean Q, color coded by Frozen vs Non-frozen
# Calculate monitoring duration for each site
monitoring_time <- Q_df %>%
  group_by(Field_Name) %>%
  summarise(last_Q_date = max(Q_end,na.rm=TRUE)) %>%
  left_join(DF_site_info %>% select(Field_Name,Approximate_Start_Date),
            by="Field_Name") %>%
  mutate(monitoring_years = as.numeric(difftime(last_Q_date,Approximate_Start_Date,units = "days"))/365.25)

# Annual Q volume across sites Unit: cubit ft
Q_V_site <- Q_df %>%
  group_by(Field_Name,frozen) %>%
  summarise(total_Q_cft = sum(runoff_volume,na.rm=TRUE)) %>%
  left_join(monitoring_time %>% select(Field_Name,monitoring_years),
            by="Field_Name") %>%
  mutate(mean_Q_per_year = total_Q_cft/monitoring_years)

# Percentage of Non-Frozen Q volume to Total Q volume across all years across sites
Q_pV_all <- Q_df %>%
  group_by(Field_Name,frozen) %>%
  summarise(total_Q_cft = sum(runoff_volume,na.rm=TRUE)) %>%
  pivot_wider(names_from = frozen,
              values_from = total_Q_cft) %>%
  # Calculate proportion: Non-Frozen/Total
  mutate(p_nonfrozen_V = `Non-Frozen`/(`Non-Frozen` + Frozen) * 100,
         Field_Name = factor(Field_Name,levels = site_order)) %>%
  left_join(DF_site_info %>% select(Field_Name,Monitoring),
            by="Field_Name")

# Total frozen and non-frozen volume, across sites, acorss years
Q_df %>%
  group_by(frozen) %>%
  summarize(total_Q = sum(runoff_volume,na.rm=TRUE))

# Monthly Q volume across all sites
Q_V_month <- Q_df %>%
  mutate(Month = month(Q_start),
         Year = year(Q_start)) %>%
  group_by(Year,Month,frozen) %>%
  summarise(Monthly_Q = sum(runoff_volume,na.rm=TRUE)) %>% 
  # Mean and sd of monthly total Q acorss years
  group_by(Month,frozen) %>%
  summarise(mean_monthly_Q = mean(Monthly_Q,na.rm=TRUE),
            sd_monthly_Q = sd(Monthly_Q,na.rm=TRUE)) %>%
  mutate(Month = factor(month.abb[Month],levels=month.abb))

# Percentage of Non-Frozen Q volume across months
Q_V_p_month <- Q_V_month %>%
  select(-sd_monthly_Q) %>%
  pivot_wider(names_from = frozen,
              values_from = mean_monthly_Q) %>%
  mutate(Frozen = ifelse(is.na(Frozen),0,Frozen)) %>%
  mutate(p_nonfrozen_V = `Non-Frozen`/(`Non-Frozen` + Frozen) * 100)

# Bar plots of Annual mean Q volume during frozen vs non-frozen across sites
g_VQ <- plot_bar(df = Q_V_site,x_varname = "Field_Name",y_varname = "mean_Q_per_year",fill_name = "frozen",
                 x_title = "",y_title = "Annual Q (cft)",fill_title = "",
                 label_x = 0.8,label_y = 0.9,
                 my_cols = c("Frozen" = my_color[3],"Non-Frozen" = my_color[2]))

# Bar plots showing the percentage of Non-Frozen Q volume to Total Q volume across all years across sites
g_pfrozen_V <- plot_bar(df = Q_pV_all,x_varname = "Field_Name",y_varname = "p_nonfrozen_V",
                        fill_name = "Monitoring",x_title = "",y_title = "Non-Frozen Q Volume %",fill_title = "Monitoring",
                        my_cols = c("Surface" = my_color[1],"Tile" = my_color[4]),
                        label_x = 0.8,label_y = 0.7)

# Bar plots of Monthly total Q volume across months
g_VQ_month <- plot_bar(df = Q_V_month,x_varname = "Month",y_varname = "mean_monthly_Q",fill_name = "frozen",
                       x_title = "",y_title = "Monthly Q (cft)",fill_title = "",
                       my_cols = c("Frozen" = my_color[3],"Non-Frozen" = my_color[2]),
                       label_x = 0.8)

# Bar plots of Monthly non-frozen Q volume across months
g_VQ_p_month <- plot_bar(df = Q_V_p_month,x_varname = "Month",y_varname = "p_nonfrozen_V",
                         x_title = "",y_title = "Non-Frozen Q Volume %")

# Combine these plots
g_bars_V <- plot_grid(g_VQ,g_pfrozen_V,g_VQ_month,g_VQ_p_month,ncol=1,
                      align = "hv")
print_g(g_bars_V,"Q_V_bars",10,14)

# Summarize Q depth =================
# Annual Q depth across sites Unit: in
Q_d_site <- Q_df %>%
  group_by(Field_Name,frozen) %>%
  summarise(total_Q_in = sum(runoff_in,na.rm=TRUE)) %>%
  left_join(monitoring_time %>% select(Field_Name,monitoring_years),
            by="Field_Name") %>%
  mutate(mean_Q_per_year = total_Q_in/monitoring_years)

# Monthly Q depth across all sites
Q_d_month <- Q_df %>%
  mutate(Month = month(Q_start),
         Year = year(Q_start)) %>%
  group_by(Year,Month,frozen) %>%
  summarise(Monthly_Q = sum(runoff_in,na.rm=TRUE)) %>% 
  # Mean and sd of monthly total Q acorss years
  group_by(Month,frozen) %>%
  summarise(mean_monthly_Q = mean(Monthly_Q,na.rm=TRUE),
            sd_monthly_Q = sd(Monthly_Q,na.rm=TRUE)) %>%
  mutate(Month = factor(month.abb[Month],levels=month.abb))

# Total frozen and non-frozen depth, across sites, acorss years
Q_df %>%
  group_by(frozen) %>%
  summarize(total_Q = sum(runoff_in,na.rm=TRUE))

Q_d_month %>%
  select(-sd_monthly_Q) %>%
  pivot_wider(values_from = mean_monthly_Q,
              names_from = frozen) %>%
  mutate(p = `Non-Frozen`/(`Non-Frozen` + Frozen)*100)

# Bar plots of Annual mean Q volume during frozen vs non-frozen across sites
g_dQ <- plot_bar(df = Q_d_site,x_varname = "Field_Name",y_varname = "mean_Q_per_year",fill_name = "frozen",
                 x_title = "",y_title = "Annual Q (in)",fill_title = "",
                 label_x = 0.8,label_y = 0.9,
                 my_cols = c("Frozen" = my_color[3],"Non-Frozen" = my_color[2]))

# Bar plots of Monthly total Q depth across months
g_dQ_month <- plot_bar(df = Q_d_month,x_varname = "Month",y_varname = "mean_monthly_Q",fill_name = "frozen",
                       x_title = "",y_title = "Monthly Q (in)",fill_title = "",
                       my_cols = c("Frozen" = my_color[3],"Non-Frozen" = my_color[2]),
                       label_x = 0.8)

# Combine these plots
g_bars_V <- plot_grid(g_dQ,g_dQ_month,ncol=1,
                      align = "hv")
print_g(g_bars_V,"Q_d_bars",10,7)

# Non-frozen Q volume and depth during each event ========================
# Pre-processing of non-frozen Q
Q_non_frozen <- Q_df %>%
  filter(frozen == "Non-Frozen") %>%
  mutate(Field_Name = factor(Field_Name,levels = site_order)) %>%
  left_join(DF_site_info, by = "Field_Name") %>%
  mutate(Month = month(Q_start)) %>%
  mutate(Month = factor(month.abb[Month],levels=month.abb))

# Make boxplots of non-frozen Q depth during each event
g_box_nonfrozen_Q_in <- plot_box(df = Q_non_frozen,x_varname = "Field_Name",y_varname = "runoff_in",fill_name = "Monitoring",
         x_title = "",y_title = "Non-Frozen Q (in)",fill_title = "",box_width = 0.4,jitter_offset = 0.4,label_y = 0.8,
         my_cols = c("Surface" = my_color[1],"Tile" = my_color[4]))

# Make boxplots of non-frozen Q depth across Months
g_box_nonfrozen_Q_in_Month <- plot_box(df = Q_non_frozen,x_varname = "Month",y_varname = "runoff_in",fill_name = "storm",
                                 x_title = "",y_title = "Non-Frozen Q (in)",fill_title = "",
                                 box_width = 0.1,jitter_offset = 0.2,white_box = TRUE,
                                 my_cols = c("Storm" = my_color[2]))+
  theme(legend.position = "none")

g_box_Q_in <- plot_grid(g_box_nonfrozen_Q_in,g_box_nonfrozen_Q_in_Month,ncol = 1,align="hv")
print_g(g_box_Q_in,"Q_d_box",10,7)

# Box plots of Q in grouped by other practices
# By monitoring
g_box_nonfrozen_Q_in_Monitoring <- plot_box(df = Q_non_frozen,x_varname = "Monitoring",y_varname = "runoff_in",fill_name = "Monitoring",
         x_title = "",y_title = "Non-Frozen Q (in)",fill_title = "",box_width = 0.05,jitter_offset = 0.15,
         label_x = 0.8,label_y = 0.8, white_box=TRUE,
         my_cols = c("Surface" = my_color[1],"Tile" = my_color[4]))+
  theme(legend.position = "none")
# By tillage
g_box_nonfrozen_Q_in_Tillage <- plot_box(df = Q_non_frozen,x_varname = "Tillage",y_varname = "runoff_in",fill_name = "Tillage",
                                            x_title = "",y_title = "Non-Frozen Q (in)",fill_title = "",box_width = 0.05,jitter_offset = 0.15,
                                            label_x = 0.8,label_y = 0.8, white_box=TRUE,
                                            my_cols = c("Yes" = my_color[1],"No" = my_color[4]))+
  theme(legend.position = "none")
# By Drainage class
g_box_nonfrozen_Q_in_Drainage <- plot_box(df = Q_non_frozen,x_varname = "DrainageClass",y_varname = "runoff_in",fill_name = "DrainageClass",
                                          x_title = "",y_title = "Non-Frozen Q (in)",fill_title = "",box_width = 0.05,jitter_offset = 0.15,
                                          label_x = 0.8,label_y = 0.8, white_box=TRUE,
                                          my_cols = c("Well drained" = my_color[1],"Moderately well drained" = my_color[4],
                                                      "Poorly drained" = my_color[5]))+
  theme(legend.position = "none")
# By tillage
g_box_nonfrozen_Q_in_Crop <- plot_box(df = Q_non_frozen,x_varname = "LandCover",y_varname = "runoff_in",fill_name = "LandCover",
                                         x_title = "",y_title = "Non-Frozen Q (in)",fill_title = "",box_width = 0.05,jitter_offset = 0.15,
                                         label_x = 0.8,label_y = 0.8, white_box=TRUE,
                                         my_cols = c("Cultivated Crops" = my_color[1],"Pasture/Hay" = my_color[4]))+
  theme(legend.position = "none")

g_box_Q_in_practice <- plot_grid(g_box_nonfrozen_Q_in_Monitoring,g_box_nonfrozen_Q_in_Crop,
                                 g_box_nonfrozen_Q_in_Tillage,g_box_nonfrozen_Q_in_Drainage,
                                 ncol = 4,align = "hv")
print_g(g_box_Q_in_practice,"Q_d_box_practice",12,5)

# # of frozen vs non-frozen P =====================
# Group by sites
P_n_site <- P_df %>%
  group_by(Field_Name,frozen) %>%
  summarise(n_P_events = n()) %>%
  mutate(Field_Name = factor(Field_Name,levels = site_order))

# Across all sites
P_n_all <- P_df %>%
  group_by(frozen) %>%
  summarise(n_P_events = n()) %>%
  mutate(Field_Name = "Total") %>%
  select(Field_Name,frozen,n_P_events) %>%
  bind_rows(P_n_site)

# Calculate proportion of non-frozen vs total events, at each site
P_p_all <- P_n_all %>%
  pivot_wider(names_from = frozen,
              values_from = n_P_events) %>%
  # Calculate proportion: Non-Frozen/Total
  mutate(p_nonfrozen = `Non-Frozen`/(`Non-Frozen` + Frozen) * 100,
         Field_Name = factor(Field_Name,levels = c(site_order,"Total")))

# P numbers grouped by month across all sites
P_n_month <- P_df %>%
  #filter(frozen == "Non-Frozen") %>%
  #mutate(Month = month(P_start)) %>%
  mutate(Month = ifelse(!is.na(P_start),month(P_start),month(P_end))) %>%
  group_by(Month,frozen) %>%
  summarise(n_P = n()) %>%
  mutate(Month = factor(month.abb[Month],levels=month.abb))

test <- P_n_month %>%
  pivot_wider(names_from = frozen,
              values_from = n_P)

# Make bar plots of P# across sites
g_nP <- plot_bar(df = P_n_site,x_varname = "Field_Name",y_varname = "n_P_events",fill_name = "frozen",
                 x_title = "",y_title = "# of P Events",fill_title = "",
                 label_x = 0.1,label_y = 0.9,
                 my_cols = c("Frozen" = my_color[3],"Non-Frozen" = my_color[2]))

# Make bar plots of proportion of non-frozen/total across all sites
g_pfrozen <- plot_bar(df = P_p_all,x_varname = "Field_Name",y_varname = "p_nonfrozen",
                      x_title = "",y_title = "% of Non-Frozen P",total_color = my_color[1])

# Bar plots of # of non-frozen P events during each month
g_nP_month <- plot_bar(df=P_n_month,x_varname = "Month",y_varname = "n_P",fill_name = "frozen",
                       x_title = "",y_title = "# of P",
                       my_cols = c("Frozen" = my_color[3],"Non-Frozen" = my_color[2]),
                       label_x = 0.1,label_y = 0.8)
# Output above plots
g_bars <- plot_grid(g_nP,g_pfrozen,g_nP_month,
                    ncol=1,
                    align = "hv")
print_g(g_bars,"P_n_bars",10,10)


# Summarize P depth =================
# Annual P depth across sites Unit: in
P_d_site <- P_df %>%
  group_by(Field_Name,frozen) %>%
  summarise(total_P_in = sum(rain,na.rm=TRUE)) %>%
  left_join(monitoring_time %>% select(Field_Name,monitoring_years),
            by="Field_Name") %>%
  mutate(mean_P_per_year = total_P_in/monitoring_years)

test <- P_d_site %>%
  filter(frozen == "Non-Frozen")

# Monthly P depth across all sites
P_d_month <- P_df %>%
  mutate(Month = month(P_start),
         Year = year(P_start)) %>%
  group_by(Year,Month,frozen) %>%
  summarise(Monthly_P = sum(rain,na.rm=TRUE)) %>% 
  # Mean and sd of monthly total P acorss years
  group_by(Month,frozen) %>%
  summarise(mean_monthly_P = mean(Monthly_P,na.rm=TRUE),
            sd_monthly_P = sd(Monthly_P,na.rm=TRUE)) %>%
  mutate(Month = factor(month.abb[Month],levels=month.abb))

# Total frozen and non-frozen depth, across sites, acorss years
P_df %>%
  group_by(frozen) %>%
  summarize(total_P = sum(rain,na.rm=TRUE))

P_d_month %>%
  select(-sd_monthly_P) %>%
  pivot_wider(values_from = mean_monthly_P,
              names_from = frozen) %>%
  mutate(p = `Non-Frozen`/(`Non-Frozen` + Frozen)*100)

# Bar plots of Annual mean P volume during frozen vs non-frozen across sites
g_dP <- plot_bar(df = P_d_site,x_varname = "Field_Name",y_varname = "mean_P_per_year",fill_name = "frozen",
                 x_title = "",y_title = "Annual P (in)",fill_title = "",
                 label_x = 0.8,label_y = 0.9,
                 my_cols = c("Frozen" = my_color[3],"Non-Frozen" = my_color[2]))

# Bar plots of Monthly total P depth across months
g_dP_month <- plot_bar(df = P_d_month,x_varname = "Month",y_varname = "mean_monthly_P",fill_name = "frozen",
                       x_title = "",y_title = "Monthly P (in)",fill_title = "",
                       my_cols = c("Frozen" = my_color[3],"Non-Frozen" = my_color[2]),
                       label_x = 0.8)

# Combine these plots
g_bars_V <- plot_grid(g_dP,g_dP_month,ncol=1,
                      align = "hv")
print_g(g_bars_V,"P_d_bars",10,7)

# # of NON_FROZEN P produced Q ========================
nonfrozen_P_df <- P_df %>%
  filter(frozen == "Non-Frozen") %>%
  mutate(Field_Name = factor(Field_Name,levels = c(site_order,"Total")))

# Total number of P produced Q
sum(nonfrozen_P_df$Associated_Q)

# Summarize number of P that produced Q across sites
P_Q_site_df <- nonfrozen_P_df %>%
  group_by(Field_Name,Associated_Q) %>%
  summarize(n_Q_produced = n())

# Proportion of P that produced Q across sites
P_Q_p_all <- P_Q_site_df %>%
  pivot_wider(names_from = Associated_Q,
              values_from = n_Q_produced) %>%
  mutate(p_Q_produced = `TRUE`/(`TRUE` + `FALSE`) * 100) %>%
  left_join(DF_site_info %>% 
              select(Field_Name,Monitoring,LandCover,Tillage,DrainageClass,MeanSlope_per),
            by="Field_Name")

# Bar plots of # of non-frozen P that produced Q across sites
g_n_P_Q <- plot_bar(df = P_Q_site_df,x_varname = "Field_Name",y_varname = "n_Q_produced",fill_name = "Associated_Q",
                    x_title = "",y_title = "# of P that produced Q",fill_title = "Produced Q?",
                    my_cols = c("TRUE" = my_color[1],"FALSE" = my_color[7]))+
  theme(legend.position = "top")

# Bar plots of percentage of P that generated Q across sites
g_p_P_Q <- plot_bar(df = P_Q_p_all,x_varname = "Field_Name",y_varname = "p_Q_produced",fill_name = "Monitoring",
                    x_title = "",y_title = "% of non-frozen P that produced Q",fill_title = "Monitoring",
                    my_cols = c("Surface" = my_color[1],"Tile" = my_color[4]),
                    label_x = 0.85,label_y = 0.7)

g_P_Q <- plot_grid(g_n_P_Q,g_p_P_Q,nrow=2,align="hv")
print_g(g_P_Q,"P_Q_bars",10,8)

# Box plots across different practices



# Scatter plots of percentage vs practices
ggplot(P_Q_p_all,aes(x = MeanSlope_per,y = p_Q_produced,color = Monitoring))+
  geom_point()+
  my_theme2



