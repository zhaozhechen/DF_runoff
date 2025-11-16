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
  )
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




# Make boxplots of Q volume during each event
df <- Q_df
x_varname <- "Field_Name"
y_varname <- "runoff_volume"
fill_varname <- "frozen"

ggplot(data=df,aes(x=.data[[x_varname]],y=log10(.data[[y_varname]]),fill=.data[[fill_varname]]))+
  geom_half_violin(alpha = 0.5, color=NA)+
  geom_boxplot(width = 0.5,color="black",outlier.color = NA)+
  geom_jitter(aes(x=as.numeric(.data[[x_varname]])+0.2),
              position = position_jitter(width=0.1))




g <- ggplot(data=eof_summary,aes(x=storm,y=contribution,color=storm,fill=storm))+
  geom_half_violin(alpha = 0.5, color=NA)+
  geom_boxplot(width = 0.1,color="black",outlier.color = NA)+
  geom_jitter(aes(x=as.numeric(as.factor(storm))+0.2),
              position = position_jitter(width=0.1))+





# # of frozen vs non-frozen P


# Proportion of non-frozen P


# # of P produced Q

# proportion of P produced Q



