# Author: Zhaozhe Chen
# Update date: 2025.7.22

# This code is to explore EOF dataset

# -------- Global --------
library(dplyr)
library(here)
library(lubridate)

# Data paths
eof_df <- read.csv(here("00_Data/Processed_data/DF_EOF_All.csv"))
site_info <- read.csv(here("00_Data/Processed_data/DF_site_info.csv"))
# Irrigation data
irr_data <- read.csv(here("00_Data/DiscoveryFarms_IrrigationData.csv"))
# Source plotting functions
source(here("Functions/Plotting_functions.R"))
Output_path <- here("Results/DF Exploratory/")

# ------- Main ------
# Explore the contribution of non-storm events to annual total at each site ----------
# For all sites
# sum of non-storm vs storm runoff volume across the sites
g_runoff <- var_storm_compare("runoff_volume",eof_df,"Runoff volume")
# sum of nitrate+nitrite load
g_nitrate_nitrite <- var_storm_compare("nitrate_plus_nitrite_load_pounds",eof_df,"Nitrate + Nitrite loads")
# sum of ammonia + ammonium load
g_ammonia <- var_storm_compare("ammonia_plus_ammonium_load_pounds",eof_df,"Ammonia + Ammonium loads")
# sum of TN load
g_TN <- var_storm_compare("total_nitrogen_load_pounds",eof_df,"TN loads")
# sum of organic N
g_ON <- var_storm_compare("organic_nitrogen_load_pounds",eof_df,"Organic N loads")
# Put them together
g <- plot_grid(g_runoff,g_nitrate_nitrite,g_ammonia,g_TN,g_ON,
               ncol = 3)
print_g(g,"Storm_vs_Nonstorm_nofilter",12,8)

# Only for sites that have both storm and non-storm events
eof_df_filtered <- eof_df %>%
  group_by(Field_Name) %>%
  filter(all(c("Storm","Non-storm") %in% unique(storm)))
# sum of non-storm vs storm runoff volume across the sites
g_runoff <- var_storm_compare("runoff_volume",eof_df_filtered,"Runoff volume")
# sum of nitrate+nitrite load
g_nitrate_nitrite <- var_storm_compare("nitrate_plus_nitrite_load_pounds",eof_df_filtered,"Nitrate + Nitrite loads")
# sum of ammonia + ammonium load
g_ammonia <- var_storm_compare("ammonia_plus_ammonium_load_pounds",eof_df_filtered,"Ammonia + Ammonium loads")
# sum of TN load
g_TN <- var_storm_compare("total_nitrogen_load_pounds",eof_df_filtered,"TN loads")
# sum of organic N
g_ON <- var_storm_compare("organic_nitrogen_load_pounds",eof_df_filtered,"Organic N loads")
# Put them together
g_filtered <- plot_grid(g_runoff,g_nitrate_nitrite,g_ammonia,g_TN,g_ON,
                        ncol = 3)
print_g(g_filtered,"Storm_vs_Nonstorm_filtered",12,8)

# Explore the distribution of each variable in groups -------------
# Note: the data are log transformed
varname_ls <- c("runoff_volume","runoff_in","nitrate_plus_nitrite_conc_mgL","ammonia_plus_ammonium_conc_mgL","total_nitrogen_conc_mgL","organic_nitrogen_conc_mgL")
y_title_ls <- c("log Q cf",
                "log Q in",
                "log NO3+NO2 (mg/L)",
                "log NH4+NH3 (mg/L)",
                "log TN (mg/L)",
                "log ON (mg/L)")
# Across month
g_month <- var_compare_group_all(varname_ls = varname_ls,group_var = "month",
                                 df=eof_df,x_title = "Month",y_title_ls = y_title_ls,10,18,g_title = "Var_across_month")

# Across manure
g_manure <- var_compare_group_all(varname_ls = varname_ls,group_var = "Manure.y",
                                  df=eof_df,x_title = "Manure",y_title_ls = y_title_ls,10,18,g_title = "Var_across_manure")

# Across Tillage
g_tillage <- var_compare_group_all(varname_ls = varname_ls,group_var = "Tillage.y",
                                  df=eof_df,x_title = "Tillage",y_title_ls = y_title_ls,10,18,g_title = "Var_across_tillage")


# Check sites with only storm events, compare with USGS continuous data ------------
# Test at AO1
# USGS site: https://waterdata.usgs.gov/monitoring-location/USGS-451021089064901/#dataTypeId=daily-00060-0&period=periodOfRecord
Site_ID <- "AO1"
site_df <- eof_df[eof_df$Field_Name == "AO1",] %>%
  select(storm_start,storm_end,runoff_volume)
site_df$storm_start <- ifelse(nchar(site_df$storm_start) < 19,
                              paste(site_df$storm_start,"00:00:00"),
                              site_df$storm_start)
site_df$storm_end <- ifelse(nchar(site_df$storm_end) < 19,
                              paste(site_df$storm_end,"00:00:00"),
                              site_df$storm_end)
site_df <- site_df %>%
  mutate(storm_start = as.POSIXct(storm_start,format = "%Y-%m-%d %H:%M:%S"),
         storm_end = as.POSIXct(storm_end,format = "%Y-%m-%d %H:%M:%S"))

g <- ggplot(data=site_df)+
  geom_segment(aes(x=storm_start,xend = storm_end,y=0,yend=runoff_volume))+
  my_theme2


# P-Q slopes & P-nutrient slopes -----------------------
#ggplot(data=eof_df,aes())







