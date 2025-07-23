# Author: Zhaozhe Chen
# Update date: 2025.7.22

# This code is to explore EOF dataset

# -------- Global --------
library(dplyr)
library(here)
library(lubridate)

# Data paths
eof_df <- read.csv(here("00_Data/Processed_data/DF_EOF_All.csv"))
site_indo <- read.csv(here("00_Data/Processed_data/DF_site_info.csv"))
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

# P-Q slopes & P-nutrient slopes -----------------------
ggplot(data=eof_df,aes())







