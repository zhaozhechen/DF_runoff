library(dplyr)
library(lfstat) # assign dates into USGS water years
library(sf)
library(lubridate)
library(readxl)
library(tidyr)

# Data paths ======
# USGS raw EOF Storm event data
# This is from USGS data release (https://www.sciencebase.gov/catalog/item/6696bef8d34ecb78f609f651)
usgs_eof <- read.csv("00_Data/USGS raw/All_EOF_StormEventLoadsRainCalculated.csv")

source("01_Codes/Plotting_functions.R")


df <- usgs_eof %>%
  # Only keep measured data
  filter(estimated == 0) %>%
  select(SSD = suspended_sediment_yield_pounds_per_acre,
         TP = total_phosphorus_filtered_yield_pounds_per_acre,
         OP = orthophosphate_yield_pounds_per_acre,
         time = storm_start) %>%
  mutate(
    month = month(mdy_hm(time)),
    season = case_when(
      month %in% 1:5  ~ "Spring",
      month %in% 6:9  ~ "Summer",
      month %in% 10:12 ~ "Fall"
    )
  ) %>%
  filter(!is.na(season))
 

ggplot(data=df,aes(x = SSD,y=OP,color=season))+
  geom_point() +
  my_theme2+
  theme(legend.position = "right")+
  xlim(c(0,3000))



