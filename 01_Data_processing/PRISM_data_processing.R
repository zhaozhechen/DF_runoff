# Author: Zhaozhe Chen
# Date: 2025.8.6

# -------- Global -------------
library(terra)
library(here)
library(readxl)

# Site locations
Site_info <- read_xlsx(here("00_Data/Metadata/DiscoveryFarms_SiteLocations.xlsx"))
# PRISM data path
PRISM_path <- "G:/PRISM/"
# Output_path
Output_path <- here("00_Data/Processed_data")

# Source functions for data processing
source(here("Functions","Data_processing_functions.R"))

varname <- "ppt"

#  --------- Main --------
# Get the coordinates of the sites
Site_coor <- Site_info[,c("GPS Lon","GPS Lat")]
names(Site_coor) <- c("lon","lat")

# Initialize a vector to store all dates
day_ls_all <- c()
for(year in 2003:2023){
  year_folder <- paste0(PRISM_path,varname,"_daily/",year,"/")
  # Get all days in this year
  day_ls <- format(seq(from = as.Date(paste0(year,"-01-01")),
                       to = as.Date(paste0(year,"-12-31")),
                       by = "day"),
                   "%Y%m%d")
  # Add this day list to all day list
  day_ls_all <- c(day_ls_all,day_ls)
  
  # Initialize a matrix to store output
  output_matrix <- matrix(NA,ncol=nrow(Site_info),nrow=0)
  # Loop over each date
  for(date in day_ls){
    # Get zip file path
    zip_folder <- paste0(year_folder,"prism_",varname,"_us_30s_",date,".zip")
    # Read in the tif file in this folder
    tif_raster <- read_tif(zip_folder)
    # Extract value from the raster for those sites
    values <- extract(tif_raster,Site_coor)[,2]
    # Store in in the matrix
    output_matrix <- rbind(output_matrix,values)
    print(date)
  }  
}

# Add column names
colnames(output_matrix) <- Site_info$`Site ID`
# Convert this output_matrix to dataframe
output_df <- as.data.frame(output_matrix)
output_df$Date <- day_ls_all
# Output this data frame
write.csv(output_df,paste0(Output_path,"/DF_PRISM_ppt.csv"))






