# Author: Zhaozhe Chen
# Date: 2025.8.11

# -------- Global -------------
library(terra)
library(here)

# Site locations, use the updated coordinates
Site_info <- read.csv(here("00_Data/Processed_data/DF_site_info.csv"))
# PRISM data path
PRISM_path <- "G:/PRISM/"
# Output_path
Output_path <- here("00_Data/Processed_data")

# Source functions for data processing
source(here("Functions","Data_processing_functions.R"))

varname <- "ppt"

#  --------- Main --------
# Get the coordinates of the sites
Site_coor <- Site_info[,c("LONG_approx","LAT_approx")]
names(Site_coor) <- c("lon","lat")

# Initialize a vector to store all dates
day_ls_all <- c()
# Initialize a matrix to store output
output_matrix <- matrix(NA,ncol=nrow(Site_info),nrow=0)

for(year in 2004:2023){
  year_folder <- paste0(PRISM_path,varname,"_daily/",year,"/")
  # Get all days in this year
  day_ls <- format(seq(from = as.Date(paste0(year,"-01-01")),
                       to = as.Date(paste0(year,"-12-31")),
                       by = "day"),
                   "%Y%m%d")
  # Add this day list to all day list
  day_ls_all <- c(day_ls_all,day_ls)
  
  # Loop over each date
  for(date in day_ls){
    # Get zip file path
    zip_folder <- paste0(year_folder,"prism_",varname,"_us_30s_",date,".zip")
    # Read in the tif file in this folder
    tif_raster_out <- read_tif(zip_folder)
    tif_raster <- tif_raster_out[[1]]
    tif_full_path <- tif_raster_out[[2]]
    # Extract value from the raster for those sites
    values <- extract(tif_raster,Site_coor)[,2]
    # Clean up the tif file
    file.remove(tif_full_path)
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
write.csv(output_df,paste0(Output_path,"/DF_PRISM_",varname,".csv"))






