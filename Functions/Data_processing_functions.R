# Author: Zhaozhe Chen
# Date: 2025.8.6

# This code includes functions for data processing

# This function reads in the tif file
# Input is the path to the zip file folder
# Output the tif file as a raster
read_tif <- function(zip_folder){
  # Get the files names in this folder
  zip_filenames <- unzip(zip_folder,list=TRUE)
  # Get the tif file
  tif_filename <- zip_filenames$Name[grepl("\\.tif$",zip_filenames$Name)]
  # Create a temporary file path fo this tif
  tmp_tif_path <- tempfile(fileext = ".tif")
  # Extract this tif file to this temporary file
  unzip(zip_folder,files=tif_filename,exdir = dirname(tmp_tif_path))
  tif_full_path <- file.path(dirname(tmp_tif_path),tif_filename)
  # Read the tif
  r <- rast(tif_full_path)
  return(list(r,tif_full_path))
}

# This function is to extract subdaily P from USGS station
# Input include arrayid, which indicates which site to process
# And Site_ls, which includes Site_ID, USGS_ID, Start and End time
USGS_ppt <- function(arrayid,Site_ls){
  # Site to process
  Site_ID <- Site_ls$Site_ID[arrayid]
  # The corresponding USGS_ID
  USGS_ID <- Site_ls$USGS_ID[arrayid]
  start <- Site_ls$Start[arrayid]
  end <- Site_ls$End[arrayid]
  
  # Extract subdaily P from USGS station
  ppt <- readNWISuv(siteNumbers = USGS_ID,
                    parameterCd = P_code,
                    startDate = start,
                    endDate = end,
                    tz = "UTC") %>%
    # Standardize variable name
    renameNWISColumns()
  
  return(ppt)
}
