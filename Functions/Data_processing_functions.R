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
  # Extract this tif file to a temperaty directory
  tmp_dir <- tempdir()
  unzip(zip_folder,files=tif_filename,exdir = tmp_dir)
  # Read the tif
  tif_path <- file.path(tmp_dir,tif_filename)
  r <- rast(tif_path)
  return(r)
}