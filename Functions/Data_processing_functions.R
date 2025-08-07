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