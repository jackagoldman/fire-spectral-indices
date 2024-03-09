library(targets)
library(tarchetypes)
library(rgee)

# Set target options:
tar_option_set(
  packages = c("tibble", "rgee", "sf", "tidyr", "dplyr")) # Packages that your targets need for their tasks.
  # format = "qs", # Optionally set the default storage format. qs is fast.
  

# Run the R scripts in the R/ folder with your custom functions:
tar_source("src/spectral-indices-processing.R")
tar_source("src/spectral-tools.R")


# landsat images
ls8path <- 'LANDSAT/LC08/C01/T1_SR' #landsat 8
ls7path <- 'LANDSAT/LE07/C01/T1_SR' # 7
ls5path <- 'LANDSAT/LT05/C01/T1_SR' # 5
ls4path <- 'LANDSAT/LT04/C01/T1_SR' # 4


#fire path
firePath <- "/Users/jgoldman/Library/CloudStorage/OneDrive-UniversityofToronto/Data/qc-data/clean-data/defoliated_perimeters.shp"

#Google Drive Folder
GG_DIR <- "/Users/jgoldman/Library/CloudStorage/GoogleDrive-jandrewgoldman@gmail.com/My Drive/rgee_res"

#intialize ee
ee$Initialize()

# params
startday <- 152
endday <- 273
band_list <- list("preNBR", "postNBR", "rbr", "dnbr")

# Replace the target list below with your own:
list(
  tar_target(name = ls8, command = readFile(ls8path)),
  tar_target(name = ls7, command = readFile(ls7path)),
  tar_target(name = ls5, command = readFile(ls5path)),
  tar_target(name = ls4, command = readFile(ls4path)),
  tar_target(name = ls_8, command = filter_ls(ls8, 8)),
  tar_target(name = ls_7, command = filter_ls(ls7, 8)),
  tar_target(name = ls_5, command = filter_ls(ls5, 8)),
  tar_target(name = ls_4, command = filter_ls(ls4, 8)),
  tar_target(name = fire, command = readFire(firePath)),
  tar_target(name = ls_col, command = merge_imageColl(ls_8, ls_7, ls_5, ls_4)),
  tar_target(name = fire.severity.res, command = getMetrics(fire, ls_col, GG_DIR))
  
  
  
  
  
  
  
  
)
