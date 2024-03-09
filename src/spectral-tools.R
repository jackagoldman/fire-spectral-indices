

# read in data
readFile <- function(path2file){
  file <- ee$ImageCollection(path2file) 
  return(file)
}


# read in fires and convert to ee object
## read in fires
readFire <- function(firePath){
  fires <- sf::read_sf(firePath)
  # convert sf objects to earth engine objects
  fires <- sf::st_set_crs(fires, 4326)
  fires <- rgee::sf_as_ee(fires)
  return(fires)
}



# Create landsat 8 image collection for NBR
ls8_indices <- function(ls_img){
  nbr <- ls_img$normalizedDifference(c('B5', 'B7'))$float()$rename("nbr")
  qa <- ls_img$select('pixel_qa')
  nbr$addBands(qa)$
    select(0,1, 'nbr', 'pixel_qa')$
    copyProperties(ls_img, list("system:time_start"))
  
  return(nbr)
  
}

# Create landsat 4-7 image collection for NBR
ls4_7_indices <- function(ls_img){
  nbr <- ls_img$normalizedDifference(c('B4', 'B7'))$float()$rename("nbr")
  qa <- ls_img$select('pixel_qa')
  nbr$addBands(qa)$
    select(0,1, 'nbr', 'pixel_qa')$
    copyProperties(ls_img, list("system:time_start"))
  
  return(nbr)
  
}


# create mask for clear pixels
lscf_mask <- function(ls_img){
  quality <- ls_img$select('pixel_qa')
  clear <- quality$bitwiseAnd(8)$eq(0)$ #cloud shadow
    And(quality$bitwiseAnd(32)$eq(0))$ # cloud
    And(quality$bitwiseAnd(4)$eq(0))$ # water
    And(quality$bitwiseAnd(16)$eq(0)) #snow
  ls_img <- ls_img$updateMask(clear)$select(0)$
    copyProperties(ls_img, list("system:time_start"))
  return(ls_img)
}


# function to map  around indices to get images as NBR and apply mask
filter_ls <- function(ls_img, lsVersion){
  if(lsVersion == 8){
    ls <- ls_img$map(ls8_indices)$map(lscf_mask)
  }else{
    ls <- ls_img$map(ls4_7_indices)$map(lscf_mask)
  }
 return(ls)
}


# Merge Landsat Collections
merge_imageColl <- function(ls8, ls7, ls5, ls4){
  
  ls_col <- ee$ImageCollection(ls8$merge(ls7)$merge(ls5)$merge(ls4))
  return(ls_col)
}




# export top drive

exportTable <-  function(metrics, GG_DIR){
  task <- ee_table_to_drive(
    collection = metrics,
    description = "fire_severity_stats",
    folder = GG_DIR,
    fileFormat = "CSV"
  )
  task$start()
  exported.fire.stats <- ee_drive_to_local(task = task, dsn = paste0(GG_DIR, "fire_severity_stats.csv"))
  return(exported.fire.stats)
}


#get metrics
getMetrics <- function(fires, ls_col, GG_DIR){
  
  indicies <- indices_f(fires, ls_col)
  
  metrics <-  fires$map(nbr_sev_indices)
  
  fire.sev.stats <- exportTable(metrics, GG_DIR)
  
  return(fire.sev.stats)
  
}
