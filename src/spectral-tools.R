

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

#rename Bands
renameBands <- function(coll){
  bands <- list("SR_B5", "SR_B7", "QA_PIXEL")
  imgColl <- coll$select(bands,c("B4", "B7", "pixel_qa"))
  
  return(imgColl) 
}

# filter image collection by month
filter_col_winter <- function(ls_img){
  ls_date <- ls_img$
    filter(ee$Filter$calendarRange(12,2 ,'month'))
  
  return(ls_date)
}


filter_col_date <- function(ls_img){
  ls_date <- ls_img$
    filterDate(preFireYear, fireYear)
  
  return(ls_date)
}
#get time frame
timeFrame <- function(data){
  year <- data |> select(c(Fire_Year)) |> st_drop_geometry() 
  year <- mutate(year, Fire_Year = as.numeric(Fire_Year))
  preFire <- (year - 1) 
  preFire <- lubridate::ymd(preFire, truncated = 2L) 
  month(preFire) <- 12
  preFire <- preFire |> as.character() |> as.data.frame()
  year <- lubridate::ymd(year, truncated = 2L)
  month(year) <- 3
  year <- year |> as.character() |> as.data.frame()
  res <- cbind(preFire, year)
  colnames(res) <- c("preFireYear", "fireYear")
  
  
  return(res)
}


# Create landsat 8 image collection for NBR
ls8_indices <- function(ls_img){
  require(rgee)
  nbr <- ls_img$normalizedDifference(c("SR_B5", "SR_B7"))$float()$rename("nbr")
  qa <- ls_img$select("QA_PIXEL")$
    rename("pixel_qa")
  
  nbr$addBands(qa)$
    copyProperties(ls_img, list("system:time_start"))

  
  return(nbr)
  
}

# Create landsat 4-7 image collection for NBR
ls4_7_indices <- function(ls_img){
  require(rgee)
  nbr <- ls_img$normalizedDifference(c('B4', 'B7'))$float()$rename("nbr")
  qa <- ls_img$select("pixel_qa")
  nbr$addBands(qa)$
    copyProperties(ls_img, list("system:time_start"))
  
  return(nbr)
  
}



# create mask for clear pixels
lscf_mask47 <- function(ls_img){
  require(rgee)
  quality <- ls_img$select('pixel_qa')
  clear <- quality$bitwiseAnd(8)$eq(0)$ #cloud shadow
    And(quality$bitwiseAnd(32)$eq(0))$ # cloud
    And(quality$bitwiseAnd(4)$eq(0))$ # water
    And(quality$bitwiseAnd(16)$eq(0)) #snow
  ls_img <- ls_img$updateMask(clear)$select(0)$
    copyProperties(ls_img, list("system:time_start"))
  return(ls_img)
}

#create mask for clear pixels
lscf_mask8 <- function(ls_img){
  require(rgee)
  quality <- ls_img$select('pixel_qa')
  clear <- quality$bitwiseAnd(4)$eq(0)$ #cloud shadow
    And(quality$bitwiseAnd(3)$eq(0))$ # cloud
    And(quality$bitwiseAnd(7)$eq(0))$ # water
    And(quality$bitwiseAnd(5)$eq(0)) #snow
  ls_img <- ls_img$updateMask(clear)$select(0)$
    copyProperties(ls_img, list("system:time_start"))
  return(ls_img)
}

# function to map  around indices to get images as NBR and apply mask
filter_ls <- function(ls_img, lsVersion){
  require(rgee)
  if(lsVersion == 8){
    ls <- ls_img$map(ls8_indices)
    ls <- ls$map(lscf_mask8)
  }else{
    ls <- ls_img$map(ls4_7_indices)
    ls <- ls$map(lscf_mask47)
  }
 return(ls)
}


# Merge Landsat Collections
merge_imageColl <- function(ls8, ls7, ls5, ls4){
  require(rgee)
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
