############ NBR SCRIPT ##################
# This script calculates prefire and postfire NBR as well as RBR and dNBR
# uses rgee 
#


# read in rgee
library(rgee)
library(tidyverse)
library(sf)

# intialize ee
ee$Initialize()

# read in landsat image collections
ls_8 <- ee$ImageCollection('LANDSAT/LC08/C01/T1_SR') #landsat 8
ls_7 <-  ee$ImageCollection('LANDSAT/LE07/C01/T1_SR') # 7
ls_5 <-  ee$ImageCollection('LANDSAT/LT05/C01/T1_SR') # 5
ls_4 <-  ee$ImageCollection('LANDSAT/LT04/C01/T1_SR') # 4

## read in fires
fires <- sf::st_read("")

# convert sf objects to earth engine objects
fires <- sf_as_ee()

##############################################################
########### create functions to make image collections #######
#############################################################
# landsat 8

ls8_indices <- function(ls_img){
  nbr <- ls_img$normalizedDifference(c('B5', 'B7'))$float()$rename("nbr")
  qa <- ls_img$select('pixel_qa')
  nbr$addBands(qa)$
    select(0,1, 'nbr', 'pixel_qa')$
    copyProperties(ls_img, list("system:time_start"))
  
  return(nbr)
  
}

# landsat 7
ls4_7_indices <- function(ls_img){
  nbr <- ls_img$normalizedDifference(c('B4', 'B7'))$float()$rename("nbr")
  qa <- ls_img$select('pixel_qa')
  nbr$addBands(qa)$
    select(0,1, 'nbr', 'pixel_qa')$
    copyProperties(ls_img, list("system:time_start"))
  
  return(nbr)
  
}

##################################################################
############# mask landsat surface reflectance images ############
##################################################################
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

#########################################################
###### Map functions across Landsat Collections #########
#########################################################

ls8 <-  ls_8$map(ls8_indices)$map(lscf_mask)

ls7 <-  ls_7$map(ls4_7_indices)$map(lscf_mask)

ls5 <- ls_5$map(ls4_7_indices)$map(lscf_mask)

ls4 <-  ls_4$map(ls4_7_indices)$map(lscf_mask) 

################ Merge Landsat Collections
ls_col <- ee$ImageCollection(ls8$merge(ls7)$merge(ls5)$merge(ls4))



###################################################
############# get pre fire NBR indices ############
###################################################
# first we must send the start and end day

startday <- 
  
  endday <- 
  
  band_list <- list("preNBR", "postNBR", "rbr", "dnbr")

indices <- ee$ImageCollection(fires$map(function(ft){
  ## use 'Fire_ID' as unique identifier
  fName <- ft$get("Fire_ID")
  fireBounds <- ft$geometry()$bounds()
  year <- ee$Number$parse(fire$get('Fire_Year'))
  fireYear <-  ee$Date$parse('YYYY', fire$get('Fire_Year'))
  preFireYear <- fireYear$advance(-1, 'year')
  postFireYear <- fireYear$advance(1, 'year')
  
  
  ## Create Pre-Fire NBR as mean composite
  preFireIndices <- ls_col$filterBounds(fireBounds)$
    filterDate(preFireYear, fireYear)$
    filter(ee$Filter$dayOfYear(startday, endday))$
    mean()$
    rename('preNBR')$
    
    # create post-fire NBR as mean composite
    postFireIndices <- lsCol$filterBounds(fireBounds)$
    filterDate(postFireYear, fireYear$advance(2, 'year'))$
    filter(ee$Filter$dayOfYear(startday, endday))$
    mean()$
    rename('postNBR')$
    
    # create image with pre and post bands
    fire_indices <- preFireIndices$addBands(postFireIndices)
  
  # calculate dNBR
  burn_indices <- fire_indices$expression(
    "(b('preNBR') - b('postNBR')) * 1000")$
    rename('dnbr')$float()$addBands(fire_indices)
  
  # calculate RBR  
  burn_indices2 <- burn_indices$expression(
    "b('dnbr') / (b('preNBR') + 1.001)")$
    rename('rbr')$float()$addBands(burn_indices2)
  
  
  burn_indices2 = burn_indices2$select(band_list)
  
  burn_indices2$set("fireID", ft$get("Fire_ID"))
  burn_indices2$set('fireYear' , ft$get('Year'))
  
  return(burn_indices2)
}
))


#####################################################
######## calculate indices for each fire ############
####################################################


nbr_sev_indices <-  function(ft){
  
  fireYear <- ee$Date$parse('YYYY', ft$get('Fire_Year'));
  fbounds <- ft$geometry()$bounds()
  fID <- ft$get("Fire_ID")
  index <- ft$get("system:index")
  
  
  # filter the image collection by fire ID
  indices_per_fire <-  indices$filter(ee$Filter$eq('fireID', fID))
  
  indices_per_fire <-  indices_per_fire$toBands()
  
  # get the mean preNBR for each area
  pre_nbr_indices <-indices_per_fire$reduceRegion(
    reducer= ee$Reducer$median(),
    geometry= fbounds,
    scale= 30,
    maxPixels= 30e13)
  
  # change key names in dictionary
  stringRBR <- ee$String(index)$cat("_")$cat('rbr')
  from <- 'stringRBR'
  to <- 'rbrMedian'
  median_indices = median_indices$rename(from, to)
  
  
  # get the median RBR for each area
  median_indices <-indices_per_fire$reduceRegion(
    reducer= ee$Reducer$median(),
    geometry= fbounds,
    scale= 30,
    maxPixels= 30e13)
  
  # change key names in dictionary
  stringRBR <- ee$String(index)$cat("_")$cat('rbr')
  from <- 'stringRBR'
  to <- 'rbrMedian'
  median_indices = median_indices$rename(from, to)
  
  
  # get the quantile or 90th percentile for each area
  quant_indices <- indices_per_fire$reduceRegion(
    reducer= ee$Reducer$percentile(90),
    geometry= fbounds,
    scale= 30,
    maxPixels= 30e13)
  
  # change key names in dictionary
  stringRBR <-  ee$String(index)$cat("_")$cat('rbr')
  from <- 'stringRBR'
  to <- 'rbrExtreme'
  extreme_indices <-  quant_indices$rename(from, to)
  
  
  # calculate mean indices for cv 
  mean_indices = indices_per_fire$reduceRegion(
    reducer= ee$Reducer$mean(),
    geometry= fbounds,
    scale= 30,
    maxPixels= 30e13)
  
  # change key names in dictionary
  stringRBR <- ee$String(index)$cat("_")$cat('rbr')
  from <-'stringRBR'
  to <- 'rbrMean'
  mean_indices <- mean_indices$rename(from, to)
  
  # calcualte standard deviation for cv
  stdDev_indices <-indicesPerFire$reduceRegion(
    reducer=ee$Reducer$stdDev(),
    geometry= fbounds,
    scale= 30,
    maxPixels= 30e13)
  
  # change key names in dictionary
  stringRBR <- ee$String(index)$cat("_")$cat('rbr')
  from <- 'stringRBR'
  to <- 'rbrStdDev'
  stdDev_indices = stdDev_indices$rename(from, to)
  
  # get the coefficient of variation for each image (stdev/mean)
  
  cv_indices = stdDev_indices$getNumber('rbrStdDev')$divide(meanIndices$getNumber('rbrMean'))
  
  
  # add indices
  ft$set('rbrMedian',  median_indices$getNumber('rbrMedian'))
  ft$set('rbrExtreme', extreme_indices$getNumber('rbrExtreme'))
  ft$set('rbrCV', cv_indices)
  
  ft$setGeometry(null)
  
  return(ft)
}



metrics <-  fires$map(nbr_sev_indices)


###################################
######## export as csv ############
###################################

Export.table.toDrive({
  collection: recoMetrics,
  description: taskName,
  folder: tablesFolder,
  fileFormat:  'CSV',
  fileNamePrefix : fileName
}); 


  