


#' Get nbr for pre and pod fire for 1 year and rbr
#'
#' @param ft 
#'
#' @return
#' @export
#'
#' @examples
indices_f <- function(fires, ls_col){
  
  
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
    rename('preNBR')
    
    # create post-fire NBR as mean composite
    postFireIndices <- lsCol$filterBounds(fireBounds)$
    filterDate(postFireYear, fireYear$advance(2, 'year'))$
    filter(ee$Filter$dayOfYear(startday, endday))$
    mean()$
    rename('postNBR')
    
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
  return(indices)
}


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





#' Title
#'
#' @param data 
#' @param ls_col 
#'
#' @return
#' @export
#'
#' @examples
getWinterIndices <- function(data, colNBR){
  
  require(sf)
  require(rgee)
  require(tidyverse)
  
  
  #get info
  fireID <- data$Fire_ID
  defol <- data$defoliated
  fire_Year <- data$Fire_Year
  
  #get geometry
  geom <- sf_as_ee(data$geometry)
  

  #get start date and end date the filter imagery for prefire
  preFireYear <- timeFrame(data)$preFireYear
  fireYear <- timeFrame(data)$fireYear
  #filter col by months 12-2 (winter)
  preLs <- filter_col_date(colNBR)
  preLs <- filter_col_winter(preLs) # 12-2
  
  # get mean composite over timeframe
  preComp <- preLs$select('nbr')$
    mean()$
    rename("preNBR")
  
  
  ## get post fire start date and end date and filter the imagery
  
}







  
