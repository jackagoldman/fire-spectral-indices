
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
  if(lsVerion == "8"){
    ls <- ls_img$map(ls8_indices)$map(lscf_mask)
  }else{
    ls <- ls_img$map(ls4_7_indices)$map(lscf_mask)
  }
 return(ls)
}


# Merge Landsat Collections
merge_imageColl <- function(ls8, ls7, ls5, ls4){
  
  ls_col <- ee$ImageCollection(ls8$merge(ls7)$merge(ls5)$merge(ls4))
}

# export top drive

exportTable <-  (fires, Path){
  
  
  Export.table.toDrive({
    collection: recoMetrics,
    description: taskName,
    folder: tablesFolder,
    fileFormat:  'CSV',
    fileNamePrefix : fileName
  })
  
}