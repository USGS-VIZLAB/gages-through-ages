
#' take map arguments and return a projected sp object
#' 
#' @param \dots arguments passed to \code{\link[maps]{map}} excluding \code{fill} and \code{plot}
#' 
library(maps)
library(sp)
proj.string <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
to_sp <- function(...){
  library(maptools)
  library(maps)
  map <- maps::map(..., fill=TRUE, plot = FALSE)
  IDs <- sapply(strsplit(map$names, ":"), function(x) x[1])
  map.sp <- map2SpatialPolygons(map, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
  map.sp.t <- spTransform(map.sp, CRS(proj.string))
  return(map.sp.t)
}

#' create the sp object 
#'
#'@param viz the vizlab object 
process.state_map <- function(viz){
  library(sp)
  conus <- to_sp('state')
  HI <- to_sp("world", "USA:hawaii")
  AK <- to_sp("world", "USA:alaska")
  PR <- to_sp("world", "Puerto Rico")
  
  # thanks to Bob Rudis (hrbrmstr):
  # https://github.com/hrbrmstr/rd3albers
  
  # -- if moving any more states, do it here: --
  alaska <- shift_sp(AK, 0.33, shift = c(80,-450), rotate=-50, proj.string = proj4string(conus), row.names = 'alaska') 
  hawaii <- shift_sp(HI, 1, shift=c(520, -110), rotate=-35, proj.string = proj4string(conus), row.names = 'hawaii') 
  puerto <- shift_sp(PR, 2.5, shift = c(-140, 90), rotate=20, proj.string = proj4string(conus), row.names = 'puerto')
  
  states.out <- rbind(puerto, conus, alaska, hawaii, makeUniqueIDs = TRUE)
  
  saveRDS(list(states=states.out), file = viz[['location']])
}

get_region <- function(xs){
  regions <- xs
  for (i in 1:length(xs)){
    twod <- substr(xs[i],1,2)
    if (twod == '19'){
      region = 'AK'
    } else if (twod == '20'){
      region = 'HI'
    } else if (twod == '21'){
      region = 'PR'
    } else {
      region = 'US'
    }
    regions[i] <- region
  }
  
  return(regions)
}

library(rgeos)

shift_sp <- function(sp, scale, shift, rotate = 0, ref=sp, proj.string=NULL, row.names=NULL){
  orig.cent <- rgeos::gCentroid(ref, byid=TRUE)@coords
  scale <- max(apply(bbox(ref), 1, diff)) * scale
  obj <- elide(sp, rotate=rotate, center=orig.cent, bb = bbox(ref))
  ref <- elide(ref, rotate=rotate, center=orig.cent, bb = bbox(ref))
  obj <- elide(obj, scale=scale, center=orig.cent, bb = bbox(ref))
  ref <- elide(ref, scale=scale, center=orig.cent, bb = bbox(ref))
  new.cent <- rgeos::gCentroid(ref, byid=TRUE)@coords
  obj <- elide(obj, shift=shift*10000+c(orig.cent-new.cent))
  if (is.null(proj.string)){
    proj4string(obj) <- proj4string(sp)
  } else {
    proj4string(obj) <- proj.string
  }
  
  if (!is.null(row.names)){
    row.names(obj) <- row.names
  }
  return(obj)
}

