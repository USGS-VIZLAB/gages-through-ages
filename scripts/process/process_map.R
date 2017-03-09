
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


shifts <- list(AK = list(scale = 0.33, shift = c(80,-450), rotate = -50),
               HI = list(scale = 1, shift = c(520, -110), rotate = -35),
               PR = list(scale = 2.5, shift = c(-140, 90), rotate=20))

stuff_to_move <- list(
  AK = to_sp("world", "USA:alaska"),
  HI = to_sp("world", "USA:hawaii"),
  PR = to_sp("world", "Puerto Rico")
)


#' create the sp object 
#'
#'@param viz the vizlab object 
process.state_map <- function(viz){
  library(sp)
  states.out <- to_sp('state')
  for(i in names(shifts)){
    shifted <- do.call(shift_sp, c(sp = stuff_to_move[[i]], 
                                   shifts[[i]],  
                                   proj.string = proj4string(states.out),
                                   row.names = i))
    states.out <- rbind(shifted, states.out, makeUniqueIDs = TRUE)
  }
  
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

shift_sp <- function(sp, scale = NULL, shift = NULL, rotate = 0, ref=sp, proj.string=NULL, row.names=NULL){
  if (is.null(scale) & is.null(shift) & rotate == 0){
    return(obj)
  }
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

