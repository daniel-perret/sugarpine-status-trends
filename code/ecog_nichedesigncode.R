## 1. Sampling metrics
## 
## 1a. Niche Coverage
## Niche coverage measures the proportion of a species' climatic niche bounded by a set of sampling points.

niche.coverage <- function(niche=NULL, ## this is a spatial polygon bounding the species' occurrences in a 2-dimensional climate space
                           sample.scores){ ## this is a dataframe with two columns describing sampling point coordinates in the same climate space used to describe the species' niche

  require(adehabitatHR)
  require(rgeos)
  require(dplyr)
  require(sp)
  
sample.mcp <- sample.scores %>% 
  SpatialPoints(.) %>% 
  mcp(.,percent=100)

shared <- gIntersection(niche, sample.mcp)

shared.area <- gArea(shared)
niche.area <- gArea(niche)

niche.coverage <- shared.area/niche.area

return(niche.coverage)

}

# 1b. Niche Undersampling
# Niche undersampling measures the proportion of a species' climatic niche that has a low sampling point density relative to the rest of the niche

niche.undersampling <- function(niche, ## this is a spatial polygon bounding the species' occurrences in a 2-dimensional climate space
                                sample.scores, ## this is a dataframe with two columns describing sampling point coordinates in the same climate space used to describe the species' niche
                                thresh){ ## this is the sampling density threshold
  
  require(spatialEco)
  require(sp)
  
  sample.scores.sp <- SpatialPointsDataFrame(coords=sample.scores, data=sample.scores)
  
  kernel.density <- sp.kde(x = sample.scores.sp,
                           newdata = niche,
                           mask = F,
                           standardize = T,
                           nr = 25) %>% 
    mask(., niche)
  
  kernel.density.v <- na.omit(kernel.density[])
  
  niche.undersampling <- length(which(kernel.density.v < thresh))/length(kernel.density.v)
  
  return(niche.undersampling)
  
}

## 2. Sampling designs
## These are the study designs evaluated in our study design simulation analyses
## 
##  ** Random and gridded study designs were implemented in both geographic and niche-based frameworks using the `spsample()` function from the `sp` package (Bivand et al. 2013).
## 
## 2a. Geographic latitudinal transects

latitudinal.transects <- function(range, ## this is a SpatialPolygons object of the species' geographic distribution
                                  n){ ## this is the intended sample size
  
  require(sp)
  
  coords <- coordinates(range)
  
  lat.spaced <- seq(min(coords[,2]), # evenly spaced latitudes through range
                    max(coords[,2]), 
                    length.out = n)
  
  out <- data.frame(x=NA,y=NA)
  
  for(lat in lat.spaced) {
    line <- matrix(c(min(coords[,1]), max(coords[,1]), # line that intersects the species' range at specified latitude
                     lat, lat), ncol=2) %>% 
      Line(.) %>% 
      list(.) %>% 
      Lines(.,ID = "l") %>% 
      list(.) %>% 
      SpatialLines(., proj4string = CRS(proj4string(range))) %>% 
      gIntersection(range,.)
    
    if(!is.null(line)){
      point <- spsample(line,n=1,type="random",iter=1e5) # randomly select a single point on intersecting line
      
      if(!is.null(point)){
        out <- rbind(out,point@coords) 
      }
    }
  }
  
  return(SpatialPoints(out[-1,],
                       proj4string = CRS(proj4string(range))))
  
}

## 2b. Niche-based transects
## The niche-based transect selects evenly spaced points along two lines: (1) the longest line through the center of the species' niche, and (2) the orthogonal line through the center of the species' niche. These are called the "major" and "minor" axes, respectively, in the following code:

niche.transects <- function(niche, ## this is a spatial polygon bounding the species' occurrences in a 2-dimensional climate space
                            n) { ## this is the intended sample size
  
  require(sp)
  require(rgeos)
  
  ## generating the axes
  niche.coords <- SpatialPoints(niche@polygons[[1]]@Polygons[[1]]@coords)
  
  center <- gCentroid(niche)
  
  dist2center <- gDistance(center, niche.coords, byid=T)
  
  myLine.max <- niche.coords[which(dist2center==max(dist2center)),]
  
  coords <- rbind(center,myLine.max)@coords
  
  x <- coords[,1]
  reg <- lm(coords[,2]~x)
  slope <- reg$coefficients[2]
  intercept <- reg$coefficients[1]
  
  major <- list(coords,slope,intercept,reg$coefficients)
  names(major) <- c('coords','slope','intercept','coeff')
  
  majorLine <- predict(reg, newdata=data.frame(x=c(-100,100))) %>% 
    cbind(c(-100,100),.) %>% 
    Line(.) %>% 
    list(.) %>%                   
    Lines(.,ID="line") %>% 
    list(.) %>% 
    SpatialLines(.) %>% 
    gIntersection(.,niche)
  
  next.point <- center
  next.point@coords[,1] <- center@coords[,1]+1
  next.point@coords[,2] <- center@coords[,2]+(-1/slope)
  new.coords <- rbind(center, next.point)@coords
  x <- new.coords[,1]
  new.reg <- lm(new.coords[,2]~x)
  new.slope <- new.reg$coefficients[2]
  
  minor <- list(new.coords,new.slope,new.reg$coefficients[1],new.reg$coefficients)
  names(minor) <- c('coords','slope','intercept','coeff')
  
  minorLine <- predict(new.reg, newdata=data.frame(x=c(-100,100))) %>% 
    cbind(c(-100,100),.) %>% 
    Line(.) %>% 
    list(.) %>% 
    Lines(.,ID="line") %>% 
    list(.) %>% 
    SpatialLines(.) %>% 
    gIntersection(.,niche)
  
  axes <- list(major,majorLine,
                minor,minorLine)
  names(axes) <- c('major','majorLine',
                   'minor','minorLine')
  
  ## generating the sample points
  n.per.axes <- round((n-1)/2)
  major.coords <- coordinates(axes$majorLine)[[1]][[1]]
  minor.coords <- coordinates(axes$minorLine)[[1]][[1]]
  
  if(n.per.axes==2){
    output <- data.frame(x=center@coords[,1],y=center@coords[,2])
    output <- rbind(output, major.coords, deparse.level=0)
    output <- rbind(output, minor.coords, deparse.level=0)
    names(output) <- c('x','y')
    return(SpatialPoints(output))
  } else {
    major.length <- gLength(axes$majorLine)
    minor.length <- gLength(axes$minorLine)
    
    major.interval <- major.length/(n.per.axes)
    minor.interval <- minor.length/(n.per.axes)
    
    major.v <- major.coords[2,]-major.coords[1,]
    major.vn <- major.v/sqrt(major.v[1]^2 + major.v[2]^2)
    
    minor.v <- minor.coords[2,]-minor.coords[1,]
    minor.vn <- minor.v/sqrt(minor.v[1]^2 + minor.v[2]^2)
    
    major.output <- data.frame(x=major.coords[1,1],y=major.coords[1,2])
    minor.output <- data.frame(x=minor.coords[1,1],y=minor.coords[1,2])
    
    for(i in 1:n.per.axes){
      major.output <- rbind(major.output, major.coords[1,]+((i*major.interval)*major.vn))
      minor.output <- rbind(minor.output, minor.coords[1,]+((i*minor.interval)*minor.vn))
    }
    
    output <- rbind(major.output,minor.output)
    
    return(SpatialPoints(coords = output))
  }
  
}
