###################################################################
###################################################################
# Add required libraries 
# Download data for exercises (need to set a working directory)
###################################################################
###################################################################
p = c("sp", "terra", "spdep", "spatialEco", "sf", "terra", 
      "raster", "spatstat", "spatstat.geom", "dplyr",
	  "landscapemetrics", "devtools")
  if(any(!unlist(lapply(p, requireNamespace, quietly=TRUE)))) { 
    m = which(!unlist(lapply(p, requireNamespace, quietly=TRUE)))
	  suppressMessages(invisible(lapply(p[-m], require,    
	                   character.only=TRUE)))
    stop("Missing library, please install ", paste(p[m], collapse = " "))
  } else {
    suppressMessages(invisible(lapply(p, require, character.only=TRUE)))
  }

# set your working and data directories here
setwd("C:/evans/Courses/Texas_AM/day02") 
  data.dir <- file.path(getwd(), "data")

download.file("https://spatialr.s3-us-west-2.amazonaws.com/day02.zip", 
              destfile="day02.zip", mode="wb")
  unzip("day02.zip")
  file.remove("day02.zip")

# data is in day01/data, you can use file.path to set a path to 
# the data directory eg., dat.dir <- file.path(getwd(), "data")

###################################################################
###################################################################
#  Exercise 2.1 - Vector analysis 
###################################################################
###################################################################

# Read data 
plots <- st_read(file.path(data.dir, "plots.shp"))
  soil <- st_read(file.path(data.dir, "soil.shp"))

plot(st_geometry(soil))
  points(st_coordinates(plots), pch=20, col="red")
  # plot(st_geometry(plots), pch=20, col="red", add=TRUE)
  
#******************************************************************
# 1. Buffer points to 200m see; st_buffer 
 
plots.buff <- st_buffer(plots, dist=200)
  plot(st_geometry(plots.buff))
    plot(st_geometry(plots), pch=20, add=TRUE)

#******************************************************************
# 2. Intersect (clip) soil with buffers and relate back to points 
#      (see gIntersection and intersect) 

plots.soil <- st_intersection(plots.buff, soil) 
  plot(st_geometry(plots.soil))

#******************************************************************	
# 3. Calculate soil-type ("musym" attribute) area proportion for each plot	
#  hint; table, prop.table	

# Pull unique soil types
( stype = sort(unique(plots.soil$musym)) )

# Note; polygons are non-sequential eg., 
which(plots.soil$ID == 25)

# get area for each polygon		
plots.soil$soil.area <- as.numeric(st_area(plots.soil))

# loop through each polygon and get proportion(s)
soil.pct <- list()
  for(i in unique(plots.soil$ID) ) {
    s <- plots.soil[plots.soil$ID == i ,]
	  ta <- sum(s$soil.area)
	    spct <- tapply(s$soil.area, s$musym, function(x) sum(x) / ta)  
	    scts <- table(factor(s$musym, levels=stype))
	  scts[which(names(scts) %in% names(spct))] <- round(spct,4)
    soil.pct[[i]] <- scts
    }
soil.pct <- data.frame(ID=unique(plots.soil$ID), do.call(rbind, soil.pct))
  plots <- merge(plots, soil.pct, by="ID")
    plot(plots["GpD"], pch=20, cex=2)
  
#### or, alternately using lapply as loop (AKA the R way)  
p <- lapply(unique(plots.soil$ID), function(i) {
  s <- plots.soil[plots.soil$ID == i ,]
    ta <- sum(s$soil.area)
	round(tapply(s$soil.area, s$musym, function(x) sum(x) / ta),4)  
})
  p <- plyr::ldply(p, rbind)
    p[is.na(p)] <- round(0,4)
      str(p)
	  
#******************************************************************	 
# 4. Point in polygon analysis to relate soil attributes to plots
#    see; point.in.poly  

# Point in polygon (relate soil attributes to points)
soil.plots <- point.in.poly(plots, soil, sp=FALSE)
  head(soil)
  head(plots)  
  head(soil.plots)

#******************************************************************	
# 5. Dissolve polygons
#
# using "NWBIR74" column, create column where < median is 0 else 1
# then dissolve features by this column and plot     

# read data from sf package
polys <- st_read(system.file("shape/nc.shp", package="sf"))
  polys$p <- ifelse(polys$NWBIR74 < quantile(polys$NWBIR74, p=0.5), 0 ,1)
   
# sf approach  
diss <- polys %>% 
  dplyr::group_by(p) %>% 
    summarise(m = max(p)) %>% 
      st_cast()
plot(diss["p"])
  
# sp object approach
diss <- rgeos::gUnaryUnion(as(polys, "Spatial"), id = polys$p)
  plot(diss, col=c("red","green"))
 
#******************************************************************	
# 6. Work through this zonal function that calculates the proportion 
#    of covertypes around the periphery of each polygon and then calculates 
#    Isolation by Distance (IBD). Please take the time to examine the output 
#    objects of each step, plot intermediate results and dissect the 
#    peripheral.zonal function. Please note; when you dissect a for loop 
#    you need to define the iterator so, in this case if it is defined 
#    (eg., j=1) then you can run the inside of the loop (omitting the for 
#    line and the end curly bracket. Don’t forget, to run the function you 
#    need to copy-and-paste the whole thing into R to "source" it.   
 
# Read shapefile "wetlands" and raster "landtype.tif" in data.dir
( wetlands <- st_read(file.path(data.dir, "wetlands.shp")) )
  ( wetlands <- wetlands[sample(1:nrow(wetlands),10),] )

plot(st_geometry(wetlands))

#########################################
# Landcover classes
# 3-Barren, 4-regeneration, 5-forest,   
# 10-Riparian, 11-urban, 12-pasture,  
# 13-weedy grasses, 15-crops, 19-water
#########################################
lcov <- rast(file.path(data.dir, "landtype.tif") )
  #crs(lcov) <- st_crs(wetlands)

# Syntax for erasing a polygon, follows idea of inverse 
# focal function  
p <- st_buffer(wetlands[1,], dist=1000)
  p <- st_difference(p, st_union(wetlands[1,]))	   			   
    plot(mask(crop(lcov, ext(p)),vect(p)))

# Define classes and class names
( classes <- sort(unique(lcov[])) )
lcnames <- c("barren", "regen", "forest", "riparian",   
             "urban", "pasture", "grasses",  "crops", 
			 "water")

pzonal <- function(x, y, d = 1000, classes = NULL, 
                   class.names = NULL) {
  if(is.null(classes)) {
    classes <- sort(unique(y[]))
  }
  if(!is.null(class.names)) 
    names(results) <- class.names
    suppressWarnings({
	  p <- st_buffer(x, dist=1000)
	  p <- st_difference(p, st_union(x))
	})  
	r <- extract(y, vect(p))
	  r <- lapply(unique(r$ID), function(j) {
	    prop.table(table(factor(r[r$ID==j,][,2],
		levels=classes)))})
  r <- as.data.frame(do.call(rbind, r))
    if(!is.null(class.names)) 
	  names(r) <- class.names
  return(r)
}

# Calculate peripheral zonal statistics
( class.prop <- pzonal(x=wetlands, y=lcov, d = 1000,
                      class.names=lcnames) ) 
 
# check global class proportions (column sums)
apply(class.prop, MARGIN=2, sum)
 
# Isolation by Distance (IBD) 
( d <- as.matrix(st_distance(wetlands) ))	 
  diag(d) <- NA
    d <- apply(d, MARGIN=1, FUN=function(x) min(x, na.rm=TRUE))
      d <- d / max(d)

# add proportion and IBD results to polygons
wetlands$ibd <- d
wetlands <- merge(wetlands, data.frame(ID=wetlands$ID, class.prop), by="ID")

# Plot some landcover proportions
plot(wetlands["crops"])
plot(wetlands["grasses"])
plot(wetlands["pasture"])

# Plot IBD
plot(wetlands["ibd"])

# Define class breaks, cut, create color vector and pass to plot
l <- cut(wetlands$ibd, classBreaks(wetlands$ibd, n=3, 
         type = "geometric"), include.lowest = TRUE,
		 labels=c("low", "med", "high"))

# color vector
my.col <- l
  levels(my.col) <- c("blue", "orange", "red")

# Plot results 
plot(st_geometry(wetlands), col=as.character(my.col), border = NA)
  box()
  legend("bottomleft", legend=levels(l), 
         fill=levels(my.col))  
       
###################################################################
###################################################################
#  Exercise 2.2 - Raster/Vector integration 
###################################################################
###################################################################  

# Read data
plots <- st_read(file.path(data.dir, "plots"))
  lc <- rast(file.path(data.dir, "landcover.tif")) 
 
#******************************************************************
# 1. Extract raster cell values (lc) for points (plots). Note;
#    vector objects (ie., sf) must be terra vect class objects
#    (see extract and vect)
  
extract(lc, vect(plots)) 

#******************************************************************
# 2. Buffer the plots to 200m and then extract the "lc" raster 
#    cell values. What class is the object?
# Bonus; do you see a more efficient way to do this?
# Hint; look at arguments in extract  
	 
plots.buff <- st_buffer(plots, dist=200)
  ( r.vals <- extract(lc, vect(plots.buff)) )   
     
#******************************************************************
# 3. Now write a function, to pass to lapply, that will calculate the 
#    proportion of value 23 and one for 41, 42 and 43. Add to data and
#    play with plotting results 

pct <- function(x, p = 23) { length( x[x == p] ) / length(x) }
  tapply(r.vals[,2], r.vals$ID, FUN=pct)
 
pct <- function(x, p) { length(which( x %in% p)) / length(x) }
  tapply(r.vals[,2], r.vals$ID, FUN=pct, p=c(41,42,43))

# Add to "plots" data
plots$pct.forest <- as.numeric(tapply(r.vals[,2], r.vals$ID, FUN=pct, p=c(41,42,43)))

# Plot results
plot(lc)
  plot(st_geometry(plots), pch=20, col="black", add=TRUE)
  plot(st_geometry(plots[plots$pct.forest >= 0.25 ,]), pch=20,
         cex=2, col="red", add=TRUE)

#######################################################################
#######################################################################
# Day 2.3 Raster data analysis
#######################################################################
####################################################################### 

# set your working and data directories here
setwd("C:/evans/Courses/Texas_AM/day02") 
  data.dir <- file.path(getwd(), "data")

# Add data (single band and multi band)
ppt.jan <- rast(file.path(data.dir, "ppt2000.tif"), lyrs=1) 
ppt.may <- rast(file.path(data.dir, "ppt2000.tif"), lyrs=5)
ppt.2000 <- rast(file.path(data.dir, "ppt2000.tif")) 
ppt.2001 <- rast(file.path(data.dir, "ppt2001.tif"))

#****************************************************************** 
# 1. Calculate summary statistics (global) for mean, sd
#    see; cellStats

# Summary statistics (global)
( rmin <- global(ppt.jan, stat="min") )
( rmax <- global(ppt.jan, stat="max") )
( rmean <- global(ppt.jan, stat="mean") )
summary(ppt.jan)
summary(ppt.jan)[5]

#****************************************************************** 
# 2. Row standardize (x / max(x)) "ppt.jan" then, "ppt.jan" and "ppt.may"  
#    see; cellStats and standard math operations

# Row standardization
ppt.jan.std <- ppt.jan / as.numeric(global(ppt.jan, stat="max"))

# Row standardization using global max values across months
ppt.jan.std <- ppt.jan / max(global(c(ppt.jan, ppt.may), stat="max"))
  summary(ppt.jan.std)

#****************************************************************** 
# 3. Calculate mean of "ppt.jan" and "ppt.may" 
#    see; calc, overlay 

# Difference usage between direct operator, 
#   these two calls have the same result
( ppt.mean <- (ppt.jan + ppt.may) / 2 )
( ppt.mean <- app(c(ppt.jan, ppt.may), fun=mean) )

#****************************************************************** 
# 4. Calculate focal mean of "ppt.jan" within an 11x11 window
#    see; focal, matrix 

# A focal window is defined as a matrix where 1 represents values
# and 0 non-values. A uniform valued nxn window can easily be defined 
# using: matrix(1, nrow=n, ncol=n) 

# Focal functions
ppt.mean11 <- focal(ppt.jan, w=matrix(1,nrow=11,ncol=11), fun=mean)
  par(mfrow=c(1,2)) 
    plot(ppt.mean11)
    plot(ppt.jan)

#****************************************************************** 
# 5. Calculate percent of values <= mean within an 11x11 window
#    see; focal, cellStats, matrix 

# Passing focal custom function
pct.mean <- function(x, p=22.6776) { length(x[x >= p]) / length(x) }

( p.mean = global(ppt.jan, stat="mean")[,1] )
ppt.break <- focal(ppt.jan, w=matrix(1,nrow=5,ncol=5), 
                   fun=pct.mean, p=p.mean) 

#****************************************************************** 
# 6. Calculate median for all dates in ppt.2000 and for growing  
#    season may-aug [5:8]
#    see; calc, median

( med.2000 <- app(ppt.2000, median) )
( med.2000 <- app(ppt.2000[[5:8]], median) )
  plot(med.2000)

#****************************************************************** 
# 7. Calculate abs difference in may [5] for ppt.2000 & ppt.2001 
#    see; overlay

ppt.adif <- lapp(c(ppt.2000[[5]], ppt.2001[[5]]), 
                    fun=function(x,y) { abs(x-y) } )
  plot(ppt.adif)

#****************************************************************** 
# 8. Calculate number of days with rain over median across all dates
#    see; overlay, stack, calc, median

( m <- global(ppt.2000, stat=median)[,1] )
rain.fun <- function(x, p = 23.4997) {
  if( length(which(TRUE == (x %in% NA))) == length(x) )  {
	  return(NA)
    } else {
    return( length(na.omit(x[x >= p])) )
   }
 } 

rain.freq <- app(ppt.2000, fun=rain.fun) 
ppt.mean.2000 <- app(ppt.2000, fun=mean)
ppt.sd.2000 <- app(ppt.2000, fun=sd)
ppt.mean.2000.gs <- app(ppt.2000[[5:8]], fun=mean)

par(mfrow=c(2,2)) 
  plot(ppt.mean.2000, main="Mean precipitation 2000")
  plot(ppt.sd.2000, main="Std precipitation 2000") 
  plot(rain.freq, main="Frequency precipitation 2000")  
  plot(ppt.mean.2000.gs, main="Mean growing season precipitation 2000") 

#****************************************************************** 
# 9. Reproject to UTM using "elev" as reference raster 
#    see; projectExtent, projectRaster, raster

# read elevation raster used in resampling regression equation
( elev <- rast(file.path(data.dir, "elev.tif")) )
( ppt <- rast(file.path(data.dir, "ppt_geo.tif"), lyrs=5) )

# Create a reference raster from an extent
ref <- rast(ext(elev), crs=crs(elev))
  res(ref) <- 1000 # target resolution from native DD 0.04166667 

# Reproject to UTM 
( ppt.utm <- project(ppt, ref, method="bilinear") )
						  						  
par(mfrow=c(2,1))
  plot(ppt)
  plot(ppt.utm)


#######################################################################
#######################################################################
# Quantifying landscape structure
#######################################################################
####################################################################### 

devtools::install_github("jeffreyevans/landmetrics")

setwd(setwd("C:/evans/Courses/Texas_AM/day02") )
  data.dir <- file.path(getwd(), "data")

#######################################################################
#######################################################################
# Optional Exercise 2.4
#   Calculating landscape metrics for a whole landscape
#######################################################################
#######################################################################

# add data
plots <- st_read(file.path(data.dir, "plots.shp")) #read in plot data
land.cover <- rast(file.path(data.dir, "landcover.tif")) 
  plot(land.cover)
    plot(plots, pch=20, add=TRUE)

#***********************************************************************
# 1.  You are interested in forest/non-forest.  However, there are 
#     multiple forest classes (41,42, 43).  
#     Reclassify as forest/non-forest (forest=41,42,43) 
#
# Hint: reclassify vs. writing a function (ifelse), calc.

# three reclassify solution for forest/nonforest
m <- c(0,40.8, 0,40.9,43.1,1,43.9,91,0)
reclass <- matrix(m, ncol=3, byrow=TRUE)
forest <- classify(land.cover, reclass)

# function solution, much more stable. 
#   ifelse as a function 
fnf <- function(x) { ifelse( x == 41, 1, 
                       ifelse( x == 42, 1, 
                         ifelse( x == 43, 1, 0)))
    }						 
  forest <- app(land.cover, fun=fnf)

# ifelse directly as ifel method
forest <- ifel(land.cover == 41, 1, 
            ifel(land.cover == 42, 1, 
              ifel(land.cover == 43, 1, 0)))

plot(forest)
  plot(plots, pch=20, add=TRUE)

#***********************************************************************
# 2.  While forest at a cell may be important, you want to smooth your 
#     result and put the overall amount of forest in context.  
#     Calculate percent forest within 11x11 window.  Create a function  
#     for a calculating percent and create a new raster that is pct.forest 
#     in a 11 X11 window 
#
# Hint: length of target value, focal  

pct <- function(x) { round( (length(x[x == 1]) / length(x)), 4) } 
pct.forest <- focal(forest, w=matrix(1,nrow=11,ncol=11), fun=pct)

#***********************************************************************
# 3. Next you wish to identify forest cores.  Calculate 60 percent volume 
#    of forest percent to identify core areas.  
#
# see; raster.vol

cores <- raster.vol(raster(pct.forest), p=0.60)
  cores[is.na(cores)] <- 0 #replace non-core areas (NA) with 0
    cores <- mask(rast(cores), forest)
  
par(mfrow=c(1,2))
  plot(pct.forest)  
    plot(cores)

#***********************************************************************
# 4.  Describe the landscape using landscape metrics and compare forest to
#     forest cores. HINT: landscapemetrics, pland, contagion, proportion of
#     like adjacencies (many other available metrics, select those you think
#     are most meaningful)
#
# see; lsm_c_pland, lsm_l_pladj, lsm_l_contag  
 
# Percent landscape
( forest.pland <- lsm_c_pland(forest, directions=8) )
( cores.pland <- lsm_c_pland(cores, directions=8) )

# contagion
( forest.contag <- lsm_l_contag(forest, verbose=TRUE)$value )
( core.contag <- lsm_l_contag(cores, verbose=TRUE)$value )

# proportion of like adjacencies 
( forest.plaj <- lsm_l_pladj(forest)$value )
( core.plaj <- lsm_l_pladj(cores)$value )

#######################################################################
#######################################################################
# Optional Exercise 2.5
#   Calculating landscape metrics for sample locations
#######################################################################
#######################################################################

#***********************************************************************
# 1.  Calculating landscape metrics for sample locations is a powerful 
#     way to compute covariates. Calculate landscape metrics for plots 
#     for both forest and cores.
#  
# see; land.metrics, ssplot

mfnf <- as.data.frame(sample_lsm(forest, y = plots, plot_id = plots$ID, 
           size = 500, shape="circle", level = "landscape", 
		   type = "aggregation metric", 
           classes_max = 2,
           verbose = FALSE))
  head(mfnf)
  
# Print available metrics 
am <- as.data.frame(list_lsm())

# Subset to landscape level metrics
LM <- am[am$level == "landscape",] 

# Metric by type (full names)
tapply(LM$name, LM$type, unique)
 
# Metric by type (returned names)
tapply(LM$metric, LM$type, unique)

## class and patch level metrics  
# CM <- am[am$level == "class",] 
# PM <- am[am$level == "patch",]   

# pull contagion 
contag <- mfnf[which(mfnf$metric %in% "contag"),]$value  
  plots$fnf_contag <- ifelse(is.na(contag), 0, contag)

# Evaluate core areas  
mcores <- as.data.frame(sample_lsm(cores, y = plots, plot_id = plots$ID, 
           size = 500, shape="circle", level = "landscape", 
		   type = "aggregation metric", 
           classes_max = 2,
           verbose = FALSE))
		   
contag <- mcores[which(mcores$metric %in% "contag"),]$value  
  plots$core_contag <- ifelse(is.na(contag), 0, contag)

# Plot core 
plot(cores, legend=FALSE)
  plot(plots["core_contag"], pch=20, cex=2, add=TRUE)

