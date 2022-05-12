###################################################################
###################################################################
# Add required libraries 
# Download data for exercises (need to set a working directory)
###################################################################
###################################################################
p = c("sp", "raster", "spdep", "rgdal", "rgeos",  
      "spatialEco", "sf", "terra", "spatstat", "spatstat.geom")
  if(any(!unlist(lapply(p, requireNamespace, quietly=TRUE)))) { 
    m = which(!unlist(lapply(p, requireNamespace, quietly=TRUE)))
	  suppressMessages(invisible(lapply(p[-m], require,    
	                   character.only=TRUE)))
    stop("Missing library, please install ", paste(p[m], collapse = " "))
  } else {
    suppressMessages(invisible(lapply(p, require, character.only=TRUE)))
  }

# set your working directory variable here
setwd("C:/spatialR/day01") 
  data.dir = file.path(getwd(), "data")

download.file("https://spatialr.s3-us-west-2.amazonaws.com/day01.zip", 
              destfile="day01.zip", mode="wb")
  unzip("day01.zip")
  file.remove("day01.zip")

# data is in day01/data, you can use file.path to set a path to 
# the data directory eg., dat.dir <- file.path(getwd(), "data")

###################################################################
###################################################################
#  Exercise 1.1 - Reading and writing of spatial classes
###################################################################  
###################################################################

#******************************************************************
# 1.1.1. Read the "birds" shapefile, (see st_read) and   
#    display the first few rows of associated data.  

#birds <- rgdal::readOGR(file.path(getwd(), "data"), "birds")
#as(st_read(file.path(data.dir, "birds.shp")), "Spatial")

( birds <- sf::st_read(file.path(data.dir, "birds.shp")) )
  str(birds)

#******************************************************************
# 2. Subset the birds data, using a bracket index, using the condition
#    Max2005 >= 20 (hint: x[x$col >= p,] ) 

birds.sub <- birds[birds$Max2005 >= 20,]  

#******************************************************************
# 3. Write the subset observations out as new shapefile then, read 
#    back in. See writeORG or the tricky way using st_write with 
#    as "Spatial"

# writeOGR(birds.sub, file.path(getwd(), "data"), "birds_sub", 
#          driver="ESRI Shapefile", check_exists=TRUE, 
#	 	   overwrite_layer=TRUE)

sf::st_write(birds.sub, "birds_sub.shp")

#******************************************************************
# 4. Read the isograv.img raster into a raster object (see raster) 

r <- rast(file.path(data.dir,"isograv.img"))

#******************************************************************
# 5. Coerce it into a SpatialPixelsDataFrame object
#    using "as" function with "SpatialPixelsDataFrame"
#    unfortunately, terra does not provide this coercion but,
#    the raster package does.

r.sp <- as(raster(r), "SpatialPixelsDataFrame")
  plot(r.sp)
  head(r.sp@data) 
 
#******************************************************************
# 6. Make a multi-band raster object using bouguer, isograv, magnetic 
#    img rasters. See rast, +1 for using list.files  

r <- rast(c(file.path(data.dir,"bouguer.img"), 
          file.path(data.dir,"isograv.img"), 
          file.path(data.dir,"magnetic.img")))

( f <- list.files(data.dir, "img$", full.names = TRUE) )
( r <- rast(f[c(1,3,4)]) )

#******************************************************************
# Note; you can write out the multi-band raster, or single-band,
# using writeRaster or, if a SpatialPixelsDataFrame, writeGDAL.
# I prefer the geo tiff format with LZW compression. 
#
# You do not have to specify datatype but, it is good to know
# how to control bit depth. Here we specify INT1U which is 
# float -3.4e+38 to 3.4e+38 

# Here is an example:
writeRaster(r, "x.tif", overwrite=TRUE,   
           gdal=c("COMPRESS=LZW"), 
		   datatype='INT1U')
		   
##################################################################
##################################################################
#  Exercise 1.2 - Indexing and query of spatial classes
##################################################################
##################################################################

# We will use the meuse dataset for all of these exercises, add the
# data and coerce to sf points (see: st_as_sf_
data(meuse)
meuse <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992, 
                  agr = "constant")

#  coordinates(meuse) <- ~x+y

#******************************************************************
# 1. subset the first 10 rows of meuse to a new dataset 
#
# Hint: Use a standard bracket index, remember the comma position for
#       rows verses columns. Observations in an sp object are
#       by row 

( msub <- meuse[1:10,] )

#******************************************************************
# 2. Create a random sample of meuse (n=10)
#
# Hint; sample with 1:nrow(meuse) to create sample index

( mrs <- meuse[sample(1:nrow(meuse), 10),] )

#******************************************************************
# 3. Query the meuse attribute "copper" to subset data to 
#    greater than/equal to 75th percentile of copper 
#
# Hint; bracket index using ">=" and quantile function 

( cop075 <- meuse[meuse$copper >= quantile(meuse$copper, p=0.75),] ) 

#******************************************************************
# 4. Calculate the percent of class 1 in "soil" 

nrow(meuse[meuse$soil == "1",]) / nrow(meuse)

#******************************************************************
# 5. Calculate the mean of cadmium for all soil classes.  
#    Hint; tapply

tapply(meuse$cadmium, meuse$soil, mean)

#******************************************************************
# 6. Create random samples from the polygon 
# Buffer observsations 1, 50 and 100 to 500m and then create 10 random 
# samples for each polygon. Hint; st_buffer, st_sample, for or lapply, st_as_sf 
p <- st_buffer(meuse[c(1,50,100),], dist=500)
  s <- st_sample(p, size = 10) 
    plot(st_geometry(p))
      plot(s, pch=20, add=TRUE)
   
# Wait, that's not 10 per-polygon. We need to iterate over
# the polygons (for or lapply)

s <- do.call(rbind,   
  lapply(1:nrow(p), function(i) { st_as_sf(st_sample(p[i,], 
         size = 10))} ) )     

plot(st_geometry(p))
  plot(s, pch=20, add=TRUE) 
		 
#******************************************************************
# 7. Draw a distance based random sample from a single observation 
#    drawn from meuse (sp) using; xy <- meuse[2,] Use 15-100 and 100-200 
#    for distances with 50 random samples, plot results. 
#    Hint; sample.annulus  

data(meuse)
  coordinates(meuse) <- ~x+y
  proj4string(meuse) <- CRS("+init=epsg:28992")

xy <- meuse[2,]

rs100 <- sample.annulus(xy, r1=50, r2=100, n = 50, type = "random")
rs200 <- sample.annulus(xy, r1=100, r2=200, n = 50, type = "random")

plot(rs200, pch=20, col="red")
  points(rs100, pch=20, col="blue")
  points(xy, pch=20, cex=2, col="black")
  box()
  legend("topright", legend=c("50-100m", "100-200m", "source"), 
         pch=c(20,20,20), col=c("blue","red","black"))

###################################################################
###################################################################
#  Exercise 1.3 - Functions 
###################################################################
###################################################################

# Calculate the Nearest Neighbor Index (NNI) using meuse (sp)
#   see: nni
data(meuse)
  coordinates(meuse) <- ~x+y

nni(meuse)

#******************************************************************
# 1. Dissect the following function by:
#     a. defining parameters x = meuse and win = "hull" 
#     b. stepping through each line and if block, exploring 
#        the value and class of each output 
#
# How could you make this function work with sf objects?   
   
nni <- function(x, win = "hull") {
    if (!class(x) == "SpatialPointsDataFrame" & !class(x) == "SpatialPoints") 
      stop(deparse(substitute(x)), " MUST BE A sp POINTS OBJECT")
    if (win == "hull") {
      w <- spatstat.geom::convexhull.xy(sp::coordinates(x))
    }
    if (win == "extent") {
      e <- as.vector(sp::bbox(x))
      w <- spatstat.geom::as.owin(c(e[1], e[3], e[2], e[4]))
    }
    x <- spatstat.geom::as.ppp(sp::coordinates(x), w)
    A <- spatstat.geom::area.owin(w)
      obsMeanDist <- sum(spatstat.geom::nndist(x))/x$n
        expMeanDist <- 0.5 * sqrt(A / x$n)
          se <- 0.26136 / ((x$n**2.0 / A)**0.5)
        nni <- obsMeanDist / expMeanDist
      z <- (obsMeanDist - expMeanDist) / se
    return(list(NNI = nni, z.score = z, p = 2*stats::pnorm(-abs(z)),  
	       expected.mean.distance = expMeanDist,
		   observed.mean.distance = obsMeanDist))
}

#******************************************************************   
# 2. Write a function that returns observed mean distance 
#    from the above nni function, don't worry about 
#    bells-and-whistles. The object to return from the 
#    nni function is "obsMeanDist"

omd <- function(x) {
    w <- spatstat.geom::convexhull.xy(sp::coordinates(x))
      x <- spatstat::as.ppp(sp::coordinates(x), w)
      A <- spatstat.geom::area.owin(w)
    obsMeanDist <- sum(spatstat.geom::nndist(x)) / x$n
  return(obsMeanDist)
}

omd(meuse)

##################################################################
##################################################################
#  Exercise 1.5 - Plotting
##################################################################
##################################################################

# We will use the meuse dataset for all of these exercises

data(meuse)
  coordinates(meuse) <- ~x+y

#******************************************************************
# 1. Plot only soil class 1, hint; plot can accept a bracket index 
#    to subset the data

plot( meuse[meuse$soil == "1",], pch=19)

#******************************************************************
# 2. Make a 3 panel plot window and plot soil classes 1, 2 and 3 as 
#    separate plots. Hint; par with mfrow or mfcol arguments 

par(mfrow=c(1,3))
  plot(meuse[meuse$soil == "1",], pch=19)
    box()
  plot(meuse[meuse$soil == "2",], pch=19)
    box()
  plot(meuse[meuse$soil == "3",], pch=19)
    box()

#******************************************************************
# 3. Create a color vector for the soil column and plot meuse by soil 
#    class colors (see ifelse, plot with col and pch arguments, 
#    use "red", "green" and "blue" for your "col" colors).
#    Hint; you can pass a vector of colors to the col argument in plot. 

my.col <- ifelse(meuse$soil == "1", "red",
            ifelse(meuse$soil == "2", "green", 
			  ifelse(meuse$soil == "3", "blue", NA)))
			   
plot(meuse, pch=19, col=my.col)

# With sf object
data(meuse)
meuse <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992, 
                  agr = "constant")
plot(st_geometry(meuse), pch=19, col=my.col)
