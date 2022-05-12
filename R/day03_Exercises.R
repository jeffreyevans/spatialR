###################################################################
###################################################################
# Add required libraries 
# Download data for exercises (need to set a working directory)
###################################################################
###################################################################

#   please note that you may need to 
#     install the "rts" package
p = c("sp", "terra", "spdep", "spatialEco", "sf", "terra", 
      "raster", "spatstat", "spatstat.geom", "spatstat.core",
	  "dplyr", "forecast", "RANN", "rgeoda")
  if(any(!unlist(lapply(p, requireNamespace, quietly=TRUE)))) { 
    m = which(!unlist(lapply(p, requireNamespace, quietly=TRUE)))
	  suppressMessages(invisible(lapply(p[-m], require,    
	                   character.only=TRUE)))
    stop("Missing library, please install ", paste(p[m], collapse = " "))
  } else {
    suppressMessages(invisible(lapply(p, require, character.only=TRUE)))
  }

# set your working and data directories here
setwd("C:/evans/Courses/Texas_AM/day03") 
  data.dir <- file.path(getwd(), "data")

download.file("https://spatialr.s3-us-west-2.amazonaws.com/day03.zip", 
              destfile="day03.zip", mode="wb")
  unzip("day03.zip")
  file.remove("day03.zip")

##################################################################
###################################################################
#  Exercise 3.1 - Distance and proximity
###################################################################
###################################################################

# Read data 
plots <- st_read(file.path(data.dir, "plots.shp"))
  plots$plot.id <- paste("plot", plots$ID, sep=".")

#### Distance based neighbors

#******************************************************************
# 1. Calculate a distance matrix on the "plots" point object
#    see spDists, dist

# dist matrix
dmat <- units::drop_units(st_distance(plots))
  diag(dmat) <- NA  # set 0 diag to NA
    rownames(dmat) <- plots$plot.id
    colnames(dmat) <- plots$plot.id
head(dmat)

#******************************************************************
# 2. See if you can use the distance matrix to find each obs
#    nearest neighbor

# Iterate through each row and select the minimum distance neighbor
samples <- data.frame(plot.id=plots$plot.id, kNN=NA, dist=NA)
  for(j in 1:nrow(dmat)) {
    samples[j,][2] <- names(dmat[,j])[which.min(dmat[,j])]
    samples[j,][3] <- dmat[,j][which.min(dmat[,j])]
  }

## or, using lapply
# samples <- as.data.frame(do.call(rbind,(lapply(1:nrow(dmat), function(j) {
#   c(names(dmat[,j])[which.min(dmat[,j])], 
#     dmat[,j][which.min(dmat[,j])])}))))
#   samples[,2] <- as.numeric(samples[,2])

# Or, a straight forward R-like way using apply 
nn <- apply(dmat, MARGIN=1, which.min)
  ( kNN.ids <- colnames(dmat)[nn] )

# Add kNN to plots point data and subset kNN observations  
plots <- merge(plots, samples, by="plot.id")
  plots.knn <- plots[which(plots$plot.id %in% unique(plots$kNN)),]

plot(st_geometry(plots), pch=20)
  plot(st_geometry(plots.knn), pch=20, col="red", cex=2, add=TRUE)
    box()
  
#******************************************************************  
# 3. Can you bound the distances considered before finding the
#    nearest neighbor?
#  
# Here you could set a distance bandwidth (range) in the same way
# the minimum distance was set 

min.dist = 1000
max.dist = 5000 
dmat[dmat < min.dist] <- NA
dmat[dmat > max.dist] <- NA

nn <- apply(dmat, MARGIN=1, which.min)
  ( kNN.ids <- colnames(dmat)[nn] )

#******************************************************************  
# 4. Optionally, repeat the above analysis using a graph structure 
#    see; spdep functions: knearneigh and dnearneigh 

plots.knn <- knearneigh(st_coordinates(plots), k=4)
 plot(st_geometry(plots), pch=19)
   box()
   plot(knn2nb(plots.knn), st_coordinates(plots), add=TRUE)
       title(main="K nearest neighbors, k=1")

nn1.ids <- as.vector(plots.knn$nn[1,])             
  nn1 <- plots[nn1.ids,]
  plot(st_geometry(nn1), pch=19, col="red")    
    plot(st_geometry(plots[1,]), pch=19, col="black", 
	     cex=2.5, add=TRUE)

#*** Distances of neighbors using spdep	
plots.dist <- dnearneigh(st_coordinates(plots), 0.0001, 10000)
  dist.list <- nbdists(plots.dist, st_coordinates(plots))
  plots$NNDist <- unlist(lapply(dist.list, FUN=function(x) min(x)))

plot(plots["NNDist"], pch=20, main="neighbor distances")

#******************************************************************
# 5. Now, after all that work apply this single function "knn" 
#    that does all of this
data(meuse, package = "sp")
meuse <- st_as_sf(meuse, coords = c("x", "y"), 
                  crs = 28992, agr = "constant")

idx <- sample(1:nrow(meuse), 10) 
  pts <- meuse[idx,]
  meuse <- meuse[-idx,]
    meuse$IDS <- 1:nrow(meuse)

nn <- RANN::nn2(st_coordinates(meuse), st_coordinates(pts), k=2)$nn.idx
  plot(st_geometry(pts), pch=20, main="KNN")
    plot(st_geometry(meuse[nn[,1],]), pch=20, col="red", add=TRUE)
    plot(st_geometry(meuse[nn[,2],]), pch=20, col="blue", add=TRUE)
      box()

###################################################################
###################################################################
#  Exercise 3.2 - Time-series analysis
###################################################################
###################################################################

# Add LAI raster stack with 71 monthly layers, we will also
# create a date vector representing the image dates	
( lai <- rast(file.path(data.dir, "lai.tif")) )
  ( dates <- date_seq("2000/01/01", "2005-12-01", step="month") )

# plot 1st and 50th rasters
dev.new(height=6, width=11)
  plot(lai[[c(1,50)]])

#******************************************************************
# 1. Using x,y coordinates cbind(13028094, 146990) extract a pixel 
#    time-series from the lai stack 
#
#  You can also try plotting a single raster layer and then using  
#  click() to get different coordinates printed to screen
#  see, cellFromXY  

( x <- as.numeric(lai[cellFromXY(lai, cbind(13028094, 146990))]) )

#******************************************************************
# 2. Apply a smoothing function and plot
#    see impute.loess 

x.smooth <- impute.loess(x, s = 0.4, smooth = TRUE)
  dev.new(height=6, width=14)
    plot(dates, x, type="l")
      lines(dates, x.smooth, col="red")
    legend("topleft", legend=c("raw", "smoothed"), 
           lty=c(1,1), col=c("black","red"))  

#******************************************************************
# 3. Evaluate confidence of fit via permutation test
#    see, loess.boot 
sb <- loess.boot(1:length(x), x, nreps=99, 
                 confidence=0.90, span=0.30)
  dev.new(height=6, width=14)
    plot(sb)

#******************************************************************
# 4. Create a ts (time-series) object
x.ts <- ts(x.smooth, start= c(2000, 01), frequency = 12)

#******************************************************************
# 5. Specify a time-series trend model 
#    see; ts, forecast, tslm  
fit <- forecast::tslm(x.ts ~ trend)
  trend <- forecast::forecast(fit, h=length(x), level=0)

  # Plot linear trend	
  dev.new(height=6, width=14)
    plot(dates, x, type="l", main="LAI time-series, no correction")
      lines(dates, trend$fitted, lwd = 2)

#******************************************************************
# 5. Account for seasonal effects and specify detrended linear model
#    - now you can see we are getting  
#      somewhere in representing a more 
#      accurate trend
detrend <- decompose(ts(x, start= c(2000, 01), frequency = 12))
  fit.dt <- forecast::tslm(detrend$trend ~ trend)
    trend.dt <- forecast(fit.dt, h=length(x), level=0)

  # decomposed trend and seasonal components
  dev.new(height=6, width=14)
    plot(detrend)

  dev.new(height=6, width=14)
    plot(detrend$trend, type="l", main="LAI detrended time-series")
      lines(trend.dt$fitted, lwd = 2)

###################################################################
###################################################################
#  Exercise 3.3 - Raster time-series analysis
###################################################################
###################################################################

#******************************************************************
# 1. Create a raster time-series object and calculate yearly means
#    Don't forget that dates object that we created
#    see; rts, apply.yearly 

lai.ts <- rts(lai, dates)
  means <- apply.yearly(lai.ts, mean)@raster
plot(means)

#******************************************************************
# 2. Calculate simple change and rate of change and plot

chg <- means[[1]] - means[[6]]
  roc <- lapp(means[[c(1,6)]], fun=function(x, y) { (x - y) / x * 100 })

  dev.new(height=12, width=10)
    par(mfrow=c(2,1))
      plot(chg, main="delta")
	  plot(roc, main="rate of change")

#******************************************************************
# 3. Calculate Kendall Tau temporal correlation and Theil-Sen Slope
#    see; raster.kendall
k <- raster.kendall(stack(means), tau = TRUE)
  dev.new(height=6, width=11)
    plot(k)

#******************************************************************
# 4. Now, let's put a few pieces together. Apply the detrended 
#    regression to our raster time-series to get a spatial estimate 
#    of the detrended slope 
#
#    Here is a function for returning slope of
#    time-series detrended regression
ts.trend <- function(y, d=c(2000, 01)) {
  y <- na.omit(y)
  if(length(y) >= 12) { 
    detrend <- stats::decompose(stats::ts(y, start= c(2000, 01),
	                            frequency = 12))
      trend <- forecast::tslm(detrend$trend ~ trend)
	s <- stats::coefficients(trend)[2] 
  } else {   
	s <- NA 
  }
  return(as.numeric(s))
}

# pass function to raster calc and plot
slp <- app(lai, ts.trend)

dev.new(height=11, width=8)
  par(mfcol=c(2,1))
    plot(slp, main="detrended slope")
    plot(rast(k[[1]]), main="Kendall slope")

#**********************************************************************
#**********************************************************************
# Exercise on Spatial autocorrelation and testing of model assumptions
#
# See also, SpatialDependency lecture materials
#**********************************************************************
#**********************************************************************

# set your working and data directories here
setwd("C:/spatialR/day03") 
  data.dir <- file.path(getwd(), "data")

###################################################################
###################################################################
#  Exercise optional 3.4
#  What if you wish to describe a population? 
#
#  Point pattern analyses (PPA)
###################################################################
###################################################################

# We have a csv file of wetlands from the Bighorn Crags of Idaho  
# and want to know if these wetlands are clustered. 

# read data
wetland <- read.csv(file.path(data.dir, "Wetlands.csv"), header=TRUE)
 
#****************************************************************** 
# 1.  Create a ppp object from wetland csv file. The file includes all wetlands. 
#     RALU is the presence or absences of Columbia spotted frogs at sites and  
#     can function as a mark.  
#
# Hint: owin and ppp

window <- owin(xrange=c(min(wetland$X),max(wetland$X)), 
               yrange=c(min(wetland$Y),max(wetland$Y)))

#*** window is min/max of x and Y
wetland.ppp <- ppp(wetland$X, wetland$Y, window=window, marks=wetland$RALU)
  class(wetland.ppp)

#****************************************************************** 
# 2.  Not all data are perfect.  This dataset include duplicate points 
#     that are likely an error.  Remove them from the dataset.  
# Hint: unique

wetland.ppp <- unique(wetland.ppp)

#****************************************************************** 
# 3.  Are wetlands homogeneous? Calculate Nearest Neighbor Distance (G) 
#
# Hint: Gest

plot(Gest(wetland.ppp))

# rs: Edge corrected estimator of G(r )
# km: the spatial Kaplan-Meier estimator of G(r )
# pois: the theoretical value of G(r ) for a stationary Poisson process 

#   i.e., homogeneous of the same estimated intensity. 
#   No, they are clustered

#****************************************************************** 
# 4.  So, the wetlands are clustered in space. What is the range 
#     of spatial structure?
#
# Use Ripley's-K for exercises 4a and 4b   
#
#****************************************************************** 
# 4a.  What do spatially random data look like in a Ripley's K of 
#      same size as dataset?
#
# Hint: rpoispp, Kest

random.wet <- rpoispp(length(wetland[,1]))
  plot(random.wet) #plot spatially random "wetland" synthetic data
  plot(Kest(random.wet)) #Ripley's K on synthetic data

#****************************************************************** 
# 4b.  Compare result from spatially random data to wetland data
plot(Kest(wetland.ppp))

###################################################################
###################################################################
#  Exercise optional 3.5
#  What if you wish to know if observations of species abundance are 
#  spatially autocorrelated.
#                  
#  Measure of Global and Local autocorrelation
###################################################################
###################################################################

sdata <- read.csv(file.path(data.dir, "RALU_abundance.csv"))
  sdata <- st_as_sf(sdata, coords = c("X", "Y"), 
                    crs = 26911, agr = "constant")

# distinct(sdata)

#****************************************************************** 
# 1.  Plot the distribution (pdf) of abundances using the "abundance" column
#
# Hint: density

plot(density(sdata$abundance))

#****************************************************************** 
# 2.  Create a neighbor matrix (all neighbors) from the sdata.  
#
# Hint: knearneigh and knn2nb 

( nm <- knn2nb(knearneigh(sdata)) )

#****************************************************************** 
# 3.  Create contiguity by distance.  
#     This requires: finding the max distance, then creating a 
#     distance matrix.  
#
# Hint: nbdists, max, dnearneigh

# find the max distance
#( max.dist <- units::drop_units(max(st_distance(sdata))) )

# adding 1 is a rough fix for empty neighbors 
( max.dist <- max(unlist(nbdists(nm, sdata))) + 1 ) 

#*** create contiguity by distance, including all distances

nb <- dnearneigh(sdata, 0, max.dist)

#****************************************************************** 
# 4.  Create a spatial weight matrix.  
#
# Hint: nb2listw, style?

( sweight <- nb2listw(nb, style="W") )

###################################################################
###################################################################
#  Exercise optional 3.6
#  Calculate Moran's-I (global autocorrelation)
###################################################################
###################################################################

#****************************************************************** 
# 1.  Calculate Moran's I for Columbia spotted frog abundance.  
#       NOTE: abundance data are synthetic as original data 
#             did not have estimate of abundance (just P/A)
#
# Hint: moran using previous weights (Wij) matrix "sweight" 

#  sdata[,Var] - identifies the target variable in sdata
#  sweight - this is the listw object
#  length(nb) - number of zones.
( abund.I <- moran(sdata$abundance, sweight, length(nb), Szero(sweight)) )

#****************************************************************** 
# 2.  Are the results significant? 
#
# Hint: moran.test, moran.mc

nsim <- 999

moran.test(sdata$abundance, nb2listw(nb, style="W"))
  ( Iperm <- moran.mc(sdata$abundance, listw=sweight, nsim=nsim) ) 

#*** Describe the distribution of the permuted Moran's I
mean(Iperm$res[1:nsim])
var(Iperm$res[1:nsim])
summary(Iperm$res[1:nsim])

###################################################################
###################################################################
#  Exercise optional 3.7
#  Calculate Geary's-C (global autocorrelation)
###################################################################
###################################################################

#****************************************************************** 
# 1.  Calculate Geary's C.  
# 
# Hint: geary.  What are the expectations? 

geary(sdata$abundance, sweight, length(nb), length(nb)-1, Szero(sweight))

#****************************************************************** 
# 2.  Are the results statistically significant?  Test this using 
#     a permutation test.  
#
# Hint: geary.mc

nsim <- 999

cperm <- geary.mc(sdata$abundance, nb2listw(nb),
                  nsim=nsim, alternative="greater") 
  plot(cperm)

#*** assess if normal distribution.  
mean(cperm$res)
var(cperm$res)
summary(cperm$res)

###################################################################
###################################################################
#  Exercise optional 3.8
#  Test spatial structure using Getis-Ord
#
#  This compliments local statistics
###################################################################
###################################################################

#****************************************************************** 
# 1.  Getis-ord compliments local statistics.  
#     Calculate Getis-Ord for abundance 
#
# Hint: globalG.test

sweights <- nb2listw(nb, style="B")
( globalG <- globalG.test(sdata$abundance, sweights,  
                          alternative="greater", 
						  adjust.n=TRUE) )

###################################################################
###################################################################
#  Exercise optional 3.9
#  Why do we care about global spatial autocorrelation?
# 
#  Assessing potential impact of spatial autocorrelation (global)
###################################################################
###################################################################

# read data to build the objects for a slightly different set of data
ralu.lm <- st_read(file.path(data.dir, "ralu.lm.shp"))
  
  # build neighbor list and find the max distance
  nm.ralu <- knn2nb(knearneigh(ralu.lm)) 
  max.dist <- max(unlist(nbdists(nm.ralu, ralu.lm))) + 1 
  
  # Build Wij using distance, not proximity (kNN)
  nb.ralu <- dnearneigh(ralu.lm, 0, max.dist)
    sweight.ralu <- nb2listw(nb.ralu, style="W")

#****************************************************************** 
# 1. create a linear model explaining abundance.  
#
# Hint: lm

summary( froga.lm <- lm(abundance ~ Elev + pH + Depth + Area + Dforest, 
               data= ralu.lm) )
	
#****************************************************************** 	
# 2.  Plot regression diagnostics.  
#
# Hint: graph 2X2 plot panel. 

opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
  plot(froga.lm, las = 1)    
par(opar)

#****************************************************************** 	
# 3.  Calculate Moran's-I on regression residuals.
#
#     Find residuals of lm object, attach to dataframe and Moran's I.  
#     What does it mean if there is significant autocorrelation in the 
#     residuals? 
#
# Hint: lm.morantest; lm.LMtests, moran.test, residuals
 
#*** option 1 - lm.morantest, moran residuals
( moran.res.ralu <- lm.morantest(froga.lm, nb2listw(nb.ralu, style="W")) )

#*** option 2 - lm.LMtests, Lagrange spatial dependence test on residual error
( ralu.lagrange <- lm.LMtests(residuals(froga.lm), nb2listw(nb.ralu)) )

#*** Option 3 - moran.test, moran's I directly on residuals
( ralu.moran.res <- moran.test(residuals(froga.lm), 
                      nb2listw(nb.ralu, style="W")) )

###################################################################
###################################################################
#  Exercise optional 3.10
#  Why do we care about local spatial autocorrelation?
# 
#  Assessing potential impact of spatial autocorrelation (local)
###################################################################
###################################################################

# read data
sdata <- read.csv(file.path(data.dir, "RALU_abundance.csv"))
  sdata <- st_as_sf(sdata, coords = c("X", "Y"), 
                    crs = 26911, agr = "constant")

#****************************************************************** 
# 1. Calculate measure(s) of local autocorrelation and plot them.  
#
# Hint: localG, localmoran (and weight matrix), crossCorrelation 

# adding 1 is a rough fix for empty neighbors 
nm <- knn2nb(knearneigh(sdata)) 
max.dist <- max(unlist(nbdists(nm, sdata))) + 1 
nb <- dnearneigh(sdata, 0, max.dist)

#*** local G
G <- localG(sdata$abundance, nb2listw(include.self(nb), style="B"))
  sdata$G <- as.numeric(G)
plot(sdata["G"], main="Getis-Ord Local G*", pch=20)
  plot(density(sdata$G))

#****************************************************************** 
# Now, the easy way with the GEODA API "rgeoda"

#*** local Moran's (LISA) using traditional Anselin (1995) approach
# Anselin, L. 1995. Local indicators of spatial association, 
#   Geographical Analysis, 27:93–115

# Create Queen contiguity, also see distance_weights, knn_weights 
# and  kernel_weights for alternate Wij options
w <- queen_weights(sdata)
  is_symmetric(w)
  weights_sparsity(w)
  get_neighbors(w, idx = 1)

# Observation-level spatial lags
spatial_lag(w, sdata["abundance"])  

# Calculate LISA local autocorrelation. Local Geary, 
# Getis-Ord, quantile LISA and Join Count (binomial) 
# as univariate or multivariate are also available      
( lisa <- local_moran(w, sdata["abundance"]) )
    lisa_colors <- lisa_colors(lisa)
    lisa_labels <- lisa_labels(lisa)
    lisa_clusters <- lisa_clusters(lisa)
plot(st_geometry(sdata), pch=20,  
     col=sapply(lisa_clusters, function(x){return(lisa_colors[[x+1]])}))
	 box()
  title(main = "Local Moran Map of abundance")
  legend('bottomleft', legend = lisa_labels, pch=c(20), col = lisa_colors)

#### for reference, spdep approach
#  #*** Create local autocorrelation object "I" using "sdata" 
#  nb <- dnearneigh(sdata, 0, max(spDists(sdata))/2)
#  I.local <- localmoran(sdata@data$abundance, nb2listw(nb, style="B"))
#  
#  #*** create sp object of results
#  I <- sdata
#    I@data <- data.frame(sdata@data, as.data.frame(I.local))
#      names(I)[9] <- "p.value"
#  		
#  #*** Plot critical values
#  spplot(I, "Z.Ii", xlab="Local Moran's-I", 
#         col.regions=topo.colors(50))
#  
#  #*** High and Low have no distinct I values so one must create  
#  #    a vector to distinguish significant and high hotspots
#  I@data <- data.frame(I@data, 
#    HotSpots = ifelse( I.local[,5] <= 0.05 & I.local[,4] >= mean(I.local[,4]), 1, 0) )
#      I@data$HotSpots <- as.factor(I@data$HotSpots)  
#        spplot(I, "HotSpots", xlab="Local Moran's-I Hot Spots", 
#               col.regions=c("blue","red") )

#*** LISA using a matrix algebraic approximation following Chen (2015) 
# Chen., Y. (2015) A New Methodology of Spatial Cross-Correlation Analysis.
#   PLoS One 10(5):e0126158. doi:10.1371/journal.pone.0126158 

#*** Create local autocorrelation object "I" using "sdata"
#*** using crossCorrelation function 
( I <- crossCorrelation(sdata$abundance, 
                        coords = st_coordinates(sdata), 
                        clust = TRUE, k=99) )
						
#*** create sp object of results
sdata$lisa <- I$SCI[,"lsci.xy"]
  sdata$lisa.clust <- as.factor(I$cluster)
  
      spplot(sdata, "lisa")
      spplot(sdata, "lisa.clust")	  
