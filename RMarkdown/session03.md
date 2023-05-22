---
source: Rmd
title: "spatialR workshop  session 3 - Distance, proximity, spatial depedency and time-series analysis"
author:
  - Jeffrey S. Evans^[The Nature Conservancy, jeffrey_evans@tnc.org] 
date: "5/18/2022"
output:
  html_document: 
    toc: yes
    keep_md: yes
    code_folding: hide
---

**Objectives**

We will build foundation of more advanced spatial modeling by introducing proximity, distance and nearest neighbor analysis, then progressing to Point Pattern Analysis for quantifying spatial structure.  Understanding spatial autocorrelation is fundamental for understanding spatial process, testing model assumptions and conducting spatial analysis.  In this section, we will cover what is spatial autocorrelations, importance and implications of spatial autocorrelation in modeling ecological systems, how to assess spatial autocorrelation (global and local), and how to test model assumptions using spatial autocorrelation tests. Finally, we will cover basics of time-series analysis, moving towards pratical applications on raster time-series data.   

# Setup

## Add required libraries and set working environment


```{.r .fold-show}
invisible(lapply(c("sp", "terra", "spdep", "spatialEco", "sf", "terra", 
      "raster", "spatstat", "spatstat.geom", "spatstat.core", "rts",
	    "dplyr", "forecast", "RANN", "rgeoda"), require, character.only=TRUE))

# set your working and data directories here
setwd("C:/spatialR/session03") 
  data.dir <- file.path(getwd(), "data")
```

# **3.1 - Distance and proximity**

## Read data

Read "plots.shp" in as sf objects. Plot resulting objects. See; st_read, plot, st_geometry  


```r
# Read data 

plots <- st_read(file.path(data.dir, "plots.shp"))
  plots$plot.id <- paste("plot", plots$ID, sep=".")
```
    
## Create distance matrix

Calculate a distance matrix on the "plots" point object. See; st_distance, dist


```r
# dist matrix
dmat <- units::drop_units(st_distance(plots))
  diag(dmat) <- NA  # set 0 diag to NA
    rownames(dmat) <- plots$plot.id
    colnames(dmat) <- plots$plot.id
head(dmat)
```

## Distance matrix - nearest neighbors

See if you can use the distance matrix to find each obs nearest neighbor


```r
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
```
      
## Distance matrix - conditional nearest neighbors

Deriving k nearest neighbors is a very good example of the numerous ways you can approach and correctly solve a problem in R. 

Bound the distances, in the distance matrix, considered before finding the nearest neighbor. Here you could set a distance bandwidth (range) in the same way the minimum distance was set. Set the bandwidth to 1000, 5000 then find the 1st nearest neighbor. See; apply,  which.min 


```r
# 1. Bound the distance matrix to represent a bandwidth then, 
#    find the 1st nearest neighbor

min.dist = 1000
max.dist = 5000 
dmat[dmat < min.dist] <- NA
dmat[dmat > max.dist] <- NA

nn <- apply(dmat, MARGIN=1, which.min)
  ( kNN.ids <- colnames(dmat)[nn] )
```

## Graph structures - nearest neighbors

We can use a graph structure to efficiently represent a [Wij]d matrix. This is the "under the hood" approach in the spdep library. 

Find the 1st nearest neighbor using a graph structure. See; spdep, knearneigh, knn2nb, dnearneigh 


```r
# Proximity K nearest neighbors using knearneigh and knn2nb

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

# Distance K nearest neighbors using dnearneigh and nbdists
#   this approach would allow for a bandwidth as we did previosly 
    
plots.dist <- dnearneigh(st_coordinates(plots), 0.0001, 10000)
  dist.list <- nbdists(plots.dist, st_coordinates(plots))
  plots$NNDist <- unlist(lapply(dist.list, FUN=function(x) min(x)))

plot(plots["NNDist"], pch=20, main="neighbor distances")

# Now, after all that work apply this single function "kn2", which 
# is an implementation of the Arya and Mount's Approximate 
# Nearest Neighbours (ANN) C++ library, that does all of this 
# And, very fast!

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
```

# **3.2 Spatial dependency - Point Pattern Analysis**

What if you wish to describe the spatial processes of a population? An entire field of spatial statistics exist to address exactly this question. Point Pattern Analysis (PPA) is a family of statistics developed to describe spatial processes of point observations and can represent a marked [X,Y]Z or unmarked  [X,Y] process. The Complete Spatial Randomness (CSR) null of a PPA follows a Poisson process.  

Most often PPA describes clustering process and can indicate attraction and diffusion processes. These statistics are often used in spatial epidemiology for quantifying disease spread. The spatstat library is the de facto standard for Point Pattern Analysis but, does require coercing our data into a special spatial class.      

For this exercise, we have a point locations of wetlands located in in the Bighorn Crags, Idaho and want to know if these wetlands are clustered. 

## read data


```r
wetland <- read.csv(file.path(data.dir, "Wetlands.csv"), header=TRUE)
```

## Create spatstat object

Create a ppp object from wetland csv file. The file includes all wetlands. RALU is the presence or absences of Columbia spotted frogs at sites and can function as a mark.  

|   See: owin and ppp


```r
window <- owin(xrange=c(min(wetland$X),max(wetland$X)), 
               yrange=c(min(wetland$Y),max(wetland$Y)))

#*** window is min/max of x and Y
wetland.ppp <- ppp(wetland$X, wetland$Y, window=window, marks=wetland$RALU)
  class(wetland.ppp)
```

## Identify duplicates

Not all data are perfect. This dataset include duplicate points that are likely an error, Remove them from the dataset. Note; Using distinct(x) you could do this directly on the sf object, before converting to a ppp object.  

|  See: unique


```r
wetland.ppp <- unique(wetland.ppp)
```

## Geits G(r) statistic

Are wetlands homogeneous? Calculate Nearest Neighbor Distance (G) 

|   See: Gest


```r
plot(Gest(wetland.ppp))
```

**G(r) results**
* rs: Edge corrected estimator of G(r )
* km: the spatial Kaplan-Meier estimator of G(r)
* pois: the theoretical value of G(r ) for a stationary Poisson process 

Results indicates a homogeneous representation of the same estimated intensity so, yes they are clustered

## Ripley's-K statistic

So, the wetlands are clustered in space. What is the range of spatial structure? We can use a Ripley's-K statistic to assess the spatial dispersion of the clustering process.  

What do data following a CSR process look like in a Ripley's K of same size as dataset? Apply a CSR randomization and compair the real data in a K statistic. 

|   See: rpoispp, Kest


```r
random.wet <- rpoispp(length(wetland[,1]))
  plot(random.wet)          # spatially random "wetland" synthetic data
  plot(Kest(wetland.ppp))   # Ripley's K on observed process    
  plot(Kest(random.wet))    # Ripley's K on synthetic data
```
  
# **3.3 Spatial dependency - global autocorrelation**

What if you wish to know if observations of species abundance are spatially autocorrelated (observations correlated with themselves in space), from either an inferential and violation of assumptions perspective? We will conduct an evaluation of autocorrelation and testing of effects on linear model assumptions. In a frequentest context, spatial autocorrelation is a special case of the pseudo-replication problem.  

## Read data                 

Read the "RALU_abundance.csv" data and coerce into a sf POINT object 


```r
sdata <- read.csv(file.path(data.dir, "RALU_abundance.csv"))
  sdata <- st_as_sf(sdata, coords = c("X", "Y"), 
                    crs = 26911, agr = "constant")

# distinct(sdata)
```

## Spatial Weights Matrix (Wij)

We first we need to specify a Wij matrix representing a hypothesis of spatial relationships (e., contingency, distance, weighted). The spdep library implements many well established methods for quantifying spatial depedency.  

First, plot the Probability Density Function (PDF) of "abundance" to examine the underlying distribution. Then, create a kNN neighbor matrix (all neighbors) from the sdata (specify contiguity by distance). This requires finding the max distance, then creating a distance matrix.  

|   See: density, knearneigh and knn2nb, nbdists, max, dnearneigh, nb2listw with style = "W" (for weights)


```r
plot(density(sdata$abundance))

( nm <- knn2nb(knearneigh(sdata)) )

# find the max distance
# adding 1 is a rough fix for empty neighbors 

( max.dist <- max(unlist(nbdists(nm, sdata))) + 1 ) 
#( max.dist <- units::drop_units(max(st_distance(sdata))) ) #matrix approach

# create contiguity by distance, including all distances

nb <- dnearneigh(sdata, 0, max.dist)

# Then, create Wij spatial weight matrix
( sweight <- nb2listw(nb, style="W") )
```

##  Calculate Moran's-I (global autocorrelation)

Calculate Moran's I for the abundance variable. Note; abundance data are an occupancy estimate as original data did not have measured abundance.

* sdata[,Var] - identifies the target variable in sdata
* sweight - this is the listw object
* length(nb) - number of zones.

|    See: using weights (Wij) matrix "sweight", moran.test, moran.mc


```r
# Moran's-I (global autocorrelation)
( abund.I <- moran(sdata$abundance, sweight, length(nb), Szero(sweight)) )

#  Are the results significant? 
nsim <- 999
moran.test(sdata$abundance, nb2listw(nb, style="W"))
  ( Iperm <- moran.mc(sdata$abundance, listw=sweight, nsim=nsim) ) 

#*** Describe the distribution of the permuted Moran's I
mean(Iperm$res[1:nsim])
var(Iperm$res[1:nsim])
summary(Iperm$res[1:nsim])
```

##  Calculate Geary's-C (global autocorrelation)

Calculate Geary's-C for the abundance variable. Are the results statistically significant?  Test this using a permutation test.   

Interesting side note; the Geary's-C can be decomposed into semivariance used in geostatistics. 

|    See: geary, geary.mc, what are the expectations? 


```r
geary(sdata$abundance, sweight, length(nb), length(nb)-1, Szero(sweight))

nsim <- 999
cperm <- geary.mc(sdata$abundance, nb2listw(nb),
                  nsim=nsim, alternative="greater") 
  plot(cperm)

# assess if normal distribution.  
mean(cperm$res)
var(cperm$res)
summary(cperm$res)
```

##  Calculate Getis-Ord (global autocorrelation)

Getis-ord is a global statistic that compliments local statistics.  

Calculate Getis-Ord for abundance, you will need to transform your Wij matrix to binary contingency representation.   

|   See: globalG.test


```r
bweights <- nb2listw(nb, style="B")
( globalG <- globalG.test(sdata$abundance, bweights,  
                          alternative="greater", 
						              adjust.n=TRUE) )
```

# **3.4 Spatial dependency - model assumptions**

Assessing impacts of autocorrelation on linear model assumptions such as iid and residual error. 

## Read data


```r
# read data to build the objects for a slightly different set of data
ralu.lm <- st_read(file.path(data.dir, "ralu.lm.shp"))
  
  # build neighbor list and find the max distance
  nm.ralu <- knn2nb(knearneigh(ralu.lm)) 
  max.dist <- max(unlist(nbdists(nm.ralu, ralu.lm))) + 1 
  
  # Build Wij using distance, not proximity (kNN)
  nb.ralu <- dnearneigh(ralu.lm, 0, max.dist)
    sweight.ralu <- nb2listw(nb.ralu, style="W")
```

## Create linear model

Create a linear model explaining abundance using the formula "abundance ~ Elev + pH + Depth + Area + Dforest" and plot regression diagnostics.  

|   See: lm


```r
summary( froga.lm <- lm(abundance ~ Elev + pH + Depth + Area + Dforest, 
                        data= ralu.lm) )
  plot(froga.lm, las = 1)    
```

## Moran's-I on regression residuals 

Using the Lagrange spatial dependence test test if there is an effect on the regression residual error. If you create a data.frame from the residuals of lm object, you can calculate Moran's-I. What does it mean if there is significant autocorrelation in the residuals? 

|    See: lm.morantest; lm.LMtests, moran.test, residuals
 

```r
# option 1 - lm.LMtests, Lagrange spatial dependence test on residual error
( ralu.lagrange <- lm.LMtests(residuals(froga.lm), nb2listw(nb.ralu)) )

# option 2 - lm.morantest, moran residuals
( moran.res.ralu <- lm.morantest(froga.lm, nb2listw(nb.ralu, style="W")) )

# Option 3 - moran.test, moran's I directly on residuals
( ralu.moran.res <- moran.test(residuals(froga.lm), 
                      nb2listw(nb.ralu, style="W")) )
```

# **3.4 Spatial dependency - local autocorrelation**

Historically, local autocorrelation has been used to indicate spatial outliers and is considered a nonstationary process. However, while it poses many statistical problems for traditional models, there is tremendous opportunity to leverage it in inferential ways. The Geographically Weighted Regression (GWR) method was developed specifically to address spatial nonstationarity problems (although, it is a horrible model, which is why I do not teach it. please do not use GWR unless for simple exploratory analysis of local autocorrelation!). The LISA (local Moran's-I) statistic can be used to find juxtaposition of values (not bounded -1 to 1) ie., high-low, high-high, low-low, low-high which can be very useful in understanding the spatial distribution of values. Other local statistics (eg., Local-G) can be used to indicate processes such as local hot spots.   

## read data


```r
sdata <- read.csv(file.path(data.dir, "RALU_abundance.csv"))
  sdata <- st_as_sf(sdata, coords = c("X", "Y"), 
                    crs = 26911, agr = "constant")
```

## Local-G

Calculate the local-G measure(s) of local autocorrelation and plot them.  

|    See: localG, localmoran (and weight matrix), crossCorrelation 


```r
# adding 1 is a rough fix for empty neighbors 
nm <- knn2nb(knearneigh(sdata)) 
max.dist <- max(unlist(nbdists(nm, sdata))) + 1 
nb <- dnearneigh(sdata, 0, max.dist)

#*** local G
G <- localG(sdata$abundance, nb2listw(include.self(nb), style="B"))
  sdata$G <- as.numeric(G)
plot(sdata["G"], main="Getis-Ord Local G*", pch=20)
  plot(density(sdata$G))

## For reference, spdep approach (please skip)
#  #*** Create local autocorrelation object "I" using "sdata" 
#  nb <- dnearneigh(sdata, 0, max(as.numeric(st_distance((sdata))/2)
#  I.local <- localmoran(sdata$abundance, nb2listw(nb, style="B"))
#    names(I.local)[9] <- "p.value"
#
#  #*** create sp object of results
#  I <- sdata
#    I <- cbind(sdata, as.data.frame(I.local))
#     
#  # Plot critical values
#  plot(I[, ]"Z.Ii"], xlab="Local Moran's-I")
#  
#  #*** High and Low have no distinct I values so one must create  
#  #    a vector to distinguish significant and high hotspots
#  I <- cbind(I, HotSpots = ifelse( I.local[,5] <= 0.05 & 
#               I.local[,4] >= mean(I.local[,4]), 1, 0) )
#      I$HotSpots <- as.factor(I$HotSpots)  
#        plot(I["HotSpots"], xlab="Local Moran's-I Hot Spots", 
#            col=c("blue","red") )  
```

Here is where I get taken behind the barn and shot. After all these functions for deriving Wij and complex syntax for autocorrelation statistics now, the easy way using the GEODA API "rgeoda". However, there are many good reasons for being familiar with the spdep functions 

Calculate the local Moran's (LISA) using the traditional Anselin (1995) approach using GeoDa. Note that rgeoda has several Wij options available including things like adaptive bandwidth kernel matrices so, lots of options. In this case please just the good ol' queens contingency. 

|    See; queen_weights and local_moran  

Anselin, L. 1995. Local indicators of spatial association, 
  Geographical Analysis, 27:93–115


```r
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
```

## Cross correlation algebraic approximation 

Local spatial association using a matrix algebraic approximation following Chen (2015). This is a very different way of solving for global and local autocorrelation and is predicated on the specification of the Wij matrix. The functional form of Wij represents a hypothesis of the spatial process so, is very different than just defining distance decay. 

Chen., Y. (2015) A New Methodology of Spatial Cross-Correlation Analysis. 
  PLoS One 10(5):e0126158. doi:10.1371/journal.pone.0126158 

Create local autocorrelation object "I" using "sdata" and the crossCorrelation function 

```r
( I <- crossCorrelation(sdata$abundance, 
                        coords = st_coordinates(sdata), 
                        clust = TRUE, k=99) )
						
#*** create sp object of results
sdata$lisa <- I$SCI[,"lsci.xy"]
  sdata$lisa.clust <- as.factor(I$cluster)
      plot(sdata["lisa"], pch=20)
      plot(sdata["lisa.clust"], pch=20)	  
```

# **3.5 - Time-series analysis**

## Read data

Add LAI raster object, using "lai.tif", contaning 71 monthly layers, we will also create a date vector representing the image dates	


```r
( lai <- rast(file.path(data.dir, "lai.tif")) )
  ( dates <- date_seq("2000/01/01", "2005-12-01", step="month") )

# plot 1st and 50th rasters
dev.new(height=6, width=11)
  plot(lai[[c(1,50)]])
```

## Subset single pixel time-series observation

Using x,y coordinates cbind(13028094, 146990) extract a pixel time-series from the lai stack 

You can also try plotting a single raster layer and then using click() to get different coordinates printed to screen see, cellFromXY  


```r
( x <- as.numeric(lai[cellFromXY(lai, cbind(13028094, 146990))]) )
```

## Imputing missing values

An important pre-processing set for time-series analysis is to address missing values. This can be done with an imputation approach using a local local polynomial regression. Add a few NA's into the time-series vector (copy to a new object first) and then apply an imputation. See; impute.loess with smooth = FALSE (will only modify NA's) 


```r
xna <- x
  xna[c(20, 30, 50)] <- NA

print(xna)  
impute.loess(xna, s = 0.4, smooth = FALSE)
```

## Smoothing time-series

A second consideration in exploratory analysis of time-seires data is addressing outliers and stochastic. This is commonly addressed through smoothing the series. We can use a local polynomial regression or a Savitzky-Golay convolution quadratic filter (Savitzky & Golay 1964).

Apply a smoothing function and plot. See; impute.loess impute.loess with smooth = TRUE (will smooth entire series), sg.smooth (for reference) 

Savitzky, A., and Golay, M.J.E. (1964). Smoothing and Differentiation  
  of Data by Simplified Least Squares Procedures. Analytical 
  Chemistry. 36(8):1627-39 


```r
x.smooth <- impute.loess(x, s = 0.4, smooth = TRUE)
  dev.new(height=6, width=14)
    plot(dates, x, type="l")
      lines(dates, x.smooth, col="red")
    legend("topleft", legend=c("raw", "smoothed"), 
           lty=c(1,1), col=c("black","red"))  
```

## Smoothing time-series (confidence)

Evaluate confidence of fit via permutation test using the same smoothing parameter ("s" and "span" arguments) as above. See; loess.boot 


```r
sb <- loess.boot(1:length(x), x, nreps=99, 
                 confidence=0.90, span=0.40)
  dev.new(height=6, width=14)
    plot(sb)
```

## Create time-series class object

Create a ts (time-series) object from our lai time-series vector. These object types are a real pain and I try to avoid them whenever possible however, there are some convenient functions that require a ts class. 


```r
x.ts <- ts(x.smooth, start= c(2000, 01), frequency = 12)
```

## Specify trend model

Specify trend regression model without any autoregressive (AR) terms 

|    See; ts, forecast, tslm  


```r
fit <- forecast::tslm(x.ts ~ trend)
  trend <- forecast::forecast(fit, h=length(x), level=0)

  # Plot linear trend	
  dev.new(height=6, width=14)
    plot(dates, x, type="l", main="LAI time-series, no correction")
      lines(dates, trend$fitted, lwd = 2)
```

## Periodicity 

Account for periodicity (seasonal effects) and specify detrended linear model

now you can see that we are getting somewhere in representing a more accurate trend

|    See; decompose, tslm, forecast  


```r
# decomposed trend and seasonal components
detrend <- decompose(ts(x, start= c(2000, 01), frequency = 12))
  fit.dt <- forecast::tslm(detrend$trend ~ trend)
    trend.dt <- forecast(fit.dt, h=length(x), level=0)

  dev.new(height=6, width=14)
    plot(detrend)

  dev.new(height=6, width=14)
    plot(detrend$trend, type="l", main="LAI detrended time-series")
      lines(trend.dt$fitted, lwd = 2)
```

# **3.6 - Raster time-series analysis**

We can apply the correct componets of time-series exploratory analysis to a raster time-series, treating each pixel as an independent serial measure. We can also apply formal models across the raster time-series.     

## Temporal statistical aggregation 

Create a raster time-series object and calculate yearly means, don't forget that dates object that we created. The rts library provides functionality for deriving temporal aggregations. The resulting object is a rts class and the actual raster data is in the @raster slot

|    See; rts, apply.yearly 


```r
# dates <- date_seq("2000/01/01", "2005-12-01", step="month") 

lai.ts <- rts(lai, dates)
  means <- apply.yearly(lai.ts, mean)@raster
plot(means)
```

## Rate of change

Calculate a simple delta as well as rate of change between two rasters and plot. Rate of change can be derived using (x-y)/x. 

|    See; lapp


```r
chg <- means[[1]] - means[[6]]
  roc <- lapp(means[[c(1,6)]], fun=function(x, y) { (x - y) / x })

  dev.new(height=12, width=10)
    par(mfrow=c(2,1))
      plot(chg, main="delta")
	  plot(roc, main="rate of change")
```

## Mann-Kendall 

The Kendall Tau statistic represents the temporal correlation across a time-series. The Theil-Sen is a monotonic slope based on a repeted medians. This is a non-parametric test but, does suffer from the long-run problem. 

Calculate the Kendall Tau temporal correlation and Theil-Sen Slope and plot. 

|    See; raster.kendall


```r
k <- raster.kendall(means, tau = TRUE)
  dev.new(height=6, width=11)
    plot(k)
```

## Raster time-series trend model

Now, let's put a few pieces together. Here is an approach for putting a modeling framework into a function that can then be passed to a raster time-series object. 

Apply the detrended regression to our raster time-series to get a spatial estimate of the detrended slope. Below is a function for returning slope of time-series detrended regression.


```r
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

# pass function to terra app and plot
slp <- app(lai, ts.trend)

dev.new(height=11, width=8)
  par(mfcol=c(2,1))
    plot(slp, main="detrended slope")
    plot(k[[1]], main="Kendall slope")
```
