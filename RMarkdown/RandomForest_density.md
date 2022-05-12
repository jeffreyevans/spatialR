---
source: Rmd
title: "spatialR workshop - Random Forests for estimating point-process models"
author: "Jeffrey S. Evans"
date: "5/16/2022"
output:
  html_document: 
    toc: yes
    keep_md: yes
    code_folding: hide
---

An introduction to density-based point process random forests models. Conceptually, a Poisson Point-process Model (PPM) uses point observations [X,Y] to fit an intensity process (surface) that is then used with matching covariates to estimate the Probability Density (PDF) or Intensity function. The intensity function is an estimate of the density following an assumption of a Poisson point process. We can emulate the PPM structure by simply estimating the intensity and then using random forests, rather than a linear model, to solve the regression equation. 

I would note that MaxEnt is not a presence-only model per se but, generates a large number of regular samples to act as the null (background) with the know observsations embedded in the background sample. This is not dissimilar to a PPM, (Renner & Warton 2013 demostrated this equlivancy) but, MaxEnt does not rely on an intensity function, rather solving a probability distribution via maximized entropy. 

Add required libraries, download data for exercises (need to set a working directory)


```{.r .fold-show}
invisible(lapply(c("sf", "spatialEco", "terra", "raster", "randomForest", 
                   "rfUtilities", "ggplot2", "ranger"), 
		  require, character.only=TRUE))

# set your working and data directories here
setwd("C:/evans/spatialR/RandomForests")
setwd("C:/evans/spatialR/RandomForests")
  data.dir = file.path(getwd(), "data")
    if(!dir.exists(file.path(getwd(), "results")))
      dir.create(file.path(getwd(), "results")) 
    results.dir = file.path(getwd(), "results")
    
download.file("https://spatialr.s3-us-west-2.amazonaws.com/day04.zip", 
              destfile="day04.zip", mode="wb")
  unzip("day04.zip")
    file.remove("day04.zip")
```

# Data preparation
 
## Read shapefile vector and img rasters from disk

Read species observations "spp_presence.shp" to "spp" sf object and create a stack "xvars" of rast calss rasters excluding "elev.img". Hint; st_read, list.files, rast


```r
spp <- st_read(file.path(data.dir, "spp_presence.shp"))

# Create raster stack (with elev removed)

( rlist = list.files(data.dir, pattern="img$", full.names=TRUE) )
  ( xvars <- rast(rlist[-6]) )
    names(xvars) <- rm.ext(basename(sources(xvars))) 
```

## Create intensity process

Calculate pseudo absence data using and Isotropic Kernel Intensity function then, convert to points. The function needs an sp object so use as(x, "Spatial") Hint; pseudo.absence, rasterToPoints 


```r
kde <- pseudo.absence(as(spp, "Spatial"), n=nrow(spp), 
                      window="extent", s=90, KDE = TRUE)
  kde <- rast(kde$kde)
    kde <- 1 - (kde / terra::global(kde, "max", na.rm=TRUE)[,1] )
    
# Coerce to sf points object  
den.pts <- st_as_sf(as.points(kde))    
  names(den.pts)[1] <- "y"
```
  
## Assign raster values
  
Draw and equal subsample of the intensity process above and below a threshold of presence p = 0.40 to balance the equation. Assign raster values to resulting points. 


```r
p = 0.40 # presence threshold of intensity process

# Subsample intensity point data
n = round( (nrow(den.pts) * p) / 2, 0)
  if(length(which(den.pts$y >= p))  < n |
     length(which(den.pts$y < p))  < n )
    message("Not enough samples at this threshold")

  spp.den <- rbind(
    den.pts[sample(which(den.pts$y >= p),n),],
    den.pts[sample(which(den.pts$y < p),n),])

# Assign raster values
e <- extract(xvars, vect(spp.den))
  spp.den <- cbind(spp.den, e[,-1])  
```

## Check for collinearity and multi-collinearity

Test the design matrix for multi-collinearity and remove any collinear variables. Remove by permuted frequency > 10. Hint; multi.collinear, collinear

Is there a difference when permutation (perm = TRUE) is applied? 

Do you find collinear after addressing multi-collinearity?
  

```r
# Evaluate multicolinear (multivariate redundancy)
( ml <- multi.collinear(st_drop_geometry(spp.den[,2:ncol(spp.den)]), p=0.05) )

# Address the ordering problem in QR matrix decomposition 
( ml <- multi.collinear(st_drop_geometry(spp.den[,2:ncol(spp.den)]), 
                        perm=TRUE, p=0.05) )
  rm.vars <- ml[ml$frequency > 0,]$variables
   if(length(rm.vars) > 0) 
     spp.den <- spp.den[,-which(names(spp.den) %in% rm.vars)]

# Now, see if anything is collinear (pairwise linearity)
cl <- spatialEco::collinear(st_drop_geometry(spp.den[,2:ncol(spp.den)]))
  if(length(cl) > 0) 
    spp <- spp.den[,-which(names(spp.den) %in% cl)]
```

# Specify Poisson (density) random forests model

## Apply a model selection procedure 

Create an initial random forests model object, with 501 Bootstrap replicates, to evaluate model efficacy. Then, apply a model selection procedure. Use "se" for the imp.scale argument. The response (y) is "Present" and independent variables are all columns >= 2. Use st_drop_geometry(spp) to create a data.frame "dat".  Hint; randomForest, rf.modelSel 


```r
b = 501
set.seed(42)
dat <- st_drop_geometry(spp.den) 

randomForest(x=dat[,2:ncol(dat)], y=dat[,"y"], ntree = b)

( rf.model <- rf.modelSel(x=dat[,3:ncol(dat)], y=dat[,"y"], 
                          imp.scale="se") )
```

## Fit final model	

Specify final fit model using selected variables from the model selection and paramter significance. Use probability = TRUE argument so that we are specifying a probabilistic random forests (Malley et al. 2012). Keep the inbag and importance in the model using keep.inbag and importance arguments. Hint; ranger


```r
# Create vector of selected parameter names

( sel.vars <- rf.model$selvars )

# subset to selected parameters
dat <- dat[,c("y", sel.vars)]

( rf.fit <- ranger(y ~ ., data = dat, keep.inbag=TRUE, 
                   num.trees = b, importance="permutation") )

# also fit randomForest class object for later use
rfbu <- randomForest(x=dat[,-1], y=dat[,1], ntree=b)
```

## Derive importance	

Pull the importance values from the resulting fit model and create a dotplot of the feature contribution. Hint; names(rf), order, dotchart 


```r
imp <- rf.fit$variable.importance
  imp <- imp[order(imp)]	
    dotchart(imp, names(imp), pch=19, main="Variable Importance")
```

# Validation

Calculate Root Mean Square Error (RMSE) between observed and predicted sqrt(mean( (x - y)^2


```r
rmse <- function(y, x) { sqrt(mean( (x - y)^2, na.rm=TRUE)) }
  rmse(spp.den$y, predict(rf.fit,dat[,-1])$predictions)  
```

# Spatial predictions

## Subset rasters 

Subset raster stack to selected variables, remember to use double bracket to subset raster stack/brick objects						 


```r
sel.vars <- names(rf.fit$variable.importance)
  xvars <- xvars[[which(names(xvars) %in% sel.vars)]] 
```

## Create prediction raster

Make a spatial prediction of rf model to raster object. To accommodate a ranger random forest model we need to first write a prediction wrapper function to pass to the terra::predict function. At the current version of terra the predict function is strugling with NA values in the raster paramters. We incoporate a rough fix into the wrapper function. Hint; predict.ranger, terra::predict, predict.randomForest  


```r
predict.reg <- function(model, data) {
  data <- as.data.frame(data)
  idx <- unique(as.numeric(which(is.na(data), arr.ind=TRUE)[,1]))
    data <- data[-idx,]
  p <- as.numeric(ranger:::predict.ranger(model, data = data,
                  type = "response")$predictions)
	if(length(idx) > 0) 
	  p <- spatialEco::insert.values(p, NA, idx)
	return(p)			  
}

spp.den.est <- crop(predict(xvars, rf.fit, fun=predict.reg), kde)
  writeRaster(spp.den.est, file.path(results.dir, "rf_den.tif"),
              overwrite=TRUE)

# Plot results
plot(spp.den.est)

# Now, plot species presence/absence on density estimates 
spp.pa <- st_read(file.path(data.dir, "spp.shp"))
my.col <- ifelse(spp.pa$Present == "1", "red", "blue") 
  plot(st_geometry(spp.pa), pch=20, cex=0.75, 
       col=my.col, add=TRUE)
```

# Compare density and probability results  

Examine (plot) binomial probabilities verses density estimate. What may be driving difference, aside from methods? Think about the nature of the data that went into each model and the Nth order spatial process in deriving the intensity process (eg., density bandwidth). 

Estimate the intensity process using then look at resulting 1st and 2nd order spatial variations based on bandwidth plug-in selection for the kde estimate (default sigma verses sigma = "Diggle"). How would this account for different estimates when creating pseudo absence data? Global verses local spatial estimates?  


```r
# Add probability estimates from binomial raster and plot results
probs <- rast(file.path(results.dir, "rf_probs.tif"))

# Plot probability and density estimates
par(mfrow=c(2,1))
  plot(spp.den.est)
      plot(st_geometry(spp.pa), pch=20, cex=0.75, 
       col=my.col, add=TRUE)
  plot(probs)
      plot(st_geometry(spp.pa), pch=20, cex=0.75, 
       col=my.col, add=TRUE)
  
#### Look at 1st verses 2nd order density to explain over-dispersion
spp <- st_read(file.path(data.dir, "spp_presence.shp"))
kde.1st <- pseudo.absence(as(spp, "Spatial"), n=nrow(spp), 
                       window="extent", s=90, KDE = TRUE)
  kde.1st <- rast(kde.1st$kde)
    kde.1st <- 1 - (kde.1st / terra::global(kde.1st, "max", na.rm=TRUE)[,1] )
kde.2nd <- pseudo.absence(as(spp, "Spatial"), n=nrow(spp), 
                      window="extent", s=90, KDE = TRUE, sigma = "Diggle")
  kde.2nd <- rast(kde.2nd$kde)
    kde.2nd <- 1 - (kde.2nd / terra::global(kde.2nd, "max", na.rm=TRUE)[,1] )  

par(mfrow=c(2,1))
  plot(kde.1st, main="Scott strong 1st order sigma")
  plot(kde.2nd, main="Diggle strong 2nd order sigma")
```
