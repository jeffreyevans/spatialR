---
title: "Graph-theoretic and Gravity Models"
author:
  - Jeffrey S. Evans^[The Nature Conservancy, jeffrey_evans@tnc.org] 
  - Melanie A. Murphy^[University of Wyoming, melanie.murphy@uwyo.org] 
date: "5/18/2022"
output:
  html_document: 
    toc: yes
    keep_md: yes
    code_folding: hide
---

**Objectives**

We will provide an overview to graph theoretical approaches with emphasis on gravity models. There are many ways graphs can be implemented to understand population structure and relate that structure to landscape characteristics (see Dyer and Nason 2004).  In this exercise, we will focus on one specialized case.  Gravity models are a type of inferential model that exploit graph characteristics.  Gravity models include both at site (nodes) and between side (edges) landscape data.  In this section, we will use the gravity model framework to build an empirical model of connectivity for a Columbia spotted frog dataset in central Idaho (Murphy et al. 2010).

**Overview of Worked Example**

_Background:_ Gravity models are a type of inferential model that exploit graph characteristics. Gravity models include both at-site and between-site landscape data. They are a type of graph consisting of nodes and edges. These nodes and edges of landscape characteristics associated with these graph elements.

There are many ways graphs can be implemented to understand population structure and relate that structure to landscape characteristics (see Dyer and Nason 2004). In this exercise, we will calculate various graph metrics and apply graphs to fit a gravity model.

_Data set:_ In this exercise, you will use the gravity model framework to build an empirical model of gene flow for the Columbia spotted frog dataset in central Idaho that you have used for several other exercises (Murphy et al. 2010).

# **Setup**

## Add required libraries and set working environment



```{.r .fold-show}
p <- c("raster", "rgdal", "igraph", "sp", "GeNetIt", "spatialEco", 
       "sf", "terra", "sfnetworks", "spdep", "dplyr", "tmap", "devtools") 
  if(any(!unlist(lapply(p, requireNamespace, quietly=TRUE)))) { 
    m = which(!unlist(lapply(p, requireNamespace, quietly=TRUE)))
	  suppressMessages(invisible(lapply(p[-m], require,    
	                   character.only=TRUE)))
    stop("Missing library, please install ", paste(p[m], collapse = " "))
  } else {
    if(packageVersion("GeNetIt") < "0.1-5") {
      remotes::install_github("jeffreyevans/GeNetIt")
	} 
    suppressMessages(invisible(lapply(p, require, character.only=TRUE)))
  }

# set your working directory here
setwd(wd <- "C:/spatialR/graph")
  data.dir <- file.path(getwd(), "data")

# indicates UTM 11 NAD83 projection
prj = 32611 

# Some needed functions
back.transform <- function(y) exp(y + 0.5 * stats::var(y))
rmse <- function(p, o){ sqrt(mean((p - o)^2)) }
```

# **Wetland complex data preparation** 

In this sections, we will read in all wetland locations in the study area and calculate a few graph-based metrics to assign to wetland sites that data was collected at. This allows us to put our samples into the context of the larger wetland system thus, accounting for proximity and juxtaposition. 

## Read in wetlands data 

Read file "Wetlands.csv" and make a spatial object.

  See: read.csv, coordinates, st_as_sf


```r
wetlands <- read.csv(file.path(wd, "data", "Wetlands.csv"), 
                    header = TRUE)
  head(wetlands)
  
  
wetlands <- st_as_sf(wetlands, coords = c("X", "Y"), 
                     crs = 32611, agr = "constant") 
```

## Create wetlands graph 

Create Gabriel graph from the wetlands to represent a "realization" of connectivity and spatial arrangement. 

|    See: gabrielneigh, graph2nb, nb2lines  

What other types of graphs could you build? What wetlands are connected to each other based on the graph?


```r
# Derive Gabriel graph
gg <- graph2nb(gabrielneigh(st_coordinates(wetlands)),sym=TRUE)
  plot(gg, coords=st_coordinates(wetlands))

# Coerce to sf line object (will be used to create igraph object)
gg <- nb2lines(gg, coords = sf::st_coordinates(wetlands), 
	             proj4string = prj, as_sf=TRUE)
```

## Graph metrics

Using the sf line/graph we just made coerce to sfnetworks, then calculate graph metrics of betweenness and closeness with weights and degree.   
   
|    See: as_sfnetwork, activate, degree, betweenness, closeness

Do you think this graph is ecologically meaningful? What information, in the graph structure itself, might this graph contain?

degree - the number of connections a node has Calculate 
betweenness - the number of shortest paths going through a node
closensess - 


```r
# Create sfnetwork, igraph object
wg <- as_sfnetwork(gg, edges=gg, nodes=wetlands, directed = FALSE,
                  node_key = "SiteName", length_as_weight = TRUE, 
				          edges_as_lines = TRUE)

# Calculate weights and graph metrics
w <- wg %>% activate("edges") %>% pull(weight) %>% as.numeric()
  w[w <= 0] <- 1
    w = w / sum(w)

# calculate and add betweenness, degree and  closeness     
wetlands$betweenness <- igraph::betweenness(wg, directed=FALSE, weights=w)
wetlands$degree <- igraph::degree(wg)
wetlands$closeness <- igraph::closeness(wg, weights=w)
```

##  Plot graph metric 

Plot results using the graph edges and wetlands points with the attribute "betweenness"

|    See: plot, st_geometry, add=TRUE


```r
plot(st_geometry(gg), col="grey")
  plot(wetlands["betweenness"], pch=19,  
       cex=0.75, add=TRUE)
     box()
	 title("Wetlands Gabriel graph betweenness")
```


# **Field-data preparation** 

In this section we will read the field data, add the node metrics we just calculated.

## Read site data

sing RALU_Site.csv, read in the data, add the node data (betweenness and degree), create a spatial object that includes the node data. Look at the RALU_Site file.  What are the fields here? What data are included?  

|    See: which, merge

Using names is dangerous as, small changes in names can result in  non-matches. In this case, the ID fields are not consistent (data were collected at different times for different purposes originally). However, names are standardized in a drop-down list of a database. So they are a matching field. My preference is do to this type of  operation on a numeric field.



```r
sites <- read.csv(file.path(wd, "data", "RALU_Site.csv"), 
                  header = TRUE)
  sites$SiteID <- as.character(sites$SiteID)

nodestats <- st_drop_geometry(wetlands[,c(3,5:7)])
  nodestats <- nodestats[which(nodestats$SiteName %in% sites$SiteName),] 
sites <- merge(nodestats, sites, by="SiteName")

sites <- st_as_sf(sites, coords = c("X", "Y"), 
                 crs = prj, agr = "constant") 
```

## Saturated Graph

### 1. Create graph from site locations 

|    See: knn.graph, make sure to use correct field 

To assess connectivity using a gravity model, we need to build a graph from the occupied frog sites create a graph. This could be any type of graph, but I generally use saturated or pruned by some maximum distance.


```r
dist.graph <- knn.graph(sites, row.names = sites$SiteName)
  dist.graph <- merge(dist.graph, st_drop_geometry(sites), 
                      by.y="SiteName", by.x="from_ID")
    dist.graph <- dist.graph[,-c(11:19)] # drop extra columns
    
## Can create a distance-constrained graph with max.dist arg (not run)
# dist.graph <- knn.graph(sites, row.names = sites$SiteName, max.dist=5000)
```

###  2. Merge the graph with genetic distance. 

This involves: reading in RALU_Dps.csv genetic distance, convert to flow (1-distance), unfold matrix into a dataframe then, merge graph with genetic distances 

|    See: dmatrix.df

Note; if you get stuck on this step, read in gdist.csv


```r
gdist <- read.csv(file.path(wd, "data", "RALU_Dps.csv"), header=TRUE) 
  rownames(gdist) <- t(names(gdist))
	  
# Make a matrix, gdist, unfold data  
gdist <- dmatrix.df(as.matrix(gdist)) 
  names(gdist) <- c("FROM", "TO", "GDIST") #unfold the file
    gdist <- gdist[!gdist$FROM == gdist$TO ,]
    gdist[,1] <-sub("X", "", gdist[,1])
    gdist[,2] <-sub("X", "", gdist[,2])
    gdist <- cbind(from.to=paste(gdist[,1], gdist[,2], sep="."), 
                   gdist)

# Transform genetic distance to genetic flow
gdist$GDIST <- flow(gdist$GDIST)

# Merge data  
dist.graph$from.to <- paste(dist.graph$i, dist.graph$j, sep=".")
  dist.graph <- merge(dist.graph, gdist, by = "from.to")
```

# **Spatial model data preparation**

##  Read raster data using terra

|    See: list.files, rast

Note; R uses regular expressions so, a wildcard is not "*"



```r
list.files(file.path(wd,"data"), "tif$")
xvars <- rast(list.files(file.path(wd,"data"), "tif$", 
                        full.names = TRUE))
```

##  Reclassify wetlands 

Reclassify NLCD, in xvars, into a single class

|   See: matrix, reclassify 


```r
m <- c(0,10.8, 0,10.9,12.1,1,12.9,89.5,0, 89.1,95.1,1,95.9,100,0 )
  reclass <- matrix(m, ncol=3, byrow=TRUE)
  
wetlnd <- classify(xvars[["nlcd"]], reclass)
  names(wetlnd) <- "wetlnd"
    xvars <- c(xvars, wetlnd)
```


##  Calculate the proportion of the landscape around sites 

Assign proportion of landcover that is wetland to sites as pwetland

|    See: write function, extract

You want to know if areas of dense wetlands produce more frogs? What buffer distance will you use?


```r
## method 1 (can result in Inf if all zero)
#  prop.land <- function(x) {
#   length(x[x==1]) / length(x)  
#  }

## method 2 (no divide by zero error)
prop.land <- function(x) {
  prop.table(table(factor(x, levels=c(0,1))))[2]
}

b <- st_buffer(sites, 300)
pwetland <- extract(wetlnd, vect(b))
  pwetland <- tapply(pwetland[,2], pwetland[,1], prop.land)
  
# Add the % wetland back to the dataframe
sites$pwetland <- as.numeric(pwetland)
```

### Challenge:
  
- What happens if you change this radius?
- What radius do you think makes the most sense ecologically?

You may recall, the landscapemetrics package can be used to calculate various landscape. Here is an example for calculating pland. Calculate a few different bandwidths (radi) and compare the differences across scales?  


```r
r = 300 # sample radius
nlcd_sampled <- landscapemetrics::sample_lsm(landscape = xvars[["wetlnd"]], 
                                             what = "lsm_c_pland",
                                             shape = "circle",
                                             y = sites, 
                                             size = r, 
                                             return_raster = FALSE,
                                             plot_id=sites@data$SiteID)
pwetland <- dplyr::select(dplyr::filter(nlcd_sampled, class == 1, 
                                        metric == "pland"), plot_id, value)  
names(pwetland) <- c("SiteID", "pwetland")
pwetland$pwetland <- pwetland$pwetland/100

head(pwetland)
```

##  Add values of rasters to sample sites 

This adds potential "at site" variables, keep as sf POINT class object
 
|    See: st_sf, terra, extract


```r
  stats <- extract(xvars[[-6]], vect(sites))
  sites <- st_sf(data.frame(as.data.frame(sites), stats), 
                 geometry=sites$geometry)
```


##  Add raster covariates to graph edges (lines). 

Remove nlcd and wetlnd rasters before calculating statistics  
 
|    See: graph.statistics



```r
idx <- which(names(xvars) %in% c("nlcd","wetlnd"))
suppressWarnings(
  stats <- graph.statistics(dist.graph, r = xvars[[-idx]], 
                            buffer= NULL, stats = c("min",         
                            "mean","max", "var", "median")) )

  dist.graph <- st_sf(data.frame(as.data.frame(dist.graph), 
                      stats), geometry=dist.graph$geometry)
```

##  What about categorical variables? 

Moments are nonsensical. Create a function for returning the % wetland between sites. Then use it to calculate an additional statistic and, add result to the graph.  
 
|    See: ifelse, table, prop.table, factor, graph.statistics
 
Are there other categorical variables that you think may be ecologically important?



```r
wet.pct <- function(x) {
  x <- ifelse(x == 11 | x == 12 | x == 90 | x == 95, 1, 0)
    prop.table(table(factor(x, levels=c(0,1))))[2] 
}   

suppressWarnings(
  wetstats <- graph.statistics(dist.graph, r=xvars[["nlcd"]], 
                               buffer= NULL, stats = c("wet.pct")) )
dist.graph$wet.pct.nlcd <- as.numeric(wetstats[,1]) 
```

##  Evaluate node and edge correlations
 
We need to evaluate correlations in the data to not overdispersion our models. Note, we are not going to actually remove the correlated variables but, just go through a few methods of evaluating them. The code to remove colinear variables is commented out for reference. We do have to log transform the data as to evaluate the actual model structure.   

See: cor, collinear
 

```r
node.var <- c("degree", "betweenness", "Elev", "Length", "Area", "Perim", 
              "Depth", "pH","Dforest","Drock", "Dshrub", "pwetland", "cti",
			        "dd5", "ffp","gsp","pratio","hli","rough27","srr")

p = 0.8 
s <- st_drop_geometry(sites)[,node.var]
  for(i in 1:ncol(s)) {
    s[,i] <- ifelse(s[,i] <= 0, 0.00001, log(s[,i]))
  }
  
#### site correlations  
site.cor <- cor(s, y = NULL, 
                use = "complete.obs", 
                method = "pearson")
	diag(site.cor) <- 0			  		
cor.idx <- which(site.cor > p | site.cor < -p, arr.ind = TRUE)
  cor.names <- vector()
  cor.p <- vector()
    for(i in 1:nrow(cor.idx)) {
	  cor.p[i] <- site.cor[cor.idx[i,][1], cor.idx[i,][2]]
      cor.names [i] <- paste(rownames(site.cor)[cor.idx[i,][1]],
                       colnames(site.cor)[cor.idx[i,][2]], sep="_")
	}	
data.frame(parm=cor.names, p=cor.p)

( node.cor <- collinear(s, p=p) )

# node.var <- node.var[-which(node.var %in% node.cor)]
```

##  Add node data 

### 1. Build node data 
 
Build and add node (at site) level data to graph then merge edge (distance graph) and edge (site) data.   

   See: build.node.data, merge


```r
node.var <- c("degree", "betweenness", "Elev", "Length", "Area", "Perim", 
              "Depth", "pH","Dforest","Drock", "Dshrub", "pwetland", "cti",
			        "dd5", "ffp","gsp","pratio","hli","rough27","srr")  

node <- build.node.data(st_drop_geometry(sites), group.ids = "SiteID", 
                        from.parms = node.var)
```

### 2. Merge nodes and edges 

Merge nodes and edges to create a design matrix (model data.frame) then, log transfomr all the paramters. What happens to zero values? Any ideas on how to deal with it? 


```r
gdata <- merge(st_drop_geometry(dist.graph)[c(1,2,5,11,14,7)], node, 
               by.x="SiteID", by.y="SiteID")
	gdata <- merge(gdata, st_drop_geometry(dist.graph)[c(11, 8:10, 15:55)], 
	               by.x="SiteID", by.y="SiteID") 
	
    # log transform matrix
    for(i in 5:ncol(gdata)) {
      gdata[,i] <- ifelse(gdata[,i] <= 0, 0.00001, log(gdata[,i]))
    }
```

# **Gravity model**

## Develop hypothesis   

What type of gravity model do you wish to run (constrained by production or attraction)? Think about what hypothesss that you want to test.  Write out model statements that group parameters into hypotheses and run models. Remember to run a NULL that is just distance. 

At site (node) potential parameters. These are all at site variables. Remember that we pulled all raster varibles. We want to critically think about hypotheses and not use all of these parameters.

* degree (graph) - graph degree
* betweenness (graph) - graph betweeness
* Elev (raster) - elevation (see comments below)
* Length (attributed) - geographic distance
* Area (field) - wetland area 
* Perim (field) - wetland perimeter 
* Depth (field) - wetland depth - highly correlated with predatory fish presence/abundance
* pH (field) - wetland pH 
* Dforest (field) - distance to forest 
* Drock (field) - distance to rock 
* Dshrub (field) - distance to shrub 
* pwetland (raster) - proportion of wetland in d radius 
* cti (raster) - compound topographic wetness index - steady-state measure of wetness based on topography
* dd5 (raster) - degree days >5C (sum of temp)
* ffp (raster) - frost free period
* gsp (raster) - growing season precipitation
* pratio (raster) - ratio of growing season precip to annual precip - can indicate amount of snow to rain
* hli (raster) - heat load index - topographic measure of exposure, related to productivity (ice-off and primary productivity) in this system 
* rough27 (raster) - topographic variation at a n27 window size
* ssr (raster) - measure of topographic variation at a n27 window size - for this system pulling out ridgelines 
* wet.pct.nlcd (raster) - percent of cells that are wetland class

Between site (edge) potential paramaters include: cti, dd5, ffp, gsp, pratio, hli, rough27, ssr (min, mean, max, var, median for each)

NOTE: we are adding elevation here as a covariate. HOWEVER - elevation does not represent an ecological processes in itself. I strongly encourage using the parameters (temp, moisture, rainfall, vegetation, accessibility, etc.) directly and not elevation as a surrogate parameter.


```r
# null model (under Maximum Likelihood) 
( null <- gravity(y = "GDIST", x = c("length"), d = "length", group = "from_ID", 
                  data = gdata, fit.method = "ML", ln = FALSE) )

# Fish hypothesis (under Maximum Likelihood) 
( depth <- gravity(y = "GDIST", x = c("length","from.Depth"), d = "length", 
                   group = "from_ID", data = gdata, fit.method = "ML", ln = FALSE) )

# Productivity hypothesis (under Maximum Likelihood) 
( product <- gravity(y = "GDIST", x = c("length", "from.ffp", "from.hli"), 
                     d = "length",  group = "from_ID", data = gdata, 
					 fit.method = "ML", ln = FALSE) )

# Climate hypothesis (under Maximum Likelihood) 
( climate <- gravity(y = "GDIST", x = c("length", "from.ffp", "from.pratio"), 
                     d = "length", group = "from_ID", data = gdata, 
					 fit.method = "ML",  ln = FALSE) )

# Wetlands hypothesis (under Maximum Likelihood) 
( wetlands <- gravity(y = "GDIST", x = c("length", "from.degree", "from.betweenness", "from.pwetland"), 
                      d = "length", group = "from_ID", data = gdata, fit.method = "ML",
					  ln = FALSE) )

# Topography hypothesis (under Maximum Likelihood) 
( topo <- gravity(y = "GDIST", x = c("length", "median.srr", "median.rough27"), d = "length", 
                  group = "from_ID", data = gdata, fit.method = "ML",
				  ln = FALSE) )

# Habitat hypothesis (under Maximum Likelihood) 
( habitat <- gravity(y = "GDIST", x = c("length", "wet.pct.nlcd", "median.gsp"), 
                     d = "length", group = "from_ID", data = gdata, fit.method = "ML",
					 ln = FALSE, method="ML") )

# Global model (under Maximum Likelihood) 
( global <- gravity(y = "GDIST", x = c("length", "wet.pct.nlcd", "median.gsp", 
                    "from.Depth", "from.ffp", "from.hli", "from.pratio", "from.degree", 
					"from.betweenness", "from.pwetland", "median.srr", "median.rough27"), 
					d = "length", group = "from_ID", data = gdata, fit.method = "ML",
					ln = FALSE) )
```

##  Compare competing models. 

Should you use ML or REML? Create diagnostic plots

|    See: compare.models 

Can you directly compare ML and REML? Why not?


```r
compare.models(null, depth, product, climate, wetlands, 
               topo, habitat, global)

compare.models(depth, product, climate, wetlands, topo, 
               habitat, global)

par(mfrow=c(2,3))
   for (i in 1:6) { plot(null, type=i) } 
```


##  Fit final model(s)

THen compair them and calculate effect size

|    See: compare.models, gravity,es



```r
# Habitat fit (under REML)
h <- c("length", "wet.pct.nlcd", "median.gsp")
habitat_fit <- gravity(y = "GDIST", x = h, d = "length", group = "from_ID",
                      data = gdata, ln=FALSE)

# global fit (under REML)
g <-  c("length", "wet.pct.nlcd", "median.gsp", "from.Depth", "from.ffp",
        "from.hli", "from.pratio",  "from.degree", "from.betweenness",  
        "from.pwetland", "median.srr",  "median.rough27")
global_fit <- gravity(y = "GDIST", x = g, d = "length", 
                      group = "from_ID", data = gdata, ln=FALSE)


gravity.es(habitat_fit)
gravity.es(global_fit)

par(mfrow=c(2,3))
   for (i in 1:6) { plot(global_fit, type=i) } 
dev.new()
par(mfrow=c(2,3))
   for (i in 1:6) { plot(habitat_fit, type=i) } 
```

##  Back predict global_fit model

|    See: predict


```r
gd <- back.transform(gdata$GDIST)

# Make individual-level (group) predictions (per slope) and
# show RMSE 
global.p <- predict(global_fit, y = "GDIST", x = g,  
                    newdata=gdata, groups = gdata$from_ID,
				    back.transform = "simple")
habitat.p <- predict(habitat_fit, y = "GDIST", x = h,  
                     newdata=gdata, groups = gdata$from_ID,
			         back.transform = "simple")

cat("RMSE of global", rmse(global.p, gd), "\n")
cat("RMSE of habitat", rmse(habitat.p, gd), "\n")
```

##  Aggregate estimates and plot 

We can aggregrate estimates back to the edges and nodes. An intercative map can be created using the tmap package 

|    See: tapply


```r
# Aggregate estimates to graph and node
global.p <- data.frame(EID = gdata$from.to,
                       NID = gdata$from_ID,  
                       p=global.p)

edge.p <- tapply(global.p$p, global.p$EID, mean)
  dist.graph$global.flow <- edge.p
  
node.p <- tapply(global.p$p, global.p$NID, mean)
node.var <- tapply(global.p$p, global.p$NID, var)
idx <- which(sites$SiteName %in% names(node.p))
  sites$global.flow[idx] <- node.p
  sites$global.var[idx] <- node.var

# Plot results using tmap
pal <- colorRampPalette(rev(c("red","orange","blue")), bias=0.15)
tmap_mode(c("plot", "view")[2])
  tm_shape(dist.graph) +
     tm_lines("global.flow", palette=pal(10)) +
       tm_shape(sites) +
  	     tm_symbols(col = "global.flow", 
		            size = "global.var", 
	                shape = 20, scale = 0.75, palette=pal(10))
```
