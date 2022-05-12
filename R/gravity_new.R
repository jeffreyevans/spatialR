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

#source("C:/evans/GITS/GeNetIt/R/gravity.R")
#source("C:/evans/GITS/GeNetIt/R/predict.gravity.R")

# set your working directory here
setwd(wd <- "C:/evans/GITS/Gravity")

prj = 32611 # indicates UTM 11 NAD83 projection

back.transform <- function(y) exp(y + 0.5 * stats::var(y))
rmse = function(p, o){ sqrt(mean((p - o)^2)) }

wetlands <- read.csv(file.path(wd, "data", "Wetlands.csv"), 
                    header = TRUE)
  head(wetlands)
  
  
wetlands <- st_as_sf(wetlands, coords = c("X", "Y"), 
                     crs = 32611, agr = "constant") 

# Derive Gabriel graph
gg <- graph2nb(gabrielneigh(st_coordinates(wetlands)),sym=TRUE)
  plot(gg, coords=st_coordinates(wetlands))

# Coerce to sf line object (will be used to create igraph object)
gg <- nb2lines(gg, coords = sf::st_coordinates(wetlands), 
	             proj4string = prj, as_sf=TRUE)

# Create sfnetwork, igraph object
wg <- as_sfnetwork(gg, edges=gg, nodes=wetlands, directed = FALSE,
                  node_key = "SiteName", length_as_weight = TRUE, 
				          edges_as_lines = TRUE)

# Calculate weights and graph metrics
w <- wg %>% activate("edges") %>% pull(weight) %>% as.numeric()
  w[w <= 0] <- 1
    w = w / sum(w)
    
wetlands$betweenness <- igraph::betweenness(wg, directed=FALSE, weights=w)
wetlands$degree <- igraph::degree(wg)
wetlands$closeness <- igraph::closeness(wg, weights=w)

plot(st_geometry(gg), col="grey")
  plot(wetlands["betweenness"], pch=19,  
         cex=0.75, add=TRUE)
     box()
	 title("Wetlands Gabriel graph betweenness")

sites <- read.csv(file.path(wd, "data", "RALU_Site.csv"), 
                  header = TRUE)
  sites$SiteID <- as.character(sites$SiteID)

nodestats <- st_drop_geometry(wetlands[,c(3,5:7)])
  nodestats <- nodestats[which(nodestats$SiteName %in% sites$SiteName),] 
sites <- merge(nodestats, sites, by="SiteName")

sites <- st_as_sf(sites, coords = c("X", "Y"), 
                 crs = prj, agr = "constant") 

dist.graph <- knn.graph(sites, row.names = sites$SiteName)
  dist.graph <- merge(dist.graph, st_drop_geometry(sites), 
                      by.y="SiteName", by.x="from_ID")
    dist.graph <- dist.graph[,-c(11:19)] # drop extra columns
    
## Can create a distance-constrained graph with max.dist arg (not run)
# dist.graph <- knn.graph(sites, row.names = sites$SiteName, max.dist=5000)

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

list.files(file.path(wd,"data"), "tif$")
xvars <- rast(list.files(file.path(wd,"data"), "tif$", 
                        full.names = TRUE))

m <- c(0,10.8, 0,10.9,12.1,1,12.9,89.5,0, 89.1,95.1,1,95.9,100,0 )
  reclass <- matrix(m, ncol=3, byrow=TRUE)
  
wetlnd <- classify(xvars[["nlcd"]], reclass)
  names(wetlnd) <- "wetlnd"
    xvars <- c(xvars, wetlnd)
 
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
  stats <- extract(xvars[[-6]], vect(sites))
  sites <- st_sf(data.frame(as.data.frame(sites), stats), 
                 geometry=sites$geometry)

idx <- which(names(xvars) %in% c("nlcd","wetlnd"))
suppressWarnings(
  stats <- graph.statistics(dist.graph, r = xvars[[-idx]], 
                            buffer= NULL, stats = c("min",         
                            "mean","max", "var", "median")) )
  dist.graph <- st_sf(data.frame(as.data.frame(dist.graph), 
                      stats), geometry=dist.graph$geometry)

wet.pct <- function(x) {
  x <- ifelse(x == 11 | x == 12 | x == 90 | x == 95, 1, 0)
    prop.table(table(factor(x, levels=c(0,1))))[2] 
}   

suppressWarnings(
  wetstats <- graph.statistics(dist.graph, r=xvars[["nlcd"]], 
                               buffer= NULL, stats = c("wet.pct")) )
dist.graph$wet.pct.nlcd <- as.numeric(wetstats[,1]) 


#******************************
# Evaluate log transformed correlations
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

# node.var <- node.var[-which(node.var %in% cp)]

#### edge correlations
( node.cor <- collinear(graphln[,-1], p=p) )

# graphln <- graphln[,-which(names(graphln) %in% node.cor)]

#****************************** 
# Build node data
node.var <- c("degree", "betweenness", "Elev", "Length", "Area", "Perim", 
              "Depth", "pH","Dforest","Drock", "Dshrub", "pwetland", "cti",
			  "dd5", "ffp","gsp","pratio","hli","rough27","srr")  

node <- build.node.data(st_drop_geometry(sites), group.ids = "SiteID", 
                        from.parms = node.var)
 
#****************************** 
# Merge node and edges for model data.frame
gdata <- merge(st_drop_geometry(dist.graph)[c(1,2,5,11,14,7)], node, 
               by.x="SiteID", by.y="SiteID")
	gdata <- merge(gdata, st_drop_geometry(dist.graph)[c(11, 8:10, 15:55)], 
	               by.x="SiteID", by.y="SiteID") 
    # log transform matrix
    for(i in 5:ncol(gdata)) {
      gdata[,i] <- ifelse(gdata[,i] <= 0, 0.00001, log(gdata[,i]))
    }

#****************************** 
# Models
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

compare.models(null, depth, product, climate, wetlands, 
               topo, habitat, global)

compare.models(depth, product, climate, wetlands, topo, 
               habitat, global)

#### Evaluate final competing models
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

# Plot diagnostics for competing 
par(mfrow=c(2,3))
   for (i in 1:6) { plot(null, type=i) } 
dev.new()
par(mfrow=c(2,3))
   for (i in 1:6) { plot(global_fit, type=i) } 
dev.new()
par(mfrow=c(2,3))
   for (i in 1:6) { plot(habitat_fit, type=i) } 

#*************************************************************
# predict and validate
back.transform <- function(y) exp(y + 0.5 * stats::var(y))
rmse <- function(p, o){ sqrt(mean((p - o)^2)) } 

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

# Make population-level predictions 
#p <- predict(global_fit, y = "GDIST", x = g,  
#       newdata=gdata, back.transform = "simple")

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
     tm_lines("global.flow", palette=pal(10), 
	          title.lines = "Edge flow estimates") +
       tm_shape(sites) +
  	     tm_symbols(col = "global.flow", 
		            title.shape = "Node flow estimates", 
		            size = "global.var", 
	                shape = 20, scale = 0.75, palette=pal(10),
					 legend.show = FALSE)
					 
