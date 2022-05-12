---
source: Rmd
title: "spatialR workshop - Review of R basics"
author: "Jeffrey S. Evans"
date: "5/16/2022"
output:
  html_document: 
    toc: yes
    keep_md: yes
    code_folding: show
---

spatialR workshop R review

# Review of vectors
 
## Creating vectors


```r
# Numeric sequence using a range of whole numbers
( x <- 1:5 )

# Numeric and character sequence using concatenate 
( x <- c(1, 2, 3, 4, 5) )

( x <- c("A", "B", "C", "Z") )

# Numeric sequence using seq (sequence)
( x <- seq(1, 5, 0.5) )

# Numeric sequence using rep (replicate)
( x <- rep(1, 10) )

# Create a vector representing a uniform distribution 
x <- runif(10)

# Create vector with mean = 0 and variance = 0.25 (std.dev = 0.5)
rnorm(100, mean = 0, sd = 0.5)
```

## Summarizing vectors
 

```r
# Number of values in vector
length(x)

# Values of unique values in vector
unique(c(1, 1, 2, 1, 6, 2))

# Nest length and unique to return number of unuqie values 
length(unique(c(1, 1, 2, 1, 6, 2)))
```

## Indexing vectors
 

```r
# Subset 1, 4 and 5 values from vector
( x <- seq(1, 5, 0.5) )
x[c(1,4,5)]

# Subset values <= 3
( x <- seq(1, 5, 0.5) )
x[x <= 3]

# Set values <= 3 to 0
( x <- seq(1, 5 ,0.5) )
  x[x <= 3] <- 0

# Set the 5th value to NA
( x <- seq(1, 5, 0.5) )
  x[5] <- NA

# Reclassify values <= 3 to 0 else 1 using ifelse  
( x <- seq(1, 5, 0.5) ) 
( ifelse(x <= 3, 0, 1) )
```

## Logical vectors


```r
( x <- c(1,2,3,NA,5) )

# Return a vector of TRUE/FALSE where TRUE is NA
is.na(x)

# Use logical vector for assignment (NA to 0)
x[is.na(x)] <- 0

# Using which, %in% and match
x = c(1,2,3,6,10)
y = c(2,6,10,20,50,80,100)

which( x %in% y)
match(x, y)
x[which(x %in% y)]

which( y %in% x)
match(y, x)
y[which(y %in% x)]

# Check that vector is numeric
is.numeric(x)
```

## Randomly sampling a vector


```r
( x <- runif(20) )
sample(1:length(x), 5)
x[sample(1:length(x), 10)]
```

## Factoral vector


```r
# Create a character vector
( states <- c("OR", "CO", "WY", "WA", "ID", "MT") )
  class(states)

# Coerce to factor
( states <- as.factor(states) )
  class(states)

# Display levels (unique values)   
levels(states)

# Display number of levels in factor
nlevels(states)

# Order (sort) a factor
ordered(as.factor(c("LOW", "MED","HIGH")))

# Unfactor
( f <- factor(c(1,1,1,1,2,2,5,6,4,7,7,7,4)) )

# Try as.numeric, does not work as it returns order of level not value
as.numeric(f)

# A nested approach works
as.numeric(as.character(f))

# turn off automatic string to factor coercion
options(stringsAsFactors = FALSE) 
```

# Review of data.frames
 
## Creating and viewing data.frames


```r
# Create a data.frame using vectors
group=rep(c("A","B"),  5)
year=seq(2000,2009, 1)
y=round(runif(10),3)
norm=round(rnorm(10, mean=0, sd=1),3)

( x.df <- data.frame( group=group, year=year, y=y, norm) )

# Display dimensions of data.frame
dim(x.df)
nrow(x.df)
ncol(x.df)

# Display row and column names of data.frame
names(x.df)
row.names(x.df)

# Look at group column using $ and bracket
x.df$group         
x.df[,"group"]

# Look at group column using a set variable
g <- "group"
x.df[,g]    

# Look at first column using position value
x.df[,1]  
```

## Manipulating data.frames


```r
# Reclassify and add a column using a nested "ifelse" statement
( x.df <- data.frame(x.df, BIN=ifelse(x.df$norm > 0, 1, 0)) ) 

# Finding position index of a column
which( names(x.df) == "BIN")

# Remove column using position index
# x.df <- x.df[,-5]                                             

# Tricky way of using a nested which statement to index a column
x.df <- x.df[,-which(names(x.df) == "BIN")]

# Index rows (comma at the end)                                    
x.df[1,]                                      

# Index columns (comma at the beginning)                                
x.df[,x]                                      

# Subset first observation                      
x.df[1,][1]                                   

# Subset the first five rows.
x.df[1:5,]

# Subset the first two columns
x.df[,1:2]

# Subset ten rows and two columns. 
x.df[1:5,][1:2]

# Subset (query) using "subset"               
subset(x.df, norm > 0)                      

# Subset "group = A" using indexing
x.df[x.df$group == "A" ,]

# Subset (query) using indexing 
x.df[x.df$norm > 0 ,]   
```

## Describing data.frames


```r
#### Summary and structure
data(iris)

# Structure of an object
str(iris)

# Summary of a vector
summary(iris$ Sepal.Length)

# Summary of a data.frame
summary(iris)

# Return column types
lapply(iris, class)

# Index of factor columns
which(unlist(lapply(iris, function(x) class(x) == "factor")))
```

## Basic sampling


```r
data(iris) 
x <- runif(20) 

# Sample vector
sample(round(runif(20),0), 5)

# Randomly sampling a dataframe
iris[sample(1:dim(iris)[1], 5),]

# Sample with replacement
sample(c("A", "B"), 10, replace = TRUE)
```

# Review of lists

## Creating and working with list objects


```r
( l <- list(runif(10), runif(15)) )

l[[1]]

( l <- list( study.info=c("Experiment-1", "Evans", "pending"), 
             data=data.frame(group=rep(c("A","B"),5), year=seq(2000,2009, 1), 
			 y=round(runif(10),3), norm=round(rnorm(10, mean=0, sd=1),3))) )

# Using lapply
( l <- list(rnorm(20), runif(20), sample(1:1000, 20)) )   
    unlist(lapply(l, mean))

# Use do.call to combine multiple data.frames in a list 
#   to single data.frame
l <- list(data.frame(rnorm(20),rnorm(20),rnorm(20)), 
            data.frame(rnorm(20),rnorm(20),rnorm(20)),  
            data.frame(rnorm(20),rnorm(20),rnorm(20))) 
  do.call(rbind, l)

# Nested list objects
( dat <- list(list(rnorm(20),rnorm(20),rnorm(20)),    
              list(rnorm(20),rnorm(20),rnorm(20)), 
              list(rnorm(20),rnorm(20),rnorm(20))) ) 

  dat[[1]][[2]] # look at 2nd element in 1st 
 
  lapply(dat, function(x) sapply(x, mean)) 
  rapply(dat, mean, how="list")

# Working with model object that is a list
( mdl <- stats::aov(yield ~ block + N*P*K, npk) ) 
    summary(mdl)
    summary(mdl)[[1]][,"Pr(>F)"] 

# Working with multiple model objects stored in list
l <- list() 
  for(i in 1:5) { l[[i]] <- summary(mdl) } 

  p <- data.frame(p=row.names(l[[1]][[1]])[-8]) 
    for(i in 1:length(l)){ 
      p <- cbind(p, na.omit(l[[i]][[1]][,5])) 
    } 
  names(p) <- c("parameter", paste0("p_", 1:length(l))) 
    print(p)
```

# Review of matrices

## Creating and working with matrix/array objects


```r
x <- as.matrix(rbind( c(0, 0.509, 0.842), 
                      c(0.509, 0, 0.299),
					  c(0.842, 0.299, 0)))
  rownames(x) <- c("site1", "site2", "site3")
  colnames(x) <- c("site1", "site2", "site3") 
print(x)

# Diagonal	
diag(x)		

# Upper triangle
upper.tri(x)
x[upper.tri(x)]

# Lower triangle	
lower.tri(x)	
x[lower.tri(x)]

# Change diagonal to NA                            
diag(x) <- NA                                           

# Change diagonal to NA 
diag(x) <- NA
  x

# Change lower triangle to NA 
x[lower.tri(x)] <- NA
x

( x <- as.matrix(rbind( c(0, 0.509, 0.842), 
                        c(0.509, 0, 0.299), 
                        c(0.842, 0.299, 0))) ) 
( y <- matrix( c(5, 1, 0, 3,-1, 2, 4, 0,-1), 
               nrow=3, byrow=TRUE) ) 


# Inverse of matrix 		
solve(y)

# Matrix multiplication		
y %*% x 

# Transpose			
t(y) 

# Outer product			
y %o% x 

# Cross product 			
crossprod(x,y) 

# Eigan vector			
eigen(y) 

# Single value decomposition 	
svd(y)

#### Flatten matrix to a data.frame with from, to, value attributes
mat.df <- function(x, rm.diag = TRUE) {
  if(rm.diag == TRUE) { diag(x) <- NA }
    varnames = names(dimnames(x))
    values <- as.vector(x)
    dn <- dimnames(x)
    char <- sapply(dn, is.character)
    dn[char] <- lapply(dn[char], utils::type.convert, as.is=TRUE)
    indices <- do.call(expand.grid, dn)
      names(indices) <- varnames
      indices <- data.frame(indices, distance = values)
      indices <- stats::na.omit(indices)	
  return( indices )
}

x <- as.matrix(rbind( c(0, 0.509, 0.842), 
                      c(0.509, 0, 0.299),
					  c(0.842, 0.299, 0)))
   rownames(x) <- c("site1", "site2", "site3")
   colnames(x) <- c("site1", "site2", "site3")
mat.df(x)
```

# Review of tables

## Creating and working with tables objects


```r
( x <- sample(1:5, 100, replace=TRUE) ) 
( y <- sample(3:7, 100, replace=TRUE) ) 

# tabulation
table(x) 
prop.table(table(x)) 

# cross-tabulation
table(x, y) 
prop.table(table(x, y)) 

# adding fixed levels  
x <- factor(x, levels=c(unique(x), "new")) 
  table(x)

# Using fixed levels to evaluate agreement in unequal vectors
spp <- paste0("sp",1:9)
site1 <- factor(c("sp1","sp2","sp4","sp6",
                "sp7","sp9"), levels=spp)
site2 <- factor(c("sp3","sp5", "sp6","sp8"), 
                levels=spp)

cbind(table(site1), table(site2))
```

# Review of object types

## Check and verfy object classes


```r
( x <- runif(10) )
( y <- c(rep("A",5),rep("B",5),rep("C",5))  )

class(x)		
class(y)

is.numeric(x)	
is.numeric(y)

if(!is.numeric(y))  print( "y is not numeric" )

if(class(y) == "character")  print( "We can do something to a character now" )
```

## Object defination and coersion


```r
# Create empty vector, list, data.frame with no dimensions
( x <- vector() ) 
( x <- list() )
( x.df <- data.frame() )

# Create empty vector, list, data.frame with dimensions 

( x <- vector("numeric", 5) )
( x <- rep(NA, 5) )
( x <- vector("list", 5) )

( x <- data.frame(matrix(NA, nrow = 2, ncol = 3)) )

# data.frame with defined column types
( x <- data.frame(matrix(vector(mode = 'numeric',length = 6), 
                        nrow = 2, ncol = 3)) )

# use data.frame rather than cbind to create or append columns
( x.df <- data.frame(x=runif(5), y=runif(5)^2) )

# Coercion: as.vector(),  as.data.frame(),  as.matrix(),  as.list(); 
( x.df <- as.data.frame( cbind(runif(5), runif(5))) )
( x.mat <- as.matrix( cbind(runif(5), runif(5))) )

as.numeric(factor(c(1,3,5))) # wrong, returns levels
as.numeric(as.character(factor(c(1,3,5)))) # right, returns values
```

# Loops and apply family functions

## Iterators using for and which loops


```r
data(iris)

# Iterate to fill an empty vector
x <- vector()
  for(i in 1:5) x <- append(x, i)
x

# Iterate to fill an empty list
x.list <- list()
  for(i in 1:3) x.list[[i]] <- round(runif(5),3)
x.list
						 
# Iterate to calculate mean for each species
sepal <- vector()
  for (i in unique(iris$Species)) {
    sepal <- append(sepal, mean(iris[iris$Species == i,]$Sepal.Length))
  }
sepal

# Loop until a condition is met ( while x is < 5 )
x = 0  
  while(x < 5) { x = x + 1; print(x) }

#### Special case, avoiding double loops using outer (not run)
# prox <- matrix(NA, nrow(pred), nrow(pred))
#  for (i in 1:n) {
#     for (j in 1:n) {
#       tree_idx <- inbag[i,] == 0 & inbag[j,] == 0
#       prox[i,j] <- sum(pred[i,tree_idx] == pred[j,tree_idx])
#     }
#   }
#
#### becomes;
#
# prox <- outer(1:n, 1:n, Vectorize(function(i,j) {
#   tree_idx <- inbag[i,] == 0 & inbag[j,] == 0
#   sum(pred[i,tree_idx] == pred[j,tree_idx]) })) 
```

## Iterators using apply type functions


```r
data(iris)

# Use "apply" to sum rows and columns of a data.frame
apply(iris[,1:4], MARGIN=1, FUN=sum) # rows
apply(iris[,1:4], MARGIN=2, FUN=sum) # columns

# Use "lapply" to return the mean of each list object data.frame Sepal.Length column
spp <- list()
  for(i in unique(iris$Species)) { spp[[i]] <- iris[iris$Species == i,] }
lapply(spp, FUN= function(x) mean(x$Sepal.Length) )

# Use "tapply" to return an aggregated mean (same as above)
tapply(iris$Sepal.Length, iris$Species, FUN=mean)

# Using sapply to evaluate equivalency of unequal vectors
spp <- paste0("sp",1:9)
site1 <- c("sp1","sp2","sp4","sp6","sp7","sp9")
site2 <- c("sp3","sp5", "sp6","sp8")

sapply(list(site1, site2), function(x) {
  y <- rep(NA, length(spp))
  y[which(spp %in% x)] <- x
  return(y)})
```

# File manipulation

## Various base functions for on disk file manipulation 


```r
# set working directory
setwd("C:/Users")

# get current working directory
getwd()

# Construct file path (with file if needed). Easier than paste()
file.path("C:/spatialR", "newfile.txt")

# Create new directory
dir.create("new_folder")

# Copy a file to new location
file.copy("newfile.txt", "C:/spatialR")

# Create new empty file, with example of creating 10
file.create("newfile.txt")
sapply(paste0("file", 1:10, ".txt"), file.create)

# List all the files in a directory
list.files(getwd())
list.files("C:/path/to/somewhere/else")

# list all files within main and sub-directories. Using the argument 
# full.names = TRUE will return the full paths (helpful for data stored
# outside of working directory.  
list.files(getwd(), recursive = TRUE)
list.files(getwd(), full.names = TRUE)

# We can also use a wildcard, following common expressions
#   example for shp extension. The $ is after the file 
#   extension to denote that it should be at the end
list.files(getwd(), "shp$")

# Now, say you want to read many shapefiles in a directory
ddir = "C:/spatialR/data" 
all_shp <- lapply(list.files(ddir, "shp$", full.names = TRUE), sf::st_read)

# two ways to delete a file
unlink("file.csv")
file.remove("file.csv")
 
# delete a directory, must add recursive = TRUE if there 
# are subdirectories.
unlink("C:/mydir/junk", recursive = TRUE)

# Delete multiple files
sapply(paste0("file", 1:10, ".txt"), unlink)

# check if a file exists
file.exists("C:/path/to/file/some_file.txt")
 
# check if a folder exists
file.exists("C:/path/to/file/some_folder")
 
# alternatively, check if a folder exists with dir.exists
dir.exists("C:/path/to/file/some_folder")

# Return basename of file (filename from path)
basename("C:/path/to/file.txt")
```

# Spatial classes

## sp vector objects (historic class, being depreciated)


```r
library(sp)

data(meuse)
  class(meuse)
  str(meuse)

# Coerce into SpatialPointsDataFrame
coordinates(meuse) <- ~x+y

# Information about object
class(meuse)
str(meuse)

# Information about @data slot (contaning attributes)
class(meuse@data)
str(meuse@data)

# Bounding box slot
meuse@bbox

# simple plot
spplot(meuse, "lead")

#### Create sp S4 spatial class - polygons
p1 <- Polygons(list(Polygon(cbind(c(2,4,4,1,2),c(2,3,5,4,2)))), "p1")  
p2 <- Polygons(list(Polygon(cbind(c(5,4,2,5),c(2,3,2,2)))), "p2") 
poly <- SpatialPolygonsDataFrame(SpatialPolygons(list(p1, p2), 1:2), 
                          data.frame(row.names=c("p1","p2"),ID=1:2))

# Extract polygon areas
sapply(slot(poly, "polygons"), function(i) slot(i, "area"))

#### Create sp S4 spatial class - lines
s1 = Lines(list(Line(cbind(c(1,2,3),c(3,2,2)))), ID="a")
s2 = Lines(list(Line(cbind(c(1,2,3),c(1,1.5,1)))), ID="b")
l = SpatialLinesDataFrame(SpatialLines(list(s1,s2)), 
           data.frame(row.names=c("a","b"), ID=1:2))

# Extract line ID's
sapply(slot(l, "lines"), function(i) slot(i, "ID"))
```

## sf vector objects (modern class)


```r
library(sf)
library(dplyr)

data(meuse, package = "sp")
( meuse <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992, 
                    agr = "constant") )

# simple plot
plot(meuse["lead"])

# Coercion of into sf into sp
meuse <- as(meuse, "Spatial")
  class(mesue)
  
# Coercion of into sp into sf
meuse <- as(meuse, "sf")
  class(mesue)
# or st_as_sf(meuse)  
  
#### Create sf spatial class - polygons
( xy <- rbind(
  data.frame(ID=1, x=c(16.48438, 17.49512,24.74609, 22.59277, 16.48438), 
  y=c(59.736328125, 55.1220703125, 55.0341796875, 61.142578125,   59.736328125)),
  data.frame(ID=2, x=c(18.48438,  19.49512,  26.74609, 24.59277, 18.48438), 
  y=c(61.736328125, 57.1220703125, 57.0341796875, 63.142578125,   61.736328125)),
  data.frame(ID=3, x=c(-118.48438,  -119.49512,  -126.74609, -124.59277,   -118.48438), 
  y=c(35.736328125, 33.1220703125, 33.0341796875, 37.142578125,   35.7363281255))) )

xy <- xy %>%
  st_as_sf(coords = c("x", "y"), crs = 4326) %>%
    group_by(ID) %>%
      summarise(geometry = st_combine(geometry)) %>% 
        st_cast("POLYGON")
```

## terra raster objects (replaces raster library)


```r
library(terra)
library(raster)

r <- rast(system.file("ex/logo.tif", package="terra"))   
  class(r)

# simple single band and multiple band (RGB) plots  
plot(r[[1]])
plotRGB(r)

# Create "from scratch" raster and assign uniform random values  
r <- rast(nrows=100, ncols=100)
  r[] <- runif(ncell(r))
    plot(r)

# Using known extent and projection 
e <- ext(571823.5658, 616763.5658, 4423539.5980, 4453689.5980)    
  r <- rast(e, nrows=100, ncols=100, crs="EPSG:26912")
    r[] <- runif(ncell(r))
      plot(r)

# Coercion of into raster into terra     
r <- raster(nrows=100, ncols=100)
  r[] <- runif(ncell(r)) 
rast(r)  

# Coercion of into terra into raster and stack (multi-band)      
r <- rast(nrows=100, ncols=100)
  r[] <- runif(ncell(r)) 
raster(r) 

( r <- c(r,r,r) )
stack(r)
```
