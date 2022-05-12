# Example of grid search cross-validation for
#   parameter optimization
library(data.table)
library(ggplot2)
library(viridisLite)
library(sp)
library(gstat)

# Settings
seed=42

# Convert data to SpatialPointsDataFrame
data(meuse)
coordinates(meuse)= ~x+y

# Log transform
meuse$logZinc=log(meuse$zinc)

# Save data.frame
meuse.df=as.data.table(meuse)

# View data
spplot(meuse['logZinc'],,auto.key=TRUE,
       key.space='right',scales=list(draw=T))

# Initialize cv variables
nfold=10
pmax=8
nTimes=5
cv=expand.grid(B=1:nTimes,p=0:pmax,RMSE=NA_real_)
setDT(cv)
setorder(cv,B,p)
dim(cv) # 180 xx
fit.cv=vector(mode='list',length=nrow(cv))

# B-times k-fold CV
i=1L
set.seed(seed)
for(i in 1:nrow(cv)){
    fit.cv[[i]]=krige.cv(logZinc~1,locations=meuse,nfold=nfold,set=list(idp=cv[i,p]))
    head(fit.cv[[i]])
    spplot(fit.cv[[i]]['var1.pred'],auto.key=T,key.space='right',scales=list(draw=T))
    cv[i,RMSE:=sqrt(mean(fit.cv[[i]]$residual^2))]
} # END Loop
# cv

# Aggregate across B times
cvAvg=cv[,.(RMSE=mean(RMSE)),
            by=p]
# cvAvg

# Plot CV results
par(mar=c(4.5,4.5,1,1))
plot(RMSE~p,data=cv,type='p')
lines(RMSE~p,data=cvAvg,lwd=3,col=4)

# Set optimum p
which.min(cvAvg$RMSE) # 4
p=3

# Create gridded newdata
data(meuse.grid)
newdata=meuse.grid
coordinates(newdata)= ~x+y

# Fit IDW to newdata
rm(fit)
  fit=idw(logZinc~1,locations=meuse,newdata=newdata,idp=p)

# Equivalent
# fit=krige(logZinc~1,locations=meuse,newdata=newdata,set=list(idp=p)) 

  fit.df=as.data.table(fit)

# Plot - Observed
ggplot(meuse.df,aes(x,y,fill=zinc))+ 
    geom_point(size=2, shape=21)+
    coord_equal()+
    scale_fill_viridis_c()+
    labs(y='Y',x='X',title='Observed Data',fill='Zinc, mg/L')+
    theme_classic()+
    theme(legend.position='bottom')

# Plot - Fitted
ggplot(fit.df,aes(x,y,fill=var1.pred))+
    geom_raster()+
    geom_point(data=meuse.df,aes(y=coordinates(meuse)[,'y'],x=coordinates(meuse)[,'x']),
	           inherit.aes=F,size=2,shape=21,fill='orange')+ # Observed
    coord_equal()+
    scale_fill_viridis_c()+
    labs(y='Y',x='X',title=sprintf('IDW Fitted (p=%d)',p),fill='Zinc, mg/L')+
    theme_classic()+
    theme(legend.position='bottom')
