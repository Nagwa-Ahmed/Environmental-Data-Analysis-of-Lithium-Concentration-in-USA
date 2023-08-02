setwd('D:/CV Nagwa/Level 4 term 2/Environmental/Lithium project')
#loading packages
library(readxl)
library(ape)
library(sp)
library(spdep)
library(codep)
library(spatialEco)
library(lattice)
library(gstat)
library(lctools)
library(raster)
library(sf)
library(rnaturalearth)
library(phylin)
library(ggplot2)

df=read_excel('Lithium data.xlsx')


#Data Exploration
hist(df$Concetration, main='Histogram of Lithium Concentration', xlab='Lithium',
     col = 'light blue')


symbols(df$Long,df$Lat,circles = df$Concetration,xlab = 'Longitude',
        ylab = 'Lattitude',fg='blue',bg='blue',inches = 0.25)

data=df[,c('Concetration')]
coords=df[,c('Long','Lat')]
crs    <- CRS("+proj=longlat +datum=WGS84 +no_defs")

spdf <- SpatialPointsDataFrame(coords      = coords,
                               data        = data, 
                               proj4string = crs)

summary(spdf)

spplot(spdf,xlab= "Longitude", ylab= "Latitude")

country_name <- "United States of America"  

country <- ne_countries(country = country_name, returnclass = "sf")#boundaries


country_sp <- st_as_sf(country)

ext <- extent(country_sp)

resolution <- 0.1

grid <- raster(ext, res = resolution)

grid_cropped <- crop(grid, country_sp)

values(grid_cropped) <- 1

"""data_sf <- st_as_sf(df, coords = c("Long", "Lat"))

st_write(data_sf, "D:/CV Nagwa/Level 4 term 2/Environmental/Lithium project/Lithium_USA.shp")"""

USA=read_sf('D:/CV Nagwa/Level 4 term 2/Environmental/Lithium project/Lithium_USA.shp')
ggplot(USA) + geom_sf(aes(colour = Cnctrtn))

#Autocorrelation

df.dists<-as.matrix(dist(cbind(df$Long,df$Lat)))
coor_df <- as.data.frame(cbind(df$Long,df$Lat))

gcd1<-gcd.slc(coor_df, radius = 6371)
gcd1inv<-1/gcd1
gcd1inv<-as.matrix(gcd1inv)

Moran.I(df$Concetration,gcd1inv)#Global

knn<-knearneigh(spdf, k=3, longlat = NULL)
knn2nb<-knn2nb(knn)
mp <- moran.plot(df$Concetration, nb2listw(knn2nb),xlab = 'Lithium Concentration',
                 ylab = 'Spatially lagged Lithium Concentration')


l.moran<-l.moransI(df.dists,6,df$Concetration, WType='Bi-square', scatter.plot = TRUE, family = "adaptive")
l.moran
summary(l.moran$p.value)
hist(l.moran$p.value,main='Histogram of P-value of local moran I', xlab='Lithium',
     col = 'light blue')


#idw interpolation
num_points <- 20000  

set.seed(42) 
sample_points <- spsample(as(country_sp, "Spatial"), n = num_points, type = "random")

idw=gstat::idw(formula = Concetration~1,locations = spdf,newdata=sample_points,
               idp=2)

"""
idw=as.data.frame(idw)
idw=idw[,c('x','y','var1.pred')]data_sf <- st_as_sf(idw, coords = c("x", "y"))

st_write(data_sf, "D:/CV Nagwa/Level 4 term 2/Environmental/Lithium project/Lithium_USA_interpolation.shp")"""



USA_interpolated=read_sf('D:/CV Nagwa/Level 4 term 2/Environmental/Lithium project/Lithium_USA_interpolation.shp')
ggplot(USA_interpolated) + geom_sf(aes(colour = var1_pred))+
  scale_colour_continuous(breaks = seq(2, 
                                       50, by = 5))+ggtitle('Interpolated map of Lithium Concentration(idw)')+
  geom_point(data = df, aes(x = Long, y = Lat), colour = "black", size = 0.5)
