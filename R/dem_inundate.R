#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Name: Floodplain Inundation
#Coder: C. Nathan Jones
#Date: 5/16/2020
#Purpose: Estimate floodplain inundation metrics at reach scale. Similar to 
#         methods used in Jones et al., 2015 (https://doi.org/10.1021/acs.est.5b02426)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Download data file here: https://alabama.box.com/s/6zice6t3cr0ielbuznv7pdz8mfzk4ovc

#In this script, the first step is to create a stream network using standard
#methods (i.e., Smooth DEM --> Flow direction --> flow accumulation --> reclassify 
#based on a threshold). Then, we capture the elevation values along the stream 
#network and interpolate those values accross the entire DEM. Essentially, this
#creates a raster of 'nearest stream elevation' for any point in the raster. Finally, 
#we estimate hand by subtracting the interpolated raster from the dem.  

#Note, we utlize whitebox tools gis to run this script. To do this, you will need 
#to write spatial data to a directory, then run the whitebox command. [Its actually 
#running an executable outside of the R environment.]  Read whiteboxR documentation 
#for more details.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: Setup Workspace--------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Clear Memoryz
rm(list=ls(all=TRUE))

#Upload relevant libraries 
library(mapview)
library(whitebox)
library(gstat)
library(raster)
library(sf)
library(tidyverse)

#download data
dem_grd<-raster("data/I_Data/sipsey_lidar.tif")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 2: Define flooplain channel network---------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Smooth DEM
wbt_feature_preserving_smoothing(
  dem="data/I_Data/sipsey_lidar.tif", 
  output="./data/II_Work/smoothed.tif", 
  filter=9, 
  verbose_mode = TRUE)

#Breach Depressions [Note, this is an alternative to 'filling']
wbt_breach_depressions(
  dem = "./data/II_Work/smoothed.tif", 
  output = "./data/II_Work/breached.tif", 
  verbose_mode = TRUE)

#Flow direction raster
wbt_d8_pointer(
  dem = "./data/II_Work/breached.tif",
  output = "./data/II_Work/fdr.tif", 
  verbose_mode = TRUE)

#Flow Accumulation
wbt_d_inf_flow_accumulation(
  input = "./data/II_Work/breached.tif",
  output = "./data/II_Work/fac.tif", 
  verbose_mode = TRUE)

#Reclassify fac to stream layer using threshold method
fac_grd<-raster("./data/II_Work/fac.tif")
vals<-paste0('NaN;0;',quantile(fac_grd, 0.999),';1.0;',quantile(fac_grd, 0.999),";",quantile(fac_grd, 1))
wbt_reclass(
  input = "./data/II_Work/fac.tif",
  output = "./data/II_Work/reclass.tif",
  reclass_vals = vals, 
  verbose_mode = T
)

#Convert stream raster to sf object
wbt_raster_streams_to_vector(
  streams= "./data/II_Work/reclass.tif", 
  d8_pntr = "./data/II_Work/fdr.tif", 
  output = "./data/II_Work/streams.shp",
  verbose_mode = T) 
stream_shp<-st_read("./data/II_Work/streams.shp", 
                    crs=st_crs(dem_grd@crs))

#Plot
mapview(stream_shp)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 3: Create normalized dem--------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create blank raster
blank<-dem_grd*0
writeRaster(blank, "./data/II_Work/blank.tif")

#Aggregate blank raster by a factor of ten for speed
wbt_aggregate_raster(
  input = "./data/II_Work/blank.tif",
  output = "./data/II_Work/blank_10.tif",
  agg_factor = 10)
blank_10<-raster("./data/II_Work/blank_10.tif")

#Create raster of stream elevation
wbt_multiply(
  input1 = "./data/II_Work/reclass.tif", 
  input2 = "./data/II_Work/smoothed.tif",
  output = "./data/II_Work/stream_ele.tif"
)

#Convert stream elevation to points
stream_pnt<-
  raster("./data/II_Work/stream_ele.tif") %>% 
  rasterToPoints() %>% 
  data.frame()

#Create IDW Raster
IDW<-gstat(id="layer", 
           formula=stream_ele~1, 
           locations=~x+y, 
           data=stream_pnt, 
           nmax=7, 
           set=list(idp=4.2))
IDW<-interpolate(blank_10, IDW)
IDW<-resample(IDW, blank)
IDW[IDW>100]<-NA

#Create HAND Raster
hand<-dem_grd-IDW

#Plot
mapview(hand)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 4: Estimate static inundation area
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Below is old code I used to develop static estimates of stage and volume 
#   relationships. I haven't integrated this into the code above, but feel free
#   to check it out and use/adapt as you see fit. 

# #Define max increase and step increase heights
# zmax<-3
# dz<-0.1
# 
# #Create function to return conditional raster 
# Con<-function(condition, trueValue, falseValue){
#   return(condition * trueValue + (!condition)*falseValue)
# }
# 
# #Create Dataframe to house information
# df<-data.frame(matrix(0, ncol=3, nrow=zmax/dz))
# colnames(df)<-c("relative_ele", "area", "volume")
# df$relative_ele<-seq(dz,zmax, dz)
# 
# #Loop through inundation sims
# for(i in 1:(zmax/dz)){
#   #define satge increase
#   z<-dz*i
#   
#   #calculate area and volume rasters
#   area<-Con(dem_norm>(dem_min+z),0,1)
#   volume<-(((z+dem_min)-dem_norm)*area)*res(dem)[1]*res(dem)[2]
#   
#   #add to df
#   df$area[i]<-cellStats(area, 'sum')*res(area)[1]*res(area)[2]
#   df$volume[i]<-cellStats(volume, 'sum')
#   
#   #Export volume raster
#   fun <- function(x) { x[x<0.001] <- NA; return(x) }
#   volume<-calc(volume, fun)
#   writeRaster(volume,paste0("Inundate",i), format="GTiff", overwrite=T)
# }
# 
# #Export df
# write.csv(df, paste0(wd,"output.csv"))
