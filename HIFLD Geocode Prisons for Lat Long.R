#---
#title: HIFLD Geocode Prisons for Lat Long
#author: Ben Millam
#date: November 20, 2019
#description: reads in GIS shapefiles of prison boundaries, geocodes, and exports metadata csv with lat/long added;
    #   shapefiles downloaded from same source as HIFLD metadata csv we've been working with and include all the 
    #   same info: https://hifld-geoplatform.opendata.arcgis.com/datasets/prison-boundaries
#---
  
library(sf) #for working with shape files
library(tidyverse)
setwd("C:/hack-ca-local/epa-facilities")

#read shapefile, returns class "sf" "data.frame", dfs with extra info for sf package
prisons <- st_read("Prison_Boundaries_Shapefiles/Prison_Boundaries.shp", stringsAsFactors=FALSE) 

#calculate centroids of prison boundaries
prisons <- st_transform(prisons, crs = 32617) %>% #convert to utm for calculating centroids
  st_centroid() %>% #centroids from original multipolygons
  st_transform(crs = 4326) #back to 4326 for long/lat coordinates

#add columns for coordinates
prisons$longitude <- st_coordinates(prisons)[,1] #st_coordinates returns a matrix of long/lat
prisons$latitude <- st_coordinates(prisons)[,2]

#drop 'geometry' column for csv output, now redundant
prisons <- as.data.frame(prisons) #class "sf" "data.frame" was preventing you from dropping column or indexing with logical in next line
prisons <- prisons[!(names(prisons) %in% "geometry")]

#write csv
write_csv(prisons, 'hifld-prison_boundaries-geocoded-from-shapefiles-by-ucd-team.csv')

#the following references were helpful at some point, path through them not linear!
    #https://gis.stackexchange.com/questions/296170/r-shapefile-transform-longitude-latitude-coordinates
    #comment in https://github.com/r-spatial/sf/issues/75
    #https://stackoverflow.com/questions/46176660/how-to-calculate-centroid-of-polygon-using-sfst-centroid
    #https://gis.stackexchange.com/questions/43543/how-to-calculate-polygon-centroids-in-r-for-non-contiguous-shapes
    #https://ryanpeek.github.io/2017-08-03-converting-XY-data-with-sf-package/ for converting from utm back to long lat