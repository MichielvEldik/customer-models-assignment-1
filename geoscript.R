library(sp)
library(rgdal)
library(spatialEco)

df_sub_imputed <- read.csv('data_sub_imputed.csv')
coordinates(df_sub_imputed) = c('longitude', 'latitude')
crs.geo1 = CRS("+proj=longlat")
proj4string(df_sub_imputed) = crs.geo1

il_map <- readOGR('tl_2017_17_place.shp')
nc_map <- readOGR('tl_2016_37_place.shp')
wi_map <- readOGR('tl_2015_55_place.shp')


il <- df_sub_imputed[df_sub_imputed$state == "IL",]
nc <- df_sub_imputed[df_sub_imputed$state == "NC",]
wi <- df_sub_imputed[df_sub_imputed$state == "WI",]




plot(il_map) + points(il, pch = 21, col = "green")
plot(nc_map) + points(nc, pch = 21, col = "green")
plot(wi_map) + points(wi, pch = 21, col = "green")


new_guy <- point.in.poly(il, sp_map_1)

# For linking to Census data
# https://data.census.gov/cedsci/profile?g=0400000US37

