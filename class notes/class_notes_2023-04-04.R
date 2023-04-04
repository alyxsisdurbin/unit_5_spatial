# Haley Durbin
# 2023-04-04

library(tidyverse)
library(sf)
library(mapdata)
library(marmap)
library(lubridate)

ais_day = read.csv('data/processed_ais/ais_2017-01-25.csv')
head(ais_day)

# Coastline data
lat_bounds = c(25, 34)
lon_bounds = c( -82, -76)
world_map = map_data("worldHires", ylim = lat_bounds, xlim = lon_bounds)
dim(world_map)

#Read in US critical habitat shapefiles 
USA_crit_hab = st_read('data/North_Atlantic_Right_Whale_Critical_Habitat/','North_Atlantic_Right_Whale_Critical_Habitat') # reads in set of shapefiles

# plot critical habitats and carcass locations
ais_map_pts = ggplot()+
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group)) + # add coastline
  geom_sf(data=USA_crit_hab, alpha = 0.5, color=NA, fill='yellow') +
  geom_point(data = ais_day, aes(x = LON, y = LAT, color = CallSign)) + 
  coord_sf(1.3, xlim = lon_bounds, ylim = lat_bounds) + # Crop map edges
  guides(color=none) +
  ylab("Latitude") + xlab("Longitude") + theme_classic() 

ais_map_pts
