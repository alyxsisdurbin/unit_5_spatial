# HAD
# 2023-03-30

library(sf) # simple features (spatial vector data) st_read, st_transform
# library(broom) # part of tidyverse
library(tidyverse)
library(mapdata)  # map_data
library(marmap) # getNOAA.bathy()

### Whales and Polygon

USA_crit_hab = st_read(dsn = "data/North_Atlantic_Right_Whale_Critical_Habitat/",
                       layer = "North_Atlantic_Right_Whale_Critical_Habitat")
USA_crit_hab_sf = st_transform(USA_crit_hab, crs=4326) #this is the WGS 84 projection,

CAN_crit_hab = read.csv("data/NARW_canadian_critical_habitat_2017.csv")
head(CAN_crit_hab)

CAN_crit_hab_sf = CAN_crit_hab %>%
  st_as_sf(coords=c("lon", "lat"), crs=4326) %>%
  dplyr::group_by(habitat, country) %>%
  dplyr::summarize(do_union=FALSE) %>%
  st_cast("POLYGON")


USA_crit_hab_sf$habitat = c("GOM", "SEUS")
USA_crit_hab$country = "USA"

USA_crit_hab_sf = USA_crit_hab_sf %>%
  dplyr::select(country, habitat, geometry)

# Carcass location data
carcass = read.csv('data/RW_carcasses_2017.csv')
