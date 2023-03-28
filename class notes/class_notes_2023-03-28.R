# Haley Durbin
# 2023-03-28

library(tidyverse)

library(raster)
library(mapdata)
library(marmap) # getNOAA.bathy()'

chl_raster = raster('data/AQUA_MODIS.20020701_20220731.L3m.MC.CHL.chlor_a.9km.nc')

names(chl_raster) = "chl_a" # Easier to type!
chl_pts = raster::rasterToPoints(chl_raster, spatial = TRUE) # convert to SpatialPointsDataFrame
chl_df  = data.frame(chl_pts) # explicitly converts spatial data (lat & lon) into columns 
head(chl_df)

# See range of data to set good limits for color palette
hist(chl_df$chl_a)
hist(log10(chl_df$chl_a)) # now we can see more of the variability in the data

cols = rainbow(7, rev=TRUE)[-1] # reverse rainbow color hex codes, drops the first color (purple);

# Plot global chl
# geom_raster() plots faster, geom_tile() is slightly more precise
global_chl_map = ggplot() +
  geom_raster(data = chl_df , aes(x = x, y = y, fill = log10(chl_a))) + # chl_a typically viewed on log scale
  scale_fill_gradientn(colors = cols, limits=c(-1.5, 0.75), name="log_10(chl_a)") +
  ggtitle("Global chl_a July climatology") +
  theme_classic()

global_chl_map
ggsave(global_chl_map, filename='figures/global_chl_July.pdf', device="pdf", height=5, width=9)

# Gulf of Maine
# set GOM map limits
lon_bounds = c(-72, -62)
lat_bounds = c(39, 47)

## crop GOM
chl_GOM_raster = raster::crop(chl_raster, extent(c(lon_bounds, lat_bounds))) # ?extent

# Convert GOM raster to points and then to a data frame
chl_GOM_df = data.frame( rasterToPoints(chl_GOM_raster, spatial = TRUE) ) # from raster package

chl_GOM_df = chl_GOM_df %>% dplyr::select(-optional) # drop the optional column

# Grab coastline data from R's worldHires data in the mapdata package:
world_map = map_data("worldHires") # from mapdata package
head(world_map)

GOM_chl_map = ggplot() +
  geom_raster(data = chl_GOM_df , aes(x = x, y = y, fill = log10(chl_a))) + # geom_tile() gives more precise lines
  geom_polygon(aes(x=long, y = lat, group = group), fill = "darkgrey", data=world_map) + # add coastline
  coord_fixed(1.3, xlim = lon_bounds, ylim = lat_bounds, expand=FALSE) + # crop map; 1.3 y/x aspect ratio (may want to use higher at the poles; use 1 near the equator)
  scale_fill_gradientn(colors = cols, limits=c(-1, 1.75)) + # Note I changed color limits from global
  ggtitle("GOM chl_a July climatology") +
  theme_bw() +
  xlab("Longitude") + ylab("Latitude") 

GOM_chl_map
ggsave(GOM_chl_map, filename='figures/GOM_chl_July.pdf', device="pdf", height=5, width=7)


##### NOAA bathymetry (from marmap package)
# same GOM map limits
lon_bounds = c(-72, -62)
lat_bounds = c(39, 47)

#Download bathymetry data from NOAA (in meters)
# install.packages("rgdal") # may need to reinstall rgdal and raster packages for marmap to work
bath_m_raw = marmap::getNOAA.bathy(lon1 = lon_bounds[1], 
                                   lon2 = lon_bounds[2],
                                   lat1 = lat_bounds[1], 
                                   lat2 = lat_bounds[2], 
                                   resolution = 4) # resolution default: 4 arcminutes


