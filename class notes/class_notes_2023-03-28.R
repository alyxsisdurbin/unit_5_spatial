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

bath_m_df = marmap::fortify.bathy(bath_m_raw) # positive values are land, negative values are sea
bath_m = bath_m_df %>%
  mutate(depth_m = ifelse(z>20, NA, z)) %>% # 20m gives us wiggle room from sea level for tides/coastline
  dplyr::select(-z)

head(bath_m)
head(world_map)

# plot raster data
# plot raster data
GOM_bath_map = ggplot()+
  geom_raster(data = bath_m , aes(x = x, y = y, fill = depth_m)) + 
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "darkgrey", color = "black") + # add coastline; group keeps multipolygon coordinates separated into distinct groups # color=NA
  coord_fixed(1.3, xlim = lon_bounds, ylim = lat_bounds, expand=FALSE) + # Crop map edges
  scale_fill_gradientn(colors=c("black", "darkblue", "lightblue"), 
                       values = scales::rescale(c(-6000, -300, 0)), # rescale to make 2 different gradients (rescale function from scales package in tidyverse)
                       name="Depth (m)") +
  ylab("Lat") + xlab("Lon") + theme_bw() 

GOM_bath_map # print to screen

# plot contour lines
GOM_bath_map_contours = ggplot() +
  geom_contour(data=bath_m, aes(x=x, y=y, z=depth_m), 
               breaks=c(-100), linewidth=c(0.25), color="grey") +
  geom_contour(data=bath_m, aes(x=x, y=y, z=depth_m), 
               breaks=c(-200), linewidth=c(0.5), color="grey") +
  geom_contour(data=bath_m, aes(x=x, y=y, z=depth_m), 
               breaks=c(-500), linewidth=c(0.75), color="grey") +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "darkgrey", color = "black") + 
  coord_fixed(1.3, xlim = lon_bounds, ylim = lat_bounds, expand=FALSE) 
  
ggsave(GOM_bath_map_contours, filename='figures/GOM_bath_contours.pdf', device="pdf", height=5, width=7)

#rasterize the bathy object

bath_m_raster = marmap::as.raster(bath_m_raw)
chl_GOM_raster
bath_m_raster
# Note the CRS is the same for both rasters WGS84
# Extent is slightly different 
# bath_m resolution is higher than chl resolution

names(bath_m_raster) = "bath_m"
names(chl_GOM_raster)="chl_a"

# resample bath_m to match chl_a
bath_layer_chl_dims = raster::resample(bath_m_raster, chl_GOM_raster)

# now that extent, origin, resolution and projection match, create raster stack
raster_stack = stack(chl_GOM_raster, bath_layer_chl_dims)
raster_stack
plot(raster_stack)

# convert to data frame
stack_df = data.frame(raster::rasterToPoints(raster_stack))
head(stack_df)
summary(stack_df)




#O'Reilly
# chl_a benchmarks for oligo- meso- and eutrophic ocean waters derived from SeaWiFS data
oligo_chl_a = 0.1 # chl_a < 0.1 mg/m^3
eutro_chl_a = 1.67 # chl_a > 1.67 mg/m^3

stack_df = stack_df %>%
  mutate(trophic_index = case_when(chl_a < oligo_chl_a ~ "oligotrophic",
                                   chl_a > oligo_chl_a & chl_a < eutro_chl_a ~ "mesotrophic",
                                   chl_a > eutro_chl_a ~ "eutrophic"))  %>%
  mutate(trophic_index = as.factor(trophic_index))

# What portion of our area of interest is classified as oligotrophic, mesotrophic and eutrophic?
table(stack_df$trophic_index)  # no oligotrophic waters in GOM region
## 

trophic_status = stack_df %>% 
  filter(!is.na(trophic_index)) %>%
  group_by(trophic_index) %>% 
  summarize(n=n()) %>%
  mutate(proportion = n/sum(n))

# Plot histogram of bathymetric depth (m) for each trophic index
ggplot() +
  geom_histogram(aes(x=bath_m), data=stack_df) + # %>% filter(!is.na(trophic_index))) +
  facet_wrap(~trophic_index)

ggplot() +
  geom_boxplot(aes(y=bath_m, x=trophic_index), data=stack_df)

stack_df %>% filter(trophic_index=="eutrophic", bath_m > 50) 
# plot trophic raster data
trophic_map = ggplot()+
  geom_raster(data = stack_df, aes(x = x, y = y, fill = trophic_index)) + 
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group)) + # add coastline; group keeps multipolygon coordinates separated into distinct groups
  coord_fixed(1.3, xlim = lon_bounds, ylim = lat_bounds, expand=FALSE) + # Crop map edges
  ylab("Lat") + xlab("Lon") + theme_bw() 
trophic_map