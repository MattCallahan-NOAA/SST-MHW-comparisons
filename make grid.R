#### create 0.5 degree longitude by 0.25 degree latitude grid for the Gulf of Alaska
library(akmarineareas2)
library(sf)
library(tidyverse)
# i. get GOA extent
ext<-st_bbox(esr_dd%>%filter(Ecosystem_Area=="Gulf of Alaska"))

# ii. determine whether to have grid edges or centers on lat lon lines
# import crwsst to determine if those points are on corners or within lat/lon lines
crw<-readRDS("data/crw_spatial_lookup.RDS") %>% rename_with(tolower)
crw %>% ggplot() +
  geom_point(aes(x=longitude, y=latitude))+
xlim(c(-140, -139))+ylim(c(56,56.5))
#points would be within boxes, not on edges.

# 1) create grid of centroids and edges
lon <- seq(from=-164, to=-132, by=0.5)
lat <- seq(from=50.5, to=60.5, by=0.25)
#offset the centroids
lon1 <- seq(from=-164, to=-132.5, by=0.5)
lat1 <- seq(from=50.5, to=60.25, by=0.25)

#make grid to create polygon from
grid_ext <-expand.grid(lon, lat) %>%
  rename(longitude=Var1, latitude=Var2 ) 

#make grid that includes centroids
grid <-expand.grid(lon1, lat1) %>%
  rename(longitude=Var1, latitude=Var2 ) %>%
  mutate(longitude=longitude+0.25,
         latitude=latitude+0.125,
         LON=longitude,
         LAT=latitude)

#convert to sf objects
grid_ext <-st_as_sf(grid_ext, coords = c("longitude", "latitude"),crs=4326 )
grid_sf_center <-st_as_sf(grid, coords = c("LON", "LAT"),crs=4326 )

grid_poly <-st_make_grid(x=grid_ext, what="polygons", cellsize=c(0.5, 0.25))

grid_poly <-st_sf(grid_poly)

#create id field
grid_poly_id <- grid_poly %>% 
  mutate(id=row_number())

#add id to points
grid_id <- st_join(grid_sf_center, grid_poly_id, join=st_within)

#qa
ggplot()+geom_sf(data=esr_dd%>%filter(Ecosystem_Area=="Gulf of Alaska"), color="red", fill=NA)+
  geom_sf(data=grid_poly_id, fill=NA)+
  geom_sf(data=grid_id)+
  geom_sf_text(data=grid_poly_id, aes(label=id),size=2, color="blue")+
  geom_sf_text(data=grid_id, aes(label=id),  color= "orange")+
  #geom_point(grid, mapping=aes(x=longitude, y=latitude), size=2, color="blue")+
  ylim(c(54, 56))+xlim(c(-140, -136))+
  theme_bw()

#plot edges to make sure extent is the same
ggplot()+geom_sf(data=esr_dd%>%filter(Ecosystem_Area=="Gulf of Alaska"), color="red", fill=NA)+
  geom_sf(data=grid_poly_id, fill=NA)+
  geom_sf(data=grid_sf_center)+
  #geom_sf_text(data=grid_poly_id, aes(label=id),size=2, color="blue")+
  #geom_sf_text(data=grid_id, aes(label=id),  color= "orange")+
  #geom_point(grid, mapping=aes(x=longitude, y=latitude), size=2, color="blue")+
  ylim(c(60, 61))+xlim(c(-133, -131))+
 # ylim(c(50, 51))+xlim(c(-165, -163))+
  theme_bw()

#save objects
st_write(grid_poly_id, "data/grid/grid_poly.shp", append=FALSE)

grid_id <- grid_id %>% data.frame()
grid_id <- grid_id %>% dplyr::select(id, longitude, latitude)

write.csv(grid, "data/grid/grid_025latx05lon_cent.csv", row.names=FALSE)
