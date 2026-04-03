## test run for results in delaware Bay 

library(fs)
library(dplyr)
library(sf)
library(lubridate)
library("rnaturalearth")
library("rnaturalearthdata")
library(fpc)
library(dbscan)
library(ggplot2)
library(adehabitatHR)
library(sp)

# read in del birds 

del_birds <- st_read(path("01_raw_data", "del_birds.gpkg"))

db_sum <- del_birds |> 
  st_drop_geometry() |> 
  group_by(tag.id) |> 
  summarise(n = n()) |> 
  arrange(n)

#hist(db_sum$n, breaks = 25)

# filter where less than 10 points within aoi
db_tagids <- db_sum |> 
  filter(n > 10) |> 
  pull(tag.id)


#####################################
## 1: Use db scan to remove outliers based on month and tag id. 

db <- del_birds |> filter(tag.id %in% db_tagids) 
db <- st_transform(db, crs = 4087)
db <- cbind(db, st_coordinates(db))

#plot(db$year, db$month)
#hist(db$month)

db <- db |>
  st_drop_geometry() |> 
  select(tag.id,year, month, day, X, Y)

dbsf <- st_as_sf(db, coords = c("X", "Y"), crs = 4087)

# make list of tags by month 
#tagls <- unique(db$tag.id)
taglsm <- db |> select(tag.id, month, day) |> unique()
taglsm <- taglsm |> mutate(id = seq(1:length(taglsm$tag.id)))


# identify the stopover locations 

all_pts <- purrr::map(taglsm$id, function(x){
  
  #x = taglsm$id[63]  
  taglsmi <- taglsm[x,]
  
  dbi <- db |> 
    filter(tag.id == taglsmi$tag.id) |> 
    filter(month == taglsmi$month)
  
  dbixy <- dbi |> select(X,Y)
  
  dbs <- dbscan(dbixy, 2500, MinPts = 5)
  #plot(dbs, dbixy, main = "DBSCAN", frame = FALSE)
  dbi$cluster <- dbs$cluster
  dbisf <- st_as_sf(dbi, coords = c("X", "Y"), crs = 4087)
  dbisf <- dbisf |> filter(cluster !=0)
  
}) |> bind_rows()

st_write(all_pts, path("01_raw_data", "Del_birds_month_yr_pts.gpkg"))



all_pts <- st_read(path("01_raw_data", "Del_birds_month_yr_pts.gpkg"))
# pts with remove the outlier points 

# # # Geographic distributon of tags
world <- ne_countries(scale = "medium", returnclass = "sf")
Americas <- world %>%
  dplyr::filter(region_un == "Americas")%>%
  select(admin)

# entire north America
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = dbsf, size = 1.2, alpha = 0.4, col = "darkgrey") +
  geom_sf(data = all_pts, size = 1.2, alpha = 0.2, col = "blue") +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-76, -73.5), ylim = c(38, 40.5), expand = FALSE)+
  scale_colour_viridis_c() +
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())
global


#####################################################
## Part 2: generate the polygons based on the stopover locations without outliers

all_pts<- cbind(all_pts, st_coordinates(all_pts))

# convert to sp object
all_pts_sp <- all_pts|> 
  as("Spatial") 

all_pts <- all_pts |> st_drop_geometry()

taglsm <- all_pts |> select(tag.id, month) |> unique()

taglsm <- taglsm |> mutate(id = seq(1:length(taglsm$tag.id)))

#taglsm <- taglsm[1:3]

# loop through the combinations of month and year and id 
all_poly <- purrr::map(taglsm$id, function(x){
  
  #print(x)
  #x = taglsm$id[5]  
  taglsmi <- taglsm[x,]
  
  dbi <- all_pts |> 
    filter(tag.id == taglsmi$tag.id) |> 
    filter(month == taglsmi$month) 
  
  #  kde: h reference parameter
  dbisf <- st_as_sf(dbi, coords = c("X", "Y"), crs = 4087)
  
  dbisp <- dbisf |> 
    select(tag.id) |> 
    as("Spatial")
  
  # # define the parameters (h, kern, grid, extent) 
  kde_href  <- kernelUD(dbisp, h = "href", kern = c("bivnorm"), grid = 500, extent = 2)
  
  # add a try statement to skip to next line if error is produced in vers95
  
  ver95_sf <- tryCatch({
    ver95 <- getverticeshr(kde_href,95) # get vertices for home range
    st_as_sf(ver95) |> 
      mutate(th = 95)        # convert to sf object 
  }, error = function(e) {
    return(NULL) # return NULL if error occurs
  })
  
  ver75_sf <- tryCatch({
    ver75 <- getverticeshr(kde_href,75)
    st_as_sf(ver75 )|> 
      mutate(th = 75)
  }, error = function(e) {
    return(NULL) # return NULL if error occurs
  })
  
  ver50_sf <- tryCatch({
    ver50 <- getverticeshr(kde_href,50)
    st_as_sf(ver50) |> 
      mutate(th = 50)
  }, error = function(e) {
    return(NULL) # return NULL if error occurs
  })
  
  # if it is not null the bind 
  if(!is.null(ver95_sf) & !is.null(ver75_sf) & !is.null(ver50_sf)) {
    allvers <- bind_rows( ver95_sf, ver75_sf , ver50_sf)
    allvers$month = unique(dbi$month)
    allvers$year = unique(dbi$year)
    
    return(allvers)
  }
  
}) |> bind_rows()

st_write(all_poly, path("01_raw_data", "Del_birds_month_yr_polygons.gpkg"))




## Summarise the dataset by tag id and no of months 
## how many birds have more than 1 year and more than one month 

all_polys <- st_read(path("01_raw_data", "Del_birds_month_yr_polygons.gpkg"))

all_pts <- st_read(path("01_raw_data", "Del_birds_month_yr_pts.gpkg"))

summary_db <- all_polys |> 
  st_drop_geometry() |> 
  group_by(id) |> 
  summarise(n_months = n_distinct(month),
            n_years = n_distinct(year)) 


## note we can filter this down to 50% percentage 

all_poly_50 <- all_polys |> filter(th == 50)

# ggplot 

global_href <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  #geom_sf(data = dbsf,  size = 1, alpha = 0.2,colour = "blue") +
  geom_sf(data = all_pts,  size = 1, alpha = 0.2,colour = "lightblue") +
  #geom_sf(data = all_poly_50, linewidth = 0.5, alpha = 0.4, fill = "red")+
  geom_sf(data = all_poly_50,  alpha = 0.4,fill = "red")+
  xlab("Longitude") + ylab("Latitude") +
  #coord_sf(xlim = c(-75, -74.35), ylim = c(38.8, 39.4), expand = FALSE)+
  coord_sf(xlim = c(-76, -74), ylim = c(38, 40), expand = FALSE)+
  theme_bw()+
  facet_wrap(~month)+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global_href








###################################################
## Part 3: Calculate a density plots using points. 

##################################################
# # filter so that all points are the same density 

library(ggplot2)
library(dplyr)
library(viridis)
library(ggpointdensity)
library(ggspatial) # For base maps

all_pts <- st_read(path("01_raw_data", "Del_birds_month_yr_pts.gpkg"))

all_pts<- cbind(all_pts, st_coordinates(all_pts))

all_pts_density <- all_pts|> group_by(tag.id, year, month, day) |>
  slice_head(n = 1) |> 
  ungroup() 

ggplot(data = all_pts, mapping = aes(x = X, y = Y)) +
  geom_pointdensity() +
  scale_color_viridis()+
  theme_bw()

ggplot(data = all_pts, mapping = aes(x = X, y = Y)) +
  geom_pointdensity() +
  annotation_map_tile(type = "osm") +
  scale_color_viridis()+
  facet_wrap( ~ month ) 


ggplot(data = all_pts, mapping = aes(x = X, y = Y)) +
  geom_bin2d()+
  #annotation_map_tile(type = "osm") +
  scale_color_viridis()+
  facet_wrap( ~ month ) 



## Testing map 1 

ggplot(all_pts) +
  stat_density_2d(aes(x = st_coordinates(all_pts)[,1],
                      y = st_coordinates(all_pts)[,2]),
                  geom = "polygon") +
  scale_fill_viridis_c() +
  labs(title = "Spatial Point Density", x = "Longitude", y = "Latitude") +
  theme_minimal()

# Testing map 2

global_href <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  #geom_sf(data = dbsf,  size = 1, alpha = 0.2,colour = "blue") +
  #geom_sf(data = all_pts,  size = 1, alpha = 0.2,colour = "lightblue") +
  geom_pointdensity(all_pts, mapping = aes(x = X, y = Y)) +
  
  #geom_sf(data = all_poly_50,  alpha = 0.4,fill = "red")+
  xlab("Longitude") + ylab("Latitude") +
  #coord_sf(xlim = c(-75, -74.35), ylim = c(38.8, 39.4), expand = FALSE)+
  coord_sf(xlim = c(-76, -74), ylim = c(38, 40), expand = FALSE)+
  theme_bw()+
  facet_wrap(~month)+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global_href