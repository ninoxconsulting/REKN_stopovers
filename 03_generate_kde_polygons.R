#03_ generate the polygons with KDE

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

# read in pts
pts <- st_read(path("02_clean_data", "pts_edited_hdscan.gpkg"))

## all cols 
#rrsf <- st_read(path("02_clean_data", "high_accuracy_raw_rufa_pts.gpkg"))



#####################################################
## Part 2: generate the polygons based on the stopover locations without outliers

all_pts <- cbind(pts, st_coordinates(pts))
all_pts <- all_pts |> 
  mutate(month = month(date_time), year = year(date_time))

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
  x = taglsm$id[5]  
  taglsmi <- taglsm[x,]
  
  dbi <- all_pts |> 
    filter(cluster_edit != 0) |> 
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





