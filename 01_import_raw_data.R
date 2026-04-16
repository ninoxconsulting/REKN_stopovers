# import raw data 

library(fs)
library(dplyr)
library(sf)
library(lubridate)
library("rnaturalearth")
library("rnaturalearthdata")
library(fpc)
library(dbscan)


#data_dir <- "../../../Users/genev/"
data_dir <- "C:\\Users\\genev\\OneDrive\\Documents\\02.Contracts\\2022_NWRC\\02_data\\REKN_gps\\output_final\\draft_outputs_2026\\"

list.files(data_dir)

loc <- read.csv(fs::path(data_dir,"location_data_2017_2025_usable_no_outliers.csv"))
ref <- read.csv(fs::path(data_dir,"reference_data_2020_2025_20260124.csv"))

ts <- read.csv(fs::path(data_dir,"final_tags_list_edited_20260126.csv"))
               


# concentrate on rufa individuals with at least one stop (is usable tags)

rufa_ids <- ts |> 
  filter(subspecies  == "rufa") |> 
  filter(usable == "y") |> 
  pull(tag.id)

rloc <- loc |> 
  filter(tag.id %in% rufa_ids)

# temp write out
rrloc <- st_as_sf(rloc, coords = c("location.long", "location.lat"), crs = 4326)
rrloc <- rrloc |> 
  select(tag.id, date_time,year, month,  day ) 

st_write(rrloc, path("01_raw_data", "all_raw_rufa_pts.gpkg"), append = FALSE)

 ## Filter by accuracy levels 
# unique(rloc$gps.fix.type.raw)
# unique(rloc$argos.lc)
# 
# rloc |> 
#   group_by(gps.fix.type.raw) |> 
#   summarise(n = n()) |> 
#   arrange(n)
# 
# rloc |> 
#   group_by(argos.lc) |> 
#   summarise(n = n()) |> 
#   arrange(n)

# drop z, A, B, 0 and keep the remaining 
rloc <- rloc |> 
  filter(argos.lc %in% c("3", "2", "1", NA)) 
  
## keeping the lotek.crc.status or G (good) or C/F corrected or fixed
# rloc |> 
#   group_by(argos.lc, lotek.crc.status, gps.fix.type.raw) |> 
#   summarise(n = n()) |> 
#   arrange(n)


# create an id number to link back spatial datasets 

rloc <- rloc |> 
  mutate(id_order = seq(1,length(rloc$tag.id),1))


# subset only the cols of interest as first pass
rr <- rloc |> 
  select(tag.id, tag.model, location.lat, location.long, date_time, timestamp, 
         year, month, day, hour, minute, bearing, diff, gcd_m, speed_mhr, id_order,
         location.lat_prior,location.long_prior) |> 
  mutate(across(c(gcd_m, bearing), ~round(.x,0))) |> 
  mutate(across(c(diff, speed_mhr), ~round(.x,1))) |> 
  group_by(tag.id, date_time) |> 
  slice_head(n = 1)


rrsf <- st_as_sf(rr, coords = c("location.long", "location.lat"), crs = 4326)

# get summary of no of birds 

no.birds <- rrsf |> 
  group_by(tag.id) |>
  summarise(n = n()) 

low_count_birds <- no.birds |> 
  filter(n < 10) |> 
  pull(tag.id)

# filter birds which have less than 10 points? (this might need to be higher?)
rrsf <- rrsf |> 
  filter(tag.id %in% low_count_birds == FALSE) 

# add cocordinates as columns for dbscan
rrsf <- cbind(rrsf, st_coordinates(rrsf))


st_write(rrsf, path("02_clean_data", "high_accuracy_raw_rufa_pts.gpkg"), append = FALSE)


# skip to second script





##########################################################################
## Generate a few files with the tag_id to test QGIS


rrsf <- st_read(path("02_clean_data", "high_accuracy_raw_rufa_pts.gpkg"))

rrsf <- rrsf|>
  dplyr::select(tag.id, X, Y,date_time) |> 
  dplyr::mutate(timestamp = as.POSIXct(date_time)) |> 
  dplyr::select(-date_time) 

rrsf <- st_transform(rrsf, crs = 4087)
tags <- unique(rrsf$tag.id)
tags <- tags[50:80] # just test with the first 10 tags for now.

# write out a csv for each tag id to test in QGIS
for (i in 1:length(tags)) {
  tag_i <- tags[i]
  
  rrsf_i <- rrsf |> 
    filter(tag.id == tag_i) #|> 
    #st_drop_geometry() |> 
    #select(tag.id, X, Y, date_time)
  
  st_write(rrsf_i, path("01_raw_data", paste0("tag_", tag_i, "_locations.gpkg")), append = FALSE)
  
}





## transform to equal area projections 
# projections with equal area distance metrics. 
# In this case testing the Plate Carree https://epsg.io/32663 EPSG:4087
# EPSG:32662 is deprecated. 

db <- st_transform(rrsf, crs = 4087)

db <- db |>
  dplyr::select(tag.id, X, Y,date_time) |> 
  dplyr::mutate(timestamp = as.POSIXct(date_time)) |> 
  dplyr::select(-date_time) |> 
  st_drop_geometry()























################################################
# subset birds which stopped at Delaware Bay 

sf_use_s2(FALSE)
st_polys <- st_read(path("01_raw_data", "stopover_region_polygons.gpkg"))
del_birds <- sf::st_intersection(rrsf, st_polys)
del_birds <- del_birds |> 
  group_by(tag.id, date_time) 
st_write(del_birds, path("01_raw_data", "del_birds.gpkg"), append = FALSE)


# get a summary of points per bird in the DB area

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

## try db scan 
db <- del_birds |> filter(tag.id %in% db_tagids) 
#db <- rrsf |>  filter(tag.id == 260808) 
db <- st_transform(db, crs = 4087)

db <- db |>
  st_drop_geometry() |> 
  select(tag.id, X, Y)


dbi <- db |> filter(tag.id == 260808) 
write.csv(dbi, path("01_raw_data", "test_db_locations.csv"), row.names = F)

data <- dbi |> 
  select(-tag.id)

# projections with equal area distance metrics. 
#In this case testing the Plate Carree https://epsg.io/32663 EPSG:4087
#EPSG:32662 is deprecated. 

# run dbscan for one bird to test
# eps is the distance threshold for points to be considered neighbors, 
# MinPts is the minimum number of points required to form a dense region (cluster).

eps = 2500 # distance threshold in meters (adjust as needed)
#eps = 5000

#https://www.sthda.com/english/wiki/wiki.php?id_contents=7940

dbs <- dbscan(data, eps, MinPts = 5)


#method = c("hybrid", "raw", "dist")

plot(dbs, data, main = "DBSCAN", frame = FALSE)


dbscan::kNNdistplot(data, k =  5)
abline(h = 5000, lty = 2)
abline(h = 2500, lty = 2)


## Remove the points that are outside the main clusters 
## add the cluster number back to full dataset and filter out

dbi$cluster <- dbs$cluster

dbisf<- st_as_sf(dbi, coords = c("X", "Y"), crs = 4087)
  
dbisf <- dbisf |> 
  filter(cluster !=0)

plot(dbisf)


# entire north America 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  #geom_sf(data = all_seasons, aes(colour = id), alpha = 0.1, colour = "dark blue") +
  geom_sf(data = dbisf, aes(colour = cluster), size = 1.2, alpha = 0.6) +
  #facet_wrap(~movement_dir)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-76, -74), ylim = c(38, 40), expand = FALSE)+
  #coord_sf(xlim = c(-130, -40), ylim = c(10, 80), expand = FALSE)+
  scale_colour_viridis_c() +
  #coord_sf() +
  #facet_wrap(vars(type)) +
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())


global




## Generate kernal density ####
# library(sf)
library(mapview)
library(adehabitatHR)
library(sp)


mcp <- st_convex_hull(st_union(dbisf))
plot(mcp)
plot(dbisf$geom, col = "blue", add = T)


# 2) Create SpatialPointsDataFrame object
dbisfgeo <- dbisf |>  
   dplyr::select(tag.id) |>
   as("Spatial")

## Calculate MCPs 

c.mcp <- mcp(dbisfgeo, percent = 100)

# # convert this to a sf object 
c.mcp_sf <- st_as_sf(c.mcp)
# 
# # plot with mapview
mapview::mapview(c.mcp_sf)

# lets look at 75% inclusion
c.mcp.75 <- mcp(dbisfgeo, percent = 75)

 # Plot
plot(dbisfgeo, col = as.factor(dbisfgeo$tag.id), pch = 16)
plot(c.mcp , col = scales::alpha(1:5, 0.5), add = TRUE)
plot(c.mcp.75, col = scales::alpha(1:5, 0.5), add = TRUE)
 
# # Calculate the MCP by including 50 to 100 percent of points
hrarea <- mcp.area(dbisfgeo, percent = seq(50, 100, by = 10))


# ### 3. Kernel Density Estimates (kde)
 
# Kernel Density Estimates (kde) are a popular method to estimate the distribution of data, hence are used to estimate home ranges. Calculations for KDE require consideration of the mathematical method to describe the density function. A detailed description of the parameters can be found [here](http://www.spatialecology.com/gme/kde.htm).
# KDEs are very sensitive to input parameters, specifically the bandwidth (h) which determines the [smoothing parameter](https://cran.r-project.org/web/packages/adehabitatHR/vignettes/adehabitatHR.pdf). H parameters can be estimated using three methods:
#   
# - a reference bandwidth (h = σ × n \^−1/6), however this is generally an overestimate of the range, and is not suitable for multi-modal distributions.
# - Least Square Cross Validation (LSCV), which minimizes the difference in volume between the true UD and the estimates UD.
# - A subjective visual choice for the smoothing parameter, based on successive trials (Silverman, 1986; Wand & Jones 1995).
# 
# Other parameters include:
# - Kernel Type: The type of kernel is limited to Gaussian (bivariate normal), quadratic or normal. We used the bivariate normal model as default.
# - Grid/extent: These determines the area or extent over which the home range will be estimated. This is a mix of fine scale and time consuming processing and faster blocky resolution over a continuous surface. As a rule of thumb, Geospatial Modelling Environment program (GME) formally Hawth's tools suggest: take the square root of the x or y variance value (whichever is smaller) and divide by 5 or 10 (I usually round to the nearest big number - so 36.7 becomes 40). Before using this rule of thumb value calculate how many cells this will result in for the output (take the width and height of you input points, divide by the cell size, and multiply the resulting numbers together). If you get a value somewhere between 1-20 million, then you have a reasonable value.
# 
# To create home ranges for each unique id we used a bivariate normal kernel with a variety of h (smoothing parameters). We then interpreted visually to determine size to use based on successive trials. This is supported by the literature (Hemson et al. 2005; Calenge et al. 2011).
# 
# It is also possible to run KDE with set barriers and boundaries.



# #### 3.1 kde: h reference parameter
dbisfgeo <- dbisf |>  
  dplyr::select(tag.id) |>
  as("Spatial")

# # define the parameters (h, kern, grid, extent) 
kde_href  <- kernelUD(dbisfgeo, h = "href", kern = c("bivnorm"), grid = 500, extent = 2)

# kde_href
# From this object (**Utilization distribution of several Animals**) we can extract the vertices or polygons which define the percentage we wish to include.
# 
# ```{r}
 ver95 <- getverticeshr(kde_href,95) # get vertices for home range
 ver95_sf <- st_as_sf(ver95)         # convert to sf object 
   
 ver75 <- getverticeshr(kde_href,75)
 ver75_sf <- st_as_sf(ver75 )
   
 ver50 <- getverticeshr(kde_href,50)
 ver50_sf<- st_as_sf(ver50)
  
# # plot the outputs 
 mapview(ver50_sf, zcol = "id") 
 mapview (ver75_sf, zcol = "id") 
 mapview (ver95_sf, zcol = "id")
  
 plot(st_geometry(ver95_sf),col = "yellow") 
 plot(st_geometry(ver75_sf),col = "blue", add = TRUE)
 plot(st_geometry(ver50_sf),col = "purple", add = TRUE)
 plot(dbisfgeo, pch = 1, size = 0.5, add = TRUE)     # Add points 

 
# #### 3.2 kde: Least Squares Cross Validation (lscv) method.
# 
kde_lscv  <- kernelUD(dbisfgeo, h = "LSCV", kern = c("bivnorm"), grid = 500, extent = 2)
 
ver95ls <- getverticeshr(kde_lscv,95) # get vertices for home range
ver95ls_sf <- st_as_sf(ver95ls) 
 
ver50ls <- getverticeshr(kde_lscv,50)
ver50ls_sf <- st_as_sf(ver50ls) 

# plot the outputs 
mapview(ver50ls_sf, zcol = "id") 
mapview (ver95ls_sf, zcol = "id")

# 
# #### 3.3 kde: variable smoothing parameters (h)
# 
# To test the sensitivity of the h value we can test a bivariate normal kernel with a variety of smoothing parameters (h = 1000, 2000, 4000).
# We can then interpreted visually to determine size to use based on successive trials. This approach is supported by literature (Hemson et al. 2005; Calenge et al. 2011).
# 

kde_h1000  <- kernelUD(dbisfgeo, h = 1000, kern = c("bivnorm"), grid = 500,extent = 2)
kde_h500  <- kernelUD(dbisfgeo, h = 500, kern = c("bivnorm"), grid = 500,extent = 2)
kde_h3000  <- kernelUD(dbisfgeo, h = 3000, kern = c("bivnorm"), grid = 500,extent = 2)
 
# Lets extract the vertices and compare the outputs by building a plot.
# # kde - href = 1000
ver95_1000 <- getverticeshr(kde_h1000, 95) # get vertices for home range
ver95_1000_sf <- st_as_sf(ver95_1000) |>     mutate(h = 1000) # convert to sf object 

# # kde - href = 500
ver95_500 <- getverticeshr(kde_h500, 95) # get vertices for home range
ver95_500_sf <- st_as_sf(ver95_500)  |> 
   mutate(h = 500) # convert to sf object 
#   
# kde - href = 3000
ver95_3000 <- getverticeshr(kde_h3000, 95) # get vertices for home range
ver95_3000_sf <- st_as_sf(ver95_3000)  |> 
   mutate(h = 3000) # convert to sf object 
#   
# # bind all data together 
all_verts <- bind_rows(ver95_1000_sf,  ver95_500_sf,  ver95_3000_sf)

# # lets plot the output 
 library(ggplot2)
ggplot(data = all_verts) +
 geom_sf(
     aes(colour = id), 
     alpha = 0.1
   ) + 
   scale_colour_viridis_d() + 
   facet_wrap(vars(h)) +
   theme_bw()
   



# ## Comparison of kde parameters
# 
# Lets rerun the home range by season and compare the methods for the KDE. 
dbisfgeo <- dbisf |>  
  dplyr::select(tag.id) |>
  as("Spatial")
 
 # href 
kde_href  <- kernelUD(dbisfgeo, h = "href", kern = c("bivnorm"), grid = 500, extent = 2)
 
 # custome h values 
 kde_h1000  <- kernelUD(dbisfgeo, h = 1000, kern = c("bivnorm"), grid = 500,extent = 2)
 
 # lscv 
 kde_lscv  <- kernelUD(dbisfgeo, h = "LSCV", kern = c("bivnorm"), grid = 500, extent = 2)
  
 
 # build function to get vertices 
 
get_verts <- function(in_kde, percent = 95, fieldname){

  ver <- getverticeshr(in_kde, percent) # get vertices for home range
  ver_sf <- st_as_sf(ver)  |>
    mutate(type = fieldname)

  return(ver_sf)
}

dbisfgeo
# genertate vertices for each KDE

href <- get_verts(kde_href, percent = 95, fieldname = "href")
h1000 <- get_verts(kde_h1000, percent = 95, fieldname = "h1000")
kde_lscv <- get_verts(kde_lscv, percent = 95, fieldname = "lscv")
 
# bind all data together 
all_seasons <- bind_rows(href, h1000, kde_lscv)
   
# lets plot the output
ggplot(data = all_seasons) +
geom_sf(
    aes(colour = id),
    alpha = 0.1
  ) +
  scale_colour_viridis_d() +
  coord_sf() +
  facet_wrap(vars(type)) +
  theme_bw()




# Geographic distributon of tags
world <- ne_countries(scale = "medium", returnclass = "sf")

Americas <- world %>% 
  dplyr::filter(region_un == "Americas")%>% 
  select(admin)

# entire north America 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = all_seasons, aes(colour = id), alpha = 0.1, colour = "dark blue") +
  geom_sf(data = dbisf, size = 1.2, alpha = 0.2, colour = "dark blue") +
  #facet_wrap(~movement_dir)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-76, -74), ylim = c(38, 40), expand = FALSE)+
  #coord_sf(xlim = c(-130, -40), ylim = c(10, 80), expand = FALSE)+
  #scale_colour_viridis_d() +
  #coord_sf() +
  facet_wrap(vars(type)) +
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global
#ggsave(file.path(out.plots,"figure1_all_reknnoalpha0.2.jpg"), width = 30, height = 30,units = "cm", dpi = 600)





# ```
# 
# From this example we can see the large difference in home range estimates, depending on the type of method used and parameters selected. 
# 
# When generating home range estimates it is important to think about the application of use, and test various parameters to estimate the best fit. 
# 
# 
# ### References
# 
# -   Packages: Estimate Kernel home range Utilization Distribution Using adehabitatHR [(Calenge et al. 2011)](https://cran.r-project.org/web/packages/adehabitatHR/vignettes/adehabitatHR.pdf)
# -   [KDE]("http://www.spatialecology.com/gme/kde.htm")
# -   https://www.ckwri.tamuk.edu/sites/default/files/publication/pdfs/2017/leonard_analyzing_wildlife_telemetry_data_in_r.pdf
# -   Seaman, D. E., Millspaugh, J. J., Kernohan, B. J., Brundige, G. C., Raedeke, K. J., & Gitzen, R. A. (1999). Effects of sample #size on kernel home range estimates. The journal of wildlife management, 739-747.
# -   Kernohan, B. J., R. A. Gitzen, and J. J. Millspaugh. 2001. Analysis of animal space use and movements. Pages 125--166 in J. J. #Millspaugh and J. M. Marzluff, editors. Radio tracking and animal populations. Academic Press, San Diego, CA, USA
# -   Hemson, G., Johnson, P., South, A., Kenward, R., Ripley, R., & MACDONALD, D. (2005). Are kernels the mustard? Data from global positioning system (GPS) collars suggests problems for kernel home‐range analyses with least‐squares cross‐validation. Journal of Animal Ecology, 74(3), 455-463








# Geographic distributon of tags
world <- ne_countries(scale = "medium", returnclass = "sf")

Americas <- world %>% 
  dplyr::filter(region_un == "Americas")%>% 
  select(admin)

# entire north America 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = rf_sf, size = 1.2, alpha = 0.2, colour = "dark blue") +
  #facet_wrap(~movement_dir)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-185, -20), ylim = c(-60, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global
ggsave(file.path(out.plots,"figure1_all_reknnoalpha0.2.jpg"), width = 30, height = 30,units = "cm", dpi = 600)






#plot(data)

############################################################################
## Calculate distance between points and bearing

bdd_det <- out  |> 
  #filter(tag.id == 230318) |> 
  group_by(tag.id) |> 
  mutate(location.long_prior = lag(location.long, 1L),
         location.lat_prior = lag(location.lat, 1L))

bdd_det <- bdd_det |> 
  rowwise() %>%
  dplyr::mutate(gcd_m = distHaversine(c(location.long_prior,location.lat_prior), c(location.long, location.lat)),
                bearing = bearing(c(location.long_prior,location.lat_prior), c(location.long, location.lat)),
                speed_mhr = round((gcd_m/diff)/1000,1))%>% 
  ungroup()


#length(unique(bdd_det$tag.id))


## Add id values 

bdd_det <- bdd_det %>%
  dplyr::mutate(id = seq(1, length(bdd_det$tag.id), 1))

