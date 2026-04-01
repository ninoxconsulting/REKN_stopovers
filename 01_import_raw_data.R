# import raw data 

library(fs)
library(dplyr)
library(sf)
library(lubridate)

#install.packages("fpc")
#install.packages("dbscan")
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


# create an id number to link back spatial datasets 

names(rloc)

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


hist(db_sum$n, breaks = 25)

db_tagids <- db_sum |> 
  filter(n > 10) |> 
  pull(tag.id)



#####################################

## try db scan 
db <- del_birds |> filter(tag.id %in% db_tagids) 
db <- st_transform(db, crs = 4087)
db <- cbind(db, st_coordinates(db))

db <- db |>
  st_drop_geometry() |> 
  select(tag.id, X, Y)




data <- db |> filter(tag.id == 260808) |> 
  select(-tag.id)

# projections with equal area distance metrics. 
#In this case testing the Plate Carree https://epsg.io/32663 EPSG:4087
#EPSG:32662 is deprecated. 

# run dbscan for one bird to test
# eps is the distance threshold for points to be considered neighbors, 
# MinPts is the minimum number of points required to form a dense region (cluster).

eps = 2500 # distance threshold in meters (adjust as needed)

#https://www.sthda.com/english/wiki/wiki.php?id_contents=7940

dbs <- dbscan(data, eps, MinPts = 5)
#method = c("hybrid", "raw", "dist")

plot(dbs, data, main = "DBSCAN", frame = FALSE)


dbscan::kNNdistplot(data, k =  5)
abline(h = 5000, lty = 2)
abline(h = 2500, lty = 2)



## convert to kernal density metrics 







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

