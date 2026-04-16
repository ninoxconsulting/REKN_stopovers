## Run the dbscan at multiple eps distances for entire data set 
#2500
#5000

# dpendant on the output of script (01_import_data.R) 
# this is the filtered dataset by accuracy level and min number 

library(fs)
library(dplyr)
library(sf)
library(lubridate)
#library("rnaturalearth")
#library("rnaturalearthdata")
library(fpc)
library(dbscan)


rrsf <- st_read(path("02_clean_data", "high_accuracy_raw_rufa_pts.gpkg"))

## transform to equal area projections 
# projections with equal area distance metrics. 
# In this case testing the Plate Carree https://epsg.io/32663 EPSG:4087
# EPSG:32662 is deprecated. 
db <- st_transform(rrsf, crs = 4087)

db <- db |>
  dplyr::select(tag.id,id_order, X, Y,date_time) |> 
  dplyr::mutate(timestamp = as.POSIXct(date_time)) |> 
  st_drop_geometry()


## Cycle through each tag and generate the 2500 dbcan cluster analysis 
# eps = 2500 # distance threshold in meters (adjust as needed)

db2500 <- purrr::map(
  .x = unique(db$tag.id),
  .f = function(x) {
    
    # testing line
    #x <- unique(db$tag.id)[38]
    
    dbi <- db |> dplyr::filter(tag.id == x)
    ddata <- dbi |> dplyr::select( X,Y )
    
    MinPts = 5
    #eps = 2500 # distance threshold in meters (adjust as needed)

    #dbs <- dbscan(ddata, eps, MinPts = MinPts)
    hdbs <- hdbscan(ddata, minPts = MinPts)
    
   #dbscan::kNNdistplot(ddata, k =  5)
   #dbi$cluster <- dbs$cluster
    dbi$cluster <- hdbs$cluster
    
    # check the distance between locations? 
    sf_sub <- st_as_sf(dbi, coords = c("X", "Y"), crs = 4326)
    sf_sub <- st_transform(sf_sub, crs = 4087)
    
    # estimate for each point : 
    #    - the distance to the previous point 
    #    - time difference to previous point, 
    #    -if cluster is different from previous 
    #    -if point is well beyond threshold (e.g. 20km) from previous point.
    
    sf_sub <- sf_sub |> 
      arrange(tag.id, timestamp) |>    # ensure correct order
      mutate(
        dist_prev = as.numeric(round(
          st_distance(geometry, lag(geometry), by_element = TRUE),0)
        ),
        time_diff_prev = as.numeric(difftime(timestamp, lag(timestamp), units = "days")),
        cluster_dif = ifelse(cluster != lag(cluster), 1, 0),
        cluster_dis_dif = ifelse(dist_prev >20000,1,0)
      ) 
    
    sf_sub <- st_transform(sf_sub, crs = 4326)
    
    # library(ggplot2)
    # 
    # ggplot(dbi, aes(X, Y, color = cluster)) +
    #   geom_point(size = 2, alpha = 0.8) +
    #   scale_colour_viridis_c() +
    #   labs(
    #     title = "HDBSCAN Clustering",
    #     subtitle = "Cluster 0 = Noise",
    #     x = "X", y = "Y",
    #     color = "Cluster"
    #   ) +
    #   theme_minimal()
    # 
    return(sf_sub)
  }
)

db2500 <- bind_rows(db2500)

# dbisf <- dbisf |> 
#   filter(cluster !=0)
# plot(dbisf)

db2500 <- db2500 |> 
  dplyr::mutate(cluster_edit = cluster)

# write out to check 
st_write(db2500, path("02_clean_data", "pts_raw_hdscan.gpkg"), delete_dsn = TRUE)



####
# Tried a series of different options for clustering as original methods were not 
# showing promising resulsts. Tested heirachial HDCSCAN, spatio-temporal dbcan clustering
# and also compared the outputs from QGIS for both DBscan and ST_DBCAN. 
# the algorithms by QGIS seemed to capture the clusters better than the R implementations,
# I added a few more metrics and applied the HDBCAN clustering to the data.
# Next step is to code up the changes and review these in Qgis to see what needs to be updated in code to 
# make this repeatable. 




####################################################################################
####################################################################################

# review in QGIS and edit the file as per review 


pt <- st_read(path("02_clean_data", "pts_raw_hdscan.gpkg"))
              
              
st_write(pt, path("02_clean_data", "pts_edited_hdscan.gpkg"), delete_dsn = TRUE)
                   










# # run dbscan for one bird to test
# # eps is the distance threshold for points to be considered neighbors, 
# # MinPts is the minimum number of points required to form a dense region (cluster).
# 
# eps = 2500 # distance threshold in meters (adjust as needed)
# #eps = 5000
# 
# #https://www.sthda.com/english/wiki/wiki.php?id_contents=7940
# 
# dbs <- dbscan(data, eps, MinPts = 5)
# #method = c("hybrid", "raw", "dist")
# 
# plot(dbs, data, main = "DBSCAN", frame = FALSE)
# 
# dbscan::kNNdistplot(data, k =  5)
# abline(h = 5000, lty = 2)
# abline(h = 2500, lty = 2)
# 
# ## Remove the points that are outside the main clusters 
# ## add the cluster number back to full dataset and filter out
# 
# dbi$cluster <- dbs$cluster
# 
# dbisf<- st_as_sf(dbi, coords = c("X", "Y"), crs = 4087)
# 
# dbisf <- dbisf |> 
#   filter(cluster !=0)
# 
# plot(dbisf)
# 
# 
# # entire north America 
# global <- ggplot(data = Americas) +
#   geom_sf(color = "grey") +
#   #geom_sf(data = all_seasons, aes(colour = id), alpha = 0.1, colour = "dark blue") +
#   geom_sf(data = dbisf, aes(colour = cluster), size = 1.2, alpha = 0.6) +
#   #facet_wrap(~movement_dir)+
#   # geom_point(ru, aes(x = lng, y = lat), size = 4) +
#   xlab("Longitude") + ylab("Latitude") +
#   coord_sf(xlim = c(-76, -74), ylim = c(38, 40), expand = FALSE)+
#   #coord_sf(xlim = c(-130, -40), ylim = c(10, 80), expand = FALSE)+
#   scale_colour_viridis_c() +
#   #coord_sf() +
#   #facet_wrap(vars(type)) +
#   theme_bw()+
#   theme(axis.text.x=element_blank(),
#         axis.text.y=element_blank())
# 
# 
# global
# 

