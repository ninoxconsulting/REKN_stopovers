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
pts <- st_read(path("02_clean_data", "pts_edited_hdscan.gpkg")) |> 
  arrange(tag.id, date_time)

## all cols 
#rrsf <- st_read(path("02_clean_data", "high_accuracy_raw_rufa_pts.gpkg"))

pts <- st_transform(pts, crs = 4087)

#####################################################
## Part 2: generate the polygons based on the stopover locations without outliers

all_pts <- cbind(pts, st_coordinates(pts))

all_pts <- all_pts |> 
  mutate(month = month(date_time), year = year(date_time))

# convert to sp object
all_pts_sp <- all_pts|> 
  as("Spatial") 

all_pts <- all_pts |> st_drop_geometry()

taglsm <- all_pts |> select(tag.id) |> unique() |> pull()

#taglsm <- taglsm[1:20]

# loop through the combinations of month and year and id 
all_poly <- purrr::map(taglsm, function(x){
  
  print(x)
  #x = taglsm[1]  
  #taglsmi <- taglsm[x]
  
  dbi <- all_pts |>
    filter(tag.id == x) |> 
    filter(cluster_edit != 0)
  
  dbi_clusters <- dbi |> 
    group_by(cluster_edit) |> 
    summarise(n = n()) |> 
    filter(n > 5) # only keep clusters with more than 5 points
  
  if(nrow(dbi_clusters) < 1) {
    return(NULL) # skip if less than 10 points
  }
  
  # cycle through all the clusters and 
  clu_vers <- purrr::map(dbi_clusters$cluster_edit, function(xx){
    
   # xx <- dbi_clusters$cluster_edit[1]
    
    dbi_cl <- dbi |> filter(cluster_edit == xx)
    min_month <- min(dbi_cl$month)
    max_month <- max(dbi_cl$month)
    min_yr <- min(dbi_cl$year)
    max_yr <- max(dbi_cl$year)
    
     #  kde: h reference parameter
    #dbisf <- st_as_sf(dbi, coords = c("X", "Y"), crs = st_crs(pts))
    dbisf <- st_as_sf(dbi_cl, coords = c("X", "Y"), crs = 4087)
  
    dbisp <- dbisf |> 
      select(tag.id) |> 
      as("Spatial")
  
    # # define the parameters (h, kern, grid, extent) 
    kde_href  <- kernelUD(dbisp, h = "href", kern = c("bivnorm"), grid = 500, extent = 10)
  
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
    
    ver65_sf <- tryCatch({
      ver65 <- getverticeshr(kde_href,65)
      st_as_sf(ver65 )|> 
        mutate(th = 65)
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
    kdelist <- list(ver95_sf, ver75_sf, ver65_sf, ver50_sf)
    allvers <- kdelist |>  purrr::discard(is.null) |> bind_rows()
    allvers$cluster = xx
    allvers$min_month = min_month
    allvers$max_month = max_month
    allvers$min_yr =  min_yr
    allvers$max_yr = max_yr
    
    return(allvers)
    
  }) |> bind_rows()

  #clu_vers

  clu_vers <- st_transform(clu_vers, crs = 4087)
  clu_vers

}) |> bind_rows()
  
  

plot(all_poly)

all_poly_wgs <- st_transform(all_poly, crs = 4326)

# updated to new name already 
st_write(all_poly_wgs, path("03_kde", "tagid_cluster_polygons_20260525.gpkg"), append = FALSE)







# Review the first pass to identify where the polygon includes multiple months or multiple years
# then output a new file and update the pt file to split out multi years (or remove polygons where the end time point is unclear)
#all_polys <- all_poly_wgs

all_polys <- st_read(path("03_kde", "tagid_cluster_polygons_20260525.gpkg"))

ap <- all_polys |> 
  filter(th == 50) |> 
  mutate(multi_month = ifelse(min_month != max_month, "yes", "no"),
         multi_year = ifelse(min_yr != max_yr, "yes", "no")) |> 
  mutate(check = case_when(
    multi_year == "yes" & min_month ==1 & max_month == 12 ~ "no",
    multi_year == "no" & multi_month == "no" ~ "no")) |> 
  mutate(check = ifelse(is.na(check) & multi_month == "yes" & abs(max_month - min_month) <= 1, "no", check)) |> 
  mutate(check = ifelse(is.na(check), "yes", check)) 
 
 
st_write(ap, path("03_kde", "tagid_cluster_polygons50_20260525.gpkg"), append = FALSE)



# check ids of the points with flagged locations 

cc <- ap |> filter(check == "yes") 

cid <- cc |> pull(id)

# Edited 
# "201139" ,"228166", "228184", "229364", "232984"
# "232985"                      - removed the second and third year 
# "238555", "238561","238576"
# "240164", "240172"
# "241167", "242574" - delete multi yr
# "242656", "242657"
# "242658", "260689", "260691", "260692",  "260694", "260817"
# "260697", "260803", "260805", "260807", "260812","260816", "262989","280809"
#"282294", "282296" "282299" "282312"

# not changed 
# "201143" - breeding grounds - likley dropped 
# "201146" - breeding grounds - likley dropped 
# "201165", "213828",  "213830", "213841", "230301",  "230308","230312"
# "230314", "230318",  "232601"
# "232345", ""232351" ,  "232353"- "239408"  "239409"  "239411"  - Mingagn 
# "239412" "239413""239419" "239420" "239422" "239423" "239424" "239425" - Mingagn 
# "232982", "234236",  "234370", "234381", "238544"
# "238582" "240159","261445" likley dropped
# "240156""240168", "240174" , "261451"
# "242583", "255007",  "260693" ,"260695",  "260696", "260699", "261448"
# "260811", "260817", "261373","261435","261436","261438", "261441","261446"
# "261453" ,"262940","262941" "262946", "280807", "282283"
#"280811""280812" "280813","281662","281663" ,"281664", "282285"
#  "282286","282287","282288","282291","282292", "282295" "282297"
#"282306",  "282309" "282310" "282312" "285995" "285996"


# To check 
#260692, 241167 

dont_change <- c("201143" ,"201146", "201165" ,"213828", "213830", "213841", "230301",
                 "230308", "230312", "230314", "230318", "232345","232351", "232353",
                 "232601",  "232982",  "234236",  "234370", "234381","238544",
                 "238582", "239408", "239409", "239411","239412", "239413", "239419",
                 "239420", "239422", "239423", "239424", "239425", "240156", "240159",
                 "240168","240174", "242583","255007", "260693","260695", "260696",
                 "260699","260811", "261373","261435", "261436", "261438", "261441",
                "261445", "261446", "261448", "261451", "261453", "262940", "262941",
                "262946", "280807" ,"280811", "280812", "280813", "281662", "281664", 
                "282283", "282285", "282286", "282287","282288", "282291", "282292", 
                "282295", "282297","282306","282309", "282310", "282312", "285995",
                "285996",
                "242656", "242657",  "260691","260697","260805", "260812","260816", 
                "260817",  "280809", "242574", "240172")

