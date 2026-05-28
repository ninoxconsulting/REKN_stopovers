
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

pts <- st_transform(pts, crs = 4087)



## read in ref data  

#data_dir <- "../../../Users/genev/"
data_dir <- "C:\\Users\\genev\\OneDrive\\Documents\\02.Contracts\\2022_NWRC\\02_data\\REKN_gps\\output_final\\draft_outputs_2026\\"
list.files(data_dir)

#ref <- read.csv(fs::path(data_dir,"reference_data_2020_2025_20260124.csv"))
ts <- read.csv(fs::path(data_dir,"final_tags_list_edited_20260126.csv"))


# Review the first pass to identify where the polygon includes multiple months or multiple years
# then output a new file and update the pt file to split out multi years (or remove polygons where the end time point is unclear)
#all_polys <- all_poly_wgs

all_polys <- st_read(path("03_kde", "tagid_cluster_polygons_20260525.gpkg"))

# check which projects these belong to 

# summarise all the polygons
summary_db <- all_polys |> 
  select(id) |> 
  st_drop_geometry() |> 
  unique() 
  
tss <- ts |> 
  filter(tag.id  %in% summary_db$id) |> 
  select(tag.id,  proj) |> 
  group_by(proj) |>
  summarise(n_tags = n_distinct(tag.id))
  


## All polys 

# Geographic distributon of tags
world <- ne_countries(scale = "medium", returnclass = "sf")

Americas <- world %>% 
  dplyr::filter(region_un == "Americas")%>% 
  select(admin)

# # entire north America 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = all_polys, size = 1.2, alpha = 0.1, color = "dark blue") +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-130, -30), ylim = c(-60, 80), expand = FALSE)+
  scale_colour_viridis_d(begin = 0.2, end = 0.7) +
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        legend.position = "none")

global



## All polys by month 
th50 <- all_polys |> filter(th == 50)

# # entire north America 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = th50, size = 1.2, alpha = 0.1, color = "dark blue") +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-130, -30), ylim = c(-60, 80), expand = FALSE)+
  scale_colour_viridis_d(begin = 0.2, end = 0.7) +
  facet_wrap(~min_month)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        legend.position = "none")

global





# summarise all the polygons
summary_db <- all_polys |> 
  st_drop_geometry() |> 
  group_by(id) |> 
  summarise(n_months = n_distinct(min_month),
            n_years = n_distinct(min_yr)) 


# 274 individuals 
# summary of how many polygons per id
summary_db2 <- summary_db |> 
  st_drop_geometry() |> 
  group_by(n_months, n_years) |> 
  summarise(n_ids = n_distinct(id))

#plot summary by no of ids
summary_db2 |> 
  ggplot(aes(summary_db2, x = n_months, y = n_ids)) +
  geom_col(position = "dodge") +
  theme_bw() +
  #facet_grid(n_years ~ .) +
  xlab("Number of stopovers") +
  ylab("Number of individuals") +
  ggtitle("Summary of number of stopovers")


# plot summary by month 
summary_month <- all_polys |> 
  st_drop_geometry() |> 
  group_by(min_month,min_yr) |> 
  summarise(n_ids = n_distinct(id))
            

#plot summary by no of ids
summary_month |> 
  ggplot(aes(summary_month, x = as.factor(min_month), y = n_ids)) +
  geom_col(position = "dodge") +
  theme_bw() +
  facet_grid(min_yr ~ .) +
  xlab("Month in which stopover began") +
  ylab("Number of individuals") +
  ggtitle("Summary of stopovers per month")



# rough cutoff for latitude 
## breeding areas above 
## wintering areas




## note we can filter this down to 50% percentage 

all_poly_50 <- all_polys |> filter(th == 50)

# ggplot 

global_href <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  #geom_sf(data = dbsf,  size = 1, alpha = 0.2,colour = "blue") +
  geom_sf(data = pts,  size = 1, alpha = 0.2,colour = "lightblue") +
  #geom_sf(data = all_poly_50, linewidth = 0.5, alpha = 0.4, fill = "red")+
  geom_sf(data = all_poly_50,  alpha = 0.4,fill = "red")+
  xlab("Longitude") + ylab("Latitude") +
  #coord_sf(xlim = c(-75, -74.35), ylim = c(38.8, 39.4), expand = FALSE)+
  coord_sf(xlim = c(-76, -74), ylim = c(38, 40), expand = FALSE)+
  theme_bw()+
  facet_wrap(~min_month)+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global_href




###############################################
## Density based calculations

#################################################
# review the density based on hex grid 

kde  <- all_polys 
kde <- st_transform(kde,  crs = 4087)

# 25km Build a 25km hex grid over the extent of the KDE polygons, and calculate the number of KDE polygons that overlap with each hexagon.
hex <- st_make_grid(
  kde,
  cellsize = 25000,
  square = FALSE
)

hex <- st_sf(id = 1:length(hex), geometry = hex)
ints <- st_intersects(hex, kde)

hex$n_overlap <- lengths(ints)
kde$weight <- case_when(
  kde$th == 50 ~ 1,
  kde$th == 75 ~ 0.5,
  kde$th == 95 ~ 0.25
)
hex$ud <- sapply(ints, function(i) {
  sum(kde$weight[i], na.rm = TRUE)
})

st_write(hex, path("03_kde", "hex_all_polywt_20260527.gpkg"), append = FALSE)



library(sf)
library(dplyr)
library(purrr)
library(tidyr)

kde  <- all_polys 
kde <- st_transform(kde, crs = 4087)

# -------------------------------------------------------------------
# Create 25 km hex grid ONCE
# -------------------------------------------------------------------
hex <- st_make_grid(
  kde,
  cellsize = 25000,
  square = FALSE
)

hex <- st_sf(
  hex_id = 1:length(hex),
  geometry = hex
)

# -------------------------------------------------------------------
# Weight KDE levels
# -------------------------------------------------------------------

kde <- kde %>%
  mutate(
    weight = case_when(
      th == 50 ~ 1,
      th == 75 ~ 0.5,
      th == 95 ~ 0.25,
      TRUE ~ 0
    )
  )

# -------------------------------------------------------------------
# Expand rows by month presence
# Handles wraparound automatically (e.g. Nov-Feb)
# -------------------------------------------------------------------

kde_month <- kde %>%
  rowwise() %>%
  mutate(
    month = list(
      if (min_month <= max_month) {
        seq(min_month, max_month)
      } else {
        c(seq(min_month, 12), seq(1, max_month))
      }
    )
  ) %>%
  unnest(month) %>%
  ungroup()



# -------------------------------------------------------------------
# Calculate overlap density for each month - long format
# -------------------------------------------------------------------

monthly_hex <- map_dfr(1:12, function(m) {
  
  message("Processing month: ", m)
  
  # subset KDEs present in month
  kde_sub <- kde_month %>%
    filter(month == m)
  
  # skip empty months
  if (nrow(kde_sub) == 0) return(NULL)
  
  # intersections
  ints <- st_intersects(hex, kde_sub)
  
  # copy hex
  h <- hex
  
  # number overlapping
  h$n_overlap <- lengths(ints)
  
  # weighted utilization density
  h$ud <- sapply(ints, function(i) {
    sum(kde_sub$weight[i], na.rm = TRUE)
  })
  
  # add month
  h$month <- m
  
  h
})

st_write(monthly_hex, path("03_kde", "hex_all_monthly_long_20260527.gpkg"), append = FALSE)



# -------------------------------------------------------------------
# Calculate overlap density for each month - wide 
# -------------------------------------------------------------------

monthly_list <- map(1:12, function(m) {
  
  message("Processing month: ", m)
  
  kde_sub <- kde_month %>%
    filter(month == m)
  
  ints <- st_intersects(hex, kde_sub)
  
  tibble(
    hex_id = hex$hex_id,
    
    !!paste0("n_", m) := lengths(ints),
    
    !!paste0("ud_", m) := sapply(ints, function(i) {
      sum(kde_sub$weight[i], na.rm = TRUE)
    })
  )
})


monthly_hex <- reduce(
  monthly_list,
  left_join,
  by = "hex_id"
) %>%
  right_join(hex, by = "hex_id") %>%
  st_as_sf()


monthly_hex
st_write(monthly_hex, path("03_kde", "hex_all_monthly_wide_20260527.gpkg"), append = FALSE)



######################################################################
## plot the densities per month 

monthly_hex <- st_read(path("03_kde", "hex_all_monthly_long_20260527.gpkg"))

# intersect with aoi for testing 

# -------------------------------------------------------------------
# Build AOI from X/Y coordinate limits
# -------------------------------------------------------------------

del_birds <- st_read(path("01_raw_data", "Del_birds_polygons.gpkg"))
aoi <- st_as_sfc(
  st_bbox(del_birds),
    crs = st_crs(monthly_hex)
  )
aoi <- st_buffer(aoi, 500000)

# filter to AOI area 
hex <- monthly_hex[lengths(st_intersects(monthly_hex, aoi)) > 0,]
hex$n_overlap[hex$n_overlap == 0] <- NA


# delaware region 
p1<- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = hex, aes(fill = n_overlap, alpha = 0.2), color = NA) +
  xlab("Longitude") + ylab("Latitude") +
 # coord_sf(xlim = c(-130, -30), ylim = c(-60, 80), expand = FALSE)+
  coord_sf(xlim = c(-78, -72), ylim = c(36, 41), expand = FALSE)+
  scale_fill_viridis_c(na.value = "transparent") +
  facet_wrap(~month)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        legend.position = "none")

p1


# Based on the entire extent.

# convert to NA
monthly_hex$n_overlap[monthly_hex$n_overlap == 0] <- NA


# delaware region 
p2 <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = monthly_hex, aes(fill = n_overlap, alpha = 0.2), color = NA) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-130, -30), ylim = c(-60, 80), expand = FALSE)+
  #coord_sf(xlim = c(-78, -72), ylim = c(36, 41), expand = FALSE)+
  scale_fill_viridis_c(na.value = "transparent") +
  facet_wrap(~month)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        legend.position = "none")

p2
