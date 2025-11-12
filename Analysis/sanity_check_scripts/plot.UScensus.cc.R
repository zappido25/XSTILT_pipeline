# -- core processing pipeline --
# #comapre c40 and Umich shpfiles



library(ggplot2)
library(dplyr)
library(ggmap)
library(raster)
library(sf)
library(ggnewscale)
library(patchwork)
library(tidyr)
library(ncdf4)
library(ggridges)
library(ggrepel) 
library(lwgeom)   # st_make_valid()
library(readxl)
library(gridExtra)
library(grid)

api.key = readLines('../../insert_ggAPI.csv')
register_google(key = api.key)
stadia.api.key = readLines('../../insert_stadia_key.csv')
register_stadiamaps(key = stadia.api.key)

functions_files <- list.files('/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Analysis/functions/', full.names = TRUE, pattern = "\\.r$")
invisible(lapply(functions_files, source))
# print(functions_files)

# cities_file <- file.path("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Cities_input_files/CONUS_cities_list.txt")
# cities <- read.table(cities_file, header = FALSE, skip = 1, stringsAsFactors = FALSE)[, 1]

# Read in cities file and prompt user to select city
cities_file <- file.path("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Cities_input_files/simulated_cities.txt")
cities <- read.table(cities_file, header = FALSE, skip=1, stringsAsFactors = FALSE)[,1]

urban_core<-read.table("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Cities_input_files/urban_core_info.txt", header=T, stringsAsFactors = F)
# cluster code for this city 

# all clusters - subset to this cluster
cluster<-readRDS("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/possible_clusters_07212022.rds")
## get geographic extent and city emissions for city clustering method 


# pdf_filename_emi_conv <- paste0("sanity_check_figures/", city, "_C40_Umich_shp.pdf")
pdf_filename_emi_conv <- paste0("sanity_check_figures/",  "cities_UScensus_cc.pdf")
pdf(pdf_filename_emi_conv, width = 8, height = 6)


# 1) READ
# cities="Baltimore"
for (city in cities) {
  # C40_shp   <- st_read(file.path(C40_path, paste0(city, ".shp")), quiet = TRUE)
  print(paste0("city is :",city))

 
  Umich_shp <- shp.urban.coords(city)   # <- fix the function name here
  
  # get bounding boxes
  Umich_bb= get_urban_extent(city)

  xmin_Umich <- Umich_bb$lon[1]
  xmax_Umich <- Umich_bb$lon[2]
  ymin_Umich <- Umich_bb$lat[1]
  ymax_Umich <- Umich_bb$lat[2]
  #get bounding box for c40
 
  # read.ghs.tif(GHS_tif,city)
  # stop("test")  

  cluster_code<-urban_core$ID[urban_core$city == city]
  cluster_val<-cluster[cluster$cluster_id == cluster_code, ] 
  # print(urban_core$city)
  
  city_points <- SpatialPointsDataFrame(coords = cluster_val[,1:2], data = cluster_val[,1:2],
                                            proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))

  city_extent <- extent(city_points)
  xmin_cc <- city_extent@xmin
  xmax_cc <- city_extent@xmax
  ymin_cc <- city_extent@ymin
  ymax_cc <- city_extent@ymax

  # 2) ASSIGN CRS IF MISSING (C40); Umich comes from NAD83 per your printout
  
  if (is.na(st_crs(Umich_shp))) st_crs(Umich_shp) <- 4269  # your print showed NAD83

  # 3) VALIDATE + TRANSFORM BOTH TO WGS84
  
  Umich_shp <- st_make_valid(Umich_shp) |> st_transform(4326)

  # 4) DIAGNOSTICS
  
  cat("US census:", nrow(Umich_shp), "rows; empty geometries:", sum(st_is_empty(Umich_shp)), "\n")



  # make sure both layers are valid and in EPSG:4326 already
  bb <- st_bbox(st_union(Umich_shp))
  padx <- 0.02; pady <- 0.02
  xlim <- c(bb["xmin"] - padx, bb["xmax"] + padx)
  ylim <- c(bb["ymin"] - pady, bb["ymax"] + pady)

   diff_rects <- data.frame(
    xmin = c(xmin_Umich,  xmin_cc),
    xmax = c(xmax_Umich,  xmax_cc),
    ymin = c(ymin_Umich,  ymin_cc),
    ymax = c(ymax_Umich,  ymax_cc),
    method = c("US-Census","City Clustering")
)

# center_lon=urban_core$lon[urban_core$city == city]
# center_lat=urban_core$lat[urban_core$city == city]
  bbox <- c(left = as.numeric(xlim[1]-0.25), bottom = as.numeric(ylim[1]-0.25), right = as.numeric(xlim[2]+0.25), top = as.numeric(ylim[2]+0.25))
  
# bbox <- c(
#   left   = center_lon - 0.5,
#   bottom = center_lat - 0.5,
#   right  = center_lon + 0.5,
#   top    = center_lat + 0.5
# )
#   print(bbox)
  print(diff_rects)

  basemap <- get_stadiamap(
      bbox = bbox,
      zoom = 10,
      maptype = "stamen_terrain"#"stamen_terrain"  # Options: stamen_terrain, stamen_toner, stamen_watercolor
  ) 

# --- assume you already have: basemap, C40_shp, Umich_shp, diff_rects, city ---

# 1️⃣ Convert to tibble (to avoid dplyr::select errors)
diff_rects <- as_tibble(diff_rects)

# 2️⃣ Build coordinate summary table
tbl <- diff_rects %>%
  mutate(across(c(xmin, xmax, ymin, ymax), ~ round(.x, 3))) %>%
  dplyr::select(Method = method, xmin, xmax, ymin, ymax)

# 3️⃣ Convert table to a grob for plotting
tbl_grob <- gridExtra::tableGrob(
  tbl,
  rows = NULL,
  theme = gridExtra::ttheme_minimal(
    core = list(fg_params = list(cex = 0.7)),
    colhead = list(fg_params = list(cex = 0.8, fontface = "bold"))
  )
)

# 4️⃣ Build the map
p2 <- ggmap(basemap) +
   geom_sf(data = Umich_shp, aes(color = "US-Census"), fill = NA, linewidth = 0.8, inherit.aes = FALSE) +
   geom_rect(
    data = diff_rects,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, color = method),
    inherit.aes = FALSE,
    fill = NA, linetype = "dashed", linewidth = 1.0
  ) +
  scale_color_manual(
    name = "Boundary Source",
    values = c(
      "US-Census" = "#E4572E",
      "US-Census" = "#E4572E",
      "City Clustering" = "#1d0fedff"
    ),
    breaks = c("US-Census",  "City Clustering")
  ) +
  labs(
    title = paste(city, ":: US-Census vs city clustering"),
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 12) +
  guides(color = guide_legend(override.aes = list(fill = NA, linewidth = 2)))

# 5️⃣ Combine: map on top, table below
final_plot <- gridExtra::grid.arrange(
  p2,
  tbl_grob,
  ncol = 1,
  heights = c(4, 1.5)  # map taller than table
)

# Display or save
print(final_plot)
# ggsave("city_boundaries_with_table_below.png", final_plot, width = 8, height = 8, dpi = 300)

}


dev.off()