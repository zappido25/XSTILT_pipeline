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

cities_file <- file.path("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Cities_input_files/CONUS_cities_list.txt")
cities <- read.table(cities_file, header = FALSE, skip = 1, stringsAsFactors = FALSE)[, 1]

# Read in cities file and prompt user to select city
# cities_file <- file.path("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Cities_input_files/simulated_cities.txt")
# cities <- read.table(cities_file, header = FALSE, skip=1, stringsAsFactors = FALSE)[,1]

urban_core<-read.table("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Post-processing/post_process_outfiles/urban_core_info.txt", header=T, stringsAsFactors = F)
# cluster code for this city 

# all clusters - subset to this cluster
cluster<-readRDS("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/possible_clusters_07212022.rds")
## get geographic extent and city emissions for city clustering method 


# pdf_filename_emi_conv <- paste0("sanity_check_figures/", city, "_C40_Umich_shp.pdf")
pdf_filename_emi_conv <- paste0("sanity_check_figures/",  "cities_C40_UScensus_cc_ghs.pdf")
pdf(pdf_filename_emi_conv, width = 8, height = 6)

C40_path <- "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/C40_shapefiles/C40_boundaries_shp"
GHS_tif <- "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GHS_C40/GHS_BUILT_S_E2025_GLOBE_R2023A_4326_3ss_V1_0.tif"

# read the xlxs file form anh
anh_etal_file <- "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/C40_shapefiles/Ahn_etal_2025.xlsx"
df_anh=read_excel(anh_etal_file, sheet=1, skip=14)


# 1) READ
for (city in cities) {
  # C40_shp   <- st_read(file.path(C40_path, paste0(city, ".shp")), quiet = TRUE)
  print(paste0("city is :",city))

 
  if (city=="NewYork") {
    C40_file <- file.path(C40_path, paste0("New York City.shp"))
  }else {
    C40_file <- file.path(C40_path, paste0(city, ".shp"))
  }

  if (!file.exists(C40_file)) {
    cat("C40 shapefile does not exist for", city, "- skipping.\n")
    next
  }
  C40_shp <- st_read(C40_file, quiet = TRUE)
  # IMPORTANT: your helper is named hp.urban.coords() in the code you sent,
  # but in your main script you call shp.urban.coords(). Use the right one:
  Umich_shp <- shp.urban.coords(city)   # <- fix the function name here
  
  # get bounding boxes
  Umich_bb= get_urban_extent(city)

  xmin_Umich <- Umich_bb$lon[1]
  xmax_Umich <- Umich_bb$lon[2]
  ymin_Umich <- Umich_bb$lat[1]
  ymax_Umich <- Umich_bb$lat[2]
  #get bounding box for c40
  xmin_C40 <- df_anh$lon_min_c40[df_anh$TargetName==city]
  xmax_C40 <- df_anh$lon_max_c40[df_anh$TargetName==city]
  ymin_C40 <- df_anh$lat_min_c40[df_anh$TargetName==city]
  ymax_C40 <- df_anh$lat_max_c40[df_anh$TargetName==city]
  #get bounding box for ghs
  xmin_ghs <- df_anh$lon_min_ghs[df_anh$TargetName==city]
  xmax_ghs <- df_anh$lon_max_ghs[df_anh$TargetName==city]
  ymin_ghs <- df_anh$lat_min_ghs[df_anh$TargetName==city]
  ymax_ghs <- df_anh$lat_max_ghs[df_anh$TargetName==city]

 
  # read.ghs.tif(GHS_tif,city)
  # stop("test")  

  cluster_code<-urban_core$ID[urban_core$city == city]
  cluster_val<-cluster[cluster$cluster_id == cluster_code, ] 

  # print(cluster_code)
  # print(paste0(min(cluster_val$long), max(cluster_val$long), min(cluster_val$lat),max(cluster_val$lat)) )


  city_points <- SpatialPointsDataFrame(coords = cluster_val[,1:2], data = cluster_val[,1:2],
                                            proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))

  city_extent <- extent(city_points)
  xmin_cc <- city_extent@xmin
  xmax_cc <- city_extent@xmax
  ymin_cc <- city_extent@ymin
  ymax_cc <- city_extent@ymax

  # 2) ASSIGN CRS IF MISSING (C40); Umich comes from NAD83 per your printout
  if (is.na(st_crs(C40_shp))) {
    prj_file <- file.path(C40_path, paste0(city, ".prj"))
    if (file.exists(prj_file)) {
      wkt <- paste(readLines(prj_file, warn = FALSE), collapse = " ")
      st_crs(C40_shp) <- wkt
    } else {
      # Heuristic: in degrees? Use NAD83 to match Census layers
      bb <- st_bbox(C40_shp)
      looks_deg <- all(bb[c("xmin","xmax")] >= -180 & bb[c("xmin","xmax")] <= 180) &&
                  all(bb[c("ymin","ymax")] >=  -90 & bb[c("ymin","ymax")] <=  90)
      if (looks_deg) st_crs(C40_shp) <- 4269
    }
  }
  if (is.na(st_crs(Umich_shp))) st_crs(Umich_shp) <- 4269  # your print showed NAD83

  # 3) VALIDATE + TRANSFORM BOTH TO WGS84
  C40_shp   <- st_make_valid(C40_shp)   |> st_transform(4326)
  Umich_shp <- st_make_valid(Umich_shp) |> st_transform(4326)

  # 4) DIAGNOSTICS
  cat("C40:", nrow(C40_shp), "rows; empty geometries:", sum(st_is_empty(C40_shp)), "\n")
  cat("US census:", nrow(Umich_shp), "rows; empty geometries:", sum(st_is_empty(Umich_shp)), "\n")



  # make sure both layers are valid and in EPSG:4326 already
  sfc_combined <- c(st_geometry(C40_shp), st_geometry(Umich_shp))   # combine sfc objects
  geom_union   <- st_union(st_sfc(sfc_combined, crs = 4326))        # union
  bb <- st_bbox(geom_union)

  padx <- 0.02; pady <- 0.02
  xlim <- c(bb["xmin"] - padx, bb["xmax"] + padx)
  ylim <- c(bb["ymin"] - pady, bb["ymax"] + pady)

   diff_rects <- data.frame(
    xmin = c(xmin_Umich, xmin_C40,xmin_ghs, xmin_cc),
    xmax = c(xmax_Umich, xmax_C40,xmax_ghs, xmax_cc),
    ymin = c(ymin_Umich, ymin_C40,ymin_ghs, ymin_cc),
    ymax = c(ymax_Umich, ymax_C40,ymax_ghs, ymax_cc),
    method = c("US-Census","C40", "ghs","City Clustering")
)

  bbox <- c(left = as.numeric(xlim[1]-0.25), bottom = as.numeric(ylim[1]-0.25), right = as.numeric(xlim[2]+0.25), top = as.numeric(ylim[2]+0.25))
  # bbox <- c(left = as.numeric(min(diff_rects$xmin)), bottom = as.numeric(min(diff_rects$ymin)), right = as.numeric(max(diff_rects$xmax)),
  #            top = as.numeric(max(diff_rects$ymax)))

  print(bbox)
  print(diff_rects)

  basemap <- get_stadiamap(
      bbox = bbox,
      zoom = 10,
      maptype = "stamen_terrain"#"stamen_terrain"  # Options: stamen_terrain, stamen_toner, stamen_watercolor
  ) 


  # # Make a concise table (rounded coords)
  # tbl <- diff_rects %>%
  #   mutate(across(c(xmin, xmax, ymin, ymax), ~ round(.x, 2))) %>%
  #   dplyr::select(Method = method, xmin, xmax, ymin, ymax)

  # grob_tbl <- tableGrob(tbl, rows = NULL, theme = ttheme_minimal(
  #   core = list(fg_params = list(cex = 0.7))
  # ))



  # p2 = ggmap(basemap) +
  #   geom_sf(data = C40_shp, aes(color = "C40"), fill = NA, linewidth = 0.8, inherit.aes = FALSE) +
  #   geom_sf(data = Umich_shp, aes(color = "US-Census "), fill = NA, linewidth = 0.8, inherit.aes = FALSE) +
  #   # scale_color_manual(
  #   #   name = "Shapefile Source",
  #   #   values = c("C40" = "#E4572E", "Umich" = "#3B7DDD"),
  #   #   labels = c("C40", "Umich")
  #   # ) +
  #   geom_rect(
  #         data = diff_rects,
  #         # aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, color = method),
  #         aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, color = method ),
  #           inherit.aes = FALSE,
  #         fill = NA, linetype = "dashed", linewidth = 1.0
  #       ) +
  #         scale_color_manual(
  #         name = "Boundary Source",
  #         values = c(
  #           "C40" = "#E4572E",
  #           "US-Census" = "#0ae8a5ff",
  #           "US-census" = "#0ae8a5ff",
  #           "ghs" = "#0a0501ff",
  #           "City Clustering" = "#1d0fedff"
  #   ),
  #   breaks = c("C40", "US-census", "US-census", "ghs", "City Clustering"))+
  #   labs(title = paste(city, "- C40 vs Umich (WGS84)"),
  #       x = NULL, y = NULL) +
  #   theme_minimal(base_size = 12) +
  #   guides(color = guide_legend(override.aes = list(fill = NA, linewidth = 2)))

  #   p2 <- p2 + 
  # annotation_custom(
  #   grob = grob_tbl,
  #   xmin = -Inf, xmax = -Inf, ymin = -Inf, ymax = Inf
  # )
  # print(p2)


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
  # --- C40 boundary (orange-red) ---
  geom_sf(data = C40_shp, aes(color = "C40"), fill = NA,
          linewidth = 0.8, inherit.aes = FALSE) +
  
  # --- Umich / US Census boundary (green) ---
  geom_sf(data = Umich_shp, aes(color = "US-Census"), fill = NA,
          linewidth = 0.8, inherit.aes = FALSE) +
  
  # --- Difference rectangles (dashed outlines) ---
  geom_rect(
    data = diff_rects,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, color = method),
    inherit.aes = FALSE,
    fill = NA, linetype = "dashed", linewidth = 1.0
  ) +
  
  # --- Color legend ---
  scale_color_manual(
    name   = "Boundary Source",
    values = c(
      "C40"            = "#E4572E",   # orange-red
      "US-Census"      = "#009E73",   # bright green for Umich
      "US-Census"      = "#009E73",   # bright green for Umich
      "ghs"            = "#0a0501ff",
      "City Clustering"= "#1d0fedff"
    ),
    breaks = c("C40", "US Census","US-Census", "ghs", "City Clustering")
  ) +
  
  labs(
    title = paste(city, "- C40 vs US Census (WGS84)"),
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