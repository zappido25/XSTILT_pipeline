library(tidygeocoder)
library(ggplot2)
library(maps)
library(dplyr)
library(tidyr)
library(sf)
library(lubridate)
library(scales)
 
library(FSA)   # For Dunn test
library(rstatix)
library(multcompView)
# library(rlang)
# # --- Read cities ---
# cities_file <- file.path("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Cities_input_files/simulated_cities.txt")
# cities <- read.table(cities_file, header = FALSE, skip = 1, stringsAsFactors = FALSE)[,1]
remove_outliers <- function(df) {
  df <- df %>%
    mutate(Epc = as.numeric(Epc)) %>%
    filter(!is.na(Epc))
  med <- median(df$Epc, na.rm = TRUE)
  s <- sd(df$Epc, na.rm = TRUE)
  df %>% filter(Epc >= (med - 2 * s) & Epc <= (med + 2 * s))
  
}

plot.us.maps=function(cities, plot_name="../figures/conus_cities.png") {

     
    cities <- sub("^NewYork$", "New York", cities)
    cities <- sub("^LosAngeles$", "Los Angeles", cities)
    cities <- sub("^SanDiego$", "San Diego", cities)
   

    df_cities <- tibble::tibble(city = cities,
                                query = paste0(city, ", USA"))

    # print(df_cities)
    # --- Geocode in the US only ---
    geo <- df_cities %>%
    tidygeocoder::geocode(address = query, method = "osm", lat = lat, long = lon,
            limit = 1,
            custom_query = list(countrycodes = "us"))

    # flag any odd results (outside CONUS bbox)
    geo_bad <- geo %>% filter(lon < -130 | lon > -60 | lat < 22 | lat > 52)
    if (nrow(geo_bad)) {
    message("These geocodes look outside CONUS:\n",
            paste(geo_bad$city, sprintf("(%.2f, %.2f)", geo_bad$lat, geo_bad$lon),
                    collapse = "\n"))
    }
    geo <- geo %>% filter(!is.na(lat), !is.na(lon))
#     print(geo)    
    # --- US states map (drop AK/HI) ---
    us_map <- map_data("state") %>%
    filter(!region %in% c("alaska", "hawaii"))

    # --- Plot (CONUS only) ---
    p <- ggplot() +
    geom_polygon(data = us_map, aes(x = long, y = lat, group = group),
                fill = "grey90", color = "white") +
    geom_point(data = geo, aes(x = lon, y = lat),
                color = "red", size = 3) +
    geom_text(data = geo, aes(x = lon, y = lat, label = city),
                vjust = -1, size = 3) +
    coord_quickmap(xlim = c(-125, -66), ylim = c(24, 50)) +  # single coord, keeps CONUS
    theme_bw(base_size = 12) +
    labs(title = "Selected Cities in Contiguous US (CONUS)", x = NULL, y = NULL)

    p
    # ggsave(plot_name, plot = p, width = 8, height = 6, dpi = 300)
}

plot.us.maps_all_cities <- function(cities_subset, cities_all,
                                    plot_name = "../figures/conus_cities.png") {
  library(ggplot2)
  library(dplyr)
  library(tidygeocoder)
  library(maps)
  library(ggrepel)

  # --- Normalize names ---
  fix_city_names <- function(x) {
    x <- sub("^NewYork$", "New York", x)
    x <- sub("^LosAngeles$", "Los Angeles", x)
    x <- sub("^SanDiego$", "San Diego", x)
    x <- sub("^Eastern_Florida$", "Eastern Florida", x)
    x <- sub("^Dallas_FortWorth$", "Dallas-Fort Worth", x)
    x <- sub("^SanFransico_SanJose$", "San Francisco - San Jose", x)
    x <- sub("^LasVegas$", "Las Vegas", x)
    x <- sub("^SanAntonio$", "San Antonio", x)
    x <- sub("^SaltLakeCity$", "Salt Lake City", x)
    x
  }
  cities_all    <- fix_city_names(cities_all)
  cities_subset <- fix_city_names(cities_subset)

  # --- Common cities (to be highlighted) ---
  common_cities <- intersect(cities_all, cities_subset)

  # --- Build DF and geocode (note: address = "query" as a string) ---
  df_all <- tibble::tibble(city = cities_all,
                           query = paste0(city, ", USA"))

  df_all <- df_all %>%
    tidygeocoder::geocode(
      address = "query",           # <- use string column name for compatibility
      method  = "osm",
      lat     = "lat",
      long    = "lon",
      limit   = 1,
      custom_query = list(countrycodes = "us")
    )

  # Drop bad/missing geocodes and keep CONUS
  df_all <- df_all %>%
    filter(!is.na(lat), !is.na(lon)) %>%
    filter(dplyr::between(lon, -125, -66),
           dplyr::between(lat,  24,  50)) %>%
    mutate(color = ifelse(city %in% common_cities, "red", "grey50"))

  # --- CONUS basemap ---
  us_map <- map_data("state") %>%
    filter(!region %in% c("alaska", "hawaii"))

  # --- Plot ---
  p <- ggplot() +
    geom_polygon(data = us_map,
                 aes(x = long, y = lat, group = group),
                 fill = "grey95", color = "white") +
    geom_point(data = df_all,
               aes(x = lon, y = lat, color = color),
               size = 3) +
    # Label only the highlighted common cities
    ggrepel::geom_text_repel(
      data = df_all %>% filter(city %in% common_cities),
      aes(x = lon, y = lat, label = city),
      size = 3, color = "red", seed = 42, box.padding = 0.3, min.segment.length = 0
    ) +
    scale_color_identity() +
    coord_quickmap(xlim = c(-125, -66), ylim = c(24, 50)) +
    labs(title = "U.S. Cities: Simulated (Red) vs Planned (Grey)",
         x = NULL, y = NULL) +
    theme_bw(base_size = 12) +
    theme(panel.grid = element_blank(),
          plot.title = element_text(face = "bold", hjust = 0.5))

  # Save
  ggsave(plot_name, plot = p, width = 8, height = 6, dpi = 300)
  message("✅ Saved map to: ", plot_name)

  invisible(p)
}

# assign regions to cities based on lat/lon
auto_region <- function(cities, lat_col = "lat", lon_col = "long", coast_km = 120) {
        cities <- sub("^NewYork$", "New York", cities)
        cities <- sub("^LosAngeles$", "Los Angeles", cities)
        cities <- sub("^SanDiego$", "San Diego", cities)

        df_cities <- tibble::tibble(city = cities) %>%
                        tidygeocoder::geocode(city, method = "osm")

        # sanity: must have lat/long
        stopifnot(all(c(lat_col, lon_col) %in% names(df_cities)))

        # points sf
        # convert to spatial points
        pts <- sf::st_as_sf(df_cities, coords = c(lon_col, lat_col), crs = 4326, remove = FALSE)

        # try to get coastlines
        use_coast <- TRUE
        coast <- try(
        rnaturalearth::ne_download(scale = "medium", type = "coastline",
                                category = "physical", returnclass = "sf"),
        silent = TRUE
        )
        if (inherits(coast, "try-error")) use_coast <- FALSE

        if (use_coast) {
        # crop to rough Pacific/Atlantic windows (CONUS)
                pac_box <- sf::st_bbox(c(xmin = -130, ymin = 24, xmax = -115, ymax = 50), crs = sf::st_crs(4326))
                atl_box <- sf::st_bbox(c(xmin =  -85, ymin = 24, xmax =  -66, ymax = 50), crs = sf::st_crs(4326))
                coast_pac <- suppressWarnings(sf::st_crop(coast, pac_box))
                coast_atl <- suppressWarnings(sf::st_crop(coast, atl_box))

                # helper: rowwise min distance (meters); NA if no coast segs in crop
                rowmin_dist <- function(p, coast_sf) {
                if (is.null(coast_sf) || nrow(coast_sf) == 0) return(rep(NA_real_, nrow(p)))
                        m <- sf::st_distance(p, coast_sf)  # matrix: n_pts x n_segs
                        apply(m, 1, min)                   # min per point
                }

                d_pac <- rowmin_dist(pts, coast_pac)
                d_atl <- rowmin_dist(pts, coast_atl)

                near_pac <- !is.na(d_pac) & d_pac <= coast_km * 1000
                near_atl <- !is.na(d_atl) & d_atl <= coast_km * 1000

                df_cities$region <- dplyr::case_when(
                near_pac                         ~ "West Coast",
                near_atl                         ~ "East Coast",
                df_cities[[lat_col]] < 35        ~ "South",
                TRUE                              ~ "North"
                )
        } else {
        # fallback by lon/lat thresholds
                df_cities$region <- dplyr::case_when(
                df_cities[[lon_col]] <= -120     ~ "West Coast",
                df_cities[[lon_col]] >=  -80     ~ "East Coast",
                df_cities[[lat_col]]   <    35   ~ "South",
                TRUE                              ~ "North"
                )
        }

        df_cities
}

# --------------------------------------------------------------------
# NEW: auto_census_region() — classify cities by US Census Region/Division


# ======================================================================
# Use urban_core lat/lon to assign US Census Division & Region
# ======================================================================
# ======================================================================
# Assign US Census Division & Region based on authoritative urban_core
# ======================================================================
auto_us_region_from_urban_core <- function(cities, urban_core) {
  library(dplyr)
  library(sf)
  library(rnaturalearth)
  
  # normalize naming to match X-STILT conventions
  normalize_city <- function(x) {
    x <- trimws(x)
    x <- sub("^New York$", "NewYork", x)
    x <- sub("^Los Angeles$", "LosAngeles", x)
    x <- sub("^San Diego$", "SanDiego", x)
    x
  }
  
  cities_norm <- normalize_city(cities)

  # select lat/lon from urban_core
  uc <- urban_core %>%
    mutate(city = normalize_city(city)) %>%
    dplyr::select(city, lat, lon)

  df <- tibble::tibble(city = cities_norm) %>%
    left_join(uc, by = "city")

  if (any(is.na(df$lat) | is.na(df$lon))) {
    missing <- df$city[is.na(df$lat) | is.na(df$lon)]
    stop("Missing lat/lon in urban_core for: ", paste(missing, collapse = ", "))
  }

  pts <- st_as_sf(df, coords = c("lon","lat"), crs = 4326, remove = FALSE)

  # US states
  us_states <- rnaturalearth::ne_states(
    country = "United States of America", returnclass = "sf"
  ) %>% dplyr::select(state_name = name, state_abbr = postal)

  pts_state <- st_join(pts, us_states, join = st_within) %>%
    st_drop_geometry()

  # Census divisions map
  census_div <- tibble::tribble(
    ~division, ~state_abbr,
    "New England", "CT","New England","ME","New England","MA",
    "New England","NH","New England","RI","New England","VT",
    "Middle Atlantic", "NJ","Middle Atlantic","NY","Middle Atlantic","PA",
    "East North Central","IL","East North Central","IN","East North Central","MI",
    "East North Central","OH","East North Central","WI",
    "West North Central","IA","West North Central","KS","West North Central","MN",
    "West North Central","MO","West North Central","NE",
    "West North Central","ND","West North Central","SD",
    "South Atlantic","DE","South Atlantic","FL","South Atlantic","GA",
    "South Atlantic","MD","South Atlantic","NC","South Atlantic","SC",
    "South Atlantic","VA","South Atlantic","WV","South Atlantic","DC",
    "East South Central","AL","East South Central","KY","East South Central","MS","East South Central","TN",
    "West South Central","AR","West South Central","LA","West South Central","OK","West South Central","TX",
    "Mountain","AZ","Mountain","CO","Mountain","ID","Mountain","MT",
    "Mountain","NV","Mountain","NM","Mountain","UT","Mountain","WY",
    "Pacific","AK","Pacific","CA","Pacific","HI","Pacific","OR","Pacific","WA"
  )

  out <- pts_state %>%
    left_join(census_div, by = "state_abbr") %>%
    mutate(
      census_region = case_when(
        division %in% c("New England","Middle Atlantic") ~ "Northeast",
        division %in% c("East North Central","West North Central") ~ "Midwest",
        division %in% c("South Atlantic","East South Central","West South Central") ~ "South",
        division %in% c("Mountain","Pacific") ~ "West",
        TRUE ~ "Unknown"
      )
    ) %>%
    dplyr::select(city, lat, lon, state_name, state_abbr, division, census_region)

  return(out)
}


parse_times <- function(df) {
  if (!nrow(df)) return(df)
  df %>%
    dplyr::mutate(
      date_time = lubridate::parse_date_time(
        timestr,
        orders = c("%Y%m%d%H","%Y%m%d","%Y-%m-%d %H:%M:%S","%Y-%m-%d")
      ),
      year  = lubridate::year(date_time),
      month = lubridate::month(date_time),
      season = factor(dplyr::case_when(
        month %in% c(12,1,2)  ~ "DJF",
        month %in% c(3,4,5)   ~ "MAM",
        month %in% c(6,7,8)   ~ "JJA",
        month %in% c(9,10,11) ~ "SON"
      ), levels = c("DJF","MAM","JJA","SON"))
    )
}

# helper functions to get seasonal df: seasonal means + SD + N
seasonal_epc_us <- function(df) {

    df  %>%
    group_by(region, season) %>%
    summarise(
      epc_mean = mean(as.numeric(Epc), na.rm = TRUE),
      epc_sd   = sd(as.numeric(Epc),   na.rm = TRUE),
      n        = sum(!is.na(Epc)),
      .groups = "drop"
    ) 

}

# helper function that adds light/dark season based on month
# light: Mar–Aug; dark: Sep–Feb
light_dark_us = function(df) {
   df %>% mutate(
        date_time = as.Date(date_time),                 # or as.POSIXct if you prefer
        mo   = lubridate::month(date_time),
        light_dark = if_else(mo %in% 3:8, "Light (Mar–Aug)", "Dark (Sep–Feb)"),
        light_dark = factor(light_dark, levels = c("Light (Mar–Aug)", "Dark (Sep–Feb)"))
      )
}


##' 2x2 EPC summary plot by region and period
#' - One panel per region (2x2 grid for four regions)
#' - X: period (2015–2019, 2020, 2021–2024)
#' - Y: mean Epc
#' - Error bars: ±1 SD
#' - Labels: sample size (n) on each bar
#'
#' @param enh_df  Data frame with columns: region, Epc, timestr (or year), optional method
#' @param method  Optional filter for enh_df$method (e.g., "CC"); use NULL to include all
#' @param regions Optional character vector of exactly four region names (order = facet order).
#'                If NULL, uses the first four (sorted) found in the data.
#' @param bins_label_y  Numeric offset for count labels (in data units) above bar tops
#' @return ggplot object
plot_epc_by_region_period <- function(enh_df,
                                      method = NULL,
                                      value_col = c("Epc", "CI"),
                                      regions = NULL,
                                      label_n_offset = 0.1,
                                      scale_multiplier = NULL,
                                      title = NULL,
                                      y_label = NULL) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(ggplot2)
    library(FSA)
    library(multcompView)
  })

  stopifnot(is.data.frame(enh_df), "region" %in% names(enh_df))

  value_col <- value_col[1]
  if (!value_col %in% names(enh_df))
    stop("Column '", value_col, "' not found!")

  df <- enh_df

  if (!is.null(method) && "method" %in% names(df)) {
    df <- df %>% filter(.data$method == !!method)
  }

  if (!"year" %in% names(df)) {
    df <- df %>% 
      mutate(year = as.integer(substr(as.character(timestr), 1, 4)))
  }

  if (is.null(scale_multiplier)) {
    scale_multiplier <- if (value_col == "CI") 1e6 else 1
  }
  if (is.null(title)) {
    title <- if (value_col == "CI") "Carbon Intensity by Period (Mean ± SD)"
             else "Integrated EPC by Period (Mean ± SD)"
  }
  if (is.null(y_label)) {
    y_label <- if (value_col == "CI") "Carbon Intensity (kg CO2 / USD)"
               else "Integrated EPC (tg CO2 / person / year)"
  }

  df <- df %>%
    mutate(
      value_raw = as.numeric(.data[[value_col]]),
      value     = value_raw * scale_multiplier,
      period = case_when(
        year >= 2015 & year <= 2019 ~ "2015–2019",
        year >= 2020 & year <= 2021 ~ "2020–2021",
        year >= 2022 & year <= 2024 ~ "2022–2024",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(period), !is.na(value))

  avail_regions <- sort(unique(df$region))
  if (is.null(regions)) {
    regions <- avail_regions
  }
  df <- df %>% filter(region %in% regions)

  df <- df %>%
    mutate(
      region = factor(region, levels = regions),
      period = factor(period, levels = c("2015–2019", "2020–2021", "2022–2024"))
    )

  # ----- Stats + Letters per region -----
  sig_letters_list <- list()
  for (reg in unique(df$region)) {
    df_reg <- df %>% filter(region == reg)
    if (n_distinct(df_reg$period) <= 1) next
    
    kw <- kruskal.test(value ~ period, data = df_reg)
    cat("Kruskal–Wallis:", reg, "p=", kw$p.value, "\n")
    
    dunn <- FSA::dunnTest(value ~ period, data = df_reg, method = "bh")$res
    groups <- levels(df_reg$period)
    
    pv_mat <- matrix(1, nrow=length(groups), ncol=length(groups),
                     dimnames=list(groups, groups))
    
    for (i in seq_len(nrow(dunn))) {
      pair <- unlist(strsplit(as.character(dunn$Comparison[i]), " - "))
      pval <- dunn$P.adj[i]
      if (length(pair) == 2) {
        pv_mat[pair[1], pair[2]] <- pval
        pv_mat[pair[2], pair[1]] <- pval
      }
    }
    
    letters <- multcompView::multcompLetters(pv_mat)$Letters
    
    sig_letters_list[[reg]] <-
      data.frame(region = reg,
                 period = names(letters),
                 sig_letter = letters,
                 stringsAsFactors = FALSE)
  }

  sig_letters <- bind_rows(sig_letters_list)

  # ----- Summary stats for plotting -----
  sum_df <- df %>%
    group_by(region, period) %>%
    summarise(mean_val = mean(value, na.rm = TRUE),
              sd_val = sd(value, na.rm = TRUE),
              n = sum(!is.na(value)),
              .groups = "drop") %>%
    mutate(
      ymin = mean_val - sd_val,
      ymax = mean_val + sd_val
    )

  y_max <- max(sum_df$ymax, na.rm = TRUE) * 1.30

  # ----- Plot -----
  p <- ggplot(sum_df, aes(x = period, y = mean_val, fill = period)) +
    geom_col(width = 0.7, alpha = 0.9, color = "black") +
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.25, linewidth = 0.6) +
    geom_text(aes(label = paste0("n=", n), y = ymax + y_max*0.02),
              vjust = -0.1, size = 3.4) +
    geom_text(
      data = sum_df %>% left_join(sig_letters, by = c("region","period")),
      aes(x = period, y = y_max * 0.97, label = sig_letter),
      inherit.aes = FALSE,
      fontface = "bold", size = 5) +
    coord_cartesian(ylim = c(0, y_max)) +
    facet_wrap(~ region, ncol = 2) +
    labs(title = title, x = NULL, y = y_label, fill = "Period") +
    scale_fill_brewer(palette = "Set2") +
    theme_bw(base_size = 12) +
    theme(
      plot.title = element_text(face="bold"),
      strip.background = element_rect(fill="grey92", color=NA),
      panel.grid.minor = element_blank()
    )
  
  # Save automatically
  plot_name <- "../figures/us_epc_period.png"
  ggsave(plot_name, plot = p, width = 8, height = 6, dpi = 300)

  return(p)
}

# Simple time-series trends by US region (no rlang)
# - enh_df must have: region, and either date (POSIXct/Date) or timestr (YYYYMMDDHH[MM])
# - value_col: character name of the column to plot (e.g., "Epc", "CI")
# - agg: "monthly" or "yearly"
# - ci_style: "none", "sd", "se"
# - show_mk: shows Mann–Kendall p-values per region if the Kendall package is available

plot_yearly_epc_by_region <- function(enh_df) {
  stopifnot(is.data.frame(enh_df),
            "region" %in% names(enh_df),
            any(c("date","timestr") %in% names(enh_df)),
            "Epc" %in% names(enh_df))



  df <- enh_df

  # Add year column
  if (!"year" %in% names(df)) {
    if ("date" %in% names(df)) {
      df$year <- year(df$date)
    } else {
      df$year <- suppressWarnings(as.integer(substr(as.character(df$timestr), 1, 4)))
    }
  }

  df$Epc <- suppressWarnings(as.numeric(df$Epc))

  # Yearly summary
  sum_df <- df %>%
    group_by(region, year) %>%
    summarise(
      mean_epc = mean(Epc, na.rm = TRUE),
      sd_epc   = sd(Epc,   na.rm = TRUE),
      n        = n(),
      .groups = "drop"
    ) %>%
    mutate(
      ymin = mean_epc - sd_epc,
      ymax = mean_epc + sd_epc
    )
  sum_df <- sum_df %>%
    mutate(
      region = factor(region, levels = regions)
    )
  # ---- Mann–Kendall per region ----
  mk_df <- NULL
  if (requireNamespace("Kendall", quietly = TRUE)) {
    mk_vals <- lapply(split(sum_df, sum_df$region), function(d) {
      d <- d[order(d$year), ]
      out <- tryCatch(Kendall::MannKendall(d$mean_epc), error = function(e) NULL)
      if (is.null(out)) return(data.frame(p = NA, tau = NA))
      data.frame(p = as.numeric(out$sl), tau = as.numeric(out$tau))
    })
    mk_df <- do.call(rbind, mk_vals)
    mk_df$region <- names(mk_vals)
  }

  # ---- Plot ----

# print(sum_df, n = Inf)
# after you build sum_df with mean_epc, sd_epc, n, ymin, ymax ...

# subset used for ribbon only: need valid ymin/ymax (i.e., sd available, n > 1)
# subset used for ribbon only (need valid bounds)
ribbon_df <- subset(sum_df, is.finite(ymin) & is.finite(ymax) & n >= 1)

p <- ggplot(sum_df, aes(x = year, y = mean_epc)) +
  # draw ribbon only where we have SD (note: single mapping=...)
  geom_ribbon(
    data = ribbon_df,
    mapping = aes(x = year, ymin = ymin, ymax = ymax, group = region),
    inherit.aes = FALSE,
    alpha = 0.20,
    fill = "skyblue"
  ) +
  geom_line(color = "black") +
  geom_point(color = "black") +
  geom_text(aes(label = paste0("n=", n)), vjust = -1, size = 3.5, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "firebrick") +
  facet_wrap(~ region, ncol = 2, scales = "free_y") +
  scale_x_continuous(breaks = sort(unique(sum_df$year))) +
  theme_bw(base_size = 12) +
  labs(
    title = "Yearly EPC by Region (mean ± SD)",
    x = "Year",
    y = "EPC (tCO2 / person / year)"
  )
  # Add MK annotations
  if (!is.null(mk_df)) {
    ann <- aggregate(year ~ region, sum_df, min)
    ann$y <- Inf
    ann <- merge(ann, mk_df, by = "region", all.x = TRUE)
    ann$label <- paste0("MK p=", sprintf("%.3g", ann$p),
                        ", tau=", sprintf("%.2f", ann$tau))
    p <- p + geom_text(
      data = ann,
      aes(x = year, y = y, label = label),
      vjust = 1.2, hjust = 0, size = 3.4,
      inherit.aes = FALSE
    )
    p <- p + theme(
      axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1)
    )
  }
  plot_name="../figures/us_epc_timeseries.png"
  ggsave(plot_name, plot = p, width = 8, height = 6, dpi = 300)
  return(p)
}

# epc per region per season
plot_epc_region_season <- function(enh_df, value = "Epc", scale_multiplier = 1.0) {
  stopifnot(all(c("region","season",value) %in% names(enh_df)))

  library(dplyr)
  library(ggplot2)
  library(FSA)
  library(multcompView)

  df <- enh_df %>% mutate(val = suppressWarnings(as.numeric(.data[[value]])) * scale_multiplier)

  # Drop bad rows
  df <- df %>% filter(!is.na(val), !is.na(season), !is.na(region))

  # ---- Kruskal + Dunn + letters ----
  stats_list <- compute_kw_dunn_letters(
    df        = df,
    value_col = value,
    group_col = "season",
    region_col = "region"
  )
  sig_letters <- stats_list$letters

  # ---- summary per region × season ----
  sum_df <- df %>%
    group_by(region, season) %>%
    summarise(
      mean_val = mean(val, na.rm = TRUE),
      sd_val   = sd(val,   na.rm = TRUE),
      n        = sum(!is.na(val)),
      .groups  = "drop"
    ) %>%
    mutate(
      ymin = mean_val - sd_val,
      ymax = mean_val + sd_val
    )

  # Y limit
  y_max <- max(sum_df$ymax, na.rm = TRUE) * 1.20

  # ---- Plot ----
  p <- ggplot(sum_df, aes(x = season, y = mean_val, fill = season)) +
    geom_col(color = "black", alpha = 0.85) +
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.25) +
    geom_text(aes(label = paste0("n=", n), y = ymax + y_max*0.02),
              vjust = -0.5, size = 3.3) +
    geom_text(
      data = sum_df %>% left_join(sig_letters, by = c("region","season")),
      aes(x = season, y = y_max * 0.97, label = sig_letter),
      inherit.aes = FALSE,
      fontface = "bold", size = 5) +
    coord_cartesian(ylim = c(0, y_max)) +
    facet_wrap(~ region, ncol = 2) +
    labs(title = paste("Seasonal Mean ± SD of", value, "by Region"),
         x = "Season",
         y = paste0(value, " (mean ± SD)"),
         fill = "Season") +
    scale_fill_brewer(palette = "Set2") +
    theme_bw(base_size = 12)

  # Save your figure
  ggsave("../figures/us_epc_season.png", p, width = 8, height = 6, dpi = 300)
  return(p)
}

plot_epc_vs_latitude <- function(enh_df,
                                epc_col = "Epc",
                                lat_col = "lat",
                                city_col = "city",
                                region_col = "region",
                                show_points = TRUE,
                                show_smooth = TRUE) {
  
 
  df <- enh_df %>%
    filter(
      !is.na(.data[[epc_col]]),
      !is.na(.data[[lat_col]]),
      !is.na(.data[[city_col]]),
      !is.na(.data[[region_col]])
    )
  
  p <- ggplot(df, aes(x = .data[[lat_col]], y = .data[[epc_col]]))
  
  if (show_points) {
    p <- p +
      geom_point(
        aes(color = .data[[city_col]],
            shape = .data[[region_col]]),
        alpha = 0.8,
        size = 3
      )
  }
  
  if (show_smooth) {
    p <- p +
      geom_smooth(
        method = "loess",
        linewidth = 1,
        se = TRUE,
        color = "black"
      )
  }
  
  p +
    labs(
      title = "EPC vs Latitude for US Cities",
      x = "Latitude (°N)",
      y = "Urban EPC",
      shape = "Region",
      color = "City"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold"),
      legend.box = "vertical"
    )
}

plot_epc_lat_lon <- function(enh_df,
                             epc_col = "Epc",
                             lat_col = "lat",
                             lon_col = "lon",
                             city_col = "city",
                             region_col = "region") {

  
  # Aggregate to one point per city
  df_city <- enh_df %>%
    group_by(.data[[city_col]], .data[[region_col]]) %>%
    summarize(
      EPC = mean(.data[[epc_col]], na.rm = TRUE),
      lat = mean(.data[[lat_col]], na.rm = TRUE),
      lon = mean(.data[[lon_col]], na.rm = TRUE)
    ) %>%
    ungroup() %>%
    filter(!is.na(EPC), !is.na(lat), !is.na(lon))
  
  p <- ggplot(df_city, aes(x = lon, y = lat)) +
    
    # Region as shape, EPC as color
    geom_point(
      aes(color = EPC,
          shape = .data[[region_col]]),
      size = 5,
      alpha = 0.9,
      stroke = 1.2
    ) +
    
    # Continuous EPC colorbar
    scale_color_viridis_c(option = "plasma", name = "Mean EPC") +
    
    labs(
      title = "Mean EPC Spatial Variation Across US Cities",
      x = "Longitude (°W)",
      y = "Latitude (°N)",
      shape = "Region"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "right",
      plot.title = element_text(face = "bold"),
      legend.box = "vertical"
    )
  
  # print(p)
}



compute_kw_dunn_letters <- function(df, value_col, group_col, region_col) {
  library(FSA)
  library(multcompView)
  library(dplyr)

  stopifnot(all(c(value_col, group_col, region_col) %in% names(df)))

  results_letters <- list()
  regions <- unique(df[[region_col]])

  for (reg in regions) {
    df_reg <- df[df[[region_col]] == reg, ]

    if (n_distinct(df_reg[[group_col]]) < 2)
      next

    # Numeric Kruskal test (no formula)
    kt <- kruskal.test(
      x = df_reg[[value_col]],
      g = df_reg[[group_col]]
    )
    cat("\nKruskal:", reg, "p=", kt$p.value, "\n")

    # Dunn test (already fine)
    dunn <- FSA::dunnTest(
      df_reg[[value_col]] ~ df_reg[[group_col]],
      method = "bh"
    )$res

    # Letter assignment
    groups <- sort(unique(df_reg[[group_col]]))
    pv_mat <- matrix(1, length(groups), length(groups),
                     dimnames = list(groups, groups))
    
    for (i in seq_len(nrow(dunn))) {
      pair <- strsplit(dunn$Comparison[i], " - ")[[1]]
      pv <- dunn$P.adj[i]
      pv_mat[pair[1], pair[2]] <- pv
      pv_mat[pair[2], pair[1]] <- pv
    }

    letters <- multcompView::multcompLetters(pv_mat)$Letters

    results_letters[[reg]] <- data.frame(
      region = reg,
      season = names(letters),   # group_col = "season"
      sig_letter = letters,
      stringsAsFactors = FALSE
    )
  }

  return(list(letters = bind_rows(results_letters)))
}

plot_diurnal_epc_region_us <- function(df, group_var = "region") {
  library(dplyr)
  library(ggplot2)
  library(stringr)
  library(rlang)

  required_cols <- c("timestr", "Epc", group_var)
  if (!all(required_cols %in% names(df))) {
    stop("Missing required columns: ",
         paste(setdiff(required_cols, names(df)), collapse=", "))
  }

  df <- df %>%
    mutate(hour = as.numeric(str_sub(timestr, -2, -1))) %>%
    filter(!is.na(hour))

  group_sym <- sym(group_var)

  region_df <- df %>%
    group_by(!!group_sym, hour) %>%
    summarise(
      Epc_mean = mean(Epc, na.rm = TRUE),
      Epc_sd = sd(Epc, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
  colnames(region_df)[1] <- "group"

  usa_df <- df %>%
    group_by(hour) %>%
    summarise(
      Epc_mean = mean(Epc, na.rm = TRUE),
      Epc_sd = sd(Epc, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    mutate(group = "USA")

  plot_df <- bind_rows(region_df, usa_df)

  region_levels <- unique(region_df$group)
  region_shapes <- setNames(seq_along(region_levels) %% 6 + 1, region_levels)

  p <- ggplot(plot_df, aes(x = hour, y = Epc_mean, group = group)) +
    geom_line(aes(
      color = group,
      linewidth = (group == "USA"),
      linetype = (group == "USA")   # ✅ USA = dashed, regions = solid
    )) +
    geom_point(aes(color = group, shape = group), size = 3) +
    geom_ribbon(aes(ymin = Epc_mean - Epc_sd,
                    ymax = Epc_mean + Epc_sd,
                    fill = group),
                alpha = 0.12, color = NA) +
    scale_x_continuous(breaks = 0:23) +

    # ✅ Styling
    scale_color_manual(values = c("USA" = "black",
                                  setNames(scales::hue_pal()(length(region_levels)),
                                           region_levels))) +
    scale_fill_manual(values = c("USA" = "black",
                                 setNames(scales::hue_pal()(length(region_levels)),
                                          region_levels))) +
    scale_shape_manual(values = c("USA" = NA, region_shapes)) +
    scale_linewidth_manual(values = c(`TRUE` = 1.8, `FALSE` = 0.9)) +
    scale_linetype_manual(values = c(`TRUE` = "dashed", `FALSE` = "solid")) + # ✅ key for dashed USA

    labs(
      title = paste0("Diurnal Epc Profile by ", group_var, " + USA"),
      x = "Hour of Day (UTC)",
      y = "Mean Epc",
      color = "Group",
      shape = "Group",
      fill = "Group",
      linetype = "Group",   # ✅ so legend shows dashed USA
      linewidth = "Group"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "right")

  return(list(plot = p, summary = plot_df))
}

# --- Generate and save log–log regression plot ---
plot_epc_vs_pps <- function(df, use_int_epc_sd = TRUE, save_path = NULL) {
  library(ggplot2)
  library(dplyr)
  library(ggrepel)

  # Required columns
  required_cols <- c("city", "region", "Epc", "pps_mean")
  if (!all(required_cols %in% names(df))) {
    stop("Missing required columns: ",
         paste(setdiff(required_cols, names(df)), collapse = ", "))
  }

  # Handle missing uncertainty
  if (use_int_epc_sd && !"int_epc_sd" %in% names(df)) {
    warning("int_epc_sd not found; using Epc standard deviation instead.")
    use_int_epc_sd <- FALSE
  }

  # Clean and ensure numeric
  df <- df %>%
    mutate(
      Epc = as.numeric(Epc),
      pps_mean = as.numeric(pps_mean),
      int_epc_sd = ifelse("int_epc_sd" %in% names(df),
                          as.numeric(int_epc_sd), NA)
    ) %>%
    filter(!is.na(Epc), Epc > 0, !is.na(pps_mean), pps_mean > 0)

  # Aggregate by city
  df_city <- df %>%
    group_by(city, region) %>%
    summarise(
      Epc_mean = mean(Epc, na.rm = TRUE),
      pps_mean = mean(pps_mean, na.rm = TRUE),
      Epc_sd = if (use_int_epc_sd) mean(int_epc_sd, na.rm = TRUE) else sd(Epc, na.rm = TRUE)
    ) %>%
    ungroup()

  # Filter positive values
  df_city <- df_city %>% filter(Epc_mean > 0, pps_mean > 0)

  # --- Fit log10 regression ---
  fit_log <- lm(log10(Epc_mean) ~ log10(pps_mean), data = df_city)
  intercept_log <- coef(fit_log)[1]
  slope_log <- coef(fit_log)[2]
  r2_log <- summary(fit_log)$r.squared

  # Equation for annotation
  eqn <- sprintf("log₁₀-fit: Epc = %.3f × pps_mean^{%.3f}\nR² = %.3f",
                 10^intercept_log, slope_log, r2_log)

  # --- Fit linear regression (for plotting only) ---
  fit_lin <- lm(Epc_mean ~ pps_mean, data = df_city)

  # --- Build plot (linear axes, straight regression line) ---
  p <- ggplot(df_city, aes(x = pps_mean, y = Epc_mean, color = region, label = city)) +
    geom_errorbar(aes(ymin = pmax(Epc_mean - Epc_sd, 0),
                      ymax = Epc_mean + Epc_sd),
                  width = 0.02, linewidth = 0.8, alpha = 0.8) +
    geom_point(size = 4, alpha = 0.9) +
    geom_text_repel(size = 4, max.overlaps = 20, seed = 1234, show.legend = FALSE) +
    geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
    annotate("text",
             x = Inf, y = Inf,
             label = eqn,
             hjust = 1.1, vjust = 1.2,
             size = 4.5, color = "black") +
    labs(
      title = "Regression in linear scale (fit from log₁₀ regression)",
      x = "Mean pps_mean",
      y = "Mean Epc",
      color = "Region"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "right",
          plot.title = element_text(face = "bold"))

  # --- Save if requested ---
  if (!is.null(save_path)) {
    ggsave(filename = save_path, plot = p, width = 7, height = 6,
           dpi = 300, bg = "white")
    message("✅ Plot saved to: ", normalizePath(save_path))
  }

  return(p)
}

plot_epc_vs_pps_periods <- function(df, use_int_epc_sd = TRUE, save_path = NULL) {
  library(ggplot2)
  library(dplyr)

  # Required columns
  required_cols <- c("city", "region", "Epc", "pps_mean", "timestr")
  if (!all(required_cols %in% names(df))) {
    stop("Missing required columns: ",
         paste(setdiff(required_cols, names(df)), collapse = ", "))
  }

  # Handle missing uncertainty
  if (use_int_epc_sd && !"int_epc_sd" %in% names(df)) {
    warning("int_epc_sd not found; using Epc standard deviation instead.")
    use_int_epc_sd <- FALSE
  }

  # Clean data
  df <- df %>%
    mutate(
      Epc = as.numeric(Epc),
      pps_mean = as.numeric(pps_mean),
      year = as.numeric(substr(timestr, 1, 4)),
      int_epc_sd = ifelse("int_epc_sd" %in% names(df),
                          as.numeric(int_epc_sd), NA)
    ) %>%
    filter(!is.na(Epc), Epc > 0, !is.na(pps_mean), pps_mean > 0, !is.na(year))

  # Assign periods
  df <- df %>%
    mutate(period = case_when(
      year >= 2015 & year <= 2019 ~ "2015–2019",
      year >= 2020 & year <= 2021 ~ "2020–2021",
      year >= 2022 & year <= 2024 ~ "2022–2024",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(period))

  # Aggregate by city and period
  df_city <- df %>%
    group_by(period, city) %>%
    summarise(
      Epc_mean = mean(Epc, na.rm = TRUE),
      pps_mean = mean(pps_mean, na.rm = TRUE),
      Epc_sd = if (use_int_epc_sd) mean(int_epc_sd, na.rm = TRUE) else sd(Epc, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    filter(Epc_mean > 0, pps_mean > 0)

  # Fit models and store lines
  fits <- list()
  lines_list <- list()

  for (p in unique(df_city$period)) {
    df_p <- df_city %>% filter(period == p)

    # Fit log10-space model for annotation
    fit_log <- lm(log10(Epc_mean) ~ log10(pps_mean), data = df_p)
    intercept_log <- coef(fit_log)[1]
    slope_log <- coef(fit_log)[2]
    r2_log <- summary(fit_log)$r.squared

    # Fit linear regression for plotted line
    fit_lin <- lm(Epc_mean ~ pps_mean, data = df_p)
    intercept_lin <- coef(fit_lin)[1]
    slope_lin <- coef(fit_lin)[2]

    # Predicted line (straight, linear space)
    x_pred <- seq(min(df_p$pps_mean), max(df_p$pps_mean), length.out = 200)
    y_pred <- intercept_lin + slope_lin * x_pred
    lines_list[[p]] <- data.frame(pps_mean = x_pred, Epc_pred = y_pred, period = p)

    fits[[p]] <- data.frame(period = p,
                            a = 10^intercept_log, b = slope_log, R2 = r2_log)
  }

  fits_df <- do.call(rbind, fits)
  lines_df <- do.call(rbind, lines_list)

  # Build plot (points colored by period, linear regression lines)
  p <- ggplot(df_city, aes(x = pps_mean, y = Epc_mean, color = period)) +
    geom_errorbar(aes(ymin = pmax(Epc_mean - Epc_sd, 0),
                      ymax = Epc_mean + Epc_sd),
                  width = 0.02, linewidth = 0.8, alpha = 0.7) +
    geom_point(size = 4, alpha = 0.9) +
    geom_line(data = lines_df,
              aes(x = pps_mean, y = Epc_pred, color = period),
              linetype = "dashed", linewidth = 1.2, inherit.aes = FALSE) +
    labs(
      title = "City-mean Epc vs. pps_mean by period",
      subtitle = "Regression lines in linear space; fits from log₁₀ regression",
      x = "Mean pps_mean",
      y = "Mean Epc",
      color = "Period"
    ) +
    theme_bw(base_size = 14) +
    theme(legend.position = "right",
          plot.title = element_text(face = "bold"))

  # Annotation for each period’s log10-fit equation
  eq_text <- fits_df %>%
    mutate(label = sprintf("%s: Epc = %.2f × pps^{%.2f}, R²=%.2f",
                           period, a, b, R2)) %>%
    pull(label)

  p <- p + annotate("text",
                    x = Inf, y = Inf,
                    label = paste(eq_text, collapse = "\n"),
                    hjust = 1.1, vjust = 1.1,
                    size = 4, color = "black")

  # Save PNG if requested
  if (!is.null(save_path)) {
    ggsave(filename = save_path, plot = p,
           width = 8, height = 6, dpi = 300, bg = "white")
    message("✅ Plot saved to: ", normalizePath(save_path))
  }

  return(p)
}