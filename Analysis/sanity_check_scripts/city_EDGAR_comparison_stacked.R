# =====================================================================
# EDGAR v7 → Hourly downscaled sector totals per timestr (NO FOOTPRINT)
# Output: stacked plot (sector vs timestr) per city, units = µmol s^-1
# =====================================================================
#=====================================
# EDGAR v7 → Downscaled Sector Stacks by Overpass Timestamp (NO FOOTPRINT)
# Using city list: CONUS_cities_list.txt
# =====================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(ncdf4)
library(raster)
library(sf)
library(lubridate)
library(stringr)
library(forcats)

theme_set(theme_minimal())

# -------------------------------
# ✅ Load X-STILT helper functions
# -------------------------------
functions_files <- list.files('/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Analysis/functions/',
                              full.names = TRUE, pattern = "\\.r$")
invisible(lapply(functions_files, source))

# -------------------------------
# ✅ City Input
# -------------------------------
cities_file <- "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Cities_input_files/simulated_cities.txt"
cities <- read.table(cities_file, header = FALSE, stringsAsFactors = FALSE)[,1]

# -------------------------------
# ✅ EDGAR Data & Downscaling
# -------------------------------
source("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/EDGAR/downscaling/edgar.sector.weighting.vector.r")
downscaling_dir <- "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/EDGAR/downscaling"

edgar_dir <- "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/EDGAR_v7/EDGARv7_Glb_0.1x0.1_anthro_CO2_excl_short-cycle_org_C__yearly"
edgar_files <- list.files(edgar_dir, pattern="\\.nc$", full.names=TRUE)

sectors <- c("ene","ind","rco","tro_nores","pro","nmm","fff","che")

# -------------------------------
# ✅ Overpass Times
# -------------------------------
OCO.DIR <- Sys.getenv("OCO2_DIR")
overpass_to_model <- read.table(file.path(OCO.DIR, "OCO-2/overpass_city/overpass_to_model.txt"),
                                header = TRUE, sep = ",", stringsAsFactors = FALSE)
incomplete_runs <- read.table(file.path(OCO.DIR, "OCO-2/overpass_city/failed_runs.txt"),
                              header = TRUE, sep = "\t")

# Remove failed runs
if ("foot" %in% names(overpass_to_model) && "foot" %in% names(incomplete_runs)) {
  overpass_to_model <- anti_join(overpass_to_model, incomplete_runs %>% select(foot), by="foot")
}

# ✅ Extract UTC timestr column
overpass_to_model$timestr <- as.POSIXct(overpass_to_model$timestr,
                                        format="%Y%m%d%H", tz="UTC")
overpass_to_model <- overpass_to_model %>% filter(!is.na(timestr))


# -------------------------------
# ✅ Longitude Adjustment
# -------------------------------
adjust.EDGAR.long <- function(x) {
  east <- try(crop(x, extent(0,180,-90,90)), silent=TRUE)
  west <- try(crop(x, extent(180,360,-90,90)), silent=TRUE)
  east <- if (inherits(east,"try-error")) NULL else east
  west <- if (inherits(west,"try-error")) NULL else west
  if (!is.null(west)) extent(west) <- c(-180,0,-90,90)
  if (!is.null(east) && !is.null(west)) return(merge(west,east))
  if (!is.null(east)) return(east)
  return(west)
}

# -------------------------------
# ✅ Helpers
# -------------------------------
get_city_extent <- function(city) {
  bb <- get_urban_extent(city)
  pad <- 0.2
  extent(min(bb$lon)-pad, max(bb$lon)+pad,
         min(bb$lat)-pad, max(bb$lat)+pad)
}

read_sector_raster <- function(year, sector) {
  if (year > 2021) year <- 2021
  fs <- edgar_files[grepl(as.character(year), basename(edgar_files))]
  if (length(fs)==0) return(NULL)
  r <- try(brick(fs[1], varname=sector), silent=TRUE)
  if (inherits(r,"try-error")) return(NULL)
  adjust.EDGAR.long(r)
}

sector_emission_at_time <- function(sector, tt, ext, lon, lat) {
  yr <- year(tt); mo <- month(tt)
  r <- read_sector_raster(yr, sector)
  if (is.null(r)) return(NA_real_)
  rc <- try(crop(r, ext), silent=TRUE)
  if (inherits(rc,"try-error") || is.null(rc) || ncell(rc)==0) return(NA_real_)
  days <- days_in_month(tt)
  rc_month <- rc*(24*3600*days)
  tstr <- format(tt,"%Y%m%d%H0000")
  w <- edgar.sector.weighting(lon, lat, "UTC", toupper(sector),
                              temporal.downscaling.files=downscaling_dir,
                              time=tstr, monthly=TRUE)$weighting[1]
  rc_hour <- (rc_month * w) / 3600
  A <- values(area(rc_hour)) * 1e6
  kg_s <- sum(values(rc_hour)*A, na.rm=TRUE)
  umol_s <- kg_s*(1000/44.01)*1e6
  umol_s
}

# -------------------------------
# ✅ Main per-city processor
# -------------------------------
process_city <- function(city) {
  message("\n⏳ Processing: ", city)

  ext <- get_city_extent(city)
  lon <- mean(c(ext@xmin, ext@xmax))
  lat <- mean(c(ext@ymin, ext@ymax))

  # ✅ Filter only timestamps for this city
  city_times <- overpass_to_model %>% filter(site == !!city)

  if (nrow(city_times) == 0) {
    message("⚠️ No timestrs for city: ", city)
    return(NULL)
  }

  message("📌 Found ", nrow(city_times), " overpass timestamps for ", city)

  out <- bind_rows(lapply(seq_len(nrow(city_times)), function(i) {
    tt <- city_times$timestr[i]
    
    pct <- round(100 * i / nrow(city_times), 1)
    message(sprintf("  ⏱️ [%d/%d | %s%%] %s",
                    i, nrow(city_times), pct,
                    format(tt, "%Y-%m-%d %H UTC")))

    tic <- Sys.time()
    vals <- sapply(sectors, function(sv) sector_emission_at_time(sv, tt, ext, lon, lat))
    dt <- round(as.numeric(Sys.time() - tic, units="secs"), 1)
    
    message(sprintf("     ✅ Done in %s sec", dt))

    tibble(city = city,
           timestr = tt,
           sector = sectors,
           value_umol_s = vals)
  }))

  if (nrow(out) == 0) {
    message("⚠️ No EDGAR emission extracted — skip")
    return(NULL)
  }

  p <- out %>%
    mutate(timestr = format(timestr, "%Y-%m-%d %H UTC")) %>%
    ggplot(aes(x = factor(timestr), y = value_umol_s, fill = sector)) +
    geom_bar(stat = "identity") +
    labs(title = paste0("EDGAR Downscaled (µmol s⁻¹) — ", city),
         x = "Overpass time (UTC)",
         y = expression(paste(mu, "mol ", s^{-1}))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  outfile <- file.path("sanity_check_figures/", paste0(city, "_EDGAR_downscaled_by_timestr.pdf"))
  ggsave(outfile, p, width = 13, height = 6)
  message("✅ Saved Plot: ", normalizePath(outfile))

  p
}

# -------------------------------
# ✅ Run All Cities
# -------------------------------
print(cities)
plots <- lapply(cities, process_city)
message("\n🎯 COMPLETED ALL CITIES ✅")