# =====================================================================
# EDGAR v7 → Hourly downscaled sector totals per timestr (NO FOOTPRINT)
# Output: stacked plot (sector vs timestr) per city, units = µmol s^-1
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
# ✅ EDGAR Data & Sector Detection
# -------------------------------
source("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/EDGAR/downscaling/edgar.sector.weighting.vector.r")
downscaling_dir <- "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/EDGAR/downscaling"

edgar_dir <- "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/EDGAR_v7/EDGARv7_Glb_0.1x0.1_anthro_CO2_excl_short-cycle_org_C__yearly"
edgar_files <- list.files(edgar_dir, pattern="\\.nc$", full.names=TRUE)

# ✅ Auto detect sector variables in first EDGAR file
test_nc <- nc_open(edgar_files[1])
all_vars <- names(test_nc$var)
nc_close(test_nc)

# ✅ Keep only lowercase names typical of EDGAR sectors
# sectors <- all_vars[grepl("^[a-z_]+$", all_vars)]
sectors <- all_vars[
  grepl("^[a-z_]+$", all_vars, ignore.case = TRUE) & 
  !grepl("sum", all_vars, ignore.case = TRUE)
]

message("📌 Final sectors used: ", paste(sectors, collapse=", "))
message("📌 Detected EDGAR sectors: ", paste(sectors, collapse=", "))

# -------------------------------
# ✅ Overpass Times
# -------------------------------
OCO.DIR <- Sys.getenv("OCO2_DIR")
overpass_to_model <- read.table(file.path(OCO.DIR, "OCO-2/overpass_city/overpass_to_model.txt"),
                                header = TRUE, sep = ",", stringsAsFactors = FALSE)
incomplete_runs <- read.table(file.path(OCO.DIR, "OCO-2/overpass_city/failed_runs.txt"),
                              header = TRUE, sep = "\t")

if ("foot" %in% names(overpass_to_model) && "foot" %in% names(incomplete_runs)) {
  overpass_to_model <- anti_join(overpass_to_model, incomplete_runs %>% select(foot), by="foot")
}

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
  yr <- year(tt)
  r <- read_sector_raster(yr, sector)
  print(sector)
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
  kg_s*(1000/44.01)*1e6  # µmol/s
}

# -------------------------------
# ✅ Main per-city processor
# -------------------------------
process_city <- function(city) {
  message("\n⏳ Processing: ", city)

  ext <- get_city_extent(city)
  lon <- mean(c(ext@xmin, ext@xmax))
  lat <- mean(c(ext@ymin, ext@ymax))

  city_times <- overpass_to_model %>% filter(site == !!city)

  if (nrow(city_times) == 0) return(NULL)
  message("📌 Overpass times: ", nrow(city_times))

  out <- bind_rows(lapply(seq_len(nrow(city_times)), function(i) {
    tt <- city_times$timestr[i]
    pct <- round(100 * i / nrow(city_times), 1)
    message(sprintf("  ⏱️ [%d/%d | %s%%] %s",
                    i, nrow(city_times), pct,
                    format(tt, "%Y-%m-%d %H UTC")))
    vals <- sapply(sectors, function(sv) sector_emission_at_time(sv, tt, ext, lon, lat))
    tibble(city=city, timestr=tt, sector=sectors, value_umol_s=vals)
  }))

  # ✅ Determine top 7 sectors for this city
  sector_rank <- out %>%
    group_by(sector) %>%
    summarize(total=sum(value_umol_s, na.rm=TRUE)) %>%
    arrange(desc(total)) %>%
    mutate(keep = sector %in% head(sector, 7))

  keep_sectors <- sector_rank$sector[sector_rank$keep]

  out2 <- out %>%
    mutate(sector = if_else(sector %in% keep_sectors, sector, "other"))

  # ✅ Choose chunk size so labels remain readable
chunk_size <- 15
n_chunks <- ceiling(nrow(out2) / (chunk_size * length(unique(out2$sector))))

outfile <- file.path("sanity_check_figures/", paste0(city, "_EDGAR_downscaled_top7.pdf"))

pdf(outfile, width = 13, height = 6)   # ✅ multi-page PDF open

unique_times <- unique(out2$timestr)
for (k in seq(1, length(unique_times), by = chunk_size)) {

  chunk_times <- unique_times[k:min(k + chunk_size - 1, length(unique_times))]

  p <- ggplot(
      data = out2 %>% filter(timestr %in% chunk_times),
      aes(x = forcats::fct_inorder(format(timestr, "%Y-%m-%d %H UTC")),
          y = value_umol_s,
          fill = sector)
    ) +
    geom_bar(stat = "identity") +
    labs(
      title = paste0("EDGAR Downscaled (µmol s⁻¹) — ", city,
                     " (Page ", ceiling(k / chunk_size), "/", n_chunks, ")"),
      x = "Overpass Time (UTC)",
      y = expression(paste(mu,"mol ",s^{-1}))
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(p)
}

  dev.off()   # ✅ close multi-page PDF
  message("✅ Saved multi-page PDF: ", normalizePath(outfile))

  
}

# -------------------------------
# ✅ Run All Cities
# -------------------------------
print(cities)
plots <- lapply(cities, process_city)
message("\n🎯 COMPLETED ALL CITIES ✅")