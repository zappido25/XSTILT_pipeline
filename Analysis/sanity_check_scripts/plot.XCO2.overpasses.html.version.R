# ===================================================
# Offline OCO-2 XCO2 Maps with Time Slider (Working)
# ===================================================

library(dplyr)
library(sf)
library(readr)
library(leaflet)
library(htmlwidgets)
library(htmltools)
library(jsonlite)
library(geojsonsf)
library(rnaturalearth)
library(rnaturalearthdata)

# Load helper functions
functions_files <- list.files(
  '/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Analysis/functions/',
  full.names = TRUE, pattern = "\\.r$")
invisible(lapply(functions_files, source))


# ✅ Convert city extent → sf polygon
make_extent <- function(city) {
  bb <- get_urban_extent(city)

  if (inherits(bb, c("sf","sfc"))) {
    bb <- suppressWarnings(st_transform(bb, 4326))
    bbx <- st_bbox(bb)
    xmin <- bbx["xmin"]; xmax <- bbx["xmax"]
    ymin <- bbx["ymin"]; ymax <- bbx["ymax"]

  } else if (is.list(bb)) {
    lo <- as.numeric(unlist(lapply(bb, function(x) x$lon)))
    la <- as.numeric(unlist(lapply(bb, function(x) x$lat)))
    xmin <- min(lo); xmax <- max(lo)
    ymin <- min(la); ymax <- max(la)

  } else if (is.numeric(bb) && length(bb)>=4) {
    xmin <- min(bb[1:2]); xmax <- max(bb[1:2])
    ymin <- min(bb[3:4]); ymax <- max(bb[3:4])

  } else stop("Unsupported bbox format for ", city)

  coords <- matrix(
    c(xmin,ymin, xmax,ymin, xmax,ymax, xmin,ymax, xmin,ymin),
    ncol=2, byrow=TRUE)

  st_polygon(list(coords)) |> st_sfc(crs=4326) |> st_sf()
}


# ========== Inputs ==========
OCO.DIR <- Sys.getenv("OCO2_DIR")

urban_core <- read.table(
  "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Cities_input_files/urban_core_info.txt",
  header=TRUE)

overpass_to_model <- read.table(
  file.path(OCO.DIR,"OCO-2/overpass_city/overpass_to_model.txt"),
  header=TRUE, sep=",")

incomplete_runs <- read.table(
  file.path(OCO.DIR,"OCO-2/overpass_city/failed_runs.txt"),
  header=TRUE, sep="\t")

cities <- unique(urban_core$city)

states <- ne_states("United States of America", returnclass="sf") |>
  st_transform(4326)

out_dir <- "overpasses_plots_html"
dir.create(out_dir, showWarnings = FALSE)


# ✅ Loop each city
for(city in cities){

  message("Processing: ", city)

  matching <- which(overpass_to_model$site == city)
  if(length(matching)==0){ next }

  center_lon <- mean(urban_core$lon[urban_core$city == city])
  center_lat <- mean(urban_core$lat[urban_core$city == city])

  all_obs <- list()

  for(ii in matching){
    timestr <- overpass_to_model$timestr[ii]
    if(timestr < "2015000000" || timestr %in% incomplete_runs$timestr) next

    xfile <- file.path(OCO.DIR,"OCO-2/overpass_obs",
                       paste0(city,"_",timestr,".txt"))
    if(!file.exists(xfile)) next

    dat <- read.table(xfile, header=TRUE)
    dat$timestr <- timestr
    all_obs[[length(all_obs)+1]] <- dat
  }

  if(!length(all_obs)){
    message("⚠ No valid data for ", city)
    next
  }

  all_obs <- bind_rows(all_obs) |>
    mutate(
      lon = as.numeric(lon),
      lat = as.numeric(lat),
      XCO2 = as.numeric(XCO2),
      datetime = as.POSIXct(timestr, format="%Y%m%d%H", tz="UTC")
    )

  pal <- colorNumeric("viridis", domain=all_obs$XCO2)

  time_stats <- all_obs |> group_by(timestr) |>
    summarize(min_x=min(XCO2), max_x=max(XCO2))

  # ✅ Prepare GeoJSON properties (MUST be atomic)
  obs_clean <- all_obs |>
    mutate(
      datetime = format(datetime, "%Y-%m-%dT%H:%M:%SZ"),
      timestr = as.character(timestr),
      color = pal(XCO2)
    )

  obs_sf <- st_as_sf(obs_clean, coords = c("lon","lat"), crs = 4326)
  obs_geojson <- geojsonsf::sf_geojson(obs_sf)
  stats_json <- toJSON(time_stats)

  city_poly <- make_extent(city)

  # BUILD MAP
  m <- leaflet(options = leafletOptions(minZoom=3)) |>
    addPolygons(data=states, fillColor="#d9d9d9",
                fillOpacity=1, color="white") |>
    addPolygons(data=city_poly, fillOpacity=0,
                color="red", weight=3) |>
    leaflet::addGeoJSON(
      obs_geojson,
      group="obs"
    ) |>
    addLegend("bottomright",
              pal=pal, values=all_obs$XCO2,
              title="XCO2 (ppm)") |>
    setView(center_lon, center_lat, 9) |>
    onRender(sprintf("
function(el,x){
  var map = this;
  var obsLayer;

  map.eachLayer(function(layer){
    if(layer.feature && layer.feature.properties &&
       layer.feature.properties.datetime){
      obsLayer = layer;
    }
  });

  if(!obsLayer){ console.log('No obs layer'); return; }

  map.timeDimension = new L.TimeDimension({ period: 'PT1H' });

  var player = new L.TimeDimension.Player({
    loop:true,
    startOver:true
  }, map.timeDimension);

  map.addControl(new L.Control.TimeDimension({
    player:player,
    position:'bottomleft',
    autoPlay:false
  }));

  var tdLayer = L.timeDimension.layer.geoJson(obsLayer, {
    updateTimeDimension:true,
    addlastPoint:true,
    pointToLayer: function(feature, latlng){
      return L.circleMarker(latlng, {
        radius: 6,
        fillOpacity: 0.85,
        weight: 1,
        color: 'black',
        fillColor: feature.properties.color
      }).bindPopup(
        '<b>XCO₂:</b> '+feature.properties.XCO2.toFixed(2)+' ppm<br>'+
        '<b>Time:</b> '+feature.properties.datetime
      );
    }
  });

  tdLayer.addTo(map);
  setTimeout(function(){ map.invalidateSize(); }, 500);
}
", stats_json))

  outfile <- file.path(out_dir, paste0(city,"_slider_map.html"))
  saveWidget(m, outfile, selfcontained=TRUE)

  message("✅ Saved ", outfile)
}

message("🎯 ALL FINISHED ✅")
