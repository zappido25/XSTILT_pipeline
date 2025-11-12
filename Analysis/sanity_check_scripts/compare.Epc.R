library(ggplot2)
library(dplyr)
library(tidyr)

group = Sys.getenv("group")
version = Sys.getenv("version")
out.path=Sys.getenv("OUT_DF_DIR")
OCO.DIR = Sys.getenv("OCO2_DIR")

# functions_files <- list.files('/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Analysis/functions/', full.names = TRUE, pattern = "\\.r$")
# invisible(lapply(functions_files, source))
# print(functions_files)

shp_file = FALSE  # whether to use the shapefile based clustering or not


args <- commandArgs(trailingOnly = TRUE)

if (length(args) >= 1 && !is.na(args[1]) && nzchar(args[1])) {
  shp_file <- as.logical(args[1])
  print(paste0("Running for shp_file is ", ifelse(shp_file, "TRUE", "FALSE")))
}

enhancement_cols = c("city", "timestr", "enh_mean", "enh_sd", "int_mean", 
                        "int_sd", "int_epc_mean", "int_epc_sd", "CI", "CI_sd", #"int_epc_gdp_mean" is carbon intensity
                          "int_gdp_epc_mean", "int_gdp_epc_sd","pps_mean", "pps_sd", 
                        "bg_pps_mean", "bg_pps_sd", "gdp_mean", "gdp_sd", "bg_gdp_mean", "bg_gdp_sd", 
                        "consumption_mean", "consumption_sd",
                          "city_fr_mean", "city_fr_sd", "lights_mean", "lights_sd", "na_count")


cities_file <- file.path("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Cities_input_files/simulated_cities.txt")

cities <- read.table(cities_file, header = FALSE, skip = 0, stringsAsFactors = FALSE)[, 1]


  # Prompt for number of cities
cat("How many cities would you like to compare? ")
flush.console()
n_compare <- as.integer(readLines(file("stdin"), n = 1))
# Display available cities
cat("Available cities:\n")
for (i in seq_along(cities)) {
  cat(sprintf("%d: %s\n", i, cities[i]))
}

# cat("EHow many cities would you like to compare? ")
# city_idx <- as.integer(readLines(file("stdin"), n = 1))

if (is.na(n_compare) || n_compare < 1 || n_compare > length(cities)) {
  stop("Invalid number of cities.")
}

# Prompt for city indices
cat(sprintf("Enter %d city numbers separated by spaces: ", n_compare))
flush.console()
line <- readLines(file("stdin"), n = 1)         # waits for Enter
city_idx <- as.integer(strsplit(line, "\\s+")[[1]])

# validate
if (length(city_idx) != n_compare || any(is.na(city_idx))) {
  stop("Please enter exactly ", n_compare, " integers separated by spaces.")
}

# Validate
if (length(city_idx) != n_compare || any(city_idx < 1 | city_idx > length(cities))) {
  stop("Invalid city selection.")
}

# Get city names
selected_cities <- cities[city_idx]
cat("Selected cities:\n")
cat(paste0(" - ", selected_cities, collapse = "\n"), "\n")

overpass_to_model <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/overpass_to_model.txt"),
                header = TRUE, sep = ",")  # Replace "data.txt" with your file name
incomplete_runs <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/failed_runs.txt"),
                header = TRUE, sep = "\t")  
na_counts=list()
for (city in selected_cities) {
    
    matching_indices <- which(overpass_to_model$site %in% city)
    edgar_folder=file.path(out.path, "ENHANCEMENTS", city)
    vulcan_folder=file.path(out.path, "ENHANCEMENTS_vulcan", city)

    files <- list.files(edgar_folder, pattern = paste0("^", city, "_[0-9]+.*\\.txt$"),
                  full.names = FALSE)
    if (!length(files)) {
      message("  • No files found for city ", city)
      next
    }

    default_cutoff <- '2015000000'  # Default cutoff for most cities
    strict_cutoff <- '2015050000'  # strict cutoff for few cities
    ## extract timestr parts from filenames
    ts_from_files <- sub(paste0("^", city, "_([0-9]+).*"), "\\1", files)

    ## choose cutoff based on minimum timestr in folder
    min_ts <- min(ts_from_files, na.rm = TRUE)
    city_cutoff <- if (min_ts >= strict_cutoff) strict_cutoff else default_cutoff
    message("City=", city, " → min_ts=", min_ts, " → cutoff=", city_cutoff)

    for (ii in matching_indices) {

      timestr=overpass_to_model$timestr[ii]

        if (timestr < city_cutoff || timestr %in% incomplete_runs$timestr) {
                    print("skipping runs outside date range or failed runs")
                    print(paste0("CITY::", city))
                    print(paste0("Timestr::", timestr))
                    next
            }
        
        if (!shp_file) {
            edgar_file <- file.path(edgar_folder, paste0(city, "_", timestr, "_MC_brute.txt"))
            vulcan_file <- file.path(vulcan_folder, paste0(city, "_", timestr, "_MC_brute.txt"))
            method_analysis= "CC"
        } else{
            edgar_file <- file.path(edgar_folder, paste0(city, "_", timestr, "_MC_brute_shp.txt"))
            vulcan_file <- file.path(vulcan_folder, paste0(city, "_", timestr, "_MC_brute_shp.txt"))
            method_analysis= "US_Census"
        }
        
        edgar_readin <- t(read.table(edgar_file,header = FALSE, sep = ",", skip = 1) ) # transpose the table
        vulcan_readin <- t(read.table(vulcan_file,header = FALSE, sep = ",", skip = 1) ) # transpose the table
        colnames(vulcan_readin) <- enhancement_cols
        colnames(edgar_readin) <- enhancement_cols
        method_edgar= "Edgar"
        method_vulcan= "Vulcan"

        if (as.numeric(edgar_readin[,"na_count"]) == 100000 | as.numeric(vulcan_readin[,"na_count"]) == 100000) {
        cat("Skipping iteration due to city clustering na_count == 100000 for timestr:", timestr, "\n")
        na_counts[[length(na_counts) + 1]]  <- timestr
        next
        } 

        if (!exists("edgar_list")) edgar_list <- list()
        if (!exists("vulcan_list")) vulcan_list <- list()

        edgar_list[[length(edgar_list) + 1]]  <- as.data.frame(edgar_readin)
        vulcan_list[[length(vulcan_list) + 1]]  <- as.data.frame(vulcan_readin)
        
    }
}
edgar_df  <- dplyr::bind_rows(edgar_list)
vulcan_df <- dplyr::bind_rows(vulcan_list)

edgar_df=edgar_df %>%
    mutate(method=method_edgar, method_analysis=method_analysis)
vulcan_df=vulcan_df %>%
    mutate(method=method_vulcan, method_analysis=method_analysis)

print(dim(edgar_df))
print(dim(vulcan_df))

epc_df <- bind_rows(edgar_df, vulcan_df)

epc_df <- epc_df %>%
  mutate(
    int_epc_mean = as.numeric(int_epc_mean),
    enh_mean = as.numeric(enh_mean),
    int_epc_sd = as.numeric(int_epc_sd),
    enh_sd = as.numeric(enh_sd),
    timestr = as.character(timestr),
    time         = suppressWarnings(as.POSIXct(timestr, format = "%Y%m%d%H", tz = "UTC"))
  )

summary(epc_df$time)
head(epc_df[, c("timestr", "time")])
# --- Reshape to long format (keep sd info, but separate mean vs sd) ---
epc_long <- epc_df %>%
  select(city, method, method_analysis, time, int_epc_mean, enh_mean, int_epc_sd, enh_sd) %>%
  pivot_longer(
    cols = c(int_epc_mean, enh_mean, int_epc_sd, enh_sd),
    names_to = c("metric", "stat"),
    names_pattern = "(int_epc|enh)_(mean|sd)",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = stat, values_from = value)

# ✅ Now epc_long looks like:
# city | method | method_analysis | time | metric | mean | sd

# --- Rename metrics clearly ---
epc_long$metric <- recode(epc_long$metric,
  "int_epc" = "EPC (tCO₂ per capita)",
  "enh"     = "Enhancement (ppm)"
)

# --- Plot only mean values ---
p <- ggplot(epc_long, aes(x = time, y = mean, color = method, group = interaction(method, metric))) +
  geom_line(linewidth = 1.1, alpha = 0.9) +
  geom_point(size = 2, alpha = 0.8) +
  facet_grid(metric ~ city, scales = "free_y", switch = "y") +
  scale_color_manual(values = c("Vulcan" = "#1b9e77", "Edgar" = "#d95f02")) +
  labs(
    title = "Comparison of EPC and CO₂ Enhancements: Vulcan vs EDGAR",
    x = "Overpass Time (UTC)",
    y = NULL,
    color = "Inventory"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    strip.text.y.left = element_text(angle = 0, face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p)

# --- Save figure ---
plot_folder="/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Analysis/sanity_check_scripts/sanity_check_figures"
pdf_file=file.path(plot_folder, "EPC_Enhancements_inventory_comparison.pdf")
ggsave(
  filename = pdf_file,
  plot = p,
  width = 12, height = 6, dpi = 300
)