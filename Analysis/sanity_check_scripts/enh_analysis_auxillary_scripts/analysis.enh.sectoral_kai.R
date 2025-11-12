# -- core processing pipeline --
# uses hte sectoral and enhancements data to process the seasonality of the emissions per capita and sectoral 
# contributions to these emissions.

library(ggplot2)
library(dplyr)
library(purrr)

## ploting and parsing functions are here
functions_files <- list.files('/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Analysis/functions/', full.names = TRUE, pattern = "\\.r$")
invisible(lapply(functions_files, source))
print(functions_files)

kai_results_path <- file.path('/uufs/chpc.utah.edu/common/home/lin-group14/KaiW/XSTILT_OCO/enhancements/')
group = Sys.getenv("group")
version = Sys.getenv("version")
out.path=Sys.getenv("OUT_DF_DIR")
OCO.DIR = Sys.getenv("OCO2_DIR")

sector_cols <- c(
  "CHE","ENE","IND","IRO","NEU","NFE","NMM","PRO","PRU_SOL","RCO",
  "REF_TRF","TNR_Aviation_CDS","TNR_Aviation_CRS","TNR_Aviation_LTO",
  "TNR_Other","TNR_Ship","TRO_noRES"
)

# Read in cities file and prompt user to select city
cities_file <- file.path("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Cities_input_files/CONUS_cities_list.txt")
cities <- read.table(cities_file, header = FALSE, skip=1, stringsAsFactors = FALSE)[,1]
cat("Available cities:\n")
for (i in seq_along(cities)) {
  cat(sprintf("%d: %s\n", i, cities[i]))
}

# Force interactive prompt even under Rscript
cat("Enter the number of the city to analyze: ")
city_idx <- as.integer(readLines(file("stdin"), n = 1))

if (is.na(city_idx) || city_idx < 1 || city_idx > length(cities)) {
  stop("Invalid city selection.")
}
city <- cities[city_idx]
cat(paste0("Selected city: ", city, "\n"))


enhancement_cols = c("city", "timestr", "enh_mean", "enh_sd", "int_mean", 
                          "int_sd", "int_epc_mean", "int_epc_sd", "int_epc_gdp_mean", "int_epc_gdp_sd",
                           "int_gdp_epc_mean", "int_gdp_epc_sd","pps_mean", "pps_sd", 
                          "bg_pps_mean", "bg_pps_sd", "gdp_mean", "gdp_sd", "bg_gdp_mean", "bg_gdp_sd", 
                          "consumption_mean", "consumption_sd",
                            "city_fr_mean", "city_fr_sd", "lights_mean", "lights_sd", "na_count")

enh_col_kai=c("city", "timestr", "enh_mean", "enh_sd", "int_mean", "int_sd",  "int_epc_mean", "int_epc_sd", "pps_mean", 
        "pps_sd", "bg_pps_mean", "bg_pps_sd", "gdp_mean", "gdp_sd", 
    "bg_gdp_mean", "bg_gdp_sd", "consumption_mean", "consumption_sd", "city_fr_mean", "city_fr_sd", "na_count")

# save the results to a PDF in the folder "figures"
# pdf_filename_emi_conv <- paste0("figures/", city, "_Seasonality.pdf")
# pdf(pdf_filename_emi_conv, width = 8, height = 6)
pdf_filename_emi_conv <- paste0("figures/", city, "_kaicomp.pdf")
pdf(pdf_filename_emi_conv, width = 8, height = 6)

# get enhancements and sectoral data folders
enh_folder=file.path(out.path, "ENHANCEMENTS", city)
sect_folder=file.path(out.path, "SECTORAL", city)

overpass_to_model <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/overpass_to_model.txt"),
                header = TRUE, sep = ",")  # Replace "data.txt" with your file name
incomplete_runs <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/failed_runs.txt"),
                header = TRUE, sep = "\t")  


# main loop start here
# saves the enhancements and sectoral data to a dataframe

for (ss in city) {
    matching_indices <- which(overpass_to_model$site %in% ss)

    for (ii in matching_indices) {

      timestr=overpass_to_model$timestr[ii]

      if (timestr < '2015000000' || timestr %in% incomplete_runs$timestr) {
              print("skipping runs outside date range or failed runs")
              print(paste0("CITY::", ss))
              print(paste0("Timestr::", timestr))
              next
      }
  
      enh_shp_file <- file.path(enh_folder, paste0(city, "_", timestr, "_MC_brute_shp.txt"))
      enh_cc_file <- file.path(enh_folder, paste0(city, "_", timestr, "_MC_brute.txt"))
      kai_file = file.path(kai_results_path, paste0(city, "_", timestr, ".txt"))
      
      # Try to read kai_file, but don't skip the whole iteration if missing
      if (!file.exists(kai_file)) {
        cat("Skipping kai_file for timestr:", timestr, "\n")
        next
        # kai_readin <- NULL
      } else {
        kai_readin <- t(read.table(kai_file, header = FALSE, sep = ",", skip = 1))
        colnames(kai_readin) <- enh_col_kai
      }


      sect_shp_file <- file.path(sect_folder, paste0(city, "_", timestr, "_MC_brute_shp.txt"))
      sect_cc_file <- file.path(sect_folder, paste0(city, "_", timestr, "_MC_brute.txt"))

      # Read the data from the files, skipping the first row and transposing
      enh_shp_readin <- t(read.table(enh_shp_file, header = FALSE, sep = ",", skip = 1))
      enh_cc_readin <- t(read.table(enh_cc_file,header = FALSE, sep = ",", skip = 1) ) # transpose the table

      colnames(enh_cc_readin) <- enhancement_cols
      colnames(enh_shp_readin) <- enhancement_cols

      # Check if na_count is 100000 in either file
      # indicates that the enhamcements and sectorla data is empty or filled with NaNs
      if (as.numeric(enh_cc_readin[,"na_count"]) == 100000 ) {
        cat("Skipping iteration due to city clustering na_count == 100000 for timestr:", timestr, "\n")
        next
      } else {
        sect_cc_readin <- t(read.table(sect_cc_file,header = FALSE, sep = ",", skip = 1) ) # transpose the table
        # # Add column names to enhacnements and sectoral files
        colnames(sect_cc_readin) <- sector_cols
      }
      if (as.numeric(enh_shp_readin[,"na_count"]) == 100000) {
        cat("Skipping iteration due to Umich na_count == 100000 for timestr:", timestr, "\n")
        next
      } else {
        sect_shp_readin <- t(read.table(sect_shp_file, header = FALSE, sep = ",", skip = 1))
        # # Add column names to enhacnements and sectoral files
        colnames(sect_shp_readin) <- sector_cols
      }

      # Append the entire enh_shp_readin to a new list for each iteration
      if (!exists("shp_list")) shp_list <- list()
      if (!exists("cc_list")) cc_list <- list()
      if (!exists("kai_list")) kai_list <- list()

      # Combine enhancement and sectoral data by columns, then add to the lists
      shp_combined <- cbind(as.data.frame(enh_shp_readin), as.data.frame(sect_shp_readin))
      cc_combined  <- cbind(as.data.frame(enh_cc_readin),  as.data.frame(sect_cc_readin))
      kai_out <- as.data.frame(kai_readin)
      
      shp_list[[length(shp_list) + 1]] <- shp_combined
      cc_list[[length(cc_list) + 1]]  <- cc_combined
      kai_list[[length(kai_list) + 1]] <- kai_out
    }
}


# Final per-method dataframes
shp_df <- dplyr::bind_rows(shp_list)
cc_df  <- dplyr::bind_rows(cc_list)
kai_df <- dplyr::bind_rows(kai_list)

# check nrows of ood data
message("Rows → SHP = ", nrow(shp_df), " ; CLUSTER = ", nrow(cc_df), " ; KAI = ", nrow(kai_df), "\n")

# MAIN DATAFRAME with pasrsed timestrs
shp_df <- parse_times(shp_df)
cc_df  <- parse_times(cc_df)
kai_df <- parse_times(kai_df)

## add method to the dataframe
shp_df <- shp_df %>% mutate(method = "Umich")
cc_df  <- cc_df  %>% mutate(method = "CC")
kai_df <- kai_df %>% mutate(method = "KAI")



# Scatter plot: KAI EPC vs CC EPC, and KAI ENH vs CC ENH

# Ensure numeric columns
kai_df$int_epc_mean <- as.numeric(kai_df$int_epc_mean)
cc_df$int_epc_mean  <- as.numeric(cc_df$int_epc_mean)
shp_df$int_epc_mean  <- as.numeric(shp_df$int_epc_mean)
kai_df$enh_mean     <- as.numeric(kai_df$enh_mean)
cc_df$enh_mean      <- as.numeric(cc_df$enh_mean)
shp_df$enh_mean      <- as.numeric(shp_df$enh_mean)



kai_cx_merged <- list(
  kai_df %>% select(city, timestr, kai_epc = int_epc_mean, kai_enh = enh_mean),
  cc_df  %>% select(city, timestr, cc_epc  = int_epc_mean, cc_enh  = enh_mean),
  shp_df %>% select(city, timestr, shp_epc = int_epc_mean, shp_enh = enh_mean)
) %>%
  reduce(inner_join, by = c("city", "timestr"))

# Scatter plot for EPC
p_kai_vs_cc_epc <- ggplot(kai_cx_merged, aes(x = cc_epc, y = kai_epc)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "KAI EPC vs CC EPC",
    x = "CC int_epc_mean",
    y = "KAI int_epc_mean"
  ) +
  theme_minimal()
# Scatter plot for EPC vs SHP

p_kai_vs_shp_epc <- ggplot(kai_cx_merged, aes(x = shp_epc, y = kai_epc)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "KAI EPC vs SHP EPC",
    x = "CC int_epc_mean",
    y = "KAI int_epc_mean"
  ) +
  theme_minimal()

# Scatter plot for ENH
p_kai_vs_cc_enh <- ggplot(kai_cx_merged, aes(x = cc_enh, y = kai_enh)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "KAI ENH vs CC ENH",
    x = "CC enh_mean",
    y = "KAI enh_mean"
  ) +
  theme_minimal()

p_kai_vs_shp_enh <- ggplot(kai_cx_merged, aes(x = shp_enh, y = kai_enh)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "KAI ENH vs SHP ENH",
    x = "SHP enh_mean",
    y = "KAI enh_mean"
  ) +
  theme_minimal()

print("means of epc before outlier removal:")
# print(mean(kai_cx_merged$kai_epc, na.rm=TRUE))
# print(mean(kai_cx_merged$cc_epc, na.rm=TRUE))
# print(mean(kai_cx_merged$shp_epc, na.rm=TRUE))

print(paste0("kai_df_epc_mean ",mean(kai_df$int_epc_mean, na.rm=TRUE)))
print(paste0("cc_df_int_epc ",mean(cc_df$int_epc_mean, na.rm=TRUE)))
print(paste0("shp_df_int_epc ",mean(shp_df$int_epc_mean, na.rm=TRUE)))


print(p_kai_vs_cc_epc)
print(p_kai_vs_cc_enh)
print(p_kai_vs_shp_epc)
print(p_kai_vs_shp_enh)

shp_df <- remove_outliers(shp_df)
cc_df  <- remove_outliers(cc_df)
kai_df <- remove_outliers(kai_df) 

# Merge by city and timestr for direct comparison (without outliers)

kai_cx_merged_wo <- list(
  kai_df %>% select(city, timestr, kai_epc = int_epc_mean, kai_enh = enh_mean),
  cc_df  %>% select(city, timestr, cc_epc  = int_epc_mean, cc_enh  = enh_mean),
  shp_df %>% select(city, timestr, shp_epc = int_epc_mean, shp_enh = enh_mean)
) %>%
  reduce(inner_join, by = c("city", "timestr"))

# Scatter plot for EPC (without outliers)
p_kai_vs_cc_epc_wo <- ggplot(kai_cx_merged_wo, aes(x = cc_epc, y = kai_epc)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "KAI EPC vs CC EPC (without outliers)",
    x = "CC int_epc_mean",
    y = "KAI int_epc_mean"
  ) +
  theme_minimal()

# Scatter plot for EPC (without outliers)
p_kai_vs_shp_epc_wo <- ggplot(kai_cx_merged_wo, aes(x = shp_epc, y = kai_epc)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "KAI EPC vs SHP EPC (without outliers)",
    x = "SHP int_epc_mean",
    y = "KAI int_epc_mean"
  ) +
  theme_minimal()

# Scatter plot for ENH (without outliers)
p_kai_vs_cc_enh_wo <- ggplot(kai_cx_merged_wo, aes(x = cc_enh, y = kai_enh)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "KAI ENH vs CC ENH (without outliers)",
    x = "CC enh_mean",
    y = "KAI enh_mean"
  ) +
  theme_minimal()

# Scatter plot for ENH (without outliers)
p_kai_vs_shp_enh_wo <- ggplot(kai_cx_merged_wo, aes(x = shp_enh, y = kai_enh)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "KAI ENH vs SHP ENH (without outliers)",
    x = "SHP enh_mean",
    y = "KAI enh_mean"
  ) +
  theme_minimal()

print(p_kai_vs_cc_epc_wo)
print(p_kai_vs_cc_enh_wo)
print(p_kai_vs_shp_epc_wo)
print(p_kai_vs_shp_enh_wo)

print("means of epc after removing outliers:")
print(mean(kai_cx_merged_wo$kai_epc, na.rm=TRUE))
print(mean(kai_cx_merged_wo$cc_epc, na.rm=TRUE))
print(mean(kai_cx_merged_wo$shp_epc, na.rm=TRUE))

print(paste0("kai_df_epc_mean_wo ",mean(kai_cx_merged_wo$kai_epc, na.rm=TRUE)))
print(paste0("cc_df_int_epc_wo ",mean(kai_cx_merged_wo$cc_epc, na.rm=TRUE)))
print(paste0("shp_df_int_epc_wo ",mean(kai_cx_merged_wo$shp_epc, na.rm=TRUE)))

dev.off()

