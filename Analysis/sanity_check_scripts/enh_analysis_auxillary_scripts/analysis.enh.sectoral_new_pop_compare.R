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


# save the results to a PDF in the folder "figures"
# pdf_filename_emi_conv <- paste0("figures/", city, "_Seasonality.pdf")
# pdf(pdf_filename_emi_conv, width = 8, height = 6)
pdf_filename_emi_conv <- paste0("sanity_check_figures/", city, "_pop_comp.pdf")
pdf(pdf_filename_emi_conv, width = 8, height = 6)

# get enhancements and sectoral data folders
enh_folder=file.path(out.path, "ENHANCEMENTS", city)
sect_folder=file.path(out.path, "SECTORAL", city)

enh_folder_pop=file.path(out.path, "ENHANCEMENTS_pop", city)
sect_folder_pop=file.path(out.path, "SECTORAL_pop", city)



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

      if (timestr < '2021000000' || timestr %in% incomplete_runs$timestr) {
              print("skipping runs outside date range or failed runs")
              print(paste0("CITY::", ss))
              print(paste0("Timestr::", timestr))
              next
      }
  
      enh_shp_file <- file.path(enh_folder, paste0(city, "_", timestr, "_MC_brute_shp.txt"))
      enh_cc_file <- file.path(enh_folder, paste0(city, "_", timestr, "_MC_brute.txt"))
      enh_shp_file_pop <- file.path(enh_folder_pop, paste0(city, "_", timestr, "_MC_brute_shp.txt"))
      enh_cc_file_pop <- file.path(enh_folder_pop, paste0(city, "_", timestr, "_MC_brute.txt"))
      
      
           

      # Read the data from the files, skipping the first row and transposing
      enh_shp_readin <- t(read.table(enh_shp_file, header = FALSE, sep = ",", skip = 1))
      enh_cc_readin <- t(read.table(enh_cc_file,header = FALSE, sep = ",", skip = 1) ) # transpose the table
      enh_shp_readin_pop <- t(read.table(enh_shp_file_pop, header = FALSE, sep = ",", skip = 1))
      enh_cc_readin_pop <- t(read.table(enh_cc_file_pop,header = FALSE, sep = ",", skip = 1) ) # transpose the table

      colnames(enh_cc_readin) <- enhancement_cols
      colnames(enh_shp_readin) <- enhancement_cols
      colnames(enh_cc_readin_pop) <- enhancement_cols
      colnames(enh_shp_readin_pop) <- enhancement_cols

      # Check if na_count is 100000 in either file
      # indicates that the enhamcements and sectorla data is empty or filled with NaNs
      # if (as.numeric(enh_cc_readin[,"na_count"]) == 100000 ) {
      #   cat("Skipping iteration due to city clustering na_count == 100000 for timestr:", timestr, "\n")
      #   next
      # } else {
      #   sect_cc_readin <- t(read.table(sect_cc_file,header = FALSE, sep = ",", skip = 1) ) # transpose the table
      #   # # Add column names to enhacnements and sectoral files
      #   colnames(sect_cc_readin) <- sector_cols
      # }
      # if (as.numeric(enh_shp_readin[,"na_count"]) == 100000) {
      #   cat("Skipping iteration due to Umich na_count == 100000 for timestr:", timestr, "\n")
      #   next
      # } else {
      #   sect_shp_readin <- t(read.table(sect_shp_file, header = FALSE, sep = ",", skip = 1))
      #   # # Add column names to enhacnements and sectoral files
      #   colnames(sect_shp_readin) <- sector_cols
      # }

      # Append the entire enh_shp_readin to a new list for each iteration
      if (!exists("shp_list")) shp_list <- list()
      if (!exists("cc_list")) cc_list <- list()
      if (!exists("shp_list_pop")) shp_list_pop <- list()
      if (!exists("cc_list_pop")) cc_list_pop <- list()


      # Combine enhancement and sectoral data by columns, then add to the lists
      shp_combined <- as.data.frame(enh_shp_readin)
      cc_combined  <- as.data.frame(enh_cc_readin)
      shp_combined_pop <- as.data.frame(enh_shp_readin_pop)
      cc_combined_pop  <- as.data.frame(enh_cc_readin_pop)

      shp_list[[length(shp_list) + 1]] <- shp_combined
      cc_list[[length(cc_list) + 1]]  <- cc_combined
      shp_list_pop[[length(shp_list_pop) + 1]] <- shp_combined_pop
      cc_list_pop[[length(cc_list_pop) + 1]]  <- cc_combined_pop

    }
}


# Final per-method dataframes
shp_df <- dplyr::bind_rows(shp_list)
cc_df  <- dplyr::bind_rows(cc_list)
shp_df_pop <- dplyr::bind_rows(shp_list_pop)
cc_df_pop  <- dplyr::bind_rows(cc_list_pop)


# check nrows of ood data
message("Rows → SHP = ", nrow(shp_df), " ; CLUSTER = ", nrow(cc_df), " ; shp_pop = ", nrow(shp_df_pop), " ; cc_pop = ", nrow(cc_df_pop), "\n")

# MAIN DATAFRAME with pasrsed timestrs
shp_df <- parse_times(shp_df)
cc_df  <- parse_times(cc_df)
shp_df_pop <- parse_times(shp_df_pop)
cc_df_pop  <- parse_times(cc_df_pop)
## add method to the dataframe
shp_df <- shp_df %>% mutate(method = "Umich")
cc_df  <- cc_df  %>% mutate(method = "CC")
shp_df_pop <- shp_df_pop %>% mutate(method = "Umich_pop")
cc_df_pop  <- cc_df_pop  %>% mutate(method = "CC_pop")


# Scatter plot: KAI EPC vs CC EPC, and KAI ENH vs CC ENH

# Ensure numeric columns

cc_df$int_epc_mean  <- as.numeric(cc_df$int_epc_mean)
shp_df$int_epc_mean  <- as.numeric(shp_df$int_epc_mean)
cc_df_pop$int_epc_mean  <- as.numeric(cc_df_pop$int_epc_mean)
shp_df_pop$int_epc_mean  <- as.numeric(shp_df_pop$int_epc_mean)

cc_df$enh_mean      <- as.numeric(cc_df$enh_mean)
shp_df$enh_mean      <- as.numeric(shp_df$enh_mean)
cc_df_pop$enh_mean      <- as.numeric(cc_df_pop$enh_mean)
shp_df_pop$enh_mean      <- as.numeric(shp_df_pop$enh_mean)

shp_df <- remove_outliers(shp_df)
cc_df  <- remove_outliers(cc_df)
shp_df_pop <- remove_outliers(shp_df_pop)
cc_df_pop  <- remove_outliers(cc_df_pop)

merged <- list(
  cc_df  %>% select(city, timestr, cc_epc  = int_epc_mean, cc_enh  = enh_mean),
  shp_df %>% select(city, timestr, shp_epc = int_epc_mean, shp_enh = enh_mean),
  cc_df_pop  %>% select(city, timestr, cc_epc_pop  = int_epc_mean, cc_enh_pop  = enh_mean),
  shp_df_pop %>% select(city, timestr, shp_epc_pop = int_epc_mean, shp_enh_pop = enh_mean)
) %>%
  reduce(inner_join, by = c("city", "timestr"))

# Scatter plot for EPC
p_cc_epc <- ggplot(merged, aes(x = cc_epc, y = cc_epc_pop)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "CC GPW vs pop_new",
    x = "GPW int_epc_mean",
    y = "pop_new"
  ) +
  theme_minimal()
p_shp_epc <- ggplot(merged, aes(x = shp_epc, y = shp_epc_pop)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "SHP GPW vs pop_new",
    x = "GPW int_epc_mean",
    y = "pop_new"
  ) +
  theme_minimal()
# Scatter plot for EPC vs SHP

p_cc_enh <- ggplot(merged, aes(x = cc_enh, y = cc_enh_pop)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "cc GPW vs pop_new",
    x = "GPW int_enh_mean",
    y = "pop int_enh_mean"
  ) +
  theme_minimal()
p_shp_enh <- ggplot(merged, aes(x = shp_enh, y = shp_enh_pop)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "SHP GPW vs pop_new",
    x = "GPW int_enh_mean",
    y = "pop int_enh_mean"
  ) +
  theme_minimal()





print(p_cc_epc)
print(p_shp_epc)
print(p_shp_enh)
print(p_cc_enh)



dev.off()

