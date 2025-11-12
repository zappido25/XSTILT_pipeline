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
incomplete_runs <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/incomplete_log_all.txt"),
                header = TRUE, sep = "\t")  


files <- list.files(kai_results_path, pattern = city, full.names = TRUE)
print(files)
print(length(files))

# main loop start here
# saves the enhancements and sectoral data to a dataframe

for (ii in files) {
          
     
        kai_readin <- t(read.table(ii, header = FALSE, sep = ",", skip = 1))
        colnames(kai_readin) <- enh_col_kai
     
      
      kai_out <- as.data.frame(kai_readin)

      if (!exists("kai_list")) kai_list <- list()
      kai_list[[length(kai_list) + 1]] <- kai_out
    }



# Final per-method dataframes

kai_df <- dplyr::bind_rows(kai_list)
print(dim(kai_df))

# check nrows of ood data
kai_df <- parse_times(kai_df)

kai_df <- remove_outliers(kai_df) 
print(dim(kai_df))
stop("debug")





# Scatter plot: KAI EPC vs CC EPC, and KAI ENH vs CC ENH

# Ensure numeric columns
kai_df$int_epc_mean <- as.numeric(kai_df$int_epc_mean)
cc_df$int_epc_mean  <- as.numeric(cc_df$int_epc_mean)
kai_df$enh_mean     <- as.numeric(kai_df$enh_mean)
cc_df$enh_mean      <- as.numeric(cc_df$enh_mean)



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
print(mean(kai_cx_merged$kai_epc, na.rm=TRUE))
print(mean(kai_cx_merged$cc_epc, na.rm=TRUE))
print(mean(kai_cx_merged$shp_epc, na.rm=TRUE))

print(mean(kai_df$int_epc_mean, na.rm=TRUE))
print(mean(cc_df$int_epc_mean, na.rm=TRUE))
print(mean(shp_df$int_epc_mean, na.rm=TRUE))

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

dev.off()

