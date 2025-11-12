library(ggplot2)
library(dplyr)

group = Sys.getenv("group")
version = Sys.getenv("version")
out.path=Sys.getenv("OUT_DF_DIR")
r_files <- list.files('/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/plotting_XSTILT/get.urban.extent.r')
 
print(r_files)
invisible(lapply(r_files, source))
OCO.DIR = Sys.getenv("OCO2_DIR")
overpass_to_model <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/overpass_to_model.txt"),
                header = TRUE, sep = ",")  # Replace "data.txt" with your file name
incomplete_runs <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/incomplete_log_all.txt"),
                header = TRUE, sep = "\t")  


city=c("Phoenix")

enh_folder=file.path(out.path, "ENHANCEMENTS", city)

# columns for enhancements
colnames_readin = c("city", "timestr", "enh_mean", "enh_sd", "int_mean", 
                         "int_sd", "int_epc_mean", "int_epc_sd", "pps_mean", "pps_sd", 
                         "bg_pps_mean", "bg_pps_sd", "gdp_mean", "gdp_sd", "bg_gdp_mean", "bg_gdp_sd", 
                         "consumption_mean", "consumption_sd",
                          "city_fr_mean", "city_fr_sd", "lights_mean", "lights_sd", "na_count")
for (ss in city) {
    matching_indices <- which(overpass_to_model$site %in% ss)

# Print matching indices for verification
    print(ss) 
    print(matching_indices)

    
    for (ii in matching_indices) {
   
        site=ss
        timestr=overpass_to_model$timestr[ii]
    
        if (timestr < '2015000000' || timestr %in% incomplete_runs$timestr) {
            print("skipping runs outside date range or failed runs")
            print(paste0("CITY::", ss))
            print(paste0("Timestr::", timestr))
            next
        }
 
        enh_shp_file <- file.path(enh_folder, paste0(city, "_", timestr, "_MC_brute_shp.txt"))
        enh_cc_file <- file.path(enh_folder, paste0(city, "_", timestr, "_MC_brute.txt"))
        # transpose the table

        enh_shp_readin <- t(read.table(enh_shp_file, header = FALSE, sep = ",", skip = 1))
        enh_cc_readin <- t(read.table(enh_cc_file,header = FALSE, sep = ",", skip = 1) ) # transpose the table
        
        # Add column names to enh_cc_readin and enh_shp_readin
        colnames(enh_cc_readin) <- colnames_readin
        colnames(enh_shp_readin) <- colnames_readin
        #
     

        # Append the entire enh_shp_readin to a new list for each iteration
        if (!exists("shp_list")) shp_list <- list()
        if (!exists("cc_list")) cc_list <- list()
        
        shp_list[[length(shp_list) + 1]] <- enh_shp_readin
        cc_list[[length(cc_list) + 1]] <- enh_cc_readin
        
    }
}

# Convert shp_list and cc_list to data frames
shp_df <- do.call(rbind, lapply(shp_list, as.data.frame))
cc_df <- do.call(rbind, lapply(cc_list, as.data.frame))

# Convert enh_mean to numeric (in case it's character)
shp_df$enh_mean <- as.numeric(shp_df$enh_mean)
cc_df$enh_mean <- as.numeric(cc_df$enh_mean)
shp_df$int_mean <- as.numeric(shp_df$int_mean)
cc_df$int_mean <- as.numeric(cc_df$int_mean)

# Add method labels
shp_df$method <- "Umich"
cc_df$method <- "City Clustering"

# Combine both datasets
combined_df <- rbind(shp_df, cc_df)


# Create comparison plots
# 1. Box plot comparison
# pdf_filename <- file.path(enh_folder, paste0(city, "_enhancement_comparison_plots.pdf"), 
#                          width = 10, height = 8)
# pdf(pdf_filename, width = 10, height = 8)
p1 <- ggplot(combined_df, aes(x = method, y = enh_mean, fill = method)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.6) +
  labs(title = paste("Enhancement Mean Comparison for", city),
       x = "Method", 
       y = "Enhancement Mean (ppm)",
       fill = "Method") +
  theme_minimal() +
  scale_fill_manual(values = c("Umich" = "#3498db", "City Clustering" = "#e74c3c"))

print(p1)

p2 <- ggplot(combined_df, aes(x = method, y = int_mean, fill = method)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.6) +
  labs(title = paste("Intergrated Enhancement Mean Comparison for", city),
       x = "Method", 
       y = "Intergated Enhancement Mean (ppm)",
       fill = "Method") +
  theme_minimal() +
  scale_fill_manual(values = c("Umich" = "#3498db", "City Clustering" = "#e74c3c"))

print(p2)

# 2. Scatter plot for direct comparison
# Match observations by index (assuming same order)
if(nrow(shp_df) == nrow(cc_df)) {
  comparison_df <- data.frame(
    shp_enh = shp_df$enh_mean,
    cc_enh = cc_df$enh_mean,
    timestr = shp_df$timestr
  )
}
  


# # Optionally, also keep the original scatter plot with colored points
# correlation <- cor(comparison_df$shp_enh, comparison_df$cc_enh, use = "complete.obs")

# p2 <- ggplot(comparison_df) +
#   geom_point(aes(x = shp_enh, y = cc_enh), color = "#3498db", alpha = 0.7, size = 3) +
#   geom_point(aes(x = cc_enh, y = shp_enh), color = "#e74c3c", alpha = 0.7, size = 3) +
#   geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
#   labs(title = paste("Mean Enhancement: Shapefile vs City Clustering for", city),
#        x = "Umich Enhancement Mean (ppm)",
#        y = "City Clustering Enhancement Mean (ppm)") +
#   theme_minimal() +
#   coord_equal() +
#   annotate("text", 
#              x = min(comparison_df$shp_enh, na.rm = TRUE), 
#              y = max(comparison_df$cc_enh, na.rm = TRUE), 
#              label = paste("Correlation =", round(correlation, 3)),
#              hjust = 0, vjust = 1, size = 5, color = "black")
#   print(p2)
  
#   # Calculate correlation
#   cat("Correlation between Shapefile and City Clustering enh_mean:", round(correlation, 3), "\n")
# }

# 3. Time series comparison (if timestr can be converted to dates)
combined_df$timestr <- as.character(combined_df$timestr)
combined_df$date <- as.POSIXct(combined_df$timestr, format = "%Y%m%d%H", tz = "UTC")

p3 <- ggplot(combined_df, aes(x = date, y = enh_mean, color = method)) +
  geom_line(alpha = 0.7) +
  geom_point(alpha = 0.8) +
  labs(title = paste("Enhancement Mean Time Series for", city),
       x = "Date", 
       y = "Enhancement Mean (ppm)",
       color = "Method") +
  theme_minimal() +
  scale_color_manual(values = c("Umich" = "#3498db", "City Clustering" = "#e74c3c"))

# print(p3)

# 4. Summary statistics
summary_stats <- combined_df %>%
  group_by(method) %>%
  summarise(
    mean_enh = mean(enh_mean, na.rm = TRUE),
    median_enh = median(enh_mean, na.rm = TRUE),
    sd_enh = sd(enh_mean, na.rm = TRUE),
    min_enh = min(enh_mean, na.rm = TRUE),
    max_enh = max(enh_mean, na.rm = TRUE),
    count = n(),
    .groups = 'drop'
  )

print("Summary Statistics:")
print(summary_stats)

# 5. Difference plot (if same number of observations)
if(nrow(shp_df) == nrow(cc_df)) {
  comparison_df$diff <- comparison_df$shp_enh - comparison_df$cc_enh
  comparison_df$date <- as.POSIXct(shp_df$timestr, format = "%Y%m%d%H", tz = "UTC")
  
  p4 <- ggplot(comparison_df, aes(x = date, y = diff)) +
    geom_line(alpha = 0.7) +
    geom_point(alpha = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = paste("Enhancement Mean Difference (Shapefile - City Clustering) for", city),
         x = "Date", 
         y = "Difference in Enhancement Mean (ppm)") +
    theme_minimal()
  
  # print(p4)
  
  # dev.off()

  
  cat("Mean difference (Shapefile - City Clustering):", round(mean(comparison_df$diff, na.rm = TRUE), 3), "ppm\n")
  cat("Standard deviation of difference:", round(sd(comparison_df$diff, na.rm = TRUE), 3), "ppm\n")
}
