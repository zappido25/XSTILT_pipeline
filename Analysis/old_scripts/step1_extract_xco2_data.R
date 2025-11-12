# Load necessary libraries
library(readr)
library(rslurm)

extract_and_save_xco2 <- function(out_dir) {
  particles_dir <- file.path(out_dir, "particles")
  print(particles_dir)
  
  if (!dir.exists(particles_dir)) {
    return(data.frame(out_dir = out_dir, status = "No particles directory"))
  }
  
  files <- list.files(path = particles_dir, pattern = "\\.rds$", full.names = TRUE)
  
  if (length(files) == 0) {
    return(data.frame(out_dir = out_dir, status = "No .rds files found"))
  }
  
  print(paste("Found files for:", out_dir))
  
  extract_info <- function(file_path) {
    data <- tryCatch({
      readRDS(file_path)
    }, error = function(e) {
      message(paste("Error reading file:", file_path, ":", e$message))
      return(NULL)
    })
    if (is.null(data)) {
      return(NULL)  # Skip processing if the file could not be read
    }
    
    print(names(data))
 
    if (!"receptor" %in% names(data)) {
      print("here")
      return(NULL)  # Skip files without the expected structure
    }
    
    receptor_info <- data$receptor
    if (!all(c("lati", "long", "XCO2", "XCO2.uncert") %in% names(receptor_info))) {
      return(NULL)  # Skip files missing expected fields
    }
    
    return(data.frame(
      lati = receptor_info$lati,
      long = receptor_info$long,
      XCO2 = receptor_info$XCO2,
      XCO2_uncert = receptor_info$XCO2.uncert
    ))
  }
  
  for (file in files) {
    print(paste("Processing file:", file))
    extracted_data=extract_info(file)
    stop("Error: Debugging")
  }
  # extracted_data <- do.call(rbind, lapply(files, extract_info))
  # print(extracted_data)
  
  if (is.null(extracted_data) || nrow(extracted_data) == 0) {
    return(data.frame(out_dir = out_dir, status = "No valid data extracted"))
  }
  
  year_month <- extract_year_month(out_dir)
  output_file <- file.path(out_dir, paste0("xco2_data_", year_month$year, year_month$month, ".csv"))
  
  write.csv(extracted_data, file = output_file, row.names = FALSE)
  
  return(data.frame(out_dir = out_dir, status = "Completed"))
}

# Function to extract year and month from the directory name
extract_year_month <- function(directory_path) {
  # Extract year and month from the directory name (assumes format "out_YYYYMMDD")
  matches <- regexpr("\\d{8}", basename(directory_path))  # Match the first 8 digits (YYYYMMDD)
  year_month <- substr(basename(directory_path), start = regexpr("\\d{8}", basename(directory_path)), 
                       stop = regexpr("\\d{8}", basename(directory_path)) + 7)
  year <- substr(year_month, 1, 4)
  print(year)
  month <- substr(year_month, 5, 6)
  print(month)
  return(list(year = as.integer(year), month = sprintf("%02d", as.integer(month))))
}

# Define the base directory containing the out_* directories
base_directory <- "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/OCO2_2021_test/XSTILT_output/Phoenix/V11r/"

# Find all "out_*" directories
out_dirs <- list.dirs(path = base_directory, recursive = FALSE, full.names = TRUE)
out_dirs <- out_dirs[grepl("/out_[^/]+$", out_dirs)]

# Convert to data frame for slurm_apply
out_dirs_df <- data.frame(out_dir = out_dirs)


extract_and_save_xco2(out_dirs_df)
# Define SLURM options
# slurm_options <- list(time = '3:00:00', 
#                       account = 'owner-guest', 
#                       partition = 'notchpeak-shared-guest', 
#                       mem = "0")

# # Submit jobs to SLURM using rslurm
# sjob <- slurm_apply(extract_and_save_xco2, out_dirs_df, 
#                     jobname = "extract_and_save_xco2",
#                     global_objects = c("extract_and_save_xco2","extract_year_month"),
#                     nodes = 5, cpus_per_node = 4, 
#                     slurm_options = slurm_options)


