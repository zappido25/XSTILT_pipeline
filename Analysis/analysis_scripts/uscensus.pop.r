
# year.by.year.city.population=function(city_name, year){
  library(dplyr)
  library(stringr)
  library(readr)
  pop.path <- "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data"

  # pop_files <- list.files(pop.path, pattern = paste0(year, "\\.csv$"), full.names = TRUE)
  pop_files <- list.files(pop.path, pattern =  "\\.csv$", full.names = TRUE)

  print(pop_files)

  # Read all CSVs into a list
  pop_list <- list()
  years <- sub(".*([0-9]{4})\\.csv$", "\\1", basename(pop_files))

  # cities_file <- file.path("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Cities_input_files/simulated_cities_modnames.txt")
  cities_file <- file.path("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Cities_input_files/CONUS_cities_list.txt")
  cities <- read.table(cities_file, header = FALSE, skip = 0, stringsAsFactors = FALSE)[, 1]

  pop_results <- data.frame(city = character(),
                            year = integer(),
                            population = numeric(),
                            stringsAsFactors = FALSE)

  # Automatically insert spaces before uppercase letters (except the first one)
  insert_spaces <- function(x) {
    gsub("([a-z])([A-Z])", "\\1 \\2", x)
  }



  for (city_name in cities){
      city_out=city_name

      city=insert_spaces(city_out)
      if (city == "Portland") city <- "Portland, OR"
      if (city == "Phoenix") city <- "Phoenix--Mesa"
      if (city == "Dallas_Fort Worth") city <- "Dallas--Fort Worth"
      if (city == "San Francisco_San Jose") city <- "San Francisco--Oakland"
      if (city == "Indianapolis") city <- "Indianapolis, IN"
      if (city == "Columbus") city <- "Columbus, OH"
      if (city == "Cleveland") city <- "Cleveland, OH"
      # city <- sub("^New York$", "NewYork", city)
      # city <- sub("^Los Angeles$", "LosAngeles", city)
      # city <- sub("^San Diego$", "SanDiego", city)
      # city <- sub("^Salt Lake City$", "SaltLakeCity", city)
      message("city_in: ", city_name)
      message("city_out: ", city)
    for (f in pop_files) {
      message("Reading: ", f)
      year_readin= as.integer(sub(".*([0-9]{4})\\.csv$", "\\1", basename(f)))
      df <- read.csv(f, skip = 1, header = TRUE,stringsAsFactors = FALSE)
      # print(colnames(df))
      
      
      df_city_pop = df[,c("Geo_NAME","SE_A00001_001")]
      colnames(df_city_pop) <- c("city_name", "population")
        # Add a year column
      df_city_pop$year <- year_readin
      # print(dim(df_city_pop))
      match_idx <- grep(tolower(city), tolower(df_city_pop$city_name))
      print(match_idx)
      city_pop <- mean(as.numeric(df_city_pop$population[match_idx]), na.rm = TRUE)

      pop_results <- rbind(pop_results, data.frame(
        city = city_out,
        year = year_readin,
        population = city_pop
      ))
      print(city_name)
      message("  ✅ Found ", length(match_idx), " match(es) → Pop = ", round(city_pop, 0))
    }
   
}
# }
pop_results <- pop_results %>% arrange(city, year)
print(head(pop_results, 10))
# # --- Optional: pivot wide for easy comparison ---
pop_wide <- tidyr::pivot_wider(pop_results, names_from = year, values_from = population)

# --- Save results ---
out_file <- file.path(pop.path, "City_Population_By_Year_update.csv")
write_csv(pop_results, out_file)

# cat("\n✅ Population extraction complete.\n")
# cat("Saved to:", out_file, "\n")
# print(head(pop_results, 10))
# --- Combine into one dataframe ---
# pop_all <- bind_rows(pop_list)
# print(dim(pop_all))
# # --- Optional: reshape wide (cities × years) ---
# pop_wide <- tidyr::pivot_wider(pop_all, names_from = year, values_from = population)

# # --- Print or inspect ---
# print(dim(pop_wide))
