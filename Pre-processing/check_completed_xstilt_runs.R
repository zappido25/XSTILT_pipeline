# Step 1:checks if the xstilt runs are successful based on the presence of X_foot.nc files are present in the by-id folders
# Step 2: outputs a log file with the cities and timestr of the runs that are not successful

OCO.DIR = Sys.getenv("OCO2_DIR")
lin26 = Sys.getenv("lin26")
oco.ver = "V11r"

overpass_to_model <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/overpass_to_model.txt"),
                header = TRUE, sep = ",")  # Replace "data.txt" with your file name
nrecp <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/nrecp.txt"),
                header = TRUE, sep = ",")  # Replace "data.txt" with your file name
cities <- read.table(
  "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Cities_input_files/simulated_cities.txt",
  header = FALSE,
  sep = ","
)
# cities=unique(overpass_to_model$site)
print(cities)
# Step1:  Check if 'X_foot.nc' is present in all 'by-id' folders
check_foot_files <- function(byid.path) {
    subdirs <- list.dirs(byid.path, full.names = TRUE, recursive = FALSE)
    results <- sapply(subdirs, function(subdir) {
        foot.fns <- list.files(subdir, 'X_foot.nc', full.names = TRUE, recursive = FALSE)
        # print(paste0("Checking subdir: ", subdir, " - Found files: ", length(foot.fns)))
        # print(length(foot.fns) > 0)
        # if (length(foot.fns) == 0) {
        #     print(paste0("No X_foot.nc files found in subdir: ", subdir))
        # }
        return(length(foot.fns) > 0)  # Return TRUE if 'X_foot.nc' is found, FALSE otherwise
    })
    print(length(results))
    # If >=95% of subdirs have X_foot.nc, treat all as present
    # if (length(results) > 0) {
    #     print(mean(as.logical(results)))
    #     if (mean(as.logical(results)) >= 0.95) {
    #         results <- rep(TRUE, length(results))
    #     }
    # }
    return(results)
}

incomplete_log <- list()  # Initialize an empty list to store results
cities=("Sacramento")
# for (ss in cities$V1) {
for (ss in cities) {
    matching_indices <- which(overpass_to_model$site %in% ss)
    site=ss
    city.path  = file.path(OCO.DIR, '/XSTILT_output/', paste0(site, collapse = "_"), oco.ver)
    # city.path  = file.path(lin26, '/XSTILT_output/', paste0(site, collapse = "_"), oco.ver)
    # city.path  = file.path('/scratch/general/vast/u6065567/XSTILT_output/', paste0(site, collapse = "_"), oco.ver)
    # err.path2  = file.path('/scratch/general/vast/u6065567/XSTILT_output/', paste0(site, collapse = "_"), oco.ver)
  
    
    # Skip this iteration if the city is "skip"
    if (tolower(site) == "skip") {
        cat("Skipping city:", site, "\n")
        next
    }
    for (ii in matching_indices) {
        timestr=overpass_to_model$timestr[ii]
        # print(paste0("CITY::", site))
        # print(paste0("Timestr::", timestr))
        

        # if (!dir.exists(city.path)) {
        #     cat("This city has not been processed:", site, "\n")
        #     next
        # }
        out.path    = list.files(city.path, pattern = paste0('out_', timestr), full.names = T)
        err.path    = list.files(city.path, pattern = paste0('outerr_', timestr), full.names = T)
        # err.path    = list.files(err.path2, pattern = paste0('outerr_', timestr), full.names = T)

        byid.path = file.path(out.path, 'by-id')
        byid_err.path = file.path(err.path, 'by-id')
        foot.fns  = list.files(byid.path, 'X_foot.nc', full.names = T, recursive = T)
        foot_err.fns  = list.files(byid_err.path, 'X_foot.nc', full.names = T, recursive = T)


        results <- check_foot_files(byid.path)
        results_err <- check_foot_files(byid_err.path)
        print(paste0("timestr::", ' ', timestr))
        # print(paste0("length of results: ", length(results),' ',length(results_err),' ', timestr))
        
        # if (length(results) == nrecp$n_recp[ii] && length(results_err) == nrecp$n_recp[ii]) {
            
        #     print(paste0("number of folders in foot.fns and footerr.fns: ", length(foot.fns),' ',length(foot_err.fns),
        #             " number of X_foot.nc files in the byid folder: ",length(results),
        #             " length(nrecp$n_recp): ",nrecp$n_recp[ii],' ', timestr))
        # }
        # print(results)
        if (all(results) == TRUE && all(results_err) == TRUE) {

            print(paste0("X_foot.nc present in all folders: ", length(results),' ',length(results_err),
                    ' ', timestr))
        }


        if (any(results == FALSE) || any(results_err == FALSE)) {
            print(paste0("No .nc files found for timestr: ", timestr, " city: ", site))
            # stop("One or more 'by-id' folders are missing 'X_foot.nc' files. Exiting.")
            
            # incomplete_log <- rbind(incomplete_log, data.frame(site = site, timestr = timestr, stringsAsFactors = FALSE))
            incomplete_log <- rbind(incomplete_log, overpass_to_model[ii, , drop = FALSE])          
        }        
    }
} # end Step 1
    print(incomplete_log)     

    # Step 2:  Save the incomplete_log to a file
    write.table(incomplete_log, file = file.path(OCO.DIR, "/OCO-2/overpass_city/failed_runs_sac.txt"), sep = "\t",
        row.names = FALSE, col.names = TRUE, quote = FALSE)

