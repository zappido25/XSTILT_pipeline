##Code used to generate output files for the post-processing of Xstilt based on Kai Wilmots approach
### Carlton Xavier 14.04.2025
### slurm script to generate post-processing outputs for X-STILT, uses the function output_functions()



# the $XSTILT_PATH is set by executing Login_init.sh


args <- commandArgs(trailingOnly = TRUE)
debug=FALSE
fresh_start=FALSE # creates city and urban txt files by running only the city_convolution function
run_failed_jobs=FALSE
select_city=TRUE



if (length(args) >= 1 && !is.na(args[1]) && nzchar(args[1])) {
  debug <- as.logical(args[1])
  print(paste0("Debug mode is ", ifelse(debug, "ON", "OFF")))
}
if (length(args) >= 2 && !is.na(args[2]) && nzchar(args[2])) {
  fresh_start <- as.logical(args[2])
  print(paste0("Fresh start mode is ", ifelse(fresh_start, "ON", "OFF")))
}
if (length(args) >= 3 && !is.na(args[3]) && nzchar(args[3])) {
  run_failed_jobs <- as.logical(args[3])
  print(paste0("Run failed jobs mode is ", ifelse(run_failed_jobs, "ON", "OFF")))
}
if (length(args) >= 4 && !is.na(args[4]) && nzchar(args[4])) {
  city <- as.character(args[4])
  print(paste0("Selected city is ", city))
}


#slurmoptions
timeout = 12 * 60 * 60              # time allowed before terminations in sec
job_time = '8:00:00'               # total job time
slurm_account = 'lin-np'
slurm_partition = 'lin-shared-np'

# Paths
homedir = Sys.getenv("XSTILT_PATH")
xstilt_wd = file.path(homedir)
setwd(xstilt_wd); 
source('r/dependencies.r')
print(xstilt_wd) 

r_files <- list.files('/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Post-processing/functions/socio_economic_convolutions/', 
            pattern = '\\.r$', full.names = TRUE)
# print(r_files)
for (f in r_files) {
#   cat("Sourcing:", f, "\n")
  source(f, local = TRUE)
}


## read the population cluseter tif
data_dir= "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data"
pop_data <- file.path(data_dir, "Global_2020_PopulationDensity30sec_GPWv4.tiff")
# pop_data <- file.path(data_dir, "gpw_v4_population_density_rev11_2020_30_sec.tif")
print(pop_data)
oco.ver     = 'V11r'
oco.sensor  = c('OCO-2', 'OCO-3')[1]
input.path  = file.path(homedir, '../OCO2_Obs/')
oco.path    = file.path(input.path, oco.sensor, paste0('L2_Lite_FP_', oco.ver))


OCO.DIR = Sys.getenv("OCO2_DIR")
lin26 = Sys.getenv("lin26")

lin26_dir=FALSE
lin20_dir=TRUE
scratch_dir=FALSE
    
if (run_failed_jobs) {
       overpass_to_model <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/failed_runs.txt"),
                header = TRUE, sep = "\t")  
        
} else {
        overpass_to_model <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/overpass_to_model.txt"),
                header = TRUE, sep = ",")  # Replace "data.txt" with your file name
        incomplete_runs <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/failed_runs.txt"),
                header = TRUE, sep = "\t") 
}

# check_time=c("2022081319", "2022111019","2024041219","2024081819")
for (ss in city) {
    matching_indices <- which(overpass_to_model$site %in% ss)

# Print matching indices for verification
    print(ss)
    print(matching_indices)

    for (ii in matching_indices) {
    # for (ii in 1:4) {
    
        site=ss
        # timestr="2024041418"
        # timestr=check_time[ii]
        timestr=overpass_to_model$timestr[ii]
        
        if ( timestr < '2015000000' ) {
        # if ( timestr < '2024040000' ) {
                print("skipping runs before 2018 ")
                print(paste0("CITY::", ss))
                print(paste0("Timestr::", timestr))
                next
            
        }

        if (!run_failed_jobs) {
            if ((timestr %in% incomplete_runs$timestr)) {
                print("skipping failed runs")
                print(paste0("CITY::", ss))
                print(paste0("Timestr::", timestr))
                next
            }
        } 
        
        ### data directory containng the population data
        if (lin26_dir) {
            store.path  = file.path(lin26, '/XSTILT_output', site, oco.ver)   
        } else if (lin20_dir) {
            store.path  = file.path(OCO.DIR, '/XSTILT_output', site, oco.ver)
        } else if (scratch_dir) {
            store.path  = file.path('/scratch/general/vast/u6065567/XSTILT_output', site, oco.ver)
        }   
        # store.path  = file.path(OCO.DIR, '/XSTILT_output', site, oco.ver)
        # store.path  = file.path('/scratch/general/vast/u6065567/XSTILT_output', site, oco.ver)
        # store.path  = file.path(lin26, '/XSTILT_output', site, oco.ver)
        out.path    = list.files(store.path, pattern = paste0('out_', timestr), full.names = T)
        # out.path    = list.files('/scratch/general/vast/u6065567/XSTILT_output/', pattern = paste0('out_', timestr), full.names = T)
        err_out.path  = store.path #list.files(store.path, pattern = paste0('outerr_', timestr), full.names = T) #file.path('/scratch/general/vast/u6065567/XSTILT_output/', paste0(site, collapse = "_"), oco.ver)
        # err_out.path  = file.path('/scratch/general/vast/u6065567/XSTILT_output/', paste0(site, collapse = "_"), oco.ver)

        outerr.path    = list.files(err_out.path, pattern = paste0('outerr_', timestr), full.names = T)
        # outerr.path    = list.files(store.path, pattern = paste0('outerr_', timestr), full.names = T)
        # Filter out files ending with .png from out.path
        out.path <- out.path[!grepl("\\.png$", out.path)]
        outerr.path <- outerr.path[!grepl("\\.txt$", outerr.path)]
        byid.path = file.path(out.path, 'by-id')
        byiderr.path = file.path(outerr.path, 'by-id')
       
        foot.fns  = list.files(byid.path, 'X_foot.nc', full.names = T, recursive = T)
        traj.fns  = list.files(byid.path, 'traj.rds', full.names = T, recursive = T)

        ### horizontal error path
        trajerr.fns  = list.files(byiderr.path, 'traj.rds', full.names = T, recursive = T)
        # print(out.path)
        # print(outerr.path)

        check_foot_files <- function(byid.path) {
            subdirs <- list.dirs(byid.path, full.names = TRUE, recursive = FALSE)
            results <- sapply(subdirs, function(subdir) {
                foot.fns <- list.files(subdir, 'X_foot.nc', full.names = TRUE, recursive = FALSE)
                return(length(foot.fns) > 0)  # Return TRUE if 'X_foot.nc' is found, FALSE otherwise
            })
            unname(results)
        }
        # print(length(foot.fns) > 0)
        # print(length(trajerr.fns) > 0)
        results <- check_foot_files(byid.path)
        results_err <- check_foot_files(byiderr.path)
        # print(out.path)
        # print(outerr.path)
        # print(trajerr.fns)
        if (debug) {
           generate_outputtxtfile_simple(byid.path,byiderr.path, pop_data, results, results_err,
                            site)

        }else {
            
            print(paste0("Slurm job submission for timestr:", timestr))
            # Define parameters for the function
            # Only create params if all vectors have the same length and are not empty
            print(length(foot.fns))
            print(length(trajerr.fns))
            print(length(traj.fns))
            if (length(foot.fns) > 0 && length(trajerr.fns) == length(foot.fns) && length(traj.fns) == length(foot.fns)) {
                print(paste0("Submitting job for timestr:", timestr))
                print(paste("Length of foot.fns:", length(foot.fns)))
                print(paste("Length of trajerr.fns:", length(trajerr.fns)))
                print(paste("Length of traj.fns:", length(traj.fns)))
                params <- data.frame(
                    foot_fn = foot.fns,          # Footprint file paths
                    trajerr_fn = trajerr.fns,    # Trajectory error file paths
                    traj_fn = traj.fns,          # Trajectory file paths
                    pop_data = rep(pop_data, length(foot.fns)),  # Repeat for each row
                    results=results,
                    results_err=results_err,
                    site = rep(site, length(foot.fns),
                    fresh_start=fresh_start)
                )
                # params <- data.frame(
                #     byid.path = byid.path,          # Footprint file paths
                #     byiderr.path  = byiderr.path,    # Trajectory error file paths
                #     pop_data = rep(pop_data, length(foot.fns)),  # Repeat for each row
                #     results=results,
                #     results_err=results_err,
                #     site = rep(site, length(foot.fns))
                # )
            } else {
                warning(paste0("Mismatch in file counts or no files found. Skipping job submission for this timestr:" , timestr))
                print(paste("Length of foot.fns:", length(foot.fns)))
                print(paste("Length of trajerr.fns:", length(trajerr.fns)))
                print(paste("Length of traj.fns:", length(traj.fns)))
                next
            }
        
            print(colnames(params))
            slurm_options = list(time = '08:00:00', 
                                account = slurm_account, 
                                partition = slurm_partition,
                                mem = '380000')
           
            slurm_job <- slurm_apply(
                f = generate_outputtxtfile,  # Function to execute
                params = params,
                nodes = 1,            # Number of nodes
                cpus_per_node = 2,    # Number of CPUs per node
                slurm_options = slurm_options,     # SLURM options
                jobname = paste0(timestr,"_generate_outputs_files")#"generate_postprocess"   # Job name
                )     

                jobname <- paste0(timestr,"_generate_output_files")
                cat("Submitted job with name:", jobname, "\n")
                job_status <- system(paste("squeue --name", jobname, "--format=%i"), intern = TRUE)
                cat("Job ID(s):\n", job_status, "\n")    
            # Run the function in parallel
            # generate_outputtxtfiles(foot.fns, trajerr.fns, traj.fns, pop_data, site)
        }
        
    }
 }



  

  


# # Define parameters for the function
#     params <- data.frame(
#     foot_fn = foot.fns,          # Footprint file paths
#     trajerr_fn = trajerr.fns,    # Trajectory error file paths
#     traj_fn=traj.fns,          # Trajectory file paths
#     pop_data = pop_data,          # Population data file path
#     site= site
#     )
# slurm_options = list(time = '8:00:00', 
#                     account = slurm_account, 
#                     partition = slurm_partition,
#                     mem = '380000')

# slurm_apply(
#     f = generate_outputtxtfiles,  # Function to execute
#     param=params,
#     nodes = 1,            # Number of nodes
#     cpus_per_node = 1,    # Number of CPUs per node
#     slurm_options = slurm_options,     # SLURM options
#     jobname = "generate_postprocess"   # Job name
#     )              