##Code used to generate output files for the post-processing of Xstilt based on Kai Wilmots approach
### Carlton Xavier 14.04.2025
### slurm script to generate post-processing outputs for X-STILT, uses the function output_functions()

# the $XSTILT_PATH is set by executing Login_init.sh
homedir = Sys.getenv("XSTILT_PATH")
xstilt_wd = file.path(homedir)
print(xstilt_wd) 
setwd(xstilt_wd); source('r/dependencies.r')

r_files <- list.files('Post-processing/functions', pattern = '\\.r$', full.names = TRUE)
invisible(lapply(r_files, source))



### data directory containng the population data
data_dir= "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data"

# choose the site
site = 'Phoenix'
oco.ver     = 'V11r'
oco.sensor  = c('OCO-2', 'OCO-3')[1]
input.path  = file.path(homedir, 'OCO2_2021_test/')
store.path  = file.path(input.path, '/XSTILT_output', site, oco.ver)
out.path    = list.files(store.path, pattern = 'out_20', full.names = T)
outerr.path    = list.files(store.path, pattern = 'outerr', full.names = T)
# Filter out files ending with .png from out.path
out.path <- out.path[!grepl("\\.png$", out.path)]
outerr.path <- outerr.path[!grepl("\\.txt$", outerr.path)]


# all.timestr = strsplit.to.df(basename(out.path))$V2
oco.path    = file.path(input.path, oco.sensor, paste0('L2_Lite_FP_', oco.ver))

byid.path = file.path(out.path, 'by-id')
foot.fns  = list.files(byid.path, 'X_foot.nc', full.names = T, recursive = T)
traj.fns  = list.files(byid.path, 'traj.rds', full.names = T, recursive = T)

### horizontal error path
byiderr.path = file.path(outerr.path, 'by-id')
trajerr.fns  = list.files(byiderr.path, 'traj.rds', full.names = T, recursive = T)

# print(byiderr.path)
# print(trajerr.fns)

## read the population clusete tif
pop_data <- file.path(data_dir, "gpw_v4_population_density_rev11_2020_30_sec.tif")

  

  
# slurm options
timeout = 12 * 60 * 60              # time allowed before terminations in sec
job_time = '8:00:00'               # total job time
slurm_account = 'lin-np'
slurm_partition = 'lin-np'

# Define parameters for the function
    params <- data.frame(
    foot_fn = foot.fns,          # Footprint file paths
    trajerr_fn = trajerr.fns,    # Trajectory error file paths
    traj_fn=traj.fns,          # Trajectory file paths
    pop_data = pop_data,          # Population data file path
    site= site
    )
slurm_options = list(time = '4:00:00', 
                    account = slurm_account, 
                    partition = slurm_partition,
                    mem = '380000')

slurm_apply(
    f = generate_outputtxtfiles,  # Function to execute
    param=params,
    nodes = 1,            # Number of nodes
    cpus_per_node = 1,    # Number of CPUs per node
    slurm_options = slurm_options,     # SLURM options
    jobname = "generate_postprocess"   # Job name
    )              