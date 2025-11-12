# create dataframes for enhancements calculations
library('rslurm')
timeout = 12 * 60 * 60              # time allowed before terminations in sec
job_time = '8:00:00'               # total job time
slurm_account = 'lin-np'
slurm_partition = 'lin-shared-np'

slurm_options = list(time = '24:00:00', 
                     account = 'lin-np', 
                     partition = 'lin-shared-np',
                     mem = '380000')

homedir = Sys.getenv("XSTILT_PATH")
xstilt_wd = file.path(homedir)
setwd(xstilt_wd)

# rslurm_out_path=file.path(Sys.getenv("XSTILT_PATH"), "rslurm_out")

# if (!dir.exists(rslurm_out_path)) {
#   dir.create(rslurm_out_path, recursive = TRUE)
# }


r_files <- list.files('Post-processing/functions/enh_calcs', pattern = '\\.r$', full.names = TRUE)
invisible(lapply(r_files, source))
print(r_files)

# reset wd to direct _rslum_** files to the rslurm_out_path
# setwd(rslurm_out_path)
args <- commandArgs(trailingOnly = TRUE)

run_failed_jobs=FALSE


if (length(args) >= 1 && !is.na(args[1]) && nzchar(args[1])) {
  run_failed_jobs <- as.logical(args[1])
  print(paste0("Run failed jobs mode is ", ifelse(run_failed_jobs, "ON", "OFF")))
}

if (length(args) >= 2 && !is.na(args[2]) && nzchar(args[2])) {
  step_number <- as.numeric(args[2])
  print(paste0("Step number is ", step_number))
}
if (length(args) >= 3 && !is.na(args[3]) && nzchar(args[3])) {
  city <- as.character(args[3])
  print(paste0("Selected city is ", city))
}

# create dataframe

df=data.frame(
  run_failed_jobs=run_failed_jobs,
  city=city
  )

if (step_number==4) {
  slurm_job <- slurm_apply(
                f = create.binned.df,  # Function to execute
                params = df,
                nodes = 1,            # Number of nodes
                cpus_per_node = 2,    # Number of CPUs per node
                slurm_options = slurm_options,     # SLURM options
                jobname = paste0(city,"overpass_binned_df")#"generate_postprocess"   # Job name
                # tmp_path =rslurm_out_path
                )
} else if (step_number==5) {
  slurm_job <- slurm_apply(
                f = overpass.output.enhancements,  # Function to execute
                params = df,
                nodes = 2,            # Number of nodes
                cpus_per_node = 40,    # Number of CPUs per node
                slurm_options = slurm_options,     # SLURM options
                jobname = paste0(city,"enhancements")   # Job name
                # tmp_path = rslurm_out_path
                )
                
} else {
  print("No valid step number provided.")
}
