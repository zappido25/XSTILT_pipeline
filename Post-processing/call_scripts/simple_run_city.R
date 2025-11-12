#!/usr/bin/env Rscript

library(rslurm)
library(dplyr)

## ---------------- User settings ----------------

source("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Post-processing/call_scripts/city_specific_run.r")
# Cities: each becomes its own Slurm task

homedir = Sys.getenv("XSTILT_PATH")
xstilt_wd = file.path(homedir)
setwd(xstilt_wd)

# lin20
# cities <- c(
#   "Atlanta","Austin","Baltimore","Cleveland","Columbus",
#   "Dallas_FortWorth","Detroit","Houston","Indianapolis",
#   "NewYork","Philadelphia","Portland","SanDiego",
#   "Washington","Denver"
# )

cities=c("Seattle")

# scratch
# cities <- c(
#   "Chicago","Phoenix","Miami")

# lin26
# cities <- c(
#   "Orlando")



# Slurm settings
slurm_account   <- "lin-np"
slurm_partition <- "lin-shared-np"
slurm_options <- list(
  time     = "48:00:00",
  account  = slurm_account,
  partition= slurm_partition,
  mem      = "380000"
)



for (city in cities) {



  ## ------------- Submit one Slurm task per city -------------

  params <- data.frame(city_df=city)
  print(params)

  # Unique jobname per run
  run_tag  <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", sample(1000:9999, 1))
  job_name <- paste0("GRAPES_city_conv_", run_tag)

  job <- slurm_apply(
    f               = run_city_specific_conv,
    params          = params,
    nodes           = nrow(params),
    cpus_per_node   = 1,
    jobname         = job_name,
    slurm_options   = slurm_options
    )
  

  cat("Submitted job array: ", job_name, " with ", nrow(params), " city-tasks.\n")
  }