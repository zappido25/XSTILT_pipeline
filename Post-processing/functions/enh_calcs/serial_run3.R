
library(rslurm)

source("create.binned.df.r")
source("overpass.output.enhancements.r")
# source("overpass.output.enhancements_copy.r")
# city="Orlando"
run_failed_jobs=FALSE
# cities <- c(
# "Minneapolis", 
# )

# cities <- c(
#   "Cleveland","Columbus",
#   "Dallas_FortWorth","Detroit","Houston","Indianapolis"
# )
# cities <- c(
#   "Columbus",
# "Dallas_FortWorth")

# cities=c("Detroit","Houston")
cities=c("Seattle")

# #scratch



for (city in cities){
    print(city)
    create.binned.df(run_failed_jobs=FALSE, city=city)

    # overpass.output.enhancements(run_failed_jobs=FALSE, city=city)
#     # overpass.output.enhancements_copy(run_failed_jobs=FALSE, city=city)
# # 
# }
  # params <- data.frame(run_failed_jobs,city=city)
  # print(params)

  # # Unique jobname per run
  # run_tag  <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", sample(1000:9999, 1))
  # job_name <- paste0("MonteCarlo_epc_", run_tag)

  # job <- slurm_apply(
  #   f               = overpass.output.enhancements_copy,
  #   params          = params,
  #   nodes           = nrow(params),
  #   cpus_per_node   = 2,
  #   jobname         = job_name,
  #   slurm_options   = slurm_options
  #   )
  

  # cat("Submitted job array: ", job_name, " with ", nrow(params), " city-tasks.\n")
  }
