# This readme describes the file structure in the Post-processing folder
The files with suffix *.r contains functions, which is sourced in the main callscripts with suffi *.R

Running the "source Login_init.sh" should set the path of this folder in $PP_DIR.

## Functions calculating convolutions
- calc_bio.r             : SMUrf BIO NEE convolution
- calc_consumption.r     : consumption based emissions per capita
- calc_gdp.r             : gdp   
- calc_pps.r             : population
- calc_trans_err_pop.r   : transport errors
- city_intersect.r       : intersection of city and obs
- city_convolution.r     : urban/city  hourly convolution
- city_lights.r          : get lights informaiton for each city
- city_odiac_city_conv_usingshpfiles.r   : odiac*footprint convolutions with city bounds based on shapefiles rather than pop urban cores. This is in line with Loren Brinks and UMich apporach.

## Functions for calcualting Epc
- creates_dataframe_p1.r     : as the name suggests this cretes the dataframe used later for calculating Epc
- Monte_carlo_brute_force.r : Uses monte carlo brute force Kai Wilmots method to get Epc.

## Functions for slurm
- output_functions.r     : executes the above calc* and city* functions.

## Callscripts
- generate_post_process_outputs.R : executes output_functions.r via a slurm job in serial.
- urban_core_clustering.R         : creates the urban_core_info.txt which contians the city boundary lat lon values based on pop density
- calculate_nhrs.R                : calculates the time when the city and urban air masses are indistiguishable. 
- P1_create_dataframes_for_overpass_calcs.R : uses the creates_dataframe_p1.r
- P2_overpass_output_enhancements.R : uses Monte_carlo_brute_force.r

* make sure all the paths to resepctive files & folders are correct.

## Pipeline
- Step 1. Generate the urban_core_info.txt. This is now moved to Pre-processing folder
            "Rscript --vanilla urban_core_clusteting.R"

- Step 2. Execute "Rscript --vanilla generate_post_process_outputs_multicity.R" to generate the city and urban convolution files (and optional bio, pps, gdp *_before_nhrs.txt files) in the output out_* folders. This uses slurm and if needed to debug I would suggest to run the required function in serial on the node.

- Step 3. Execute "Rscript --vanilla calculate_nhrs.R" to output the nhrs_output_***site***.txt file. Needs city_influence and urban influence txt files generated in Step 2.

- Step 4. Execute "Rscript --vanilla convolution.R" to create pps.txt, gdp.txt and other *.txt output files in the out folder. This uses slurm and if needed to debug I would suggest to run the required function in serial on the node.

- Importrant detour: Run the scripts in Pre-processing folder to make sure things work smoothly. 
    - XCO2_obs.R is for extracting XCO2 observations along the overpass lat lon timestr.
    - check_completed_xstilt_runs.R is for checking how many runs were succesful. This outputs a file names incomplete_runs, which aids in skipping the fialed runs in post-processing.
    - urban_core_clustering -- gets the lat lon population data for the urban cores of interest.
    - ids_clsuters_from_gpw4.R -- reads the gpw4file and outputs ids for all citys with pop> 1 million.





