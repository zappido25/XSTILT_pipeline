#!/bin/bash

set -e
### Carlton Xavier made this bash code to run the run_xstilt_Carlton_Updates.r
## lets add some color to life

# Define color codes
RED='\033[1;3;31m'    # Bold, Italic, Red
GREEN='\033[1;3;32m'  # Bold, Italic, Green
YELLOW='\033[1;3;33m' # Bold, Italic, Yellow
BLUE='\033[1;3;34m'   # Bold, Italic, Blue
MAGENTA='\033[1;3;35m' # Bold, Italic, Magenta
NC='\033[0m'          # Reset formatting

run_failed_jobs=${1:-FALSE}   # Default to "" if not provided


cities_file="$XSTILT_PATH/Cities_input_files/CONUS_cities_list.txt"

# Read city list safely
mapfile -t city_list < "$cities_file"

# Print cities list and select the city to run XSTILT for
echo -e "${MAGENTA}🌆 Cities list:${NC}"
for ((i=0; i<${#city_list[@]}; i++)); do
  echo -e "${YELLOW}$((i+1)). ${city_list[$i]}${NC}"
done

# Prompt for user selection
echo -ne "Please enter the number corresponding to your city choice: "
read city_choice

# Convert back to array index (subtract 1)
selected_city="${city_list[$((city_choice-1))]}"

echo -e "${GREEN}You selected: $selected_city${NC}"

echo -e "1. get shp and cc files urban and city convolution .. executes generate_postprocess_outputs_multicity.r"
echo -e "2. get nhrs.. executes calculate_nhrs.R"
echo -e "3. run convolution_v1.R to perform convolutions for various socioeconomic variables"
echo -e "4. over pass calculations Part 1 to create dataframes, by running P1_create_dataframes_for_overpasses.R"
echo -e "5. enhancement and Epc calculations Part 2 .., by running P2_overpass_output_enhancements.R"
echo -e "6. XCO2 observation data extract .., by running XCO2_obs.R"
echo -ne "Please enter the convolution step: "
read select_conv_step
echo -e "${GREEN}You selected step: $select_conv_step${NC}"

if [[ $select_conv_step == 1 ]]; then
  
  # defaults
  fresh_start=TRUE
  debug=FALSE
  echo -e "${GREEN}✅ Option 1 to run urban and city convolution.${NC}"
  echo -e "${GREEN}✅ Running the Rscript --vanilla generate_postprocess_outputs_multicity.r with the provided command line options...${NC}"
  echo -e "make sure input options are correct. Manually change here if needed."
  

  Rscript --vanilla P1.get.city.odiac.convolutions.R $debug $fresh_start $run_failed_jobs $selected_city

elif [[ $select_conv_step == 2 ]]; then
  echo -e "OPTION 2"
  echo -e "${YELLOW}⚠️ Option 2 to calculate nhrs${NC}"
  Rscript --vanilla P2.calculate.nhrs.R $run_failed_jobs $selected_city

elif [[ $select_conv_step == 3 ]]; then
  echo -e "OPTION 3"
  echo -e "${YELLOW}⚠️ Option 3 to calculate convolutions. This step runs pps, gdp, consumption, lights, epd, bio and edgar in parallel for all
                  the successful simulations ${NC}"
  
  Rscript --vanilla P3.convolutions.R $run_failed_jobs $selected_city

elif [[ $select_conv_step == 4 || $select_conv_step == 5 ]]; then
  if [[ $select_conv_step == 4 ]]; then
    echo -e "OPTION 4"
    echo -e "${YELLOW}⚠️ Option 4 to create dataframes for overpass calculations.${NC}"
    Rscript --vanilla P4.dataframes.enhancements.R $run_failed_jobs $select_conv_step $selected_city
    # Rscript --vanilla P1_create_dataframes_for_overpass_calcs.R $run_failed_jobs $selected_city
  else
    slurm_run=TRUE
    echo -e "OPTION 5"
    echo -e "${YELLOW}⚠️ Option 5 to perform overpass output enhancements.${NC}"
    Rscript --vanilla P4.dataframes.enhancements.R $run_failed_jobs $select_conv_step $selected_city
    # Rscript --vanilla P2_overpass_output_enhancements.R $run_failed_jobs $slurm_run$selected_city
  fi

# elif [[ $select_conv_step == 5 ]]; then
#   slurm_run=TRUE
#   echo -e "OPTION 5"
#   echo -e "${YELLOW}⚠️ Option 5 to perform overpass output enhancements.${NC}"
#   Rscript --vanilla dataframes_enhancements.R $run_failed_jobs $select_conv_step $selected_city
#   Rscript --vanilla P2_overpass_output_enhancements.R $run_failed_jobs $slurm_run$selected_city

elif [[ $select_conv_step == 6 ]]; then
  echo -e "OPTION 6"
  echo -e "${YELLOW}⚠️ Option 6 to extract XCO2 observation data.${NC}"
  Rscript --vanilla $XSTILT_PATH/Pre-processing/XCO2_obs.R $run_failed_jobs $selected_city

else
  echo -e "${RED}❌ Error: Invalid command line option'. Please provide a valid option (1 or 2).${NC}"
  exit 1
fi
