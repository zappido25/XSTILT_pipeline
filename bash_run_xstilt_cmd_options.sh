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



## options for the Rscript code
path=$OCO2_DIR
sensor=1 ## 1 for OCO-2, 2 for OCO-3, 3 for TROPOMI, 4 for NA
version=1 ## 1 for V11r, 2 for V10p4r, 3 for NA
species=1 ## 1 for CO2, 2 for CO, 3 for NO2, 4 for CH4
multi_city=TRUE ## 1 for multi-city, 2 for single city

## options to run different Rscript codes
cmd_flag=${1:-1} ## Default to 1 for run_xstilt_Carlton_Updates.r if not provided
error_flag=${2:-FALSE} ## Default to FALSE for no error estimates if not provided
create_overpass_files_only=${3:-FALSE} ## Default to FALSE for run_xstilt_Carlton_Updates.r if not provided
run_failed_jobs=${4:-FALSE} ## Default to FALSE for run_xstilt_Carlton_Updates.r if not provided

# read city list
city_list=()
while IFS= read -r line; do
    city_list+=("$line")
done < "Cities_input_files/CONUS_cities_list_bashfile_variant.txt"

#print cities list and select the city to run Xstilt for
echo -e "${MAGENTA}🌆 Cities list:${NC}"
for ((i=1; i<${#city_list[@]}; i++)); do
  echo -e "${YELLOW}$((i+1)). ${city_list[$i]}${NC}"
done
echo -ne "Please enter the number corresponding to your city choice: "
read city_choice
selected_city="${city_list[$((city_choice-1))]}"
echo -e "${GREEN}You selected: $selected_city${NC}"


if [[ $multi_city == "TRUE" ]]; then
  if [ ! -f "Cities_input_files/CONUS_cities_list_bashfile_variant.txt" ] || [ ! -s "Cities_input_files/CONUS_cities_list_bashfile_variant.txt" ]; then
    echo -e "${RED}❌ Error: CONUS_cities_list.txt file does not exist or is empty.${NC}"
    echo -e "creating the output file with city names"
    echo -e "Rscript --vanilla Cities_input_files/create_cities_list.r"
    Rscript --vanilla Cities_input_files/create_cities_list.r
  else
    echo -e "${GREEN}✅ Success: CONUS_cities_list.txt file exists and is not empty.${NC}"
  fi
fi

# Check if the insert_ggAPI.csv file exists
if [ ! -f "insert_ggAPI.csv" ]; then
  echo -e "${RED}❌ Error: insert_ggAPI.csv file not found.${NC}"
  exit 1
fi

# Read the API key from the file
API_KEY=$(cat insert_ggAPI.csv | tr -d '\n')

# Check if the API key is empty
if [ -z "$API_KEY" ]; then
  echo -e "${RED}❌ Error: API key in insert_ggAPI.csv is empty.${NC}"
  exit 1
fi

# Test the API key with a sample request to the Google Geocoding API
echo -e "${BLUE}🔍 Testing the API key using Helsinki as a sample site...${NC}"
TEST_RESPONSE=$(curl -s "https://maps.googleapis.com/maps/api/geocode/json?address=Helsinki&key=$API_KEY")

# Check if the response contains an error message
if echo "$TEST_RESPONSE" | grep -q '"error_message"'; then
  echo -e "${RED}❌ Error: The API key is invalid or not working. Make sure the API key is valid.${NC}"
  echo -e "${YELLOW}⚠️ Response: $TEST_RESPONSE${NC}"
  exit 1
else
  echo -e "${GREEN}✅ Success: The API key in insert_ggAPI.csv is valid and working.${NC}"
  echo -e "${MAGENTA}🚀 Running the Rscript run_xstilt_Carlton_Updates.r with the following default command line options...${NC}"
  echo -e "${NC} The arguments to provide the Rscript are:\n\
    ${NC}📂 Arg1: path where data is located: ${YELLOW}$path\n\
    ${NC}🛰️ Arg2: select the option of the satellite used (OCO-2, OCO-3, TROPOMI, NA): ${YELLOW}$sensor\n\
    ${NC}📦 Arg3: select the version of the satellite data (V11r, V10p4r, NA): ${YELLOW}$version\n\
    ${NC}🌍 Arg4: select the species of the satellite data (CO2, CO, NO2, CH4): ${YELLOW}$species\n\
    ${NC}🌍 Arg5: multiple timestrings or single time: ${YELLOW}$multi_city\n\
    ${NC}🌍 Arg6: create_overpass_files_only: ${YELLOW}$create_overpass_files_only\n\
    ${NC}🌍 Arg7: run_failed_jobs: ${YELLOW}$run_failed_jobs\n\
    ${NC}🌍 Arg8: city_choice: ${YELLOW}$selected_city${NC}"

  if [[ $cmd_flag == 1 ]]; then
    echo -e "${GREEN}✅ Running the Rscript run_xstilt_Carlton_Updates.r with the provided command line options...${NC}"
    
    if [[ $error_flag == "FALSE" && $multi_city == "FALSE" ]]; then
      echo -e "OPTION 1"
      echo -e "${YELLOW}⚠️ $error_flag Running without error estimates.. only back trajectories and footprint...${NC}"
      echo -e "${YELLOW}⚠️ $multi_city Running for a single site...${NC}"
      Rscript --vanilla run_xstilt_Carlton_Updates.r $path $sensor $version $species $multi_city $create_overpass_files_only $error_flag 
    
    elif [[ $error_flag == "FALSE" && $multi_city == "TRUE" ]]; then
      echo -e "OPTION 2"
      echo -e "${YELLOW}⚠️ $error_flag Running without error estimates.. only back trajectories and footprint...${NC}"
      echo -e "${YELLOW}⚠️ $multi_city Running for multiple cities and overpasses...${NC}"

      Rscript --vanilla run_xstilt_Carlton_Updates_multicity.r $path $sensor $version $species $multi_city $create_overpass_files_only $error_flag $run_failed_jobs $selected_city

    elif [[ $error_flag == "TRUE" && $multi_city == "TRUE" ]]; then
      echo -e "OPTION 3"
      echo -e "${YELLOW}⚠️ $error_flag Running with error estimates.. back trajectories+footprint+wind pertubations...${NC}"
      echo -e "${YELLOW}⚠️ $multi_city Running for multiple cities and overpasses...${NC}"
      Rscript --vanilla run_xstilt_Carlton_Updates_multicity.r $path $sensor $version $species $multi_city $create_overpass_files_only $error_flag $run_failed_jobs $selected_city
      
    elif [[ $error_flag == "TRUE" && $multi_city == "FALSE" ]]; then
      echo -e "OPTION 4"
      echo -e "${YELLOW}⚠️  $error_flag Running with error estimates.. back trajectories+footprint+wind pertubations.${NC}"
      echo -e "${YELLOW}⚠️ $multi_city Running for a single site...${NC}"
      Rscript --vanilla run_xstilt_Carlton_Updates.r $path $sensor $version $species $multi_city $create_overpass_files_only $error_flag 
 
    else
     echo -e "${RED}❌ Error: Invalid command line option. Please provide a valid option.${NC}"
     exit 1
    fi
    
    # Rscript --vanilla run_xstilt_Carlton_Updates.r $path $sensor $version $species
  elif [[ $cmd_flag == 2 ]]; then
    echo -e "${GREEN}✅ Running the Rscript compute_bg_Carlton_updates.r with the provided command line options...${NC}"
    Rscript --vanilla compute_bg_Carlton_updates_multicity.r $path $sensor $version $species $multi_city $create_overpass_files_only $error_flag 
    
  else
    echo -e "${RED}❌ Error: Invalid command line option '${cmd_flag}'. Please provide a valid option (1 or 2).${NC}"
    echo -e "${YELLOW}⚠️ Usage: ./bash_run_xstilt_cmd_options.sh [cmd_flag] [error_flag] [create_overpass_files_only]${NC}"
    exit 1
  fi
fi