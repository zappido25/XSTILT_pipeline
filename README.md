[README.md](https://github.com/user-attachments/files/23507862/README.md)
# Running X-STILT

This document provides additional details beyond the main [X-STILT GitHub README](https://github.com/NOAA-X-STILT).  
It covers setup, execution, troubleshooting, and post-processing steps for running CO₂ simulations using X-STILT.

---

## 📚 Table of Contents
- [1. Getting Started](#1-getting-started)
  - [CO₂ Data Download](#co₂-data-download)
  - [API Keys](#api-keys)
- [2. Running X-STILT](#2-running-x-stilt)
  - [P1. Run X-STILT](#p1-run-x-stilt)
- [3. Background CO₂ Computation](#3-background-co₂-computation)
- [4. Error Estimation](#4-error-estimation)
  - [Debugging Notes](#debugging-notes)
  - [Running Error Estimates for XCO₂](#running-error-estimates-for-xco₂)
- [5. Convolutions](#5-convolutions)
  - [a. Generate City & Urban Influence Files](#a-generate-city--urban-influence-files)
  - [b. Compute NHRS](#b-compute-nhrs)
  - [c. Convolution](#c-convolution)
- [6. Overpass Calculations](#6-overpass-calculations)
- [7. Multi-City and Multi-Timestr Runs](#7-multi-city-and-multi-timestr-runs)
- [8. Check for Failed Runs](#8-check-for-failed-runs)
- [9. Post-Processing](#9-post-processing)
- [10. Updates](#10-updates)
- [Author](#author)

---

## 1. Getting Started

### CO₂ Data Download
To obtain OCO-2 or OCO-3 CO₂ data:
1. Log in to [NASA Earthdata](https://urs.earthdata.nasa.gov/).
2. Follow the instructions to generate the `file_names.txt` list.
3. Use the following `wget` command to download data:
   ```bash
   wget --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies         --keep-session-cookies -i "file_names.txt"
   ```
4. Ensure your `.urs_cookies` file is correctly configured. You can use the provided Python helper script on Earthdata to generate it.

### API Keys
Download your **Google Maps API key** from  
<https://console.developers.google.com/>  
and save it as `insert_ggAPI.csv`.

---

## 2. Running X-STILT

Use `Run_Xstilt_Carlton_updated` along with the command-line wrapper `bash_run_xstilt_cmd_options.sh`.  
The R script automatically downloads or locates HRRR meteorological data (from ARL) in the provided directory.

### P1. Run X-STILT
```bash
"For X-STILT run"
./bash_run_xstilt_cmd_options.sh "" "" "" "" 
```
"For X-STILT error run"
./bash_run_xstilt_cmd_options.sh "" TRUE "" "" 
```

This runs either:
- `run_xstilt_Carlton_updates.r`, or  
- `run_xstilt_Carlton_updates_multicity.r`

These scripts take **nine arguments** (modifiable in the bash script):

| Argument | Description |
|-----------|--------------|
| 1 | OCO-2 data path |
| 2 | Sensor (OCO-2 or OCO-3, default = 1) |
| 3 | Data version (e.g., V11r, default = 1) |
| 4 | Species (CO₂, CH₄, NO₂; default = 1) |
| 5 | Multi-city flag |
| 6 | Create overpass files (TRUE/FALSE) |
| 7 | Error estimation (TRUE/FALSE) |
| 8 | run failed jobs (TRUE/FALSE) |
| 9 | city choice (TRUE/FALSE) |

**Notes:**
- `raob_path` (radiosonde data) and `met_path` are currently hard-coded.
- Run first with `create_overpass_files = TRUE` to generate overpass and timestr lists. This take awhile for multiple cities.

---

## 3. Background CO₂ Computation

To compute background CO₂:
```bash
bash bash_run_xstilt_cmd_options.sh 2
```

This executes `compute_bg.r` with the same command-line options.

---

## 4. Error Estimation

To compute footprint and trajectory uncertainties:
```bash
bash bash_run_xstilt_cmd_options.sh 1 TRUE
```

This runs X-STILT with `u` and `v` wind perturbations to estimate uncertainty.

### Debugging Notes

1. **Radiosonde Path Issue**  
   Original scripts used only file names.  
   Fixed version uses:
   ```r
   file.path(dir_path, file_name)
   ```
   for correct path handling.

2. **Missing `pres.raob` Variable**  
   Fix by editing `cal.wind.err.r`:
   ```r
   raob <- raob %>%
     rename(temp.raob = temp, u.raob = u, v.raob = v,
            ws.raob = ws, wd.raob = wd, pres.raob = pres)
   ```

### Running Error Estimates for XCO₂

Run `run_xstilt_Carlton_updates.sh` with the following flags:
```r
run_trajec    = T
run_slant     = F
run_foot      = T
run_hor_err   = T
run_ver_err   = T
run_emiss_err = F
run_wind_err  = T
run_sim       = F
nhrs          = -12
```

**Notes:**
- Ensure ODIAC and CT-NRT data exist in their respective folders.
- `get.combine.prof.r` script generates missing weighting profiles (`combine.prof`).

To create **X_foot_emiss.nc**, set:
```r
run_sim = T
```
and all other flags to `F`.

---

## 5. Convolutions

Scripts are organized under:
- `call_scripts/` – main SLURM submission scripts
- `functions/` – subroutines and helper functions

Further details are in `Post-processing/README.md`.

### a. Generate City & Urban Influence Files
Run:
```r
generate_postprocess_outputs_multicity.R
```
with:
```r
first_time = TRUE
```

This creates:
- `city_influence.txt`  
- `urban_influence.txt`  
for both shapefile (`odiac_city_influence`) and population methods (`city_influence`).

### b. Compute NHRS
Run:
```r
calculate_nhrs.R
```

This determines when the urban enhancement signal (E_city − E_urban) becomes indistinguishable from the background:
1. When `(E_city - E_urban) < 10% * max(E_city - E_urban)`, or  
2. Within the 10th percentile of `(E_city - E_urban)` values.

### c. Convolution
Run:
```r
convolution_v1.R
```
This overwrites previous influence files — rename outputs (e.g., `city_influence_pop`, `urban_influence_shp`) to prevent confusion.

---

## 6. Overpass Calculations

1. Merge datasets with:
   ```r
   P1_create_dataframes_for_overpasses.R
   ```
2. Compute enhancements via:
   ```r
   P2_overpass_output_enhancements.R
   ```
   Uses **Monte Carlo brute-force (100,000 iterations)** to calculate XCO₂ enhancements and statistical summaries.

---

## 7. Multi-City and Multi-Timestr Runs

Steps:
1. Create `CONUS_cities_list.txt` with city names (e.g., 24 CONUS cities).
2. Generate overpass/receptor files:
   ```bash
   bash_run_xstilt_cmd_options.sh "" "" TRUE
   ```
   Produces:
   - `overpass_to_model.txt`
   - `nrecp.txt`
3. Run simulations:
   ```bash
   bash_run_xstilt_cmd_options.sh "" "" ""
   ```
   (Run one city at a time for stability.)
4. Estimate horizontal wind errors:
   ```bash
   bash_run_xstilt_cmd_options.sh "" "FALSE" ""
   ```

If RAOB data are missing, default `sigmauverr = 3 m/s` is used.

---

## 8. Check for Failed Runs

Identify incomplete runs using:
```r
check_completed_xstilt_runs.R
```

---

## 9. Post-Processing

Main post-processing scripts are found in:
```
Analysis/analysis/
```
This folder includes tools for enhancement statistics, uncertainty quantification, and visualization.

---

## 10. Updates

Currently integrating **Vulcan FFCO₂ (2015–2022)** to compute emission per capita (EPC), replacing EDGAR-based estimates.

![Example Output](https://github.com/user-attachments/assets/453262f1-9752-452e-9bbc-0e5fe9dd270d)

---

## Author

**Carlton Xavier**  
Postdoctoral Researcher — Lund University / Lin Group  
*Updated: November 2025*
