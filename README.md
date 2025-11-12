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
   wget --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies \
        --keep-session-cookies -i "file_names.txt"
