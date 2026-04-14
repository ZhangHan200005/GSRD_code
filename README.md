# GSRD code archive

This repository contains the code used to support the GSRD data paper, including:

- data checking and QC flag generation
- manuscript figure production
- evaluation report generation

```

## What each file does

### `scripts/01_generate_qc_flags.R`
- checks REFID–REF consistency
- creates a REFID recoding map
- generates the two QC flags used in the manuscript:
  - `QC_FLAG_RS_MASS25_GLOBAL_IQR3`
  - `QC_FLAG_RS_MASS25_SITE_IQR3`
- writes an updated data file and QC outputs to `outputs/qc/`

### `scripts/02_make_manuscript_figures.R`
- generates the main manuscript figures directly from `GSRD_main.csv`
- merges the plotting logic from the earlier figure scripts into one file
- writes figures to `outputs/figures/`

### `report/evaluation_report.Rmd`
- renders the technical evaluation report based on `data/GSRD_main.csv`

## How to run

From the project root in R:

```r
source("scripts/01_generate_qc_flags.R")
source("scripts/02_make_manuscript_figures.R")
rmarkdown::render("report/evaluation_report.Rmd")
```

## Required R packages

Main packages used in this repository include:

- dplyr
- readr
- ggplot2
- patchwork
- hexbin
- ggExtra
- sf
- rnaturalearth
- tidyverse
- knitr
- kableExtra
- readxl
- reshape2
- scales
- maps
