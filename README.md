# GSRD code archive

This repository contains the code used to support the GSRD data paper, including:

- data checking and QC flag generation
- manuscript figure production
- evaluation report generation

## Suggested repository structure

```text
.
├── data/
│   ├── GSRD_main.csv
│   └── auxiliary/
│       ├── global_climate_space.csv        # optional
│       ├── site_climate.csv                # optional
│       └── sptrait_plot.csv                # optional
├── outputs/
│   ├── qc/
│   └── figures/
├── scripts/
│   ├── 01_generate_qc_flags.R
│   └── 02_make_manuscript_figures.R
└── report/
    └── evaluation_report.Rmd
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

Optional figure sections may also require additional prepared input tables in `data/auxiliary/`.

## GitHub and publication

A public GitHub repository is suitable for sharing the code with editors, reviewers, and readers. For publication, a more stable option is to create a GitHub release and archive it with Zenodo or figshare so the code has a DOI.

### Suggested Code Availability statement

> Custom R code used for data quality checks, generation of QC flags, preparation of the evaluation report, and figure production is available in the public GitHub repository [INSERT LINK HERE]. A release archived for this study is available at [INSERT DOI HERE].

If you choose not to create a DOI-backed archive, you can shorten this to:

> Custom R code used for data quality checks, generation of QC flags, preparation of the evaluation report, and figure production is available in the public GitHub repository [INSERT LINK HERE].

## Notes

- The scripts use **relative paths** and do not require `setwd()`.
- The optional climate-space and trait-matrix figures will only run when the corresponding auxiliary files are available.
- Before public release, check that the filenames in the repository exactly match the filenames mentioned in the manuscript.
