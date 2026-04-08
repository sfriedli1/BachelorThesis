# Bachelor Thesis: Frequency Changes in Late Spring Frost Events after the Start of Season and Their Impacts on Carbon Fluxes in Forest Ecosystems

**Degree Program:** Geography

**Author:** Sascha Friedli

**Supervision:** Benjamin David Stocker

## Project description

This repository contains the code, documentation, and analysis outputs/results for the bachelor thesis on frequency changes in late spring frost events after the start of season and their impacts on carbon fluxes in forest ecosystems. The project focuses particularly on gross primary production (GPP) derived from eddy-covariance measurements.

## Data source

The analysis is based on FluxDataKit provided by GECO Bern, which contains harmonized eddy-covariance flux data and site-level meteorological variables. Additional vegetation indicators may be included later if suitable remote-sensing products are available.

## Workflow

Two levels of reproduction are intended in this repository.

### Main analysis reproduction

To reproduce the main analysis and figures, it should be sufficient to create a `figures/` folder and run the `.Rmd` files in `vignettes/` using the preprocessed data stored in `data/`.

### Full reproduction from raw data

To reproduce the full workflow from scratch:

1. Create the `data_raw/` and `figures/` folder.
2. Run the scripts in `analysis/` to download and prepare the raw data.
3. Run the `.Rmd` files in `vignettes/` to reproduce analyses and figures.

## Repository structure

project/

├── analysis/

├── R/

├── data/

├── data_raw/

├── figures/

└── vignettes/

Key files:
- README.md
- Schedule.md
- Proposal.pdf

**Explanation:**

The `analysis/` folder contains R scripts used to download and prepare the raw data.

The `R/` folder contains reusable R functions used in the analysis.

The `data/` folder contains preprocessed data used by the R Markdown files in the `vignettes/` folder.

The `data_raw/` folder is intended for raw input data downloaded with the corresponding scripts.

The `figures/` folder contains figures and plots generated during the analysis.

The `vignettes/` folder contains R Markdown files documenting the main analysis workflow.



