# Data and code from: Climate and species traits give rise to complex phenological dynamics

Dataset DOI: [10.5061/dryad.sxksn03h0](https://doi.org/10.5061/dryad.sxksn03h0)

## Description of the data and file structure

### Files and variables

This data archive contains three main folders, each with sub-folders that include a Data_README.txt file describing the content and usage of that sub-folder.

#### File: DATA.zip

**Description:** This file contains the long-term dataset, including presence/absence records for all butterfly species at each site from 1985 to 2023, species natural history traits, and climate data.

#### File: Statistical_models.zip

**Description:** This file contains the R scripts for all fitted models (both with and without climate variables) used to estimate annual occurrence distributions for each species at each site using a hierarchical Bayesian framework in stan.

#### File: Figures_and_Tables.zip

**Description:** This file contains the R scripts used to generate all main figures, supplementary figures, and tables.

## Code/software

Software and packages used to run the scripts in this archive include:

* R (version 4.2.2)
* RStan package (Version 2.21.8)
* Stan
* loo package (Version 2.5.1)
* Vegan package (version 2.6-8)
