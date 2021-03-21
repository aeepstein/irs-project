This repository includes code to estimate incidence rate ratios for the impact of starting/stopping IRS in Uganda for the manuscript "The impact of stopping and starting indoor residual spraying on malaria burden in Uganda"

The data folder contains 3 datasets, each for estimating one of the 3 study objectives. These datasets are the final "clean" data (aggregated to the monthly level).

The code folder contains the datasets used to generate these aggregated datasets (R scripts 1-2):
1_IRS_project_extract_precip.R
2_IRS_project_prepare data and generate plots of raw cases over time.R


The Stata do file contains the final models run on the aggregated dataset.
3_IRS_project_models.do

The script 4_IRS_project_plot coefficients over time.R generates the plots included in the manuscript.
