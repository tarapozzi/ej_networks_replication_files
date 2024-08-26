# Replication files for "Leveraging Partnerships in the Environmental Justice Movement: A Case Study in the California Delta"

## Purpose

The folders contain all replication files for the paper: Pozzi, Tara, Lubell, Mark, and Jessica Rudnick. 2024. *Leveraging Partnerships in the Environmental Justice Movement: A Case Study in the California Delta.* American Journal of Political Science. [Submitted]

## Setup

Operating system

The analysis was performed on a macOS Monterey 12.0.1 using R-Version 4.4.0.

### R-packages

The following R-packages (and versions) were used the analysis:

-   tidyverse

-   brms

-   tidbayes

### Random Seeds

We specify the starting seed for each Bayesian model. In order to replicate our results exactly, you will need to have exact same R-version and random seed specification. If not, then the model results will different slightly from our results due to the randomness of the MCMC sampling process [check language].

## Content

There are three main folders: data, outputs, and scripts.

### 0_Data folder

-   0_data/edgelist.csv

-   0_data/nodelist.csv

### 1_Outputs folder

-   1_output/model_data.csv: Full model dataset created from running R script: 0_data_cleaning.R

-   Figures

    -   Figure_1.png

-   Model_Estimates

    -   m1_results.RDS

### 2_Scripts folder

-   0_analysis_script.R

-   1_paper_plots.R
