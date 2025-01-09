# Replication files for "The Network Structure of Environmental Justice Social Movements: A Case Study in the California Delta"

## Purpose

The folders contain all replication files for the paper: Pozzi, Tara, Lubell, Mark, and Jessica Rudnick. 2025. *The Network Structure of Environmental Justice Social Movements: A Case Study in the California Delta.* American Journal of Political Science. [Submitted]

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

-   0_data/edgelist.csv: Provides the networks data for 21 EJ groups in the study

-   0_data/nodelist.csv: List of all organizations and their characteristics used in this study

-   0_data/org_ids.csv: Organization name and key identification list

### 1_Outputs folder

-   1_output/model_dataset.csv: Full model dataset (can recreate this data by running the code in Section C of the analysis script)

-   Model Estimates

    -   m_ego.rds: Model results
    -   m_ego_alter.rds
    -   m_full.rds
    -   m_full_refined.rds
    -   m_re.rds
    -   m_bd.rds
    -   m_full_ejfactor.rds
    -   m_rbcheck_reg_egos.rds
    -   m_rbcheck_local_egos.rds
    -   m_full_subset.rds

-   Summary Tables

    -   model_comp_table.doc

    -   vpc_table.doc

### 2_Scripts folder

-   0_analysis_script.R

-   1_paper_plots.R
