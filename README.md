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

-   edgelist.csv: Provides the networks data for 21 EJ groups in the study

-   nodelist.csv: List of all organizations and their characteristics used in this study

-   0_data/org_ids.csv: Organization name and key identification list

### 1_Scripts folder

-   0_analysis.R: Code to recreate model dataset, analysis, and visualizations for this study

-   1_functions.R: Functions referenced in the analysis script

### 2_Outputs folder

-   model_dataset.csv: Full model dataset (can recreate this data by running the code in Section C of the analysis script)

-   Model Estimates

    -   Variance Components Models

        -   m_ego.rds: Model with just the ego random effect
        -   m_ego_alter.rds: Model with the ego and alter random effects

    -   Explanatory Models

        -   m_full.rds: Model with all individual and relational variables in this study

        -   m_full_refined.rds: Model with final set of individual and relational variables referenced in the main text of the paper (based on the GVIF model results)

        -   m_re.rds: Model with just resource exchange variables

        -   m_bd.rds: Model with just boundary definition variables

    -   Prior Exploration

        -   m_full_s.rds: Final model with shrinkage priors

        -   m_full_c.rds: Final model with regularizing priors

    -   Robustness Check Models

        -   m_rbcheck_local_egos.rds: Full model with only egos that work at the local scale
        -   m_rbcheck_reg_egos.rds: Full model with only egos that work at the regional scale
        -   m_full_subset.rds: Full model without one of the egos that is also a collaborative
        -   m_full_ejfactor.rds: Version of m_full_refined with EJ commitment modeled as a factor and with an interaction between ego and alter EJ commitment

-   Summary Tables

    -   model_comp_table.doc: Boundary Definition, Resource Exchange, and Full Model (model used in the main text) results comparison table

    -   vpc_table.doc: Variance Compoonents Model results in comparison to Full Model results looking at the variation explained by including 1) ego random effect and 2) ego and alter random effects.

### 3_Plots folder

-   figure_2.png: CalEnviroScreen (CES) 4.0 scores for the Delta with the three urban centers highlighted in relation to the Legal Delta designations: Sacramento, Stockton, and Eastern Contra Costa

-   figure_4.png: Combined network plot of all 21 ego networks

-   figure_5.png: Coefficient plot for resource exchange variables

-   figure_6.png: Coefficient plot for boundary definition variables

-   figure_7.png: Marginal Effects for Resource Exchange

-   figure_8.png: Marginal Effects for Boundary Definition

-   figure_a1.png: Coefficient plot for EJ commitment when modeled as an interaction between ego EJ commitment and alter EJ commitment

-   figure_a2a.png: Ego degree distribution

-   figure_a2b.png: Alter degree distribution

-   figure a3.png: Model results comparison with ego subset data

-   figure_a4.png: Model results comparison with EJCW removed
