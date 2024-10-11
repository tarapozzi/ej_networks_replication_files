# we recommend running this is a fresh R session or restarting your current session
#nstall.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

#cmdstanr::install_cmdstan()
## cmdstanr can't interface with tidybayes or marginal effects, so might be better to use rstan 
## following andrew's blog with how to do this & convert results to a stanreg format so I can use my visualization code: https://www.andrewheiss.com/blog/2021/12/20/fully-bayesian-ate-iptw/
library(rstan)
library(brms)
library(tidyverse)

## Questions ----
# Do I need to change categorical predictors to random effects?
# How to integrate post-sweeping code into my stan model?
# How can I show the results for categorical predictors?

## data 
m_df <- read.csv("outputs/model_dataset.csv") %>%
  mutate(c_diff_cat = factor(c_diff_cat, levels = c("match", "lower", "higher")), 
         ej_diff_cat= factor(ej_diff_cat, levels = c("match", "lower", "higher")), 
         geo_diff_cat = factor(geo_diff_cat, levels = c("local_match", "regional_match", "smaller", "bigger")),
         np_match = factor(np_match, levels = c("np_homophily", "lower", "higher", "no_np_homophily")), 
         ego_local = factor(ego_local, levels = c("local", "regional")), 
         alter_local = factor(alter_local, levels = c("local", "regional")), 
         distance_n = normalize(distance)) %>%
  select(-X, -distance, -c_diff_n) # remove columns not included in data

# Original Model ----

## load original model
m_full <- readRDS("outputs/m_full.rds")

## extract code
m_full$model

## stan code
#// generated with brms 2.21.0

## run model with stan
### Convert the Stan code to C++
outcome_c = stanc("original_model.stan", allow_undefined = TRUE)

# Compile C++ified Stan to a binary model object
outcome_model <- stan_model(stanc_ret = outcome_c)

# Q: did I specify this correctly? ----
# Variables come from stan model
## categorical variables are automatically turned into dummy variables
outcome_data <- brms::standata(dv ~ 1 + ego_capacity_n + alter_capacity_n + c_diff_cat + count_ego_collaboratives + count_alter_collaboratives + overlap_collab + ego_np_501c3 + alter_np_501c3 + np_match + ego_ej_mission + alter_ej_mission + ej_diff_cat + count_ego_issues + count_alter_issues + i_match + ego_local + alter_local + geo_diff_cat + distance_n + (1|ego) + (1|alter), data = m_df, family = bernoulli(link = "logit"))
str(outcome_data)

## Run the model
outcome_samples <- sampling(
  outcome_model, 
  data = outcome_data, 
  chains = 4, 
  iter = 2000,
  cores = 4,
  seed = 1992
)

## Examine results
print(outcome_samples)

## convert to brms object
## create empty model
outcome_brms <- brm(dv ~ 1 + ego_capacity_n + alter_capacity_n + c_diff_cat + count_ego_collaboratives + count_alter_collaboratives + overlap_collab + ego_np_501c3 + alter_np_501c3 + np_match + ego_ej_mission + alter_ej_mission + ej_diff_cat + count_ego_issues + count_alter_issues + i_match + ego_local + alter_local + geo_diff_cat + distance_n + (1|ego) + (1|alter), 
              data = m_df, 
              family = bernoulli(link = "logit"), 
              prior = c(prior(normal(0,1), class = b),
                        prior(normal(0,10), class = Intercept)), # very weak prior for intercept so we don't influence it
              empty = TRUE) 

outcome_brms$fit <- outcome_samples
outcome_brms <- rename_pars(outcome_brms)
summary(outcome_brms)
## YAY now I can just plug this into the rest of the script

# Post-sweeping model ----
# Q: how to include categorical predictors in a stan model with identifiability issues? ----
### Convert the Stan code to C++
ps_c = stanc("ps_model2.stan", allow_undefined = TRUE)

# Compile C++ified Stan to a binary model object
ps_model <- stan_model(stanc_ret = ps_c)

# Run model
ps_samples <- sampling(
  ps_model, 
  data = outcome_data, 
  chains = 4, 
  iter = 2000,
  cores = 4,
  seed = 1992
)

# Add to brms object
ps_brms <- brm(dv ~ 1 + ego_capacity_n + alter_capacity_n + c_diff_cat + count_ego_collaboratives + count_alter_collaboratives + overlap_collab + ego_np_501c3 + alter_np_501c3 + np_match + ego_ej_mission + alter_ej_mission + ej_diff_cat + count_ego_issues + count_alter_issues + i_match + ego_local + alter_local + geo_diff_cat + distance_n + (1|ego) + (1|alter), 
                    data = m_df, 
                    family = bernoulli(link = "logit"), 
                    prior = c(prior(normal(0,1), class = b),
                              prior(normal(0,10), class = Intercept)), # very weak prior for intercept so we don't influence it
                    empty = TRUE) 

ps_brms$fit <- ps_samples
outcome_brms <- rename_pars(outcome_brms)
summary(outcome_brms)