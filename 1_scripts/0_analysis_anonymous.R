# This scripts provides the code for creating the model dataset from the raw data, running the analysis, and data visualizations used in the paper and supplementary materials document.

# Load libraries and custom functions -------------------------------------
library(tidyverse)
library(tidybayes) # result plots
library(network)
library(ggraph) # network plot
library(sna) # network analysis
library(brms) # modeling package
library(tidybayes) # model results interpretation
library(emmeans)
source("1_scripts/1_functions.R")

# Analysis -----
## Model Data ----
m_df <- read.csv("0_data/model_dataset.csv") 
m_df <- m_df %>%
  ungroup() %>%
  mutate(c_diff_cat = factor(c_diff_cat, levels = c("match", "lower", "higher")), 
         ej_diff_cat= factor(ej_diff_cat, levels = c("match", "lower", "higher")), 
         geo_diff_cat = factor(geo_diff_cat, levels = c("local_match", "regional_match", "smaller", "bigger")),
         np_match = factor(np_match, levels = c("np_homophily", "lower", "higher", "no_np_homophily")), 
         ego_local = factor(ego_local, levels = c("local", "regional")), 
         alter_local = factor(alter_local, levels = c("local", "regional")), 
         distance_n = normalize(distance)) %>%
  select(-distance) # remove ID and un-normalized distance

## Descriptive Results -----------------------------------------------------
## 1. Table 2 ----
## Code for comparison table of independent variables across egos, alters, potential ego-alter ties, and observed ego-alter ties
### a. Egos ----
ego_summary <- m_df %>%
  select(ego, ego_np_501c3, ego_capacity_n, count_ego_collaboratives, ego_ej_mission, count_ego_issues) %>%
  unique() %>%
  summary()
ego_summary

ego_cat_summary <- m_df %>%
  select(ego, ego_local) %>%
  unique() %>%
  group_by(ego_local) %>%
  count()%>%
  ungroup() %>%
  mutate(porp = n/sum(n))
ego_cat_summary

### b. Observed Alters ----
actual_alters <- edgelist %>% select(alter) %>% distinct() %>% pull()

alter_summary <- m_df %>%
  filter(alter %in% actual_alters) %>%
  select(alter, alter_np_501c3, alter_capacity_n, count_alter_collaboratives, alter_ej_mission, count_alter_issues) %>%
  unique() %>%
  summary()
alter_summary

alter_cat_summary <- m_df %>%
  filter(alter %in% actual_alters) %>%
  select(alter, alter_local) %>%
  unique() %>%
  group_by(alter_local) %>%
  count()%>%
  ungroup() %>%
  mutate(porp = n/sum(n))
alter_cat_summary

### c. Potential ego-alter ----
pot_pair_summary <- m_df %>%
  filter(dv == 0) %>%
  select(overlap_collab, i_match, distance_n) %>%
  unique() %>%
  summary() 
pot_pair_summary
#0.000621371: conversion factor from meters to miles

m_df %>%
  filter(dv == 0) %>%
  select(ego, alter, c_diff_cat) %>%
  unique() %>%
  group_by(c_diff_cat) %>%
  count() %>%
  ungroup() %>%
  mutate(prop = n/sum(n))
# higher: means that alter has higher capacity than ego OR ego has lower capacity than alter
# lower: means that alter has lower capacity than ego OR ego has higher capacity than alter

m_df %>%
  filter(dv == 0) %>%
  select(ego, alter, np_match) %>%
  unique() %>%
  group_by(np_match) %>%
  count() %>%
  ungroup() %>%
  mutate(porp = n/sum(n))

m_df %>%
  filter(dv == 0) %>%
  select(ego, alter, ej_diff_cat) %>%
  group_by(ej_diff_cat) %>%
  count() %>%
  ungroup() %>%
  mutate(porp = n/sum(n))

m_df %>%
  filter(dv == 0) %>%
  select(ego, alter, geo_diff_cat) %>%
  unique() %>%
  group_by(geo_diff_cat) %>%
  count()

### d. Observed ego-alter ties -----
pair_summary <- m_df %>%
  filter(dv == 1) %>%
  select(overlap_collab, i_match, distance_n) %>%
  unique() %>%
  summary()
pair_summary

m_df %>%
  filter(dv == 1) %>%
  select(ego, alter, c_diff_cat) %>%
  unique() %>%
  group_by(c_diff_cat) %>%
  count()%>%
  ungroup() %>%
  mutate(porp = n/sum(n))
# higher: means that alter has higher capacity than ego OR ego has lower capacity than alter
# lower: means that alter has lower cpacity than ego OR ego has higher capacity than alter

m_df %>%
  filter(dv == 1) %>%
  select(ego, alter, np_match) %>%
  unique() %>%
  group_by(np_match) %>%
  count()%>%
  ungroup() %>%
  mutate(porp = n/sum(n))

m_df %>%
  filter(dv == 1) %>%
  select(ego, alter, ej_diff_cat) %>%
  unique() %>%
  group_by(ej_diff_cat) %>%
  count()%>%
  ungroup() %>%
  mutate(porp = n/sum(n))

m_df %>%
  filter(dv == 1) %>%
  select(ego, alter, geo_diff_cat) %>%
  unique() %>%
  group_by(geo_diff_cat) %>%
  count()%>%
  ungroup() %>%
  mutate(porp = n/sum(n))

## 2. Network figure ----
# Create network object
net <- network(x = edgelist, 
               vertices = nodelist, 
               bipartite = F,  
               directed = T, 
               isolates = F)
# Attributes
net %v% 'degree' <- sna::degree(net)

# figure out which orgs where named in multiple ego networks
alter_overlaps <- edgelist %>% select(alter) %>% group_by(alter) %>% filter(n() > 1) %>% distinct() %>% pull() # 17 overlapping alters 

# out of those orgs, which ones were also an ego in the analysis
egos <- edgelist %>% select(ego) %>% distinct() %>% pull() 
alters <- edgelist %>% select(alter) %>% distinct() %>% pull()
both <- intersect(egos, alters)
ego_alter_overlaps <- intersect(alter_overlaps, both)

# the remaining are just named multiple times as an alter
just_alter_overlaps <- setdiff(alter_overlaps, both)

net %v% 'position' <- ifelse(net %v% 'vertex.names' %in% egos, 1, 2)

net %v% 'labels' <- ifelse(net %v% 'vertex.names' %in% egos & net %v% 'position' == 1, net %v%  'vertex.names', '')


net %v% 'overlaps' <- ifelse(net %v% 'vertex.names' %in% ego_alter_overlaps, # 7 overlapping 
                             1, 
                             ifelse(net %v% 'vertex.names' %in% just_alter_overlaps, 2, 0))  



# Plot
set.seed(3)
net_plot <- ggraph(net, layout = 'fr') +
  geom_edge_link(alpha = 0.5, color = "black", show.legend = FALSE) +
  geom_node_point(aes(size = as.numeric(degree), color = as.character(overlaps), shape = as.character(position)), alpha = .9) +
  scale_size_continuous(range = c(2, 7)) +
  scale_shape_manual(breaks = c(1, 2), values = c(17, 18), labels = c("Ego", "Alter")) +
  scale_color_grey(breaks = c(0,1,2), labels = c("One Network", "Ego in \nmultiple networks", "Alter in \nmultiple networks")) + 
  #scale_color_manual(breaks = c(0,1,2), values = c("#00BFC4", "gold", "#54278F"), labels = c("None", "Ego in \nmultiple networks", "Alter in \nmultiple networks")) +
  theme_void() +
  #geom_node_text(aes(label = labels), size = 4, bg.color = "grey", repel = T, max.overlaps = Inf) +
  theme(legend.position = "right", legend.text = element_text(size = 12)) + 
  guides(size = guide_legend(title = "Degree"), shape = guide_legend(title = "Shape"), color = guide_legend(title = "Color")) 

net_plot
##ggsave("3_plots/figure_4.png", net_plot, width = 10, height = 8, units = "in")



## Inferential Results -----
## 1. Variance Components Models ----
### a. Ego Only
set.seed(1992)
m_ego <- brm(dv ~ (1|ego), data = m_df, cores = 4, file = "2_outputs/m_ego.rds", family = bernoulli(link = "logit"))
m_ego


### b. Ego + Alter 
set.seed(1992)
m_ego_alter <- brm(dv ~ (1|ego) + (1|alter),  cores = 4, file = "2_outputs/m_ego_alter.rds",  data = m_df, family = bernoulli(link = "logit"))
m_ego_alter

## 2. Explanatory Models -----
### a. Full Model (ego + alter RE)----
set.seed(1992)
m_full <- brm(dv ~ ego_capacity_n + alter_capacity_n + factor(c_diff_cat) + count_ego_collaboratives + count_alter_collaboratives + overlap_collab + ego_np_501c3 + alter_np_501c3 + factor(np_match) + ego_ej_mission + alter_ej_mission + factor(ej_diff_cat) + count_ego_issues + count_alter_issues + i_match + ego_local + alter_local + factor(geo_diff_cat) + distance_n + (1|ego) + (1|alter), 
              data = m_df, 
              family = bernoulli(link = "logit"), 
              prior = c(prior(normal(0,1), class = b),
                        prior(normal(0,10), class = Intercept)), # very weak prior for intercept so we don't influence it
              warmup = 1000, #burn in period
              iter = 2000, #actual samples
              chains = 4,
              cores = 2,
              file = "2_outputs/m_full",
              control = list(adapt_delta = 0.99))


### GVIF Test ----
library(car)
library(lme4)
# LME4 model
## Model with all potential variables 
f_m_full <- glm(dv ~ ego_capacity_n + alter_capacity_n + c_diff_cat + count_ego_collaboratives + count_alter_collaboratives + overlap_collab + ego_np_501c3 + alter_np_501c3 + np_match + ego_ej_mission + alter_ej_mission + ej_diff_cat + count_ego_issues + count_alter_issues + i_match + ego_local + alter_local + geo_diff_cat + distance_n, data = m_df, family = binomial(link = "logit"))

ld.vars <- attributes(alias(f_m_full)$Complete)$dimnames[[1]]
ld.vars
# "np_matchhigher", "np_matchno_np_homophily", "geo_diff_catsmaller", "geo_diff_catbigger"

# remove aliased coefficients
f_m_full2 <- glm(dv ~ ego_capacity_n + alter_capacity_n + c_diff_cat + count_ego_collaboratives + count_alter_collaboratives + overlap_collab + np_match + ego_ej_mission + alter_ej_mission + ej_diff_cat + count_ego_issues + count_alter_issues + i_match + geo_diff_cat + distance_n, data = m_df, family = binomial(link = "logit"))

# Let's see how much correlation is happening!
vif_values <- vif(f_m_full2)
vif_values
## EJ mission is problematic.. So just keep the relational independent variable for that too

f_m_full3 <- glm(dv ~ ego_capacity_n + alter_capacity_n + c_diff_cat + count_ego_collaboratives + count_alter_collaboratives + overlap_collab + np_match + ej_diff_cat + count_ego_issues + count_alter_issues + i_match + geo_diff_cat + distance_n, data = m_df, family = binomial(link = "logit"))

# Do the test again
vif_values <- vif(f_m_full3)
vif_values # looks good!

### GVIF Refined Models ----
### b. Resource Exchange Model (ego + alter RE)----
set.seed(1992) # set starting seed to make results replicable
m_re <- brm(dv ~ ego_capacity_n + alter_capacity_n + factor(c_diff_cat) + count_ego_collaboratives + count_alter_collaboratives + overlap_collab + factor(np_match) + (1|ego) + (1|alter), 
            data = m_df, 
            prior = c(prior(normal(0,1), class = b),
                      prior(normal(0,10), class = Intercept)),
            family = bernoulli(link = "logit"),
            warmup = 1000, #burn in period
            iter = 2000, #actual samples
            chains = 4,
            cores = 4,
            file = "2_outputs/m_re",
            control = list(adapt_delta = 0.99))


### c. Boundary Definition Model (ego + alter RE)----
set.seed(1992)
m_bd <- brm(dv ~  factor(ej_diff_cat) + count_ego_issues + count_alter_issues + i_match + factor(geo_diff_cat) + distance_n + (1|ego) + (1|alter), 
            data = m_df, 
            family = bernoulli(link = "logit"), 
            
            prior = c(prior(normal(0,1), class = b),
                      prior(normal(0,10), class = Intercept)), 
            warmup = 1000, #burn in period
            iter = 2000, #actual samples
            chains = 4,
            cores = 4,
            file = "2_outputs/m_bd", # save your model output
            control = list(adapt_delta = 0.99)) 

### d. Full Model (ego only)----
set.seed(1992)
m_full_ego <- brm(dv ~ ego_capacity_n + alter_capacity_n + factor(c_diff_cat) + count_ego_collaboratives + count_alter_collaboratives + overlap_collab + factor(np_match) + factor(ej_diff_cat) + count_ego_issues + count_alter_issues + i_match + factor(geo_diff_cat) + distance_n + (1|ego), 
                  data = m_df,
                  family = bernoulli(link = "logit"), 
                  prior = c(prior(normal(0,1), class = b),
                            prior(normal(0,10), class = Intercept)), 
                  warmup = 1000, #burn in period
                  iter = 2000, #actual samples
                  chains = 4,
                  cores = 2,
                  file = "2_outputs/m_full_ego",
                  control = list(adapt_delta = 0.99))

### e. Full Model refined after VIF test ----
set.seed(1992)
m_full_refined <- brm(dv ~ ego_capacity_n + alter_capacity_n + factor(c_diff_cat) + count_ego_collaboratives + count_alter_collaboratives + overlap_collab + factor(np_match) + factor(ej_diff_cat) + count_ego_issues + count_alter_issues + i_match + factor(geo_diff_cat) + distance_n + (1|ego) + (1|alter), 
              data = m_df, 
              family = bernoulli(link = "logit"), 
              prior = c(prior(normal(0,1), class = b),
                        prior(normal(0,10), class = Intercept)), 
              warmup = 1000, #burn in period
              iter = 2000, #actual samples
              chains = 4,
              cores = 2,
              file = "2_outputs/m_full_refined",
              control = list(adapt_delta = 0.99))

summary(m_full_refined)

## 3. Model Selection ----
loo_compare(loo(m_bd), loo(m_re), loo(m_full), loo(m_full_refined))

## 4. Prior Selection ----
### a. Shrinkage Priors ----
#The horseshoe prior is a special shrinkage prior initially proposed by Carvalho et al. (2009). It is symmetric around  zero with fat tails and an infinitely large spike at zero. This makes it ideal for sparse models that have many regression coefficients, although only a minority of them is non- zero. The horseshoe prior can be applied on all population-level effects at once (excluding the intercept) by using set_prior("horseshoe(1)"). (p. 70)
set.seed(1992)
m_full_s <- brm(dv ~ ego_capacity_n + alter_capacity_n + factor(c_diff_cat) + count_ego_collaboratives + count_alter_collaboratives + overlap_collab + factor(np_match) + factor(ej_diff_cat) + count_ego_issues + count_alter_issues + i_match + factor(geo_diff_cat) + distance_n + (1|ego) + (1|alter), 
              data = m_df, 
              prior = c(prior(horseshoe(1), class = b),
                        prior(normal(0,10), class = Intercept)), 
              family = bernoulli(link = "logit"), 
              warmup = 1000, #burn in period
              iter = 2000, #actual samples
              chains = 4,
              cores = 2,
              file = "2_outputs/m_full_s",
              control = list(adapt_delta = 0.99))

### b. Cauchy -----
## Based off gelman 2008 (https://projecteuclid.org/journals/annals-of-applied-statistics/volume-2/issue-4/A-weakly-informative-default-prior-distribution-for-logistic-and-other/10.1214/08-AOAS191.full)
set.seed(1992)
m_full_c <- brm(dv ~ ego_capacity_n + alter_capacity_n + factor(c_diff_cat) + count_ego_collaboratives + count_alter_collaboratives + overlap_collab + factor(np_match) + factor(ej_diff_cat) + count_ego_issues + count_alter_issues + i_match + factor(geo_diff_cat) + distance_n + (1|ego) + (1|ego) + (1|alter), 
              data = m_df, 
              prior = c(prior(cauchy(0, 2.5), class = b), # regularizing based on gelman 2008
                        prior(normal(0,10), class = Intercept)),
              family = bernoulli(link = "logit"), 
              warmup = 1000, #burn in period
              iter = 2000, #actual samples
              chains = 4,
              cores = 2,
              file = "2_outputs/m_full_c",
              control = list(adapt_delta = 0.99))

### c. Selection ----
loo_compare(loo(m_full), loo(m_full_s), loo(m_full_c))


## 5. Model Robustness Checks ----
### a. Full Model with Local Egos -----
#### subset data
m_df_local_egos <- m_df %>%
  filter(ego_local == "local")

#### run paper model with subsetted data
set.seed(1992)
m_rbcheck_local_egos <- brm(dv ~ ego_capacity_n + alter_capacity_n + factor(c_diff_cat) + count_ego_collaboratives + count_alter_collaboratives + overlap_collab + factor(np_match) + factor(ej_diff_cat) + count_ego_issues + count_alter_issues + i_match + factor(geo_diff_cat) + distance_n + (1|ego) + (1|alter), 
                            data = m_df_local_egos, 
                            family = bernoulli(link = "logit"), 
                            prior = c(prior(normal(0,1), class = b),
                                      prior(normal(0,10), class = Intercept)), 
                            warmup = 1000, #burn in period
                            iter = 2000, #actual samples
                            chains = 4,
                            cores = 2,
                            file = "2_outputs/m_rbcheck_local_egos",
                            control = list(adapt_delta = 0.99))

summary(m_rbcheck_local_egos)

### b. Full Model with Regional Egos -----
#### Create a subset of data that only looks at regional egos & see if there are any significant differences in factors that are important for picking collaborators
#### subset data
m_df_reg_egos <- m_df %>%
  filter(ego_local == "regional")

#### run paper model with subsetted data
set.seed(1992)
m_rbcheck_reg_egos <- brm(dv ~ ego_capacity_n + alter_capacity_n + factor(c_diff_cat) + count_ego_collaboratives + count_alter_collaboratives + overlap_collab + factor(np_match) + factor(ej_diff_cat) + count_ego_issues + count_alter_issues + i_match + factor(geo_diff_cat) + distance_n + (1|ego) + (1|alter), 
                          data = m_df_reg_egos, 
                          family = bernoulli(link = "logit"), 
                          prior = c(prior(normal(0,1), class = b),
                                    prior(normal(0,10), class = Intercept)), 
                          warmup = 1000, #burn in period
                          iter = 2000, #actual samples
                          chains = 4,
                          cores = 2,
                          file = "2_outputs/m_rbcheck_reg_egos",
                          control = list(adapt_delta = 0.99))

summary(m_rbcheck_reg_egos)

### c. Full Model without G43 ----
## G43 is an ego that is ALSO a collaborative & therefore is included in the model in multiple ways. This version of the model runs a version of the model data that does not include G43 to see if the results substantively change. 
m_df_subset <- m_df %>%
  filter(ego != "G43" & alter != "G43")

set.seed(1992)
m_full_subset <- brm(dv ~ ego_capacity_n + alter_capacity_n + factor(c_diff_cat) + count_ego_collaboratives + count_alter_collaboratives + overlap_collab + factor(np_match) + factor(ej_diff_cat) + count_ego_issues + count_alter_issues + i_match + factor(geo_diff_cat) + distance_n + (1|ego) + (1|alter), 
                data = m_df_subset, 
                prior = c(prior(normal(0,1), class = b),
                          prior(normal(0,10), class = Intercept)), 
                family = bernoulli(link = "logit"), 
                warmup = 1000, #burn in period
                iter = 2000, #actual samples
                chains = 4,
                cores = 2,
                file = "2_outputs/m_full_subset",
                control = list(adapt_delta = 0.99)) 

### d. Full Model with EJ as factors (ego + alter RE)----
set.seed(1992)
m_full_ejfactor <- brm(dv ~ ego_capacity_n + alter_capacity_n + factor(c_diff_cat) + count_ego_collaboratives + count_alter_collaboratives + overlap_collab + factor(np_match) + factor(ego_ej_mission):factor(alter_ej_mission) + count_ego_issues + count_alter_issues + i_match + factor(geo_diff_cat) + distance_n + (1|ego) + (1|alter), 
              data = m_df, 
              family = bernoulli(link = "logit"), 
              prior = c(prior(normal(0,1), class = b),
                        prior(normal(0,10), class = Intercept)), 
              warmup = 1000, #burn in period
              iter = 2000, #actual samples
              chains = 4,
              cores = 2,
              file = "2_outputs/m_full_ejfactor",
              control = list(adapt_delta = 0.99))

## 6. Model Plots ----
## Read-in model results ----
m_ego <- readRDS("2_outputs/m_ego.rds")
m_ego_alter <- readRDS("2_outputs/m_ego_alter.rds")
m_full_c <- readRDS("2_outputs/m_full_c.rds")
m_full_s <- readRDS("2_outputs/m_full_s.rds")
m_re <- readRDS("2_outputs/m_re.rds")
m_bd <- readRDS("2_outputs/m_bd.rds")
m_full <- readRDS("2_outputs/m_full.rds")
m_full_refined <- readRDS("2_outputs/m_full_refined.rds")
m_full_ejfactor <- readRDS("2_outputs/m_full_ejfactor.rds")

## Coefficient Plots ----
### a. Figures 6-7: Coefficient Plots ----
#### i. Numerical predictors ----
coefs_num <- gather_coefs_numeric(m_full_refined, "Numeric Predictors")

#### ii. Categorical predictors ----
coefs_cat <- gather_coefs_categorical(m_full_refined, "Categorical Predictors")

# full coef dataframe
coefs <- rbind(coefs_num, coefs_cat)

#### iii. Plots -----
##### Resource Exchange -----
coefs_re_plot <- coefs %>% 
  filter(mode == "Resource Exchange") %>%
  mutate(ordering = as.integer(factor(variable)) + as.integer(fct_rev(factor(hyp_type))) + value) %>%
  mutate(ordering = case_when( # look at coefs_re_plot$data if need to update ordering
    par == "Same Capacity" ~ 4,
    par == "Higher Capacity Seeking Lower" ~ 4.1, 
    par == "Lower Capacity Seeking Higher" ~ 4.2,
    par == "Forum Membership Overlap" ~ 6, 
    par == "Ego Capacity" ~ 6.1,
    par == "Ego No. of Forums" ~ 6.2, 
    par == "Alter Capacity" ~ 6.3,
    par == "Alter No. of Forums" ~ 6.4,
    TRUE ~ ordering
  )) %>%
  mutate(par = fct_reorder(par, ordering,  .desc = T)) %>%
  ggplot(., aes(value, par, color = variable, shape = hyp_type, alpha = hyp_type)) +
  geom_point(position = position_dodge(width=.75)) + 
  geom_pointinterval(aes(xmin = .lower, xmax = .upper), position=position_dodge(width=.75), height=0) + 
  geom_vline(xintercept = 0) +
  scale_alpha_discrete(range = c(0.4, 1)) +
  scale_color_manual(values = c("black", "grey40", "grey70"),
                     guide = guide_legend(reverse = TRUE, ncol = 3)) +
  #scale_color_manual(values = c("#88694B",
                                #"#657D94",
                                #"#35483F"),
                     #guide = guide_legend(reverse = TRUE, ncol = 3)) +
  labs(x = "Coefficient", y = "", color = "Variable", shape = "Type") +
  guides(alpha = "none", color = guide_legend(ncol = 1), shape = guide_legend(ncol = 1)) + 
  theme_bw() +
  theme(legend.position = "right", legend.title = element_text(size = 12), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.text = element_text(size = 12))

coefs_re_plot

#ggsave("3_plots/figure_5.png", coefs_re_plot, width = 10, height = 5, dpi = 600, units = "in")

##### Boundary Definition Coefs -----
coefs_bd_plot <- coefs %>% 
  filter(mode == "Boundary Definition") %>%
  mutate(ordering = as.integer(factor(variable)) + as.integer(fct_rev(factor(hyp_type))) + value) %>%
  mutate(ordering = case_when(
    par == "Spatial Distance" ~ 5, 
    par == "Both Local" ~ 3.6,
    par == "Ego No. of Issues" ~ 5.1, 
    par == "Alter No. of Issues" ~ 5.2,
    TRUE ~ ordering
  )) %>%
  mutate(par = fct_reorder(par, ordering,  .desc = T)) %>%
  ggplot(., aes(value, par, color = variable, shape = hyp_type, alpha = hyp_type)) +
  geom_point(position = position_dodge(width=.75)) + 
  geom_pointinterval(aes(xmin = .lower, xmax = .upper), position=position_dodge(width=.75), height=0) + 
  geom_vline(xintercept = 0) +
  scale_alpha_discrete(range = c(0.4, 1)) +
  scale_color_manual(values = c("black", "grey40", "grey70"),
                     guide = guide_legend(reverse = TRUE, ncol = 3)) +
  #scale_color_manual(values = c("#88694B",
                               #"#657D94",
                                #"#35483F"),
                     #guide = guide_legend(reverse = TRUE, ncol = 3)) +
  labs(x = "Coefficient", y = "", color = "Variable", shape = "Type") +
  guides(alpha = "none", color = guide_legend(ncol = 1), shape = guide_legend(ncol = 1)) + 
  theme_bw() +
  theme(legend.position = "right", legend.title = element_text(size = 12), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.text = element_text(size = 12))

coefs_bd_plot

#ggsave("3_plots/figure_6.png", coefs_bd_plot, width = 10, height = 5, dpi = 600, units = "in")

## Figures 8-9: Posterior Prediction Plots -------
#### a. RE predictions ----
##### i. Nonprofit Status ----
# Grand global mean
np_match_ame <- emmeans(m_full_refined, ~ np_match, 
                              var = "np_match", 
                              nesting = NULL, 
                              epred = TRUE, 
                              rg.limit = 20000, #this allows more combination of variables 
                              re_formula = NA) %>%  # no random effects 
  gather_emmeans_draws()

# ame 
np_match_ame %>% median_qi()

# plot
plot_np_match <- np_match_ame %>%
  mutate(np_match = case_when(
    np_match == "no_np_homophily" ~ "Both Non-501c3",
    np_match == "lower" ~ "Non-501c3 \nSeeking 501c3",
    np_match == "higher" ~ "501c3 \nSeeking Non-501c3",
    np_match == "np_homophily" ~ "Both 501c3",
  )) %>%
  ggplot(., 
                           aes(x = np_match, y = .value)) +
  geom_boxplot() + 
  scale_fill_brewer(palette = "Greys") +
  labs(x = "501c3 Status", y = "Tie Probability",
       fill = "Credible interval") +
  theme_minimal() +
  theme(legend.position = "none")
plot_np_match
  
##### ii. Overlap Collab ----
# what is the expectation of predicted draws for overlap collab?
# Grand global mean for overlap collab
overlap_collab_ame <- emmeans(m_full_refined, ~ overlap_collab, 
                               var = "overlap_collab", 
                               at = list(overlap_collab = c(0, 1, 2, 3, 4)),
                               nesting = NULL, 
                               epred = TRUE, 
                               rg.limit = 20000, #this allows more combination of variables 
                               re_formula = NA) %>%  # no random effects 
  gather_emmeans_draws()

# ame
overlap_collab_ame %>% median_hdi()

# plot
plot_collab_memb <- ggplot(overlap_collab_ame, 
                           aes(x = overlap_collab, y = .value)) +
  stat_lineribbon() +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4), labels = c("0 \n(Heterophily)", "1", "2", "3", "4 \n(Homophily)")) +
  scale_fill_brewer(palette = "Greys") +
  labs(x = "Overlapping Forum Membership", y = "Tie Probability",
       fill = "Credible interval") +
  theme_minimal() +
  theme(legend.position = "none")
plot_collab_memb


re_plots <- cowplot::plot_grid(plot_np_match, plot_collab_memb, labels = "auto")
re_plots

#ggsave("3_plots/figure_7.png", re_plots, width = 10, height = 4, dpi = 600, units = "in")

#### b. BD predictions ----
##### i. Issue match ----
i_match_ame <- emmeans(m_full_refined, ~ i_match, # look at effect overlap_collab while taking group-level effects into account
                               var = "i_match", 
                               at = list(i_match = seq(min(m_df$i_match), max(m_df$i_match)), by = 1),
                               nesting = NULL, 
                               epred = TRUE, 
                               rg.limit = 20000, #this allows more combination of variables
                               re_formula = NA) %>%  # no random effects 
  gather_emmeans_draws()

# ame
i_match_ame %>% median_hdi()

# plot
plot_i_match <- ggplot(i_match_ame, 
                           aes(x = i_match, y = .value)) +
  stat_lineribbon() +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6), labels = c("0 \n(Heterophily)", "1", "2", "3", "4", "5", "6 \n(Homophily)")) +
  scale_fill_brewer(palette = "Greys") +
  labs(x = "No. of Shared Issues", y = "Tie Probability",
       fill = "Credible interval") +
  theme_minimal() +
  theme(legend.position = "none")
plot_i_match


##### ii. Geographic Scale ----
geo_ame <- emmeans(m_full_refined, ~ geo_diff_cat, 
                        var = "distance_n", 
                        nesting = NULL, 
                        epred = TRUE, 
                        rg.limit = 20000, #this allows more combination of variables
                        re_formula = NA) %>%  # no random effects 
  gather_emmeans_draws()

# ame
geo_ame %>% median_hdi()

# plot
plot_geo <- geo_ame %>%
  mutate(geo_diff_cat = case_when(
    geo_diff_cat == "local_match" ~ "Both Local",
    geo_diff_cat == "smaller" ~ "Regional Seeking \nLocal",
    geo_diff_cat == "bigger" ~ "Local Seeking \nRegional",
    geo_diff_cat == "regional_match"  ~ "Both Regional",
  )) %>% 
  ggplot(., 
                       aes(x = geo_diff_cat, y = .value)) +
  geom_boxplot() + 
  scale_fill_brewer(palette = "Greys") +
  labs(x = "Geographic Scope", y = "Tie Probability",
       fill = "Credible interval") +
  theme_minimal() +
  theme(legend.position = "none")
plot_geo


##### iii. Distance ----
distance_ame <- emmeans(m_full_refined, ~ distance_n, 
                        var = "distance_n", 
                        at = list(distance_n = c(0, .2, .4, .6, .8, 1)),
                        nesting = NULL, 
                        epred = TRUE, 
                        rg.limit = 20000, #this allows more combination of variables
                        re_formula = NA) %>%  # no random effects 
  gather_emmeans_draws()

# ame
distance_ame %>% median_hdi()

# Grand Mean Plot
# Calc miles at each break in the normalized distance values: 
## summary(m_df$distance): results are in meters
###   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
###    0   11401   69889   62153   95697  260070 

## de-normalize distance formula: normalized_d * (max_d - min_d) + min_d
(.2 *(260070 - 0) + 0)*0.000621371 #0.000621371: conversion factor from meters to miles
# 32 miles
(.4 *(260070 - 0) + 0)*0.000621371 # 64 miles
(.6 *(260070 - 0) + 0)*0.000621371 # 97 miles
(.8 *(260070 - 0) + 0)*0.000621371 # 129 miles
(1 *(260070 - 0) + 0)*0.000621371 # 162 miles

plot_dist <- ggplot(distance_ame, 
                       aes(x = distance_n, y = .value)) +
  stat_lineribbon() +
  scale_x_continuous(breaks = c(0, .2, .4, .6, .8, 1), labels = c("0 \n(Homophily)", "32", "64", "97", "129", "162 \n(Heterophily)")) +
  scale_fill_brewer(palette = "Greys") +
  labs(x = "Spatial Distance (miles)", y = "Tie Probability",
       fill = "Credible interval") +
  theme_minimal() +
  theme(legend.position = "none")

plot_dist
distance_ame %>% median_hdi()
      
bd_plots <- cowplot::plot_grid(plot_i_match, plot_geo, plot_dist, labels = "auto")
bd_plots

#ggsave("3_plots/figure_8.png", bd_plots, width = 10, height = 6, dpi = 600, units = "in")

# Supplemental Information ----
## 1. Additional Descriptive Information ----
### a. Alter and Ego Degree Plot ----
ego_dist <- m_df %>%
  filter(dv == 1) %>%
  group_by(ego) %>%
  summarize(degree = n()) %>%
  ungroup() %>%
  group_by(degree) %>%
  count() %>%
  ggplot(., aes(x = as.factor(degree), y = n)) +
  geom_bar(stat = "identity", fill = "#726186") + 
  xlab("Ego Degree Distribution") + 
  ylab("Frequency") + 
  theme_minimal()
ego_dist

#ggsave("3_plots/figure_a2a.png", ego_dist, width = 6, height = 4, dpi = 600, units = "in")

alter_dist <- m_df %>%
  filter(dv == 1) %>%
  group_by(alter) %>%
  summarize(degree = n()) %>%
  ungroup() %>%
  group_by(degree) %>%
  count() %>%
  ggplot(., aes(x = as.factor(degree), y = n)) +
  geom_bar(stat = "identity", fill = "#726186") + 
  xlab("Alter Degree Distribution") + 
  ylab("Frequency") + 
  theme_minimal()
alter_dist
#ggsave("3_plots/figure_a2b.png", alter_dist, width = 6, height = 4, dpi = 600, units = "in")

org_distributions <- cowplot::plot_grid(ego_dist, alter_dist, labels = "auto")



## 2. Model Comparison ----------------------------------------------
### a. Coefficient Table ----
sjPlot::tab_model(m_re, m_bd, m_full_refined, dv.labels = c("Resource Exchange", "Boundary Definition", "Full Model"), file = "2_outputs/paper_model_table.doc")

sjPlot::tab_model(m_full_refined, m_full_refined2, dv.labels = c( "Full Model", "Full Model 2"), file = "2_outputs/paper_model_table2.doc")

### b. EJ Commitment Model with EJ variables as factors ----
# use emmeans to calculate intervals:
# columns
colnames(m_full_ejfactor$data)

# predictor list
cat_predictors  <- c("ego_ej_mission*alter_ej_mission")

# empty data.frame for for loop calc
contrasts <- data.frame()

# calc intercepts
for(i in cat_predictors) {
  d <- contrasts_calc_ejfactor(i)
  contrasts <- rbind(contrasts, d)
}

## df cleaning
coefs_cat <- contrasts %>%
  mutate(contrast = case_when(
    contrast == "ego_ej_mission1 alter_ej_mission0 effect" ~ "Ego Periperal EJ: Alter No EJ",
    contrast == "ego_ej_mission2 alter_ej_mission0 effect" ~ "Ego Key Work Area EJ: Alter No EJ", 
    contrast == "ego_ej_mission3 alter_ej_mission0 effect" ~ "Ego Central EJ: Alter No EJ",
    contrast == "ego_ej_mission1 alter_ej_mission1 effect" ~ "Ego Periperal EJ: Alter Periperal EJ",
    contrast == "ego_ej_mission2 alter_ej_mission1 effect" ~ "Ego Key Work Area EJ: Alter Periperal EJ",
    contrast == "ego_ej_mission3 alter_ej_mission1 effect" ~ "Ego Central EJ: Alter Periperal EJ",
    contrast == "ego_ej_mission1 alter_ej_mission2 effect" ~ "Ego Periperal EJ: Alter Key Work Area EJ",
    contrast == "ego_ej_mission2 alter_ej_mission2 effect" ~ "Ego Key Work Area EJ: Alter Key Work Area EJ", 
    contrast == "ego_ej_mission3 alter_ej_mission2 effect" ~ "Ego Central EJ: Alter Key Work Area EJ",
    contrast == "ego_ej_mission1 alter_ej_mission3 effect" ~ "Ego Periperal EJ: Alter Central EJ", 
    contrast == "ego_ej_mission2 alter_ej_mission3 effect" ~ "Ego Key Work Area EJ: Alter Central EJ",
    contrast == "ego_ej_mission3 alter_ej_mission3 effect" ~ "Ego Central EJ: Alter Central EJ"
  )) %>%
  mutate(contrast = factor(contrast, levels = c(
    "Ego Periperal EJ: Alter No EJ",
    "Ego Key Work Area EJ: Alter No EJ", 
    "Ego Central EJ: Alter No EJ",
    "Ego Periperal EJ: Alter Periperal EJ",
    "Ego Key Work Area EJ: Alter Periperal EJ",
    "Ego Central EJ: Alter Periperal EJ",
    "Ego Periperal EJ: Alter Key Work Area EJ",
    "Ego Key Work Area EJ: Alter Key Work Area EJ", 
    "Ego Central EJ: Alter Key Work Area EJ",
    "Ego Periperal EJ: Alter Central EJ", 
    "Ego Key Work Area EJ: Alter Central EJ",
    "Ego Central EJ: Alter Central EJ"
  ))) 

## Plot factors
contrast_ejfactor <- coefs_cat %>%
  ggplot(., aes(reorder(contrast, -estimate), estimate)) +
  geom_point(position = position_dodge(width=.75)) + 
  geom_pointinterval(aes(ymin = .lower, ymax = .upper), position=position_dodge(width=.75), height=0) +  
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("#88694B"),
                     guide = guide_legend(reverse = TRUE, ncol = 3)) +
  labs(x = "", y = "Coefficient", color = "Hypothesis", shape = "Variable") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_text(size = 12), axis.text.x = element_text(size = 12, angle = 90, vjust = 1, hjust=1), axis.text.y = element_text(size = 12), legend.text = element_text(size = 12))

contrast_ejfactor

#ggsave("3_plots/figure_a1.png", contrast_ejfactor, width = 10, height = 6, dpi = 600, units = "in")

### c. Loo Comparison Table ----
loo(m_re)
loo(m_bd)
loo(m_full_refined)


loo(m_full_c)
loo(m_full_s)

### d. VPC Table ----
sjPlot::tab_model(m_ego, m_ego_alter, m_full_ego, m_full_refined, dv.labels = c("VPC - Ego", "VPC - Ego + Alter", "Full Model - Ego", "Full Model - Ego + Alter"), file = "2_outputs/vpc_full_model_comp_table.doc")

## 3. Model Diagnostics ----
### a. Posterior Predictive Check ----
ppc <- pp_check(m_full_refined, type = "dens_overlay", ndraws = 100)
##ggsave("3_plots/ppc.png", ppc, width = 6, height = 4, dpi = 600, units = "in")

### b. Trace Plots ----
trace <- plot(m_full_refined)
##ggsave("3_plots/trace_plots.png", trace, width = 6, height = 4, dpi = 600, units = "in")

## 3. Prior Comparison ----
loo(m_full_c)
loo(m_full_s)
loo(m_full_refined)

## 4. Model Robustness Check -----------------------------------------------
### DV Bounding -----
### The model presented in the manuscript bounds potential ego-alter ties by geographic overlap between groups
### How do the results change with different bounding scenarios?
### a. Ego Subsets ----
m_rbcheck_local_egos <- readRDS("2_outputs/m_rbcheck_local_egos.rds")
m_rbcheck_reg_egos <- readRDS("2_outputs/m_rbcheck_reg_egos.rds")
m_full_refined <- readRDS("2_outputs/m_full_refined.rds")

coefs1 <- gather_coefs(m_full_refined, "Full Model")
coefs2 <- gather_coefs(m_rbcheck_local_egos, "Local Egos Only")
coefs3 <- gather_coefs(m_rbcheck_reg_egos, "Regional Egos Only")

combined_coefs_plot <- combine_coefs_plot3(coefs1, coefs2, coefs3)
combined_coefs_plot

#ggsave("3_plots/figure_a3.png", combined_coefs_plot, width = 9, height = 9, dpi = 600, units = "in")

### b. No ECJW Model Plot ----
m_full_subset <- readRDS("2_outputs/m_full_subset.rds")
coefs1 <- gather_coefs(m_full_refined, "Full Model")
coefs2 <- gather_coefs(m_full_subset, "Full Model without EJCW")

combined_coefs_plot2 <- combine_coefs_plot2(coefs1, coefs2)
combined_coefs_plot2

#ggsave("3_plots/figure_a4.png", combined_coefs_plot2, width = 9, height = 9, dpi = 600, units = "in")

