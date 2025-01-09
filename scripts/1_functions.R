## function to normalize data
normalize <- function(x){(x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))}

# gather coefficient estimates from model results for numerical predictors
gather_coefs_numeric <-  function(m, name) {
  variables <- c("b_ego_capacity_n", "b_ego_ej_mission", "b_alter_capacity_n", "b_c_diff_n", "b_alter_ej_mission",  "b_distance_n", "b_i_match", "b_count_alter_collaboratives", "b_count_ego_issues", "b_count_alter_issues", "b_count_ego_collaboratives", "b_overlap_collab")
  d <- m %>% 
    tidy_draws() %>%
    gather_variables() %>%
    filter(.variable %in% variables) %>% 
    dplyr:: rename(par=.variable, value=.value) %>%
    median_qi(.width = c(.95, .5))%>%
    mutate(hyp_type = case_when(
      par == "b_alter_capacity_n" ~ "Control", 
      par == "b_ego_capacity_n" ~ "Control",
      par == "b_alter_ej_mission" ~ "Control", 
      par == "b_ego_ej_mission" ~ "Control", 
      par == "b_distance_n" ~ "Relational", 
      par == "b_count_alter_collaboratives_s" ~ "Control",
      par == "b_count_ego_issues" ~ "Control", 
      par == "b_count_alter_issues" ~ "Control",
      par == "b_i_match" ~ "Relational", 
      par == "b_overlap_collab" ~ "Relational", 
      par == "b_count_ego_collaboratives" ~ "Control", 
      par == "b_count_alter_collaboratives" ~ "Control" 
    )) %>%
    mutate(variable_type = case_when(
      par == "b_alter_capacity_n" ~ "Control", 
      par == "b_ego_capacity_n" ~ "Control",
      par == "b_alter_ej_mission" ~ "Control", 
      par == "b_ego_ej_mission" ~ "Control", 
      par == "b_distance_n" ~ "Relational", 
      par == "b_count_alter_collaboratives_s" ~ "Control",
      par == "b_count_ego_issues" ~ "Control", 
      par == "b_count_alter_issues" ~ "Control",
      par == "b_i_match" ~ "Relational", 
      par == "b_overlap_collab" ~ "Relational",
      par == "b_ego_np_501c3" ~ "Control", 
      par == "b_c_diff_n" ~ "Relational", 
      par == "b_count_ego_collaboratives" ~ "Control", 
      par == "b_count_alter_collaboratives" ~ "Control"
    )) %>%
    mutate(mode = case_when(
      par == "b_alter_capacity_n" ~ "Resource Exchange",
      par == "b_ego_capacity_n" ~ "Resource Exchange",
      par == "b_ego_ej_mission" ~ "Boundary Definition",
      par == "b_count_alter_issues" ~ "Boundary Definition", 
      par == "b_alter_ej_mission" ~ "Boundary Definition", 
      par == "b_distance_n" ~ "Boundary Definition", 
      par == "b_i_match" ~ "Boundary Definition", 
      par == "b_count_alter_collaboratives_s" ~ "Resource Exchange",
      par == "b_count_ego_issues" ~ "Boundary Definition", 
      par == "b_count_alter_issues" ~ "Boundary Definition",
      par == "b_i_match" ~ "Boundary Definition", 
      par == "b_c_diff_n" ~ "Resource Exchange", 
      par == "b_count_ego_collaboratives" ~ "Resource Exchange", 
      par == "b_count_alter_collaboratives" ~ "Resource Exchange", 
      par == "b_overlap_collab" ~ "Resource Exchange"
    )) %>%
    mutate(mode = factor(mode, c("Resource Exchange", "Boundary Definition"))) %>%
    mutate(variable_type = factor(variable_type, c("Relational", "Control"))) %>%
    mutate(par = case_when(
      par == "b_alter_capacity_n" ~ "Alter Capacity", 
      par == "b_ego_capacity_n" ~ "Ego Capacity",
      par == "b_ego_ej_mission" ~ "Ego EJ Commitment",
      par == "b_count_alter_issues_s" ~ "Alter No. of Issues", 
      par == "b_alter_ej_mission" ~ "Alter EJ Commitment", 
      par == "b_distance_n" ~ "Spatial Distance", 
      par == "b_count_alter_collaboratives" ~ "Alter No. of Collaboratives",
      par == "b_count_ego_issues" ~ "Ego No. of Issues", 
      par == "b_count_alter_issues" ~ "Alter No. of Issues",
      par == "b_i_match" ~ "No. of Matching Issues", 
      par == "b_overlap_collab" ~ "Collaborative Membership Overlap", 
      par == "b_count_ego_collaboratives" ~ "Ego No. of Collaboratives", 
      par == "b_count_alter_collaboratives" ~ "Alter No. of Collaboratives"
    )) %>%
    mutate(variable = case_when(
      str_detect(par, regex("Capacity*")) ~ "Capacity",
      str_detect(par, regex("Collaborative*")) ~ "Collaborative",
      str_detect(par, regex("501c3*")) ~ "501c3 Status", 
      str_detect(par, regex("Commitment*")) ~ "EJ Commitment", 
      str_detect(par, regex("Issues*")) ~ "EJ Issues", 
      str_detect(par, regex("Group*")) ~ "Geography", 
      str_detect(par, regex("Distance*")) ~ "Geography"
    )) %>%
    #mutate(ordering = -as.integer(factor(variable)) + value) %>%
    #mutate(par = fct_reorder(par, ordering,  .desc = F)) %>%
   # mutate(model = name)
    select(par, value, .lower, .upper, .width, variable, mode, hyp_type, variable_type)
  return(d)
} 

# contrasts function
contrasts_calc <- function(m, spec) {
  d.50 <- emmeans(m, specs = as.formula(paste0("~", spec)), nesting = NULL) %>%
    contrast(method = "eff", adjust = "bonferroni") %>%
    confint(level = 0.50) %>%
    mutate(variable = spec,
           .width = .50) %>%
    rename(.lower = lower.HPD, .upper = upper.HPD)
  
  d.95 <- emmeans(m, specs = as.formula(paste0("~", spec)), nesting = NULL) %>%
    contrast(method = "eff", adjust = "bonferroni") %>%
    confint(level = 0.95) %>%
    mutate(variable = spec,
           .width = .95) %>%
    rename(.lower = lower.HPD, .upper = upper.HPD)
  
  d <- rbind(d.50, d.95)
  
  return(d)
}

contrasts_calc_ejfactor <- function(spec) {
  d.50 <- emmeans(m_full_ejfactor, specs = as.formula(paste0("~", spec)), nesting = NULL) %>%
    contrast(method = "eff", adjust = "bonferroni") %>%
    confint(level = 0.50) %>%
    mutate(variable = spec,
           .width = .50) %>%
    rename(.lower = lower.HPD, .upper = upper.HPD)
  
  d.95 <- emmeans(m_full_ejfactor, specs = as.formula(paste0("~", spec)), nesting = NULL) %>%
    contrast(method = "eff", adjust = "bonferroni") %>%
    confint(level = 0.95) %>%
    mutate(variable = spec,
           .width = .95) %>%
    rename(.lower = lower.HPD, .upper = upper.HPD)
  
  d <- rbind(d.50, d.95)
  
  return(d)
}

# Categorical coefficient estimates 
gather_coefs_categorical <-  function(m, name){
# use emmeans to calculate intervals:
# The basic sequence is that you create the comparison with emmeans, then calculate contrasts (with method='eff' to calculate the difference from average and adjust='bonferroni' for the Bonferroni method of correcting for multiple testing). Then since you want the confidence interval instead of the estimates, you pass the result to the confit function. 
# columns
colnames(m$data)

# predictor list
cat_predictors  <- c("np_match", "geo_diff_cat", "c_diff_cat", "ej_diff_cat")

# empty data.frame for for loop calc
contrasts <- data.frame()

# iterate on factors from the paper model
for(i in cat_predictors) {
  d <- contrasts_calc(m = m, spec = i)
  contrasts <- rbind(contrasts, d)
}

# clean up df so it is ready to plot
coefs_cat <- contrasts %>%
  mutate(contrast = case_when(
    contrast == "no_np_homophily effect" ~ "Both Non-501c3",
    contrast == "lower effect" & variable == "np_match" ~ "Non-501c3 Seeking 501c3",
    contrast == "higher effect" & variable == "np_match" ~ "501c3 Seeking Non-501c3",
    contrast == "np_homophily effect" ~ "Both 501c3",
    contrast == "local_match effect" ~ "Both Local",
    contrast == "smaller effect" ~ "Regional Seeking Local",
    contrast == "bigger effect" ~ "Local Seeking Regional",
    contrast == "regional_match effect"  ~ "Both Regional",
    contrast == "match effect" & variable == "c_diff_cat" ~ "Same Capacity",
    contrast == "lower effect" & variable == "c_diff_cat" ~ "Seeking Lower Capacity",
    contrast == "higher effect" & variable == "c_diff_cat" ~ "Seeking Higher Capacity",
    contrast == "match effect" & variable == "ej_diff_cat" ~ "Matching EJ Commitment",
    contrast == "lower effect" & variable == "ej_diff_cat" ~ "Seeking Less EJ", 
    contrast == "higher effect" & variable == "ej_diff_cat" ~ "Seeking More EJ"
  )) %>%
  mutate(mode = case_when(
    variable %in% c("geo_diff_cat","ej_diff_cat") ~ "Boundary Definition",
    variable %in% c("np_match", "c_diff_cat") ~ "Resource Exchange"
  )) %>%
  mutate(variable = case_when(
    variable == "geo_diff_cat" ~ "Geography",
    variable == "ej_diff_cat" ~ "EJ Commitment", 
    variable == "np_match" ~ "501c3 Status", 
    variable == "c_diff_cat" ~ "Capacity"
  )) %>%
  mutate(hyp_type = "Relational") %>%
  mutate(mode = factor(mode, c("Resource Exchange", "Boundary Definition")),
         variable_type = "Relational") %>%
  rename(par = contrast,
         value = estimate) # make it match the numerical dataframe
}

# Coefficient estimates from model results without doing categorical contrasts
gather_coefs <-  function(m, name) {
  variables <- c("b_ego_capacity_n", "b_alter_capacity_n", "b_factorc_diff_catlower", "b_factorc_diff_cathigher", "b_count_ego_collaboratives", "b_count_alter_collaboratives", "b_overlap_collab", "b_factornp_matchlower", "b_factornp_matchhigher", "b_factornp_matchno_np_homophily", "b_factorej_diff_catlower", "b_factorej_diff_cathigher", "b_count_ego_issues", "b_count_alter_issues",  "b_i_match", "b_factorgeo_diff_catregional_match", "b_factorgeo_diff_catsmaller", "b_factorgeo_diff_catbigger", "b_distance_n")  
  d <- m %>% 
    tidy_draws() %>%
    gather_variables() %>%
    filter(.variable %in% variables) %>% 
    dplyr:: rename(par=.variable, value=.value) %>%
    median_qi(.width = c(.95, .5))%>%
    mutate(type = case_when(
      par == "b_alter_np_501c3" ~ "Capacity", 
      par == "b_alter_capacity_n" ~ "Capacity", 
      par == "b_ego_capacity_n" ~ "Capacity",
      par == "b_alter_ej_mission" ~ "Boundary Definition", 
      par == "b_ego_ej_mission" ~ "Boundary Definition", 
      par == "b_distance_n" ~ "Boundary Definition", 
      par == "b_alter_localnonlocal" ~ "Boundary Definition", 
      par == "b_factorc_diff_catlower" ~ "Capacity",
      par == "b_factorc_diff_cathigher" ~ "Capacity",
      par == "b_i_match_s" ~ "Boundary Definition", 
      par == "b_factorej_diff_catlower" ~ "Boundary Definition",
      par == "b_factorej_diff_cathigher" ~ "Boundary Definition",
      par == "b_alter_localnonlocal" ~ "Boundary Definition",
      par == "b_count_alter_collaboratives_s" ~ "Capacity",
      par == "b_count_ego_issues" ~ "Boundary Definition", 
      par == "b_count_alter_issues" ~ "Boundary Definition",
      par == "b_i_match" ~ "Boundary Definition", 
      par == "b_factorgeo_diff_catbigger" ~ "Boundary Definition", 
      par == "b_factorgeo_diff_catsmaller" ~ "Boundary Definition", 
      par == "b_ego_localregional" ~ "Boundary Definition",
      par == "b_alter_localregional" ~ "Boundary Definition",
      par == "b_factorgeo_diff_catlocal_match" ~ "Boundary Definition", 
      par == "b_factorgeo_diff_catregional_match" ~ "Boundary Definition", 
      par == "b_overlap_collab" ~ "Capacity",
      par == "b_ego_np_501c3" ~ "Capacity", 
      par == "b_c_diff_n" ~ "Capacity", 
      par == "b_count_ego_collaboratives" ~ "Capacity", 
      par == "b_count_alter_collaboratives" ~ "Capacity", 
      par == "b_factornp_matchno_np_homophily" ~ "Capacity",
      par == "b_factornp_matchlower" ~ "Capacity", 
      par == "b_factornp_matchhigher" ~ "Capacity"
    )) %>%
    mutate(hyp = case_when(
      par == "b_alter_np_501c3" ~ "Individual", 
      par == "b_alter_capacity_n" ~ "Individual", 
      par == "b_ego_capacity_n" ~ "Individual",
      par == "b_alter_ej_mission" ~ "Individual", 
      par == "b_ego_ej_mission" ~ "Individual", 
      par == "b_distance_n" ~ "Relational", 
      par == "b_alter_localnonlocal" ~ "Relational", 
      par == "b_factorc_diff_catlower" ~ "Relational",
      par == "b_factorc_diff_cathigher" ~ "Relational",
      par == "b_i_match_s" ~ "Relational", 
      par == "b_factorej_diff_catlower" ~ "Relational",
      par == "b_factorej_diff_cathigher" ~ "Relational",
      par == "b_count_alter_collaboratives_s" ~ "Individual",
      par == "b_count_ego_issues" ~ "Individual", 
      par == "b_count_alter_issues" ~ "Individual",
      par == "b_i_match" ~ "Relational", 
      par == "b_factorgeo_diff_catbigger" ~ "Relational", 
      par == "b_factorgeo_diff_catsmaller" ~ "Relational", 
      par == "b_ego_localregional" ~ "Individual",
      par == "b_alter_localregional" ~ "Individual",
      par == "b_factorgeo_diff_catlocal_match" ~ "Relational", 
      par == "b_factorgeo_diff_catregional_match" ~ "Relational", 
      par == "b_overlap_collab" ~ "Relational",
      par == "b_ego_np_501c3" ~ "Individual", 
      par == "b_c_diff_n" ~ "Relational", 
      par == "b_count_ego_collaboratives" ~ "Individual", 
      par == "b_count_alter_collaboratives" ~ "Individual", 
      par == "b_factornp_matchno_np_homophily" ~ "Relational",
      par == "b_factornp_matchlower" ~ "Relational",
      par == "b_factornp_matchhigher" ~ "Relational"
    )) %>%
    mutate(mode = case_when(
      par == "b_alter_np_501c3" ~ "Resource Exchange", 
      par == "b_alter_capacity_n" ~ "Resource Exchange",
      par == "b_ego_capacity_n" ~ "Resource Exchange",
      par == "b_ego_ej_mission" ~ "Boundary Definition",
      par == "b_count_alter_issues_s" ~ "Boundary Definition", 
      par == "b_alter_ej_mission" ~ "Boundary Definition", 
      par == "b_distance_n" ~ "Boundary Definition", 
      par == "b_alter_localnonlocal" ~ "Boundary Definition", 
      par == "b_factorc_diff_catlower" ~ "Resource Exchange", 
      par == "b_factorc_diff_cathigher" ~ "Resource Exchange", 
      par == "b_i_match_s" ~ "Boundary Definition", 
      par == "b_factorej_diff_catlower" ~ "Boundary Definition",
      par == "b_factorej_diff_cathigher" ~ "Boundary Definition",
      par == "b_alter_localnonlocal" ~ "Boundary Definition",
      par == "b_count_alter_collaboratives_s" ~ "Resource Exchange",
      par == "b_factorgeo_diff_catregional_match" ~ "Boundary Definition",
      par == "b_factorgeo_diff_catlocal_match" ~ "Boundary Definition",
      par == "b_count_ego_issues" ~ "Boundary Definition", 
      par == "b_count_alter_issues" ~ "Boundary Definition",
      par == "b_i_match" ~ "Boundary Definition", 
      par == "b_factorgeo_diff_catbigger" ~ "Boundary Definition", 
      par == "b_factorgeo_diff_catsmaller" ~ "Boundary Definition", 
      par == "b_ego_localregional" ~ "Boundary Definition", 
      par == "b_alter_localregional" ~ "Boundary Definition",
      par == "b_ego_np_501c3" ~ "Resource Exchange", 
      par == "b_c_diff_n" ~ "Resource Exchange", 
      par == "b_count_ego_collaboratives" ~ "Resource Exchange", 
      par == "b_count_alter_collaboratives" ~ "Resource Exchange", 
      par == "b_overlap_collab" ~ "Resource Exchange", 
      par == "b_factornp_matchno_np_homophily" ~ "Resource Exchange",
      par == "b_factornp_matchlower" ~ "Resource Exchange",
      par == "b_factornp_matchhigher" ~ "Resource Exchange"
    )) %>%
    mutate(type = factor(type, c("Capacity", "Boundary Definition"))) %>%
    mutate(hyp = factor(hyp, c("Relational", "Individual"))) %>%
    mutate(par = case_when(
      par == "b_alter_np_501c3" ~ "Alter 501c3 Status", 
      par == "b_alter_capacity_n" ~ "Alter Capacity", 
      par == "b_ego_capacity_n" ~ "Ego Capacity",
      par == "b_ego_ej_mission" ~ "Ego EJ Commitment",
      par == "b_count_alter_issues_s" ~ "Alter No. of Issues", 
      par == "b_alter_ej_mission" ~ "Alter: EJ Commitment", 
      par == "b_distance_n" ~ "Heterophily: Spatial Distance", 
      par == "b_alter_localnonlocal" ~  "Alter Regional Group", 
      par == "b_factorc_diff_catlower" ~ "Heterophily: Seeking Lower Capacity",
      par == "b_factorc_diff_cathigher" ~ "Heterophily: Seeking Higher Capacity",
      par == "b_i_match_s" ~ "Homophily: Issue Overlap", 
      par == "b_factorej_diff_catlower" ~ "Heterophily: Seeking Less EJ",
      par == "b_factorej_diff_cathigher" ~ "Heterophily: Seeking More EJ",
      par == "b_count_alter_collaboratives_s" ~ "Alter No. of Collaboratives",
      par == "b_factorgeo_diff_catlocal_match" ~ "Homophily: Both Local",
      par == "b_factorgeo_diff_catregional_match" ~ "Homophily: Both Regional",
      par == "b_count_ego_issues" ~ "Ego No. of Issues", 
      par == "b_count_alter_issues" ~ "Alter No. of Issues",
      par == "b_i_match" ~ "Homophily: No. of Matching Issues", 
      par == "b_factorgeo_diff_catbigger" ~ "Heterophily: Local Seeking Regional", 
      par == "b_factorgeo_diff_catsmaller" ~ "Heterophily: Regional Seeking Local", 
      par == "b_ego_localregional" ~ "Ego Regional Group", 
      par == "b_alter_localregional" ~ "Alter Regional Group", 
      par == "b_overlap_collab" ~ "Homophily: Collaborative Membership Overlap", 
      par == "b_ego_np_501c3" ~ "Ego 501c3 Status", 
      par == "b_c_diff_n" ~ "Heterophily: Capacity Difference", 
      par == "b_count_ego_collaboratives" ~ "Ego No. of Collaboratives", 
      par == "b_count_alter_collaboratives" ~ "Alter No. of Collaboratives", 
      par == "b_factornp_matchno_np_homophily" ~ "Homophily: Both Groups without 501c3",
      par == "b_factornp_matchlower" ~ "Heterophily: 501c3 Ego Seeking a Non-501c3 Alter", 
      par == "b_factornp_matchhigher" ~ "Heterophily: Non-501c3 Ego Seeking a 501c3 Alter"
    )) %>%
    mutate(variable = case_when(
      str_detect(par, regex("Capacity*")) ~ "Capacity",
      str_detect(par, regex("Collaborative*")) ~ "Collaborative",
      str_detect(par, regex("501c3*")) ~ "501c3 Status", 
      str_detect(par, regex("Commitment*")) ~ "EJ Commitment", 
      str_detect(par, regex("Issues*")) ~ "EJ Issues", 
      str_detect(par, regex("Group*")) ~ "Geography", 
      str_detect(par, regex("Distance*")) ~ "Geography"
    )) %>%
    #mutate(ordering = -as.integer(factor(variable)) + value) %>%
    #mutate(par = fct_reorder(par, ordering,  .desc = F)) %>%
    mutate(model = name)
  return(d)
} 

# Combine two models into one coefficient plot
combine_coefs_plot2 <- function(m1, m2){
  df <- bind_rows(m1, m2)
  
  coef_palette <- c("#162B24",
                    "#726186")
  
  coefs.plot <- df %>% 
    # mutate(ordering = case_when( # this NEEDS WORK if i want to use it in the appendix
    #   par ==  "Homophily: Collaborative Membership Overlap" ~ - 2,
    #   par == "Heterophily: Spatial Distance" ~ -10, #put it at the bottom of the plot
    #   par == "Homophily: No. of Matching Issues" ~ -3.8, 
    #   par == "Heterophily: Lower Capacity Ego than Alter" ~ -5,
    #   TRUE ~ ordering
    # )) %>%
    mutate(par = fct_reorder(par, value, .desc = F)) %>% # reorder the coef names by ordering variable
    ggplot(., aes(value, par, color = model)) +
    geom_point(position = position_dodge(width=.75)) + 
    geom_pointinterval(aes(xmin = .lower, xmax = .upper), position=position_dodge(width=.75), height=0) + 
    geom_vline(xintercept = 0) +
    scale_color_manual(values = coef_palette,
                       guide = guide_legend(reverse = TRUE, ncol = 1)) +
    labs(x = "Coefficient", y = "", color = "Model") +
    theme_bw() +
    theme(legend.position = "bottom", legend.title = element_text(size = 14), axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), legend.text = element_text(size = 14))
  return(coefs.plot)
}

# Combine three models into one coefficient plot
combine_coefs_plot3 <- function(m1, m2, m3){
  df <- bind_rows(m1, m2, m3)
  
  coef_palette <- c("#162B24",
                    "#807462",
                    "#726186")
  
  coefs.plot <- df %>% 
    # mutate(ordering = case_when( # this NEEDS WORK if i want to use it in the appendix
    #   par ==  "Homophily: Collaborative Membership Overlap" ~ - 2,
    #   par == "Heterophily: Spatial Distance" ~ -10, #put it at the bottom of the plot
    #   par == "Homophily: No. of Matching Issues" ~ -3.8, 
    #   par == "Heterophily: Lower Capacity Ego than Alter" ~ -5,
    #   TRUE ~ ordering
    # )) %>%
    mutate(par = fct_reorder(par, value, .desc = F)) %>% # reorder the coef names by ordering variable
    ggplot(., aes(value, par, color = model)) +
    geom_point(position = position_dodge(width=.75)) + 
    geom_pointinterval(aes(xmin = .lower, xmax = .upper), position=position_dodge(width=.75), height=0) + 
    geom_vline(xintercept = 0) +
    scale_color_manual(values = coef_palette,
                       guide = guide_legend(reverse = TRUE, ncol = 1)) +
    labs(x = "Coefficient", y = "", color = "Model") +
    theme_bw() +
    theme(legend.position = "bottom", legend.title = element_text(size = 14), axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), legend.text = element_text(size = 14))
  return(coefs.plot)
}

