## normalize data
normalize <- function(x){(x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))}

# gather coefficient estimates from model results 
gather_coefs <-  function(m, name) {
  variables <- c("b_ego_capacity_n", "b_ego_ej_mission", "b_ego_np_501c3", "b_alter_np_501c3", "b_alter_capacity_n", "b_c_diff_n", "b_alter_ej_mission", "b_alter_localnonlocal", "b_distance_n", "b_np_match", "b_c_diff_catlower","b_c_diff_cathigher", "b_i_match_s", "b_ej_diff_catlower", "b_ej_diff_cathigher", "b_count_alter_collaboratives", "b_geo_diff_catbigger", "b_geo_diff_catsmaller", "b_ego_localregional","b_alter_localregional",  "b_geo_diff_catlocal_match", "b_geo_diff_catregional_match", "b_count_ego_issues", "b_count_alter_issues", "b_i_match", "b_count_ego_collaboratives", "b_overlap_collab", "b_np_matchno_np_homophily", "b_np_matchno_match")
  d <- m %>% 
    tidy_draws() %>%
    gather_variables() %>%
    filter(.variable %in% variables) %>% 
    dplyr:: rename(par=.variable, value=.value) %>%
    median_qi(.width = c(.89, .5))%>%
    mutate(type = case_when(
      par == "b_alter_np_501c3" ~ "Capacity", 
      par == "b_alter_capacity_n" ~ "Capacity", 
      par == "b_ego_capacity_n" ~ "Capacity",
      par == "b_alter_ej_mission" ~ "Identity", 
      par == "b_ego_ej_mission" ~ "Identity", 
      par == "b_distance_n" ~ "Geography", 
      par == "b_alter_localnonlocal" ~ "Geography", 
      par == "b_np_match" ~ "Capacity",
      par == "b_c_diff_catlower" ~ "Capacity",
      par == "b_c_diff_cathigher" ~ "Capacity",
      par == "b_i_match_s" ~ "Identity", 
      par == "b_ej_diff_catlower" ~ "Identity",
      par == "b_ej_diff_cathigher" ~ "Identity",
      par == "b_alter_localnonlocal" ~ "Geography",
      par == "b_count_alter_collaboratives_s" ~ "Capacity",
      par == "b_count_ego_issues" ~ "Identity", 
      par == "b_count_alter_issues" ~ "Identity",
      par == "b_i_match" ~ "Identity", 
      par == "b_geo_diff_catbigger" ~ "Geography", 
      par == "b_geo_diff_catsmaller" ~ "Geography", 
      par == "b_ego_localregional" ~ "Geography",
      par == "b_alter_localregional" ~ "Geography",
      par == "b_geo_diff_catlocal_match" ~ "Geography", 
      par == "b_geo_diff_catregional_match" ~ "Geography", 
      par == "b_overlap_collab" ~ "Capacity",
      par == "b_ego_np_501c3" ~ "Capacity", 
      par == "b_c_diff_n" ~ "Capacity", 
      par == "b_count_ego_collaboratives" ~ "Capacity", 
      par == "b_count_alter_collaboratives" ~ "Capacity", 
      par == "b_np_matchno_np_homophily" ~ "Capacity",
      par == "b_np_matchno_match" ~ "Capacity"
    )) %>%
    mutate(hyp = case_when(
      par == "b_alter_np_501c3" ~ "Resource Exchange", 
      par == "b_alter_capacity_n" ~ "Resource Exchange",
      par == "b_ego_capacity_n" ~ "Resource Exchange",
      par == "b_ego_ej_mission" ~ "Boundary Definition",
      par == "b_count_alter_issues_s" ~ "Boundary Definition", 
      par == "b_alter_ej_mission" ~ "Boundary Definition", 
      par == "b_distance_n" ~ "Boundary Definition", 
      par == "b_alter_localnonlocal" ~ "Boundary Definition", 
      par == "b_np_match" ~ "Resource Exchange",
      par == "b_c_diff_catlower" ~ "Resource Exchange", 
      par == "b_c_diff_cathigher" ~ "Resource Exchange", 
      par == "b_i_match_s" ~ "Boundary Definition", 
      par == "b_ej_diff_catlower" ~ "Boundary Definition",
      par == "b_ej_diff_cathigher" ~ "Boundary Definition",
      par == "b_alter_localnonlocal" ~ "Boundary Definition",
      par == "b_count_alter_collaboratives_s" ~ "Resource Exchange",
      par == "b_geo_diff_catregional_match" ~ "Boundary Definition",
      par == "b_geo_diff_catlocal_match" ~ "Boundary Definition",
      par == "b_count_ego_issues" ~ "Boundary Definition", 
      par == "b_count_alter_issues" ~ "Boundary Definition",
      par == "b_i_match" ~ "Boundary Definition", 
      par == "b_geo_diff_catbigger" ~ "Boundary Definition", 
      par == "b_geo_diff_catsmaller" ~ "Boundary Definition", 
      par == "b_ego_localregional" ~ "Boundary Definition", 
      par == "b_alter_localregional" ~ "Boundary Definition",
      par == "b_ego_np_501c3" ~ "Resource Exchange", 
      par == "b_c_diff_n" ~ "Resource Exchange", 
      par == "b_count_ego_collaboratives" ~ "Resource Exchange", 
      par == "b_count_alter_collaboratives" ~ "Resource Exchange", 
      par == "b_overlap_collab" ~ "Resource Exchange", 
      par == "b_np_matchno_np_homophily" ~ "Resource Exchange",
      par == "b_np_matchno_match" ~ "Resource Exchange"
    )) %>%
    mutate(type = factor(type, c("Capacity", "Identity", "Geography"))) %>%
    mutate(par = case_when(
      par == "b_alter_np_501c3" ~ "Alter: 501c3 Status", 
      par == "b_alter_capacity_n" ~ "Alter: Capacity", 
      par == "b_ego_capacity_n" ~ "Ego: Capacity",
      par == "b_ego_ej_mission" ~ "Ego: EJ Commitment",
      par == "b_count_alter_issues_s" ~ "Alter: No. of Issues", 
      par == "b_alter_ej_mission" ~ "Alter: EJ Commitment", 
      par == "b_distance_n" ~ "Ego-Alter: Distance Between Home Offices", 
      par == "b_alter_localnonlocal" ~  "Alter: Regional Group", 
      par == "b_np_match" ~ "Ego-Alter: 501c3 \nStatus Match",
      par == "b_c_diff_catlower" ~ "Ego-Alter: Lower Capacity Alter than Ego",
      par == "b_c_diff_cathigher" ~ "Ego-Alter: Higher Capacity Alter than Ego",
      par == "b_i_match_s" ~ "Ego-Alter: Issue Overlap", 
      par == "b_ej_diff_catlower" ~ "Ego-Alter: Lower Alter EJ Commitment than Ego",
      par == "b_ej_diff_cathigher" ~ "Ego-Alter: Higher Alter EJ Commitment than Ego",
      par == "b_count_alter_collaboratives_s" ~ "Alter: No. of Collaboratives",
      par == "b_geo_diff_catlocal_match" ~ "Ego-Alter: Both Local Groups",
      par == "b_geo_diff_catregional_match" ~ "Ego-Alter: Both Regional Groups",
      par == "b_count_ego_issues" ~ "Ego: No. of Issues", 
      par == "b_count_alter_issues" ~ "Alter: No. of Issues",
      par == "b_i_match" ~ "Ego-Alter: No. of Matching Issues", 
      par == "b_geo_diff_catbigger" ~ "Ego-Alter: Local Group seeking Regional Group", 
      par == "b_geo_diff_catsmaller" ~ "Ego-Alter: Regional Group seeking Local Group", 
      par == "b_ego_localregional" ~ "Ego: Regional Group", 
      par == "b_alter_localregional" ~ "Alter: Regional Group", 
      par == "b_overlap_collab" ~ "Ego-Alter: Collaborative Membership Overlap", 
      par == "b_ego_np_501c3" ~ "Ego: 501c3 Status", 
      par == "b_c_diff_n" ~ "Ego-Alter: Capacity Difference", 
      par == "b_count_ego_collaboratives" ~ "Ego: No. of Collaboratives", 
      par == "b_count_alter_collaboratives" ~ "Alter: No. of Collaboratives", 
      par == "b_np_matchno_np_homophily" ~ "Ego-Alter: Both Groups without 501c3",
      par == "b_np_matchno_match" ~ "Ego-Alter: Both Groups with Different 501c3"
    )) %>%
    mutate(variable = case_when(
      str_detect(par, regex("Capacity*")) ~ "Resource Capacity",
      str_detect(par, regex("Collaborative*")) ~ "Collaborative",
      str_detect(par, regex("501c3*")) ~ "501c3 Status", 
      str_detect(par, regex("Commitment*")) ~ "EJ Commitment", 
      str_detect(par, regex("Issues*")) ~ "EJ Issues", 
      str_detect(par, regex("Group*")) ~ "Geographic Scale", 
      str_detect(par, regex("Distance*")) ~ "Distance"
    )) %>%
    mutate(ordering = -as.integer(factor(variable)) + value) %>%
    mutate(par = fct_reorder(par, ordering,  .desc = F)) %>%
    mutate(model = name)
  return(d)
} 

# gather coefficient estimates from model results 
gather_coefs_re <-  function(m, name) {
  variables <- c("b_ego_capacity_n","b_ego_np_501c3", "b_alter_np_501c3", "b_alter_capacity_n", "b_c_diff_n", "b_np_match", "b_c_diff_catlower","b_c_diff_cathigher", "b_count_alter_collaboratives", "b_count_ego_collaboratives", "b_overlap_collab", "b_np_matchno_np_homophily", "b_np_matchno_match")
  d <- m %>% 
    tidy_draws() %>%
    gather_variables() %>%
    filter(.variable %in% variables) %>% 
    dplyr:: rename(par=.variable, value=.value) %>%
    median_qi(.width = c(.89, .5))%>%
    mutate(type = case_when(
      par == "b_alter_np_501c3" ~ "Capacity", 
      par == "b_np_match" ~ "Capacity",
      par == "b_c_diff_catlower" ~ "Capacity",
      par == "b_c_diff_cathigher" ~ "Capacity",
      par == "b_count_alter_collaboratives_s" ~ "Capacity",
      par == "b_overlap_collab" ~ "Capacity",
      par == "b_ego_np_501c3" ~ "Capacity", 
      par == "b_c_diff_n" ~ "Capacity", 
      par == "b_count_ego_collaboratives" ~ "Capacity", 
      par == "b_count_alter_collaboratives" ~ "Capacity", 
      par == "b_np_matchno_np_homophily" ~ "Capacity",
      par == "b_np_matchno_match" ~ "Capacity"
    )) %>%
    mutate(hyp = case_when(
      par == "b_alter_np_501c3" ~ "Resource Exchange", 
      par == "b_alter_capacity_n" ~ "Resource Exchange",
      par == "b_ego_capacity_n" ~ "Resource Exchange",
      par == "b_np_match" ~ "Resource Exchange",
      par == "b_c_diff_catlower" ~ "Resource Exchange", 
      par == "b_c_diff_cathigher" ~ "Resource Exchange", 
      par == "b_count_alter_collaboratives_s" ~ "Resource Exchange",
      par == "b_ego_np_501c3" ~ "Resource Exchange", 
      par == "b_c_diff_n" ~ "Resource Exchange", 
      par == "b_count_ego_collaboratives" ~ "Resource Exchange", 
      par == "b_count_alter_collaboratives" ~ "Resource Exchange", 
      par == "b_overlap_collab" ~ "Resource Exchange", 
      par == "b_np_matchno_np_homophily" ~ "Resource Exchange",
      par == "b_np_matchno_match" ~ "Resource Exchange"
    )) %>%
    mutate(type = factor(type, c("Capacity", "Identity", "Geography"))) %>%
    mutate(par = case_when(
      par == "b_alter_np_501c3" ~ "Alter: 501c3 Status", 
      par == "b_alter_capacity_n" ~ "Alter: Capacity", 
      par == "b_ego_capacity_n" ~ "Ego: Capacity",
      par == "b_np_match" ~ "Ego-Alter: 501c3 \nStatus Match",
      par == "b_c_diff_catlower" ~ "Ego-Alter: Lower Capacity Alter than Ego",
      par == "b_c_diff_cathigher" ~ "Ego-Alter: Higher Capacity Alter than Ego",
      par == "b_count_alter_collaboratives_s" ~ "Alter: No. of Collaboratives",
      par == "b_overlap_collab" ~ "Ego-Alter: Collaborative Membership Overlap", 
      par == "b_ego_np_501c3" ~ "Ego: 501c3 Status", 
      par == "b_c_diff_n" ~ "Ego-Alter: Capacity Difference", 
      par == "b_count_ego_collaboratives" ~ "Ego: No. of Collaboratives", 
      par == "b_count_alter_collaboratives" ~ "Alter: No. of Collaboratives", 
      par == "b_np_matchno_np_homophily" ~ "Ego-Alter: Both Groups without 501c3",
      par == "b_np_matchno_match" ~ "Ego-Alter: Both Groups with Different 501c3"
    )) %>%
    mutate(variable = case_when(
      str_detect(par, regex("Capacity*")) ~ "Resource Capacity",
      str_detect(par, regex("Collaborative*")) ~ "Collaborative",
      str_detect(par, regex("501c3*")) ~ "501c3 Status"
    )) %>%
    mutate(ordering = -as.integer(factor(variable)) + value) %>%
    mutate(par = fct_reorder(par, ordering,  .desc = F)) %>%
    mutate(model = name)
  return(d)
} 

# gather coefficient estimates from model results 
gather_coefs_bd <-  function(m, name) {
  variables <- c("b_ego_ej_mission", "b_alter_ej_mission", "b_alter_localnonlocal", "b_distance_n","b_i_match_s", "b_ej_diff_catlower", "b_ej_diff_cathigher", "b_geo_diff_catbigger", "b_geo_diff_catsmaller", "b_ego_localregional","b_alter_localregional",  "b_geo_diff_catlocal_match", "b_geo_diff_catregional_match", "b_count_ego_issues", "b_count_alter_issues", "b_i_match")
  d <- m %>% 
    tidy_draws() %>%
    gather_variables() %>%
    filter(.variable %in% variables) %>% 
    dplyr:: rename(par=.variable, value=.value) %>%
    median_qi(.width = c(.89, .5))%>%
    mutate(type = case_when(
      par == "b_alter_ej_mission" ~ "Identity", 
      par == "b_ego_ej_mission" ~ "Identity", 
      par == "b_distance_n" ~ "Geography", 
      par == "b_alter_localnonlocal" ~ "Geography",
      par == "b_i_match_s" ~ "Identity", 
      par == "b_ej_diff_catlower" ~ "Identity",
      par == "b_ej_diff_cathigher" ~ "Identity",
      par == "b_alter_localnonlocal" ~ "Geography",
      par == "b_count_ego_issues" ~ "Identity", 
      par == "b_count_alter_issues" ~ "Identity",
      par == "b_i_match" ~ "Identity", 
      par == "b_geo_diff_catbigger" ~ "Geography", 
      par == "b_geo_diff_catsmaller" ~ "Geography", 
      par == "b_ego_localregional" ~ "Geography",
      par == "b_alter_localregional" ~ "Geography",
      par == "b_geo_diff_catlocal_match" ~ "Geography", 
      par == "b_geo_diff_catregional_match" ~ "Geography"
    )) %>%
    mutate(hyp = case_when(
      par == "b_ego_ej_mission" ~ "Boundary Definition",
      par == "b_count_alter_issues_s" ~ "Boundary Definition", 
      par == "b_alter_ej_mission" ~ "Boundary Definition", 
      par == "b_distance_n" ~ "Boundary Definition", 
      par == "b_alter_localnonlocal" ~ "Boundary Definition",  
      par == "b_i_match_s" ~ "Boundary Definition", 
      par == "b_ej_diff_catlower" ~ "Boundary Definition",
      par == "b_ej_diff_cathigher" ~ "Boundary Definition",
      par == "b_alter_localnonlocal" ~ "Boundary Definition",
      par == "b_geo_diff_catregional_match" ~ "Boundary Definition",
      par == "b_geo_diff_catlocal_match" ~ "Boundary Definition",
      par == "b_count_ego_issues" ~ "Boundary Definition", 
      par == "b_count_alter_issues" ~ "Boundary Definition",
      par == "b_i_match" ~ "Boundary Definition", 
      par == "b_geo_diff_catbigger" ~ "Boundary Definition", 
      par == "b_geo_diff_catsmaller" ~ "Boundary Definition", 
      par == "b_ego_localregional" ~ "Boundary Definition", 
      par == "b_alter_localregional" ~ "Boundary Definition"
    )) %>%
    mutate(type = factor(type, c("Capacity", "Identity", "Geography"))) %>%
    mutate(par = case_when(
      par == "b_ego_ej_mission" ~ "Ego: EJ Commitment",
      par == "b_count_alter_issues_s" ~ "Alter: No. of Issues", 
      par == "b_alter_ej_mission" ~ "Alter: EJ Commitment", 
      par == "b_distance_n" ~ "Ego-Alter: Distance Between Home Offices", 
      par == "b_alter_localnonlocal" ~  "Alter: Regional Group", 
      par == "b_i_match_s" ~ "Ego-Alter: Issue Overlap", 
      par == "b_ej_diff_catlower" ~ "Ego-Alter: Lower Alter EJ Commitment than Ego",
      par == "b_ej_diff_cathigher" ~ "Ego-Alter: Higher Alter EJ Commitment than Ego",
      par == "b_geo_diff_catlocal_match" ~ "Ego-Alter: Both Local Groups",
      par == "b_geo_diff_catregional_match" ~ "Ego-Alter: Both Regional Groups",
      par == "b_count_ego_issues" ~ "Ego: No. of Issues", 
      par == "b_count_alter_issues" ~ "Alter: No. of Issues",
      par == "b_i_match" ~ "Ego-Alter: No. of Matching Issues", 
      par == "b_geo_diff_catbigger" ~ "Ego-Alter: Local Group seeking Regional Group", 
      par == "b_geo_diff_catsmaller" ~ "Ego-Alter: Regional Group seeking Local Group", 
      par == "b_ego_localregional" ~ "Ego: Regional Group", 
      par == "b_alter_localregional" ~ "Alter: Regional Group"
    )) %>%
    mutate(variable = case_when(
      str_detect(par, regex("Commitment*")) ~ "EJ Commitment", 
      str_detect(par, regex("Issues*")) ~ "EJ Issues", 
      str_detect(par, regex("Group*")) ~ "Geographic Scale", 
      str_detect(par, regex("Distance*")) ~ "Distance"
    )) %>%
    mutate(ordering = -as.integer(factor(variable)) + value) %>%
    mutate(par = fct_reorder(par, ordering,  .desc = F)) %>%
    mutate(model = name)
  return(d)
} 
# Combine two models into one coefficient plot
combine_coefs_plot2 <- function(m1, m2){
  df <- bind_rows(m1, m2)
  
  coef_palette <- c("#162B24",
                    "#726186")
  
  coefs.plot <- df %>% 
    mutate(ordering = case_when( # this NEEDS WORK if i want to use it in the appendix
      par ==  "Ego-Alter: Collaborative Membership Overlap" ~ - 2,
      par == "Ego-Alter: Distance Between Home Offices" ~ -10, #put it at the bottom of the plot
      par == "Ego-Alter: No. of Matching Issues" ~ -3.8, 
      par == "Ego-Alter: Lower Capacity Ego than Alter" ~ -5,
      TRUE ~ ordering
    )) %>%
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
    mutate(ordering = case_when( # this NEEDS WORK if i want to use it in the appendix
      par ==  "Ego-Alter: Collaborative Membership Overlap" ~ - 2,
      par == "Ego-Alter: Distance Between Home Offices" ~ -10, #put it at the bottom of the plot
      par == "Ego-Alter: No. of Matching Issues" ~ -3.8, 
      par == "Ego-Alter: Lower Capacity Ego than Alter" ~ -5,
      TRUE ~ ordering
    )) %>%
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
