## normalize data
normalize <- function(x){(x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))}

# gather coefficient estimates from model results 
gather_coefs <-  function(m, name) {
  variables <- c("b_ego_capacity_n", "b_ego_ej_mission", "b_ego_np_501c3", "b_alter_np_501c3", "b_alter_capacity_n", "b_c_diff_n", "b_alter_ej_mission", "b_alter_localnonlocal", "b_distance_n", "b_np_match", "b_c_diff_catlower","b_c_diff_cathigher", "b_i_match_s", "b_ej_diff_catlower", "b_ej_diff_cathigher", "b_count_alter_collaboratives", "b_geo_diff_catbigger", "b_geo_diff_catsmaller", "b_ego_localregional","b_alter_localregional",  "b_geo_diff_catlocal_match", "b_geo_diff_catregional_match", "b_count_ego_issues", "b_count_alter_issues", "b_i_match", "b_count_ego_collaboratives", "b_overlap_collab", "b_np_matchnp_homophily", "b_np_matchlower", "b_np_matchhigher")
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
      par == "b_alter_ej_mission" ~ "Boundary Definition", 
      par == "b_ego_ej_mission" ~ "Boundary Definition", 
      par == "b_distance_n" ~ "Boundary Definition", 
      par == "b_alter_localnonlocal" ~ "Boundary Definition", 
      par == "b_np_match" ~ "Capacity",
      par == "b_c_diff_catlower" ~ "Capacity",
      par == "b_c_diff_cathigher" ~ "Capacity",
      par == "b_i_match_s" ~ "Boundary Definition", 
      par == "b_ej_diff_catlower" ~ "Boundary Definition",
      par == "b_ej_diff_cathigher" ~ "Boundary Definition",
      par == "b_alter_localnonlocal" ~ "Boundary Definition",
      par == "b_count_alter_collaboratives_s" ~ "Capacity",
      par == "b_count_ego_issues" ~ "Boundary Definition", 
      par == "b_count_alter_issues" ~ "Boundary Definition",
      par == "b_i_match" ~ "Boundary Definition", 
      par == "b_geo_diff_catbigger" ~ "Boundary Definition", 
      par == "b_geo_diff_catsmaller" ~ "Boundary Definition", 
      par == "b_ego_localregional" ~ "Boundary Definition",
      par == "b_alter_localregional" ~ "Boundary Definition",
      par == "b_geo_diff_catlocal_match" ~ "Boundary Definition", 
      par == "b_geo_diff_catregional_match" ~ "Boundary Definition", 
      par == "b_overlap_collab" ~ "Capacity",
      par == "b_ego_np_501c3" ~ "Capacity", 
      par == "b_c_diff_n" ~ "Capacity", 
      par == "b_count_ego_collaboratives" ~ "Capacity", 
      par == "b_count_alter_collaboratives" ~ "Capacity", 
      par == "b_np_matchnp_homophily" ~ "Capacity",
      par == "b_np_matchlower" ~ "Capacity", 
      par == "b_np_matchhigher" ~ "Capacity"
    )) %>%
    mutate(hyp = case_when(
      par == "b_alter_np_501c3" ~ "Individual", 
      par == "b_alter_capacity_n" ~ "Individual", 
      par == "b_ego_capacity_n" ~ "Individual",
      par == "b_alter_ej_mission" ~ "Individual", 
      par == "b_ego_ej_mission" ~ "Individual", 
      par == "b_distance_n" ~ "Relational", 
      par == "b_alter_localnonlocal" ~ "Relational", 
      par == "b_np_match" ~ "Relational",
      par == "b_c_diff_catlower" ~ "Relational",
      par == "b_c_diff_cathigher" ~ "Relational",
      par == "b_i_match_s" ~ "Relational", 
      par == "b_ej_diff_catlower" ~ "Relational",
      par == "b_ej_diff_cathigher" ~ "Relational",
      par == "b_count_alter_collaboratives_s" ~ "Individual",
      par == "b_count_ego_issues" ~ "Individual", 
      par == "b_count_alter_issues" ~ "Individual",
      par == "b_i_match" ~ "Relational", 
      par == "b_geo_diff_catbigger" ~ "Relational", 
      par == "b_geo_diff_catsmaller" ~ "Relational", 
      par == "b_ego_localregional" ~ "Individual",
      par == "b_alter_localregional" ~ "Individual",
      par == "b_geo_diff_catlocal_match" ~ "Relational", 
      par == "b_geo_diff_catregional_match" ~ "Relational", 
      par == "b_overlap_collab" ~ "Relational",
      par == "b_ego_np_501c3" ~ "Individual", 
      par == "b_c_diff_n" ~ "Relational", 
      par == "b_count_ego_collaboratives" ~ "Individual", 
      par == "b_count_alter_collaboratives" ~ "Individual", 
      par == "b_np_matchnp_homophily" ~ "Relational",
      par == "b_np_matchno_match" ~ "Relational",
      par == "b_np_matchhigher" ~ "Relational"
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
      par == "b_np_matchnp_homophily" ~ "Resource Exchange",
      par == "b_np_matchlower" ~ "Resource Exchange",
      par == "b_np_matchhigher" ~ "Resource Exchange"
    )) %>%
    mutate(type = factor(type, c("Capacity", "Boundary Definition"))) %>%
    mutate(hyp = factor(hyp, c("Relational", "Individual"))) %>%
    mutate(par = case_when(
      par == "b_alter_np_501c3" ~ "Alter 501c3 Status", 
      par == "b_alter_capacity_n" ~ "Alter Capacity", 
      par == "b_ego_capacity_n" ~ "Ego Capacity",
      par == "b_ego_ej_mission" ~ "Ego EJ Commitment",
      par == "b_count_alter_issues_s" ~ "Alter No. of Issues", 
      par == "b_alter_ej_mission" ~ "Alter:EJ Commitment", 
      par == "b_distance_n" ~ "Heterophily: Distance Between Home Offices", 
      par == "b_alter_localnonlocal" ~  "Alter Regional Group", 
      par == "b_np_match" ~ "Homophily: 501c3 \nStatus Match",
      par == "b_c_diff_catlower" ~ "Heterophily: Lower Capacity Alter than Ego",
      par == "b_c_diff_cathigher" ~ "Heterophily: Higher Capacity Alter than Ego",
      par == "b_i_match_s" ~ "Homophily: Issue Overlap", 
      par == "b_ej_diff_catlower" ~ "Heterophily: Lower Alter EJ Commitment than Ego",
      par == "b_ej_diff_cathigher" ~ "Heterophily: Higher Alter EJ Commitment than Ego",
      par == "b_count_alter_collaboratives_s" ~ "Alter No. of Collaboratives",
      par == "b_geo_diff_catlocal_match" ~ "Homophily: Both Local Groups",
      par == "b_geo_diff_catregional_match" ~ "Homophily: Both Regional Groups",
      par == "b_count_ego_issues" ~ "Ego No. of Issues", 
      par == "b_count_alter_issues" ~ "Alter No. of Issues",
      par == "b_i_match" ~ "Homophily: No. of Matching Issues", 
      par == "b_geo_diff_catbigger" ~ "Heterophily: Local Group seeking Regional Group", 
      par == "b_geo_diff_catsmaller" ~ "Heterophily: Regional Group seeking Local Group", 
      par == "b_ego_localregional" ~ "Ego Regional Group", 
      par == "b_alter_localregional" ~ "Alter Regional Group", 
      par == "b_overlap_collab" ~ "Homophily: Collaborative Membership Overlap", 
      par == "b_ego_np_501c3" ~ "Ego 501c3 Status", 
      par == "b_c_diff_n" ~ "Heterophily: Capacity Difference", 
      par == "b_count_ego_collaboratives" ~ "Ego No. of Collaboratives", 
      par == "b_count_alter_collaboratives" ~ "Alter No. of Collaboratives", 
      par == "b_np_matchnp_homophily" ~ "Homophily: Both Groups with 501c3",
      par == "b_np_matchlower" ~ "Heterophily: 501c3 Ego Seeking a Non-501c3 Alter", 
      par == "b_np_matchhigher" ~ "Heterophily: Non-501c3 Ego Seeking a 501c3 Alter"
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
    mutate(ordering = -as.integer(factor(variable)) + value) %>%
    mutate(par = fct_reorder(par, ordering,  .desc = F)) %>%
    mutate(model = name)
  return(d)
} 

# gather coefficient estimates from model results 
gather_coefs_re <-  function(m, name) {
  variables <- c("b_ego_capacity_n","b_ego_np_501c3", "b_alter_np_501c3", "b_alter_capacity_n", "b_c_diff_n", "b_np_match", "b_c_diff_catlower","b_c_diff_cathigher", "b_count_alter_collaboratives", "b_count_ego_collaboratives", "b_overlap_collab", "b_np_matchnp_homophily", "b_np_matchlower", "b_np_matchhigher")
  d <- m %>% 
    tidy_draws() %>%
    gather_variables() %>%
    filter(.variable %in% variables) %>% 
    dplyr:: rename(par=.variable, value=.value) %>%
    median_qi(.width = c(.89, .5))%>%
    mutate(type = case_when(
      par == "b_ego_capacity_n" ~ "Capacity",
      par == "b_alter_capacity_n" ~ "Capacity",
      par == "b_alter_np_501c3" ~ "501c3 Status", 
      par == "b_c_diff_catlower" ~ "Capacity",
      par == "b_c_diff_cathigher" ~ "Capacity",
      par == "b_count_alter_collaboratives_s" ~ "Collaborative \nMembership",
      par == "b_overlap_collab" ~ "Collaborative \nMembership",
      par == "b_ego_np_501c3" ~ "501c3 Status", 
      par == "b_count_ego_collaboratives" ~ "Collaborative \nMembership", 
      par == "b_count_alter_collaboratives" ~ "Collaborative \nMembership", 
      par == "b_np_matchnp_homophily" ~ "501c3 Status",
      par == "b_np_matchlower" ~ "501c3 Status", 
      par == "b_np_matchhigher" ~ "501c3 Status"
    )) %>%
    mutate(hyp = case_when(
      par == "b_alter_np_501c3" ~ "Individual", 
      par == "b_alter_capacity_n" ~ "Individual",
      par == "b_ego_capacity_n" ~ "Individual",
      par == "b_np_match" ~ "Individual",
      par == "b_c_diff_catlower" ~ "Relational", 
      par == "b_c_diff_cathigher" ~ "Relational", 
      par == "b_count_alter_collaboratives_s" ~ "Individual",
      par == "b_ego_np_501c3" ~ "Individual", 
      par == "b_c_diff_n" ~ "Relational", 
      par == "b_count_ego_collaboratives" ~ "Individual", 
      par == "b_count_alter_collaboratives" ~ "Individual", 
      par == "b_overlap_collab" ~ "Relational", 
      par == "b_np_matchnp_homophily" ~ "Relational",
      par == "b_np_matchlower" ~ "Relational",
      par == "b_np_matchhigher" ~ "Relational"
    )) %>%
    mutate(type = factor(type, c("Capacity", "Boundary Definition"))) %>%
    mutate(hyp = factor(hyp, c("Relational", "Individual"))) %>%
    mutate(par = case_when(
      par == "b_alter_np_501c3" ~ "Alter 501c3 Status", 
      par == "b_alter_capacity_n" ~ "Alter Capacity", 
      par == "b_ego_capacity_n" ~ "Ego Capacity",
      par == "b_np_match" ~ "Homophily: 501c3 \nStatus Match",
      par == "b_c_diff_catlower" ~ "Heterophily: Lower Capacity \nAlter than Ego",
      par == "b_c_diff_cathigher" ~ "Heterophily: Higher Capacity \nAlter than Ego",
      par == "b_count_alter_collaboratives_s" ~ "Alter No. of Collaboratives",
      par == "b_overlap_collab" ~ "Homophily: Collaborative Membership \nOverlap", 
      par == "b_ego_np_501c3" ~ "Ego 501c3 Status", 
      par == "b_count_ego_collaboratives" ~ "Ego No. of Collaboratives", 
      par == "b_count_alter_collaboratives" ~ "Alter No. of Collaboratives", 
      par == "b_np_matchnp_homophily" ~ "Homophily: Both Groups without \n501c3",
      par == "b_np_matchlower" ~ "Heterophily: 501c3 Ego Seeking \na Non-501c3 Alter", 
      par == "b_np_matchhigher" ~ "Heterophily: Non-501c3 Ego Seeking \na 501c3 Alter"
    )) %>%
    mutate(variable = case_when(
      str_detect(par, regex("Capacity*")) ~ "Resource Capacity",
      str_detect(par, regex("Collaborative*")) ~ "Collaborative",
      str_detect(par, regex("501c3*")) ~ "501c3 Status"
    )) %>%
    mutate(ordering = -as.integer(factor(hyp)) + value) %>%
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
      par == "b_alter_ej_mission" ~ "Boundary Definition", 
      par == "b_ego_ej_mission" ~ "Boundary Definition", 
      par == "b_distance_n" ~ "Boundary Definition", 
      par == "b_alter_localnonlocal" ~ "Boundary Definition",
      par == "b_i_match_s" ~ "Boundary Definition", 
      par == "b_ej_diff_catlower" ~ "Boundary Definition",
      par == "b_ej_diff_cathigher" ~ "Boundary Definition",
      par == "b_alter_localnonlocal" ~ "Boundary Definition",
      par == "b_count_ego_issues" ~ "Boundary Definition", 
      par == "b_count_alter_issues" ~ "Boundary Definition",
      par == "b_i_match" ~ "Boundary Definition", 
      par == "b_geo_diff_catbigger" ~ "Boundary Definition", 
      par == "b_geo_diff_catsmaller" ~ "Boundary Definition", 
      par == "b_ego_localregional" ~ "Boundary Definition",
      par == "b_alter_localregional" ~ "Boundary Definition",
      par == "b_geo_diff_catlocal_match" ~ "Boundary Definition", 
      par == "b_geo_diff_catregional_match" ~ "Boundary Definition"
    )) %>%
    mutate(hyp = case_when(
      par == "b_ego_ej_mission" ~ "Individual",
      par == "b_count_alter_issues_s" ~ "Individual", 
      par == "b_alter_ej_mission" ~ "Individual", 
      par == "b_distance_n" ~ "Relational", 
      par == "b_alter_localnonlocal" ~ "Relational",  
      par == "b_ej_diff_catlower" ~ "Relational",
      par == "b_ej_diff_cathigher" ~ "Relational",
      par == "b_geo_diff_catregional_match" ~ "Relational",
      par == "b_geo_diff_catlocal_match" ~ "Relational",
      par == "b_count_ego_issues" ~ "Individual", 
      par == "b_count_alter_issues" ~ "Individual",
      par == "b_i_match" ~ "Relational", 
      par == "b_geo_diff_catbigger" ~ "Relational", 
      par == "b_geo_diff_catsmaller" ~ "Relational", 
      par == "b_ego_localregional" ~ "Individual", 
      par == "b_alter_localregional" ~ "Individual"
    )) %>%
    mutate(type = factor(type, c("Capacity", "Boundary Definition"))) %>%
    mutate(hyp = factor(hyp, c("Relational", "Individual"))) %>%
    mutate(par = case_when(
      par == "b_ego_ej_mission" ~ "Ego EJ Commitment",
      par == "b_count_alter_issues_s" ~ "Alter No. of Issues", 
      par == "b_alter_ej_mission" ~ "Alter EJ Commitment", 
      par == "b_distance_n" ~ "Heterophily: Distance Between \nHome Offices", 
      par == "b_alter_localnonlocal" ~  "Alter Regional Group", 
      par == "b_i_match_s" ~ "Homophily: Issue Overlap", 
      par == "b_ej_diff_catlower" ~ "Heterophily: Lower Alter EJ \nCommitment than Ego",
      par == "b_ej_diff_cathigher" ~ "Heterophily: Higher Alter EJ \nCommitment than Ego",
      par == "b_geo_diff_catlocal_match" ~ "Homophilly: Both Local Groups",
      par == "b_geo_diff_catregional_match" ~ "Homophily: Both Regional Groups",
      par == "b_count_ego_issues" ~ "Ego No. of Issues", 
      par == "b_count_alter_issues" ~ "Alter No. of Issues",
      par == "b_i_match" ~ "Homophily: No. of Matching Issues", 
      par == "b_geo_diff_catbigger" ~ "Heterophily: Local Group seeking \nRegional Group", 
      par == "b_geo_diff_catsmaller" ~ "Heterophily: Regional Group seeking \nLocal Group", 
      par == "b_ego_localregional" ~ "Ego Regional Group", 
      par == "b_alter_localregional" ~ "Alter Regional Group"
    )) %>%
    mutate(variable = case_when(
      str_detect(par, regex("Commitment*")) ~ "EJ Commitment", 
      str_detect(par, regex("Issues*")) ~ "EJ Issues", 
      str_detect(par, regex("Group*")) ~ "Geography", 
      str_detect(par, regex("Distance*")) ~ "Geography"
    )) %>%
    mutate(ordering = -as.integer(factor(hyp)) + value) %>%
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
    # mutate(ordering = case_when( # this NEEDS WORK if i want to use it in the appendix
    #   par ==  "Homophily: Collaborative Membership Overlap" ~ - 2,
    #   par == "Heterophily: Distance Between Home Offices" ~ -10, #put it at the bottom of the plot
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
    #   par == "Heterophily: Distance Between Home Offices" ~ -10, #put it at the bottom of the plot
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
