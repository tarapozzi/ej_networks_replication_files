h1_data <- m_df %>%
  tidyr::expand(c_diff_cat, np_match,overlap_collab, i_match, ej_diff_cat, geo_diff_cat) %>%
  mutate(id = 2001)


h1_predictions <- m_full_paper %>% 
  epred_draws(newdata = expand_grid(ego_capacity_n = median(m_df$ego_capacity_n), # median
                                    alter_capacity_n = median(m_df$alter_capacity_n), # median
                                    c_diff_cat = levels(m_df$c_diff_cat)[1], # homophily
                                    count_ego_collaboratives = median(m_df$count_ego_collaboratives), # median 
                                    count_alter_collaboratives = median(m_df$count_alter_collaboratives), # median
                                    overlap_collab = max(m_df$overlap_collab),
                                    ego_np_501c3 = median(m_df$ego_np_501c3), # median 
                                    alter_np_501c3 = median(m_df$alter_np_501c3), # median 
                                    np_match = levels(m_df$np_match)[1], # most common
                                    alter_ej_mission = 2, # most common
                                    ego_ej_mission = 3, 
                                    ej_diff_cat = levels(m_df$ej_diff_cat)[1], # most common category 
                                    count_ego_issues = median(m_df$count_ego_issues), # median
                                    count_alter_issues = median(m_df$count_alter_issues), # median
                                    i_match = max(m_df$i_match), # median
                                    ego_local = "local", # most common
                                    alter_local = "local", # most common
                                    geo_diff_cat = levels(m_df$geo_diff_cat)[1], # local_match
                                    distance_n = min(m_df$distance_n)),
              re_formula = NA) # does not include group effects 

plot_collab_memb <- ggplot(collab_memb_predictions, 
                           aes(x = overlap_collab, y = .epred)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Blues") +
  labs(x = "Overlapping Collaborative Membership", y = "Predicted Collaborative Tie Probability",
       fill = "Credible interval") +
  theme_minimal() +
  theme(legend.position = "none")
plot_collab_memb

#### Collaborative Membership -----
collab_memb_predictions <- m_full %>% 
  epred_draws(newdata = expand_grid(ego_capacity_n = 0.01, # median
                                    alter_capacity_n = 0.001, # median
                                    c_diff_cat = "match", # most common
                                    count_ego_collaboratives = 1.00, # median 
                                    count_alter_collaboratives = 0, # median
                                    overlap_collab = seq(0, 4, by = 1),
                                    ego_np_501c3 = 1, # median 
                                    alter_np_501c3 = 1, # median 
                                    np_match = "np_homophily", # most common
                                    alter_ej_mission = 2, # most common
                                    ego_ej_mission = 3, 
                                    ej_diff_cat = "match", # most common category 
                                    count_ego_issues = 3, # median
                                    count_alter_issues = 3, # median
                                    i_match = 1, # median
                                    ego_local = "local", # most common
                                    alter_local = "local", # most common
                                    geo_diff_cat = "local_match", # most common
                                    distance_n = 0.2684),
              re_formula = NA) # does not include group effects 

plot_collab_memb <- ggplot(collab_memb_predictions, 
                           aes(x = overlap_collab, y = .epred)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Blues") +
  labs(x = "Overlapping Collaborative Membership", y = "Predicted Collaborative Tie Probability",
       fill = "Credible interval") +
  theme_minimal() +
  theme(legend.position = "none")
plot_collab_memb

#### Alter Collab ----
alter_collab <- m_full %>%
  epred_draws(newdata = expand_grid(ego_capacity_n = 0.01, # median
                                    alter_capacity_n = 0.001, # median
                                    c_diff_cat = "match", # most common
                                    count_ego_collaboratives = 1.00, # median
                                    count_alter_collaboratives = seq(0, 7, by = 1), # median
                                    overlap_collab = 0,
                                    ego_np_501c3 = 1, # median
                                    alter_np_501c3 = 1, # median
                                    np_match = "np_homophily", # most common
                                    alter_ej_mission = 2, # most common
                                    ego_ej_mission = 3,
                                    ej_diff_cat = "match", # most common category
                                    count_ego_issues = 3, # median
                                    count_alter_issues = 3, # median
                                    i_match = 1, # median
                                    ego_local = "local", # most common
                                    alter_local = "local", # most common
                                    geo_diff_cat = "local_match", # most common
                                    distance_n = 0.2684),
              re_formula = NA) # this means ALL groups will be included

plot_alter_collab <- ggplot(alter_collab,
                            aes(x = count_alter_collaboratives, y = .epred)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Blues") +
  labs(x = "Alter Collaborative Membership", y = "",
       fill = "Credible interval") +
  theme_minimal() +
  theme(legend.position = "non")
plot_alter_collab

#### Issue Match ----
i_match_predictions <- m_full %>% 
  epred_draws(newdata = expand_grid(ego_capacity_n = 0.01, # median
                                    alter_capacity_n = 0.001, # median
                                    c_diff_cat = "match", # most common
                                    count_ego_collaboratives = 1.00, # median 
                                    count_alter_collaboratives = 0, # median
                                    overlap_collab = 0,
                                    ego_np_501c3 = 1, # median 
                                    alter_np_501c3 = 1, # median 
                                    np_match = "np_homophily", # most common
                                    alter_ej_mission = 2, # most common
                                    ego_ej_mission = 3, 
                                    ej_diff_cat = "match", # most common category 
                                    count_ego_issues = 3, # median
                                    count_alter_issues = 3, # median
                                    i_match = seq(0, 6, by = 1), 
                                    ego_local = "local", # most common
                                    alter_local = "local", # most common
                                    geo_diff_cat = "local_match", # most common
                                    distance_n = 0.2684), # median
              re_formula = NA) # this means ALL groups will be included

plot_i_match_predictions <- ggplot(i_match_predictions, 
                                   aes(x = i_match, y = .epred)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Blues") +
  labs(x = "No. of Matching Issues", y = "Predicted Collaborative Tie Probability",
       fill = "Credible interval") +
  theme_minimal() +
  theme(legend.position = "none")
plot_i_match_predictions

#### Distance ----
distance_predictions <- m_full %>% 
  epred_draws(newdata = expand_grid(ego_capacity_n = 0.01, # median
                                    alter_capacity_n = 0.001, # median
                                    c_diff_cat = "match", # most common
                                    count_ego_collaboratives = 1.00, # median 
                                    count_alter_collaboratives = 0, # median
                                    overlap_collab = 0,
                                    ego_np_501c3 = 1, # median 
                                    alter_np_501c3 = 1, # median 
                                    np_match = "np_homophily", # most common
                                    alter_ej_mission = 2, # most common
                                    ego_ej_mission = 3, 
                                    ej_diff_cat = "match", # most common category 
                                    count_ego_issues = 3, # median
                                    count_alter_issues = 3, # median
                                    i_match = 1, # median
                                    ego_local = "local", # most common
                                    alter_local = "local", # most common
                                    geo_diff_cat = "local_match", # most common
                                    distance_n = seq(0, 1, by = 0.1)), # median
              re_formula = NA) # this means ALL groups will be included

plot_distance_predictions <- ggplot(distance_predictions, 
                                    aes(x = distance_n, y = .epred)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Blues") +
  labs(x = "Distance Between Home Offices (Normalized)", y = "",
       fill = "Credible interval") +
  theme_minimal() +
  theme(legend.position = "none")
plot_distance_predictions

##### Combine plots ----
prediction_plots <- cowplot::plot_grid(plot_collab_memb, plot_alter_collab, plot_i_match_predictions, plot_distance_predictions, labels = "auto")
prediction_plots

#ggsave("plots/posterior_predictions.png", prediction_plots, width = 10, height = 6, dpi = 600, units = "in")


##### Prediction summary -----
collab_memb_predictions %>% group_by(factor(overlap_collab)) %>% summarize(mean(.epred))
alter_collab %>% group_by(factor(count_alter_collaboratives)) %>% summarize(mean(.epred))
i_match_predictions %>% group_by(factor(i_match)) %>% summarize(mean(.epred))
distance_predictions %>% group_by(factor(distance_n)) %>% summarize(mean(.epred))


#### EJ Commitment -----
ej1 <- m_full %>% 
  epred_draws(newdata = expand_grid(ego_capacity_n = 0.01, # median
                                    alter_capacity_n = 0.001, # median
                                    c_diff_cat = "match", # most common
                                    count_ego_collaboratives = 1.00, # median 
                                    count_alter_collaboratives = 0, # median
                                    overlap_collab = 0,
                                    ego_np_501c3 = 1, # median 
                                    alter_np_501c3 = 1, # median 
                                    np_match = "np_homophily", # most common
                                    alter_ej_mission = 2, # most common
                                    ego_ej_mission = seq(0, 3, by = 1), 
                                    ej_diff_cat = "match", # most common category 
                                    count_ego_issues = 3, # median
                                    count_alter_issues = 3, # median
                                    i_match = 1, # median
                                    ego_local = "local", # most common
                                    alter_local = "local", # most common
                                    geo_diff_cat = "local_match", # most common
                                    distance_n = 0.2684), # median
              re_formula = NA) # this means ALL groups will be included

plot_ej1 <- ggplot(ej1, 
                   aes(x = ego_ej_mission, y = .epred)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Blues") +
  labs(x = "Ego EJ Commitment", y = "Predicted Collaborative Tie Probability",
       fill = "Credible interval") +
  theme_minimal() +
  theme(legend.position = "none")
plot_ej1

ej2 <- m_full %>% 
  epred_draws(newdata = expand_grid(ego_capacity_n = 0.01, # median
                                    alter_capacity_n = 0.001, # median
                                    c_diff_cat = "match", # most common
                                    count_ego_collaboratives = 1.00, # median 
                                    count_alter_collaboratives = 0, # median
                                    overlap_collab = 0,
                                    ego_np_501c3 = 1, # median 
                                    alter_np_501c3 = 1, # median 
                                    np_match = "np_homophily", # most common
                                    alter_ej_mission = seq(0, 3, by = 1), 
                                    ego_ej_mission = 3, 
                                    ej_diff_cat = "match", # most common
                                    count_ego_issues = 3, # median
                                    count_alter_issues = 3, # median
                                    i_match = 1, # median
                                    ego_local = "local", # most common
                                    alter_local = "local", # most common
                                    geo_diff_cat = "local_match", # most common
                                    distance_n = 0.2684), # median
              re_formula = NA) # this means ALL groups will be included

plot_ej2 <- ggplot(ej2, 
                   aes(x = alter_ej_mission, y = .epred)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Blues") +
  labs(x = "Alter EJ Commitment", y = "",
       fill = "Credible interval") +
  theme_minimal() +
  theme(legend.position = "none")
plot_ej2


ej3 <- m_full %>% 
  epred_draws(newdata = expand_grid(ego_capacity_n = 0.01, # median
                                    alter_capacity_n = 0.001, # median
                                    c_diff_cat = "match", # most common
                                    count_ego_collaboratives = 1.00, # median 
                                    count_alter_collaboratives = 0, # median
                                    overlap_collab = 0,
                                    ego_np_501c3 = 1, # median 
                                    alter_np_501c3 = 1, # median 
                                    np_match = "np_homophily", # most common
                                    alter_ej_mission = 2, # most common
                                    ego_ej_mission = 3, 
                                    ej_diff_cat = c("lower", "higher", "match"),
                                    count_ego_issues = 3, # median
                                    count_alter_issues = 3, # median
                                    i_match = 1, # median
                                    ego_local = "local", # most common
                                    alter_local = "local", # most common
                                    geo_diff_cat = "local_match", # most common
                                    distance_n = 0.2684), # median
              re_formula = NA) # does not account for group effects

plot_ej3 <- ej3 %>%
  mutate(ej_diff_cat = case_when(
    ej_diff_cat == "higher" ~ "Ego < Alter", 
    ej_diff_cat == "lower" ~ "Ego > Alter", 
    ej_diff_cat == "match" ~ "Matching", 
  )) %>%
  ggplot(., 
         aes(x = ej_diff_cat, y = .epred)) +
  geom_boxplot() + 
  #scale_fill_brewer(palette = "Blues") +
  labs(x = "EJ Commitment Comparison", y = "",
       fill = "Credible interval") +
  theme_minimal() +
  theme(legend.position = "none")

plot_ej3

##### Combine plots ----
ej_predict_plots <- cowplot::plot_grid(plot_ej1, plot_ej2, plot_ej3, nrow = 1, labels = "auto")
ej_predict_plots

#ggsave("plots/ej_posterior_predictions.png", ej_predict_plots, width = 10, height = 6, dpi = 600, units = "in")

##### Prediction summary -----
ej1 %>% group_by(factor(ego_ej_mission)) %>% summarize(mean(.epred))
ej2 %>% group_by(factor(alter_ej_mission)) %>% summarize(mean(.epred))
ej3 %>% group_by(factor(ej_diff_cat)) %>% summarize(mean(.epred))


# Appendix comparison plot
coefs_re <- gather_coefs(m_re, "Resource Exchange")
coefs_bd <- gather_coefs(m_bd, "Boundary Definition")
coefs_full <- gather_coefs(m_full, "Full Model")

combined_coefs_plot <- combine_coefs_plot3(coefs_re, coefs_bd, coefs_full)

combined_coefs_plot

#ggsave("plots/coefs_all_models.png", combined_coefs_plot, width = 9, height = 10, dpi = 600, units = "in")


## Andrew Heiss Method for Grand Mean AME
## overlap collab
overlap_collab_ame <- emtrends(m_full, ~ overlap_collab, 
                               var = "overlap_collab", 
                               at = list(overlap_collab = c(min(m_df$overlap_collab), 1, max(m_df$overlap_collab))),
                               nesting = NULL, 
                               epred = TRUE, 
                               re_formula = NA) %>%  # no random effects 
  gather_emmeans_draws()

# Grand Mean Plot
overlap_collab_ame_plot <- ggplot(overlap_collab_ame, aes(x = .value, fill = factor(overlap_collab))) +
  stat_halfeye(slab_alpha = 0.75) +
  scale_x_manual(values = c("#88694B", "#35413F", "#35420F"), breaks = c(0, 1, 4), labels = c("Heterophily", "Ave Homophily", "High Homophily")) +
  labs(x = "Average Marginal Effect of Shared Collaboratives", y = "Density", fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom")
overlap_collab_ame_plot