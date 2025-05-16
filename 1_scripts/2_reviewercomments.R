# Script to address reviewer comments

# Libraries & Data --------------------------------------------------------
library(tidyverse)
library(tidygeocoder) # add coordinates
library(sf) # to calc distances 
library(tigris) #census data
library(tidybayes) # result plots
library(tmap) # map
library(tmaptools) #add basemap
library(network)
library(ggraph) # network plot
library(sna) # network analysis
library(brms) # modeling package
library(tidybayes) # model results interpretation
library(emmeans)
source("1_scripts/1_functions.R")

## Read in attribute data for each organization in the analysis
nodelist <- read.csv("0_data/nodelist.csv")%>%
  filter(ID != "G58" & ID != "G62")

## Read in edgelist for ego networks
edgelist <- read.csv("0_data/edgelist.csv")%>%
  filter(alter != "G58" & alter != "G62")

## Organization key
id_key <- read.csv("0_data/org_ids.csv")

## DSC EJ Roster
ej_roster <- read.csv("0_data/Delta_EJ_Roster.csv") %>%
  select(Organization) %>%
  unique() %>%
  rename(org = Organization)

## Add nodelist to edgelist ----
d <- edgelist %>%
  left_join(., nodelist, by = c("ego" = "ID")) %>% # add attributes based on group ID
  rename_with(., ~paste0("ego_", .x), .cols=-c(1:2)) %>%# change column name to signify the new attribute are with respect to the ego's characteristics 
  left_join(., nodelist, by = c("alter" = "ID")) %>% # add attributes based on group ID
  rename_with(., ~paste0("alter_", .x), .cols=-c(1:14)) # do the same thing for the alter groups but do not rename the existing columns

# Opportunity Set ---------------------------------------------------------
## How representative is the nodelist in the analysis of the orginal EJ roster?
## Need to clean EJ roster to make sure that the org names match the names in the id_key
## Orgs that need to be cleaned: 
ej_roster_clean <- ej_roster %>%
  mutate(org = case_when(
    org == "Antioch Mobility LABs" ~ "Richmond Community Foundation Connects - Mobility LABs",
    org == "Catholic Charities of Stockton" ~ "Catholic Charities - Diocese of Stockton",
    org == "Conway Homes" ~ "Conway Homes Resident Council",
    org == "Faith in the Valley - San Joaquin" ~ "Faith in the Valley",
    org == "La Familia" ~ "La Familia Counseling Center",
    org == "Ensuring Opportunity" ~ "Richmond Community Foundation Connects - Ensuring Opportunity",
    org == "Rise Stockton" ~ "RISE Stockton Coalition",
    org == "Regional Coalition to End Homelessness" ~ "Sacramento Regional Coalition to End Homelessness",
    org == "Sunrise Movement*" ~ "Sunrise Movement",
    org == "United Latinos, Promoviendo Acción Cívica" ~ "United Latinos",
    TRUE ~ org
  )) %>%
filter(org != "Solano Resource Conservation District" & org != "Unaffiliated") %>%
  unique()

## Proportion of dataset in the ej roster?
semi_join(id_key %>% select(org), ej_roster_clean)
#28 orgs overlap between them
## SO basically 28% overlap


# Model Variations --------------------------------------------------------
## FULL OS ----
## A. Data Prep ----
## Create a new column in d to signify observed collaboration between orgs based on the original edgelist
d <- d %>%
  mutate(collab = 1) 

## Independent Variables ----
### a. Geography Variables ----
#### Distance ----
## create two different dfs: one for egos and one for alters & add coordinates for home office & turn into spatial object
egos <- d %>% select(ego) %>% distinct() %>% pull()

## ego dataset
d_egos <- d %>%
  select(ego, ego_type, ego_office_location, ego_np_501c3, ego_staff, ego_rev_pp22, ego_assets_pp22, ego_geo_scale, ego_geo_scale_type, ego_ej_mission, ego_ej_issues, ego_collaboratives) %>%
  unique() %>% # repeated observations of egos becuase of original edgelist format
  separate(ego_office_location, into = c("ego_street", "ego_city", "ego_state", "ego_zip"), sep = ",") %>%
  geocode(street = ego_street, city = ego_city, state = ego_state, method = "census") %>% # must be connected to the internet for this to work
  select(-ego_street, -ego_city, -ego_state, -ego_zip) %>%
  rename(ego_lat = lat, 
         ego_long = long) 

## turn into a spatial object
d_egos.sf <- d_egos %>%
  st_as_sf(coords = c("ego_long", "ego_lat"), 
           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84")

## reproject into a CRS with distance units
d_egos.utm <- d_egos.sf %>%
  st_transform(crs = "+proj=utm +zone=11 +datum=NAD83 +ellps=GRS80")

## Alters
### alters are more complicated because we want to consider both the ACTUAL alters named in the observed dataset and the POTENTIAL alters (egos that were not named as alter originally, but could partner with other orgs)
### can do this by just extracting all the org names

#### POTENTIAL alters (egos that were not named as alter originally, but could partner with other orgs)
all_potential_alters <- nodelist %>% dplyr::select(ID) %>% pull() 
## 84 potential alters 

#### ACTUAL alters named in the observed dataset
actual_alters <- d %>% select(alter) %>% distinct() %>% pull()
## 72 actual alters 

### Add spatial attributes
d_potential_alters <- nodelist %>%
  filter(ID %in% all_potential_alters) %>% # which is every org -- every org COULD be an alter
  separate(office_location, into = c("alter_street", "alter_city", "alter_state", "alter_zip"), sep = ",") %>%
  geocode(street = alter_street, city = alter_city, state = alter_state, method = "census") %>%
  mutate(lat = case_when( # one of the group's address does not geocode so have to manually set
    ID == "G83" ~ 38.79447, 
    TRUE ~ lat
  )) %>%
  mutate(long = case_when(
    ID == "G83" ~ -121.308638, 
    TRUE ~ long
  )) %>%
  rename(alter = ID) %>%
  select(alter, type, np_501c3, staff, rev_pp22, assets_pp22, geo_scale, geo_scale_type, ej_issues, ej_mission, collaboratives, lat, long) %>%
  rename_with(., ~paste0("alter_", .x), .cols = -1) # add alter_ prefix back into column names

### make into a spatial object
d_potential_alters.sf <- d_potential_alters %>%
  st_as_sf(coords = c("alter_long", "alter_lat"), 
           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84")

## reproject into a CRS with distance units
d_potential_alters.utm <- d_potential_alters.sf %>%
  st_transform(crs = "+proj=utm +zone=11 +datum=NAD83 +ellps=GRS80")

## Cross all possible combinations between the two dfs
point_combinations <- crossing(d_potential_alters.utm, d_egos.utm, .name_repair = "unique") # specify that duplicate column names should be made unique 

## rename unique columns (these numbers might change depending on session)
point_combinations <- point_combinations %>% rename(alter_geometry = `geometry...12`,
                                                    ego_geometry = `geometry...24`) #1806

## df now considers ALL potential pairs including pairs between egos BUT we need to remove cases where the ego & alter are the SAME name, e.g. remove loops
point_combinations <- point_combinations %>%
  filter(ego != alter) # should be 1806-21: 1785

## calculate euclidian distance between points. distance will be in meters because of UTM
point_combinations <- point_combinations %>%
  group_by(alter, ego) %>% 
  mutate(distance = st_distance(alter_geometry, ego_geometry)) %>% #  calculate the distance for each ego-alter tie combination
  ungroup()

## add the "collab" variable in to distinguish which of these matches actually exists in the original data
point_combinations <- point_combinations %>%
  st_drop_geometry() %>%
  left_join(., select(d, alter, ego, collab), by = c("alter", "ego")) # this says, please join d to point_combinations by matching BOTH the alter and ego combination

## Crossing function results in NA for unobserved collaborations in the "collab" column. Need to change these to 0. 
point_combinations <- point_combinations %>% 
  mutate(collab = ifelse(is.na(collab), 0, collab)) %>% # if na than make it 0 to signal that there is no observed collaboration between ego-alter tie, if not leave it as 1
  distinct() # only keep unique combos

## save as new "d"
d <- point_combinations

#### Geographic overlap ----
## The geo scale is representative of the organization itself, i.e. if its a local chapter of a bigger org like the sierra club then its national, but if its a campaign or cbo that has a fiscal sponsor than its the geographic scope of the campaign/cbo itself (NOT THE SPONSOR). This way the data captures a sense of how has "local" vs "nonlocal" origin of an organization
## Rescale geo_scale_type to reflect this
d <-  d %>%
  mutate(alter_geo_scale_type = case_when(
    alter_geo_scale_type == "Neighborhood" ~ 1,
    alter_geo_scale_type == "City" ~ 2,
    alter_geo_scale_type == "region_cities" ~ 3,
    alter_geo_scale_type == "County" ~ 4,
    alter_geo_scale_type == "region_counties" ~ 5,
    alter_geo_scale_type == "State" ~ 6,
    alter_geo_scale_type == "region_states" ~ 7,
    alter_geo_scale_type == "Country" ~ 8,
    alter_geo_scale_type == "National" ~ 8,
    alter_geo_scale_type == "International" ~ 9,
  )) %>%
  mutate(ego_geo_scale_type = case_when(
    ego_geo_scale_type == "Neighborhood" ~ 1, 
    ego_geo_scale_type == "City" ~ 2,
    ego_geo_scale_type == "region_cities" ~ 3, 
    ego_geo_scale_type == "County" ~ 4,
    ego_geo_scale_type == "region_counties" ~ 5,
    ego_geo_scale_type == "State" ~ 6,
    ego_geo_scale_type == "region_states" ~ 7,
    ego_geo_scale_type == "Country" ~ 8,
    alter_geo_scale_type == "National" ~ 8,
    ego_geo_scale_type == "International" ~ 9,
  )) %>%
  mutate(alter_local = ifelse(alter_geo_scale_type < 5, "local", "regional"), # local is county level or smaller
         ego_local = ifelse(ego_geo_scale_type < 5, "local", "regional")) %>%
  mutate(geo_diff = alter_geo_scale_type - ego_geo_scale_type,  # different (numerical), e.g. if positive, alter is a BIGGER scale than ego
         geo_diff_cat = ifelse(ego_local == "local" & alter_local == "local", "local_match",
                               ifelse(ego_local == "regional" & alter_local == "regional", "regional_match", 
                                      ifelse(ego_local == "local" & alter_local == "regional", "bigger", "smaller")))) # difference (categorical)


### b. Resources ----
#### Non profit status homophily ----
d <- d %>%
  mutate(np_match = ifelse(ego_np_501c3 == 1 & alter_np_501c3 == 1, "np_homophily",
                           ifelse(ego_np_501c3 == 0 & alter_np_501c3 == 0, "no_np_homophily", 
                                  ifelse(ego_np_501c3 == 1 & alter_np_501c3 == 0, "lower", "higher")))) %>%
  ungroup()

#### Resource capacity ----
# In order to calc capacity, need to combine the financial resources into one variable and then normalize financial resources & staff. Finally, average these two values to get one normalized capacity measure
capacity_n_calculation <- nodelist %>%
  mutate(financials_n = normalize(rev_pp22 + assets_pp22),
         staff_n = normalize(staff)) %>%
  group_by(ID) %>%
  mutate(capacity_n = mean(c(financials_n, staff_n), na.rm = TRUE)) %>%
  mutate(capacity_n = ifelse(ID %in% c("G23", "G27", "G30"), 0, capacity_n)) %>%
  select(ID, capacity_n) %>%
  ungroup()

d <-  d %>%
  left_join(., capacity_n_calculation, by = c("ego" = "ID")) %>%
  rename(ego_capacity_n = capacity_n) %>%
  left_join(., capacity_n_calculation, by = c("alter" = "ID")) %>%
  rename(alter_capacity_n = capacity_n) %>%
  mutate(c_diff_n = alter_capacity_n - ego_capacity_n, # capacity difference (numerical)
         c_diff_cat = ifelse(c_diff_n > 0, "higher", ifelse(c_diff_n == 0, "match", "lower"))) # if c_diff_n is positive, than alter has MORE capacity than ego (i.e. ego seeking higher capacity), is lower than ego seeking lower capacity

### c. Identity ----
#### EJ commitment ----
## Rescale EJ in mission ordered categories to be numerical
d <- d %>%
  mutate(alter_ej_mission = case_when(
    alter_ej_mission == "None" ~ 0, 
    alter_ej_mission == "Peripheral" ~ 1, 
    alter_ej_mission == "Key work area" ~ 2, 
    alter_ej_mission == "Central" ~ 3
  )) %>%
  mutate(ego_ej_mission = case_when(
    ego_ej_mission == "None" ~ 0, 
    ego_ej_mission == "Peripheral" ~ 1, 
    ego_ej_mission == "Key work area" ~ 2, 
    ego_ej_mission == "Central" ~ 3
  )) %>%
  mutate(ej_diff = alter_ej_mission - ego_ej_mission, # ej difference (numerical)
         ej_diff_cat = ifelse(ej_diff > 0, "higher", ifelse(ej_diff == 0, "match", "lower"))) # ej difference (categorical) e.g., higher: alter has higher EJ commitment than ego

#### EJ issues ----
## Calculate number of overlapping issues in each ego-alter pair
## Create a list of unique issues for each ego and alter
ego_elements <- strsplit(d$ego_ej_issues, ", ")
alter_elements <- strsplit(d$alter_ej_issues, ", ")

## Calculate overlap
overlap <- mapply(intersect, ego_elements, alter_elements) %>% # calculation occurs in same order as original d
  as.matrix() %>% 
  as.data.frame() %>% 
  mutate(X = seq_along(V1)) %>% # create a unique ID to group by
  group_by(X) %>% 
  mutate(i_match = length(unique(unlist((V1))))) %>% # count of overlapping issues in each ego-alter tie
  ungroup()

## Add back to the main dataframe
d <- d %>%
  cbind(select(overlap, i_match)) %>% # attach columns to original dataframe
  group_by(ego) %>%
  mutate(count_ego_issues = length(unique(unlist(strsplit(ego_ej_issues, ", "))))) %>% # count up the number of issues each ego works on
  ungroup(ego) %>%
  group_by(alter) %>%
  mutate(count_alter_issues = length(unique(unlist(strsplit(alter_ej_issues, ", "))))) %>% # count up the number of issues each alter works on
  ungroup()

### Collaboratives ----
## Calculate overlapping collaborative memberships between ego-alter ties
ego_collab <- strsplit(d$ego_collaboratives, "; ")
alter_collab <- strsplit(d$alter_collaboratives, "; ")

## Calculate overlap
overlap <- mapply(intersect, ego_collab, alter_collab) %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  mutate(X = seq_along(V1)) %>% # add ID for the group_by
  group_by(X) %>% 
  mutate(overlap_collab = length(unique(unlist((V1))))) %>%
  ungroup()

## Add back to the main dataframe
d <- d %>%
  cbind(select(overlap, V1, overlap_collab)) %>% # attach columns to original dataframe
  group_by(ego) %>%
  mutate(count_ego_collaboratives = length(unique(unlist(strsplit(ego_collaboratives, "; ")))),
         count_ego_collaboratives = ifelse(ego_collaboratives == "None", 0, count_ego_collaboratives)) %>% # count up the number of collaboratives the ego participates in but do not count the ones that say "none"
  ungroup(ego) %>%
  group_by(alter) %>%
  mutate(count_alter_collaboratives = length(unique(unlist(strsplit(alter_collaboratives, "; ")))), 
         count_alter_collaboratives = ifelse(alter_collaboratives == "None", 0, count_alter_collaboratives)) %>%
  ungroup() 
str(d)

d <- d %>% as.data.frame()

## 3. Run model
m_df_unbounded <- d %>%
  ungroup() %>%
  mutate(c_diff_cat = factor(c_diff_cat, levels = c("match", "lower", "higher")), 
         ej_diff_cat= factor(ej_diff_cat, levels = c("match", "lower", "higher")), 
         geo_diff_cat = factor(geo_diff_cat, levels = c("local_match", "regional_match", "smaller", "bigger")),
         np_match = factor(np_match, levels = c("np_homophily", "lower", "higher", "no_np_homophily")), 
         ego_local = factor(ego_local, levels = c("local", "regional")), 
         alter_local = factor(alter_local, levels = c("local", "regional")), 
         distance_n = normalize(distance)) %>%
  select(-distance)

### Model ----
set.seed(1992)
m_unbounded <- brm(collab ~ ego_capacity_n + alter_capacity_n + factor(c_diff_cat) + count_ego_collaboratives + count_alter_collaboratives + overlap_collab + factor(np_match) + factor(ej_diff_cat) + count_ego_issues + count_alter_issues + i_match + factor(geo_diff_cat) + distance_n + (1|ego) + (1|alter), 
                      data = m_df_unbounded, 
                      family = bernoulli(link = "logit"), 
                      prior = c(prior(normal(0,1), class = b),
                                prior(normal(0,10), class = Intercept)), 
                      warmup = 1000, #burn in period
                      iter = 2000, #actual samples
                      chains = 4,
                      cores = 2,
                      file = "2_outputs/m_unbounded",
                      control = list(adapt_delta = 0.99))

summary(m_unbounded)

## RADIUS OS ----
### Dependent Variable ----
bounded_d <- d

## The dependent variable in this analysis is binary: 1 for an observed ego-alter tie and 0 for a potential ego-alter tie
## As of now, the dataset contains potential ego-alter ties between ALL organizations in the dataset. But, given that we know spatial proximity matters, what if we try a DV bounded by median radius of observed distance between home offices?

median(bounded_d$distance)
#74836.74 [m] x 0.000621371 m to mi conversion
# 46.5 miles

## keep potential ego-alter ties IF they polygons intersect OR if their distance is less than 46.5 miles
## Two steps: 1) figure out ego-alter ties with intersecting polygons and 2) figure out ego-alter ties with distance less than 46.5 miles. Then, filter these observations out of the unbouded dataset
## Ego-alter ties with intersecting polygons
int_ego_alter_ties <- read.csv("2_outputs/model_dataset.csv") %>%
  ungroup() %>%
  mutate(c_diff_cat = factor(c_diff_cat, levels = c("match", "lower", "higher")), 
         ej_diff_cat= factor(ej_diff_cat, levels = c("match", "lower", "higher")), 
         geo_diff_cat = factor(geo_diff_cat, levels = c("local_match", "regional_match", "smaller", "bigger")),
         np_match = factor(np_match, levels = c("np_homophily", "lower", "higher", "no_np_homophily")), 
         ego_local = factor(ego_local, levels = c("local", "regional")), 
         alter_local = factor(alter_local, levels = c("local", "regional"))) %>%
  select(-X) # remove ID and un-normalized distance

## Identify ego-alter ties within 46.5 miles
dist_ego_alter_ties <- bounded_d %>%
  rename(dv = collab) %>%
  select(ego, alter, dv, ego_capacity_n, alter_capacity_n, ej_diff, c_diff_n, c_diff_cat, count_ego_collaboratives, count_alter_collaboratives, overlap_collab,  ego_np_501c3, alter_np_501c3, np_match, ego_ej_mission, alter_ej_mission, ej_diff_cat, count_ego_issues, count_alter_issues, i_match, ego_local, alter_local, geo_diff_cat, distance)%>%
  mutate(
    dv = as.integer(dv),
    ej_diff = as.integer(ej_diff),
    count_alter_collaboratives = as.integer(count_alter_collaboratives),
    count_ego_collaboratives = as.integer(count_ego_collaboratives),
    ego_ej_mission = as.integer(ego_ej_mission),
    alter_ej_mission = as.integer(alter_ej_mission),
    c_diff_cat = factor(c_diff_cat, levels = c("match", "lower", "higher")), 
    ej_diff_cat = factor(ej_diff_cat, levels = c("match", "lower", "higher")), 
    geo_diff_cat = factor(geo_diff_cat, levels = c("local_match", "regional_match", "smaller", "bigger")),
    np_match = factor(np_match, levels = c("np_homophily", "lower", "higher", "no_np_homophily")), 
    ego_local = factor(ego_local, levels = c("local", "regional")), 
    alter_local = factor(alter_local, levels = c("local", "regional")),
    distance = as.numeric(distance)
  ) %>%
  filter(distance < median(as.numeric((d$distance))))

str(int_ego_alter_ties)
str(dist_ego_alter_ties)

# Combine
m_df_bounded2 <- full_join(int_ego_alter_ties, dist_ego_alter_ties) %>%
  select(ego, alter, dv) %>%
  unique()

# add attributes back in!
m_df_bounded2 <- m_df_bounded2 %>%
  left_join(., d)

## Check if all the observed ties are still in the dataframe
m_df_bounded2 %>% filter(collab == 1) %>% select(ego, alter) %>% unique() %>% nrow() # 110 rows

## select variables that will be included in model 
m_df_bounded2 <- m_df_bounded2 %>%
  select(ego, alter, dv, ego_capacity_n, alter_capacity_n, ej_diff, c_diff_n, c_diff_cat, count_ego_collaboratives, count_alter_collaboratives, overlap_collab,  ego_np_501c3, alter_np_501c3, np_match, ego_ej_mission, alter_ej_mission, ej_diff_cat, count_ego_issues, count_alter_issues, i_match, ego_local, alter_local, geo_diff_cat, distance) %>%
  mutate(distance = as.numeric(distance))

### Export model data ----
write.csv(m_df_bounded2, "2_outputs/model_dataset_bounded2.csv")

### Model ----
m_df_bounded2 <- read.csv("2_outputs/model_dataset_bounded2.csv")%>%
  ungroup() %>%
  mutate(c_diff_cat = factor(c_diff_cat, levels = c("match", "lower", "higher")), 
         ej_diff_cat= factor(ej_diff_cat, levels = c("match", "lower", "higher")), 
         geo_diff_cat = factor(geo_diff_cat, levels = c("local_match", "regional_match", "smaller", "bigger")),
         np_match = factor(np_match, levels = c("np_homophily", "lower", "higher", "no_np_homophily")), 
         ego_local = factor(ego_local, levels = c("local", "regional")), 
         alter_local = factor(alter_local, levels = c("local", "regional")), 
         distance_n = normalize(distance)) %>%
  select(-distance)

set.seed(1992)
m_bounded2 <- brm(dv ~ ego_capacity_n + alter_capacity_n + factor(c_diff_cat) + count_ego_collaboratives + count_alter_collaboratives + overlap_collab + factor(np_match) + factor(ej_diff_cat) + count_ego_issues + count_alter_issues + i_match + factor(geo_diff_cat) + distance_n + (1|ego) + (1|alter), 
                   data = m_df_bounded2, 
                   family = bernoulli(link = "logit"), 
                   prior = c(prior(normal(0,1), class = b),
                             prior(normal(0,10), class = Intercept)), 
                   warmup = 1000, #burn in period
                   iter = 2000, #actual samples
                   chains = 4,
                   cores = 2,
                   file = "2_outputs/m_bounded2",
                   control = list(adapt_delta = 0.99))

summary(m_bounded2)

# Summary Plot ----
## Create a plot that compares paper model & these two versions of the model
m_unbounded <- readRDS("2_outputs/m_unbounded.rds")
m_bounded2 <- readRDS("2_outputs/m_bounded2.rds")
m_full_refined <- readRDS("2_outputs/m_full_refined.rds")

coefs1 <- gather_coefs(m_full_refined, "Paper Bound")
coefs2 <- gather_coefs(m_bounded2, "Partial Bound")
coefs3 <- gather_coefs(m_unbounded, "Unbounded")

combined_coefs_plot <- combine_coefs_plot3(coefs1, coefs2, coefs3)
combined_coefs_plot

ggsave("3_plots/figure_a4.png", combined_coefs_plot, width = 9, height = 9, dpi = 600, units = "in")
