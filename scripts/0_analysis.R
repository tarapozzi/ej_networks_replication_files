# This scripts provides the code for creating the model dataset from the raw data, running the analysis, and data visualizations used in the paper and supplementary materials document.


# Load libraries and custom functions -------------------------------------
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
source("scripts/1_functions.R")

# Upload data (skip if you want to recreate datasets) -------------------------------------
## Read in attribute data for each organization in the analysis
nodelist <- read.csv("data/nodelist.csv")

## Read in edgelist for ego networks
edgelist <- read.csv("data/edgelist.csv")


# A. Case Study Plot ---------------------------------------------------------
## Legal Delta 
delta.boundary <- st_read("/Users/tarapozzi/Documents/R-Projects/ej_networks/data/raw/i03_Delta_PrimarySecondary_Zones/i03_Delta_PrimarySecondary_Zones.shp") %>%
  st_transform(., crs = "+proj=utm +zone=10 +datum=NAD83 +ellps=GRS80") %>%
  mutate(color = ifelse(Zone == "Primary", "#726186", "brown4"), 
         line = ifelse(Zone == "Primary", "solid", "dashed"))

### Check out the boundary
tmap_mode("plot")
tm_shape(delta.boundary) +
  tm_polygons(col = "blue") 

## US Census Data Boundaries
ca <- states(year = 2022) %>% filter(NAME == "California")
c <- counties(state = "CA", year = 2022, cb = FALSE) 
pl <- places(state = "CA", year = 2022, cb = FALSE)

### trim places to include only counties that intersect with the legal Delta:
### Alameda, Sacramento, San Joaquin, Contra Costa, Solano, Yolo
delta.c <- c %>%
  filter(NAME == "Alameda"|NAME == "Sacramento"| NAME == "San Joaquin" |NAME == "Contra Costa"|NAME == "Solano" |NAME == "Yolo") %>%
  st_transform(., crs = "+proj=utm +zone=10 +datum=NAD83 +ellps=GRS80")

### matching crs?
st_crs(delta.boundary) == st_crs(delta.c) # TRUE

## include just the boundaries of the three urban areas 
delta.pl <- pl %>%
  filter(NAME == "Sacramento"|NAME == "Stockton"| NAME == "Antioch" |NAME == "Oakley"|NAME == "Brentwood") %>%
  mutate(label = case_when(
    NAME %in% c("Antioch", "Oakley", "Brentwood") ~ "Eastern \nContra Costa", 
    TRUE ~ NAME
  )) %>%
  group_by(label) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_transform(., crs = "+proj=utm +zone=10 +datum=NAD83 +ellps=GRS80")

## CalEnviroScreen 
ces.data <- st_read("data/calenviroscreen40shpf2021shp/CES4 Final Shapefile.shp")
glimpse(ces.data) # quick visual check of df to make sure it uploaded well

### select Delta counties 
ces.data <- ces.data %>% 
  filter(County == "Alameda"|County == "Sacramento"| County == "San Joaquin" |County == "Contra Costa"|County == "Solano" |County == "Yolo"|County == "Stanislaus") 
## Vulnerability index
### filter out missing data which is marked as "-999" in ces
ces.data <- ces.data %>%
  filter(if_all(c(Ozone,PM2_5, DieselPM, Pesticide, 
                  DrinkWat, Lead, Tox_Rel, Cleanup, 
                  GWThreat, HazWaste, ImpWatBod, SolWaste),
                ~ . != -999))

### select vulnerability indicators based on factors known to contribute to pollution and public health issues
poll_exp <- ces.data %>%
  dplyr::select(Tract, Ozone, PM2_5, DieselPM, Pesticide, DrinkWat, Lead, Tox_Rel,
                Cleanup, GWThreat, HazWaste, ImpWatBod, SolWaste) 

### standardize index
poll_exp.std <- poll_exp %>%
  mutate_at(~(scale(.) %>% as.vector(.)), .vars = vars(-c(Tract, geometry)))
glimpse(poll_exp.std)

poll_exp.std <- poll_exp.std %>%
  mutate(PolInd1 = (Ozone+PM2_5+DieselPM+Pesticide+DrinkWat+Lead+Tox_Rel+
                      Cleanup+GWThreat+HazWaste+ImpWatBod+SolWaste)/12)

### now it is ready to add to our map!

## Base map
osm <- read_osm(delta.boundary, ext = 1.2, type = "apple-iphoto")

## Put it all together
tmap_mode("plot")
set.seed(1)
case.study.plot <- tm_shape(osm) + 
  tm_rgb() + 
  tm_shape(poll_exp.std) + 
  tm_fill("PolInd1", palette = "Reds", alpha = 0.8, legend.show = TRUE, title = "CES Score") +
  tm_shape(delta.boundary) +
  tm_borders(col = "#35483F", lty = c("solid", "longdash"), lwd = 1)+
  tm_fill(col = "grey", alpha = 0.4)+
  tm_shape(delta.pl) +
  tm_borders("black", alpha = 0.9) + 
  tm_fill(col = "#35483F", alpha = 0.2)+
  tm_text(text = "label", col = "black", scale = .9, fontface = "bold", auto.placement = 6, shadow = TRUE) +
  tm_scale_bar(breaks = c(0, 10, 20), text.size = 0.75, position = c("left", "bottom"), bg.color = "white", bg.alpha = .5) +
  tm_compass(type = "arrow", position = c("right", "top"), bg.color = "white", bg.alpha = .5) +
  #tm_add_legend(type = "border", labels = "Urban Center", col = "#35483F", title = "Legend") + 
  tm_add_legend(type = "line", labels = c("Primary Zone", "Secondary Zone"), lty = c("solid", "dotted")) + 
  tm_legend(position = c("left", "top"), 
            frame = FALSE,
            bg.color="white", 
            bg.alpha = .5)

case.study.plot 

tmap_save(case.study.plot, "plots/case_study_plot.png", width = 5, height = 7, dpi = 600, units = "in")


# B. Raw Data -------------------------------------------------------------

## Descriptive Plots for Case Study Section ----
collaborative_list <- data.frame(org = c("California Environmental Justice Alliance", "ClimatePlan", "Capital Region Climate Readiness Collaborative", "Edge Collaborative", "Environmental Council of Sacramento", "Environmental Justice Coalition for Water", "RISE Stockton Coalition", "Delta Adapts", "Regional Water Forum", "San Joaquin Regional Climate Collaborative", "Yolo County Climate Action Commission", "Sacramento Environmental Justice Collaborative Governance Committee", "Stockton AB 617 Steering Committee", "Stockton Rising", "San Joaquin Healthy Neighborhoods Collaborative", "Delta Tribal Environmental Coalition"), year = c(2001, 2007, 2013, 2021, 1971, 1999, 2018, 2018, 2000, 2022, 2020, 2018, 2019, 2022, 2020, 2023), Type = "Collaborative")

year_plot <- nodelist %>%
  mutate(Type = "EJ Group") %>%
  select(year, Type) %>%
  bind_rows(., select(collaborative_list, year, Type)) %>%
  group_by(year, Type) %>%
  count() %>%
  filter(year > 1969) %>%
  ggplot(., aes(year, n)) + 
  geom_bar(stat = "identity", aes(fill = Type), position = "stack") + 
  scale_fill_manual(breaks = c("EJ Group", "Collaborative"), values = c("#657D94", "brown4")) + 
  xlab("Founding Year") + 
  ylab("Count") + 
  theme_bw() +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.text = element_text(size = 12))
year_plot

ggsave("plots/year_plot.png", year_plot, width = 7, height = 5, dpi = 600, units = "in")

issue_plot <- nodelist %>%
  select(ej_issues) %>%
  separate_rows(ej_issues, sep = ", ") %>%
  group_by(ej_issues) %>%
  count() %>%
  ungroup() %>%
  ggplot(., aes(reorder(ej_issues, n), n)) + 
  geom_bar(stat = "identity", fill = "#657D94") + 
  coord_flip() + 
  xlab("EJ Issue") + 
  ylab("No. of Groups") + 
  theme_bw() +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.text = element_text(size = 12))
issue_plot

year_issue_plot <- cowplot::plot_grid(year_plot, issue_plot, labels = "auto")
year_issue_plot

#ggsave("plots/year_issue_plot.png", year_issue_plot, width = 10, height = 5, dpi = 600, units = "in")


# C. Model Dataset --------------------------------------------------------
## 1. Missing Data ----
## Two alters do not have capacity information & therefore are removed from the dataset right away. This is because the analysis does not include imputation for missing values, so observations would be dropped from the model. 
nodelist <- nodelist %>%
  filter(ID != "G58" & ID != "G62")

edgelist <- read.csv("data/edgelist.csv") %>%
  filter(alter != "G58" & alter != "G62")

## 2. Add nodelist to edgelist ----
d <- edgelist %>%
  left_join(., nodelist, by = c("ego" = "ID")) %>% # add attributes based on group ID
  rename_with(., ~paste0("ego_", .x), .cols=-c(1:2)) %>%# change column name to signify the new attribute are with respect to the ego's characteristics 
  left_join(., nodelist, by = c("alter" = "ID")) %>% # add attributes based on group ID
  rename_with(., ~paste0("alter_", .x), .cols=-c(1:14)) # do the same thing for the alter groups but do not rename the existing columns

## Create a new column in d to signify observed collaboration between orgs based on the original edgelist
d <- d %>%
  mutate(collab = 1) 

## 3. Independent Variables ----
### Geography Variables ----
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


### Resources ----
#### Non profit status homophily ----
d <- d %>%
  mutate(np_match = ifelse(ego_np_501c3 == 1 & alter_np_501c3 == 1, "np_homophily",
                           ifelse(ego_np_501c3 == 0 & alter_np_501c3 == 0, "no_np_homophily", 
                                  ifelse(ego_np_501c3 == 1 & alter_np_501c3 == 0, "lower", "higher")))) %>%
  ungroup()

#### Resource capacity ----
# In order to calc capacity, need to combine the financial resources into one variable and then normalize financial resources & staff. Finally, average these two values to get one normalized capacity measure
d <-  d %>%
  mutate(ego_financials = ego_rev_pp22 + ego_assets_pp22, 
         alter_financials = alter_rev_pp22 + alter_assets_pp22) %>% 
  mutate(ego_financials_n = normalize(ego_financials), 
         alter_financials_n = normalize(alter_financials),
         ego_staff_n = normalize(ego_staff),
         alter_staff_n = normalize(alter_staff)) %>%
  group_by(ego, alter) %>% # group by each ego-alter tie
  mutate(ego_capacity_n = mean(c(ego_financials_n, ego_staff_n), na.rm = TRUE), 
         alter_capacity_n = mean(c(alter_financials_n, alter_staff_n), na.rm = TRUE)) %>% # combined capacity measure 
  ungroup() %>%
  mutate(alter_capacity_n = case_when(
    alter %in% c("G23", "G27", "G30") ~ 0, # these groups did not have any financial or staffing information available & had minimal online presence. Assume, these groups have the lowest capacity possible.
    TRUE ~ alter_capacity_n
  )) %>%
  group_by(ego, alter) %>%
  mutate(c_diff_n = alter_capacity_n - ego_capacity_n, # capacity difference (numerical)
         c_diff_cat = ifelse(c_diff_n > 0, "higher", ifelse(c_diff_n == 0, "match", "lower"))) # capacity different (categorical)

### Identity ----
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

## 4. Dependent Variable -----
## The dependent variable in this analysis is binary: 1 for an observed ego-alter tie and 0 for a potential ego-alter tie
## As of now, the dataset contains potential ego-alter ties between ALL organizations in the dataset, but that is not realistic. Rather, some egos and alters are highly unlikely to collaborate because their areas of work do not physically overlap
## Therefore, this next set of code bounds potential ego-alter ties by geographic overlap between groups

## Some groups work in multiple areas, so need to make sure df has one row per group per area of work
network_bounding <- nodelist %>%
  select(ID, office_location, geo_scale, geo_scale_type) %>%
  mutate(geo_scale = ifelse(geo_scale_type == "Country" | geo_scale_type == "National" | geo_scale_type == "International", "United States", geo_scale)) %>% # make all international and national equal to the US
  separate_rows(geo_scale, sep = ", ") %>%
  mutate(geo_scale_name = case_when(
    geo_scale == "Conway Homes" ~ "06077002503", # census tract for neighborhood based off home office location
    geo_scale == "South Natomas" ~ "06067007001", # census tract for neighborhood based off home office location
    TRUE ~ geo_scale
  ))

## Bring in census polygons
tracts <- tracts(state = "CA", year = 2022, cb = TRUE) %>% select(GEOID, geometry) %>% rename(NAME = GEOID) # year 2023 is having issues
cities <- places(state = "CA", year = 2022, cb = TRUE) %>% select(NAME, geometry)
counties <- counties(state = "CA", year = 2022, cb = TRUE) %>% select(NAMELSAD, geometry) %>% rename(NAME = NAMELSAD)
states <- states(year = 2022, cb = TRUE) %>% select(NAME, geometry)
nation <- nation(year = 2022) %>% select(NAME, geometry)

census_geometry <- rbind(tracts, cities, counties, states, nation) 

## attach polygon for each place 
network_bounding <- network_bounding %>%
  left_join(., select(census_geometry, NAME, geometry), by = c("geo_scale_name" = "NAME")) 

## now merge polygons by ID
network_bounding <- network_bounding %>%
  st_as_sf() %>% 
  group_by(ID) %>%
  st_transform(geometry, crs = 4326) %>%
  summarise(geometry = st_union(geometry)) # chose this crs as it is very common

## check validity of geometry 
st_is_valid(network_bounding)

## add this to main dataframe 
bounded_d <- d %>%
  left_join(., select(network_bounding, ID, geometry), by = c("alter" = "ID")) %>%
  rename(alter_geom = geometry) %>%
  left_join(., select(network_bounding, ID, geometry), by = c("ego" = "ID")) %>%
  rename(ego_geom = geometry)

## see if polygons intersect or not
## Two steps: 1) remove pairs that do no intersect, 2) remove pairs that just touch borders
## BUT need to make sure we do not remove the observed ties -- there are three ties that break this "geographic homophily" assumption so we can control for this by creating a new binary variable 
bounded_d <- bounded_d %>%
  st_as_sf() %>% 
  st_transform(crs = 4326) %>%
  group_by(ego, alter) %>%
  mutate(geo_overlap = any(st_intersects(ego_geom, alter_geom, sparse = FALSE))) %>% # gives TRUE if two polygons overlap and FALSE if not, "sparse" makes it so that it returns a logical value, and "any" allows anytime it intersects to count so sometimes it might overlap more than once and the overlap could just be borders touching
  filter(geo_overlap == TRUE | collab == 1) %>% # keep observations with overlap OR if a pair has an observed tie
  mutate(touch = as.integer(st_touches(ego_geom, alter_geom)))

## Check if all the observed ties are still in the dataframe
bounded_d %>% filter(collab == 1) %>% select(ego, alter) %>% unique() %>% nrow() # 110 rows

## Second step: remove pairs that just touch borders EXCEPT for observed ties
bounded_d <- bounded_d %>% # additionally, need to find out pairs that just touch borders, but do not overlap internally
  mutate(dv = case_when(
    collab == 1 & is.na(touch) ~ 1, # na's means that these are pairs that overlap internally and not just touch, so we want to keep them!
    collab == 0 & is.na(touch) ~ 0,
    collab == 0 & touch == 1 ~ 999)) %>%
  filter(dv != 999)

## Check if all the observed ties are still in the dataframe
bounded_d %>% filter(collab == 1) %>% select(ego, alter) %>% unique() %>% nrow() # 110 rows, WOOO! 

## remove geometry
bounded_d <- bounded_d %>% select(-ego_geom) %>% select(-alter_geom) %>% st_drop_geometry() #get rid of geometry
class(bounded_d)

## select variables that will be included in model 
bounded_d <- bounded_d %>%
  select(ego, alter, dv, ego_capacity_n, alter_capacity_n, ej_diff, c_diff_n, c_diff_cat, count_ego_collaboratives, count_alter_collaboratives, overlap_collab,  ego_np_501c3, alter_np_501c3, np_match, ego_ej_mission, alter_ej_mission, ej_diff_cat, count_ego_issues, count_alter_issues, i_match, ego_local, alter_local, geo_diff_cat, distance)

## 5. Export model data ----
write.csv(bounded_d, "outputs/model_dataset.csv")

# C. Model -----
m_df <- read.csv("outputs/model_dataset.csv") 

m_df <- m_df %>%
  mutate(c_diff_cat = factor(c_diff_cat, levels = c("match", "lower", "higher")), 
         ej_diff_cat= factor(ej_diff_cat, levels = c("match", "lower", "higher")), 
         geo_diff_cat = factor(geo_diff_cat, levels = c("local_match", "regional_match", "smaller", "bigger")),
         np_match = factor(np_match, levels = c("np_homophily", "lower", "higher", "no_np_homophily")), 
         ego_local = factor(ego_local, levels = c("local", "regional")), 
         alter_local = factor(alter_local, levels = c("local", "regional")), 
         distance_n = normalize(distance)) %>%
  select(-X, -distance) # remove ID and un-normalized distance


# Descriptive Results -----------------------------------------------------
## Table 2 ----
## Code for comparison table of independent variables across egos, alters, potential ego-alter ties, and observed ego-alter ties
## 1. Egos ----
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

## 2. Observed Alters ----
actual_alters <- edgelist %>% select(alter) %>% distinct() %>% pull()
alter_summary <- m_df %>%
  filter(alter %in% actual_alters) %>%
  select(alter, alter_np_501c3, alter_capacity_n, count_alter_collaboratives, alter_ej_mission, count_alter_issues) %>%
  unique() %>%
  summary()%>%
  ungroup() %>%
  mutate(porp = n/sum(n))
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

## 3. Potential ego-alter ----
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
  ungroup() %>%
  mutate(porp = n/sum(n))
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

## 4. Observed ego-alter ties -----
pair_summary <- m_df %>%
  filter(dv == 1) %>%
  select(overlap_collab, i_match, distance_n) %>%
  unique() %>%
  summary()
#0.000621371: conversion factor from meters to miles
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

## Network figure ----
# Replace anonymous IDs with names 
anonymous_key <- read.csv("data/anonymous_key.csv")
edgelist_names <- edgelist %>%
  left_join(., anonymous_key, by = c("ego" = "ID")) %>%
  select(-ego) %>%
  rename(ego = org) %>%
  left_join(., anonymous_key, by = c("alter" = "ID")) %>%
  select(-alter) %>%
  rename(alter = org)

nodelist_names <- anonymous_key %>% select(org)

# Create network object
net <- network(x = edgelist_names, 
               vertices = nodelist_names, 
               bipartite = F,  
               directed = T, 
               isolates = F)
# Attributes
net %v% 'degree' <- sna::degree(net)

net %v% 'position' <- ifelse(net %v% 'vertex.names' %in% egos, 1, 2)

net %v% 'labels' <- ifelse(net %v% 'vertex.names' %in% egos & net %v% 'position' == 1, net %v%  'vertex.names', '')

# figure out which orgs where named in multiple ego networks
alter_overlaps <- edgelist_names %>% select(alter) %>% group_by(alter) %>% filter(n() > 1) %>% distinct() %>% pull() # 17 overlapping alters 

# out of those orgs, which ones were also an ego in the analysis
egos <- edgelist_names %>% select(ego) %>% distinct() %>% pull() 
alters <- edgelist_names %>% select(alter) %>% distinct() %>% pull()
both <- intersect(egos, alters)
ego_alter_overlaps <- intersect(alter_overlaps, both)

# the remaining are just named multiple times as an alter
just_alter_overlaps <- setdiff(alter_overlaps, both)

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
  scale_color_manual(breaks = c(0,1,2), values = c("#00BFC4", "gold", "#54278F"), labels = c("None", "Ego named as \nalter", "Alter named \nmultiple times")) +
  theme_void() +
  geom_node_text(aes(label = labels), size = 4, bg.color = "grey", repel = T, max.overlaps = Inf) +
  theme(legend.position = "right", legend.text = element_text(size = 12)) + 
  guides(size = guide_legend(title = "Degree"), shape = guide_legend(title = "Position"), color = guide_legend(title = "Overlap")) 

net_plot

ggsave("plots/full_network.png", net_plot, width = 10, height = 8, units = "in")


## Inferential Results -----
## 1. Variance Components Models ----
### a. Ego Only
set.seed(1992)
m_ego <- brm(dv ~ (1|ego), data = m_df, cores = 4, file = "outputs/m_ego.rds", family = bernoulli(link = "logit"))
m_ego


### b. Ego + Alter 
set.seed(1992)
m_ego_alter <- brm(dv ~ (1|ego) + (1|alter),  cores = 4, file = "outputs/m_ego_alter.rds",  data = m_df, family = bernoulli(link = "logit"))
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
              file = "outputs/m_full",
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
### a. Resource Exchange Model (ego + alter RE)----
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
            file = "outputs/m_re",
            control = list(adapt_delta = 0.99))


### b. Boundary Definition Model (ego + alter RE)----
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
            #file = "outputs/m_bd", # save your model output
            control = list(adapt_delta = 0.99)) 

### c. Full Model (ego only)----
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
                  file = "outputs/m_full_ego",
                  control = list(adapt_delta = 0.99))

## c. Full Model - refined after VIF test
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
              file = "outputs/m_full_refined",
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
              file = "outputs/m_full_s",
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
              file = "outputs/m_full_c",
              control = list(adapt_delta = 0.99))

### d. Selection ----
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
                            file = "outputs/m_rbcheck_local_egos",
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
                          file = "outputs/m_rbcheck_reg_egos",
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
                file = "outputs/m_full_subset",
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
              file = "outputs/m_full_ejfactor",
              control = list(adapt_delta = 0.99))

## 6. Model Plots ----
## Read-in model results ----
m_ego <- readRDS("outputs/m_ego.rds")
m_ego_alter <- readRDS("outputs/m_ego_alter.rds")
m_full_c <- readRDS("outputs/m_full_c.rds")
m_full_s <- readRDS("outputs/m_full_s.rds")
m_re <- readRDS("outputs/m_re.rds")
m_bd <- readRDS("outputs/m_bd.rds")
m_full <- readRDS("outputs/m_full.rds")
m_full_refined <- readRDS("outputs/m_full_refined.rds")
m_full_ejfactor <- readRDS("outputs/m_full_ejfactor.rds")

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
    par == "Collaborative Membership Overlap" ~ 6, 
    TRUE ~ ordering
  )) %>%
  mutate(par = fct_reorder(par, ordering,  .desc = T)) %>%
  ggplot(., aes(value, par, color = variable, shape = hyp_type)) +
  geom_point(position = position_dodge(width=.75)) + 
  geom_pointinterval(aes(xmin = .lower, xmax = .upper), position=position_dodge(width=.75), height=0) + 
  geom_vline(xintercept = 0) +
  scale_color_manual(values = c("#88694B",
                                "#657D94",
                                "#35483F"),
                     guide = guide_legend(reverse = TRUE, ncol = 3)) +
  labs(x = "Coefficient", y = "", color = "Variable", shape = "Type") +
  guides(color = guide_legend(ncol = 1), shape = guide_legend(ncol = 1)) + 
  theme_bw() +
  theme(legend.position = "right", legend.title = element_text(size = 12), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.text = element_text(size = 12))

coefs_re_plot

ggsave("plots/coefs_re.png", coefs_re_plot, width = 10, height = 5, dpi = 600, units = "in")

##### Boundary Definition Coefs -----
coefs_bd_plot <- coefs %>% 
  filter(mode == "Boundary Definition") %>%
  mutate(ordering = as.integer(factor(variable)) + as.integer(fct_rev(factor(hyp_type))) + value) %>%
  mutate(ordering = case_when(
    par == "Spatial Distance" ~ 5, 
    par == "Ego No. of Issues" ~ 3.3, 
    par == "Alter No. of Issues" ~ 3.29,
    par == "Both Local" ~ 3.6,
    TRUE ~ ordering
  )) %>%
  mutate(par = fct_reorder(par, ordering,  .desc = T)) %>%
  ggplot(., aes(value, par, color = variable, shape = hyp_type)) +
  geom_point(position = position_dodge(width=.75)) + 
  geom_pointinterval(aes(xmin = .lower, xmax = .upper), position=position_dodge(width=.75), height=0) + 
  geom_vline(xintercept = 0) +
  scale_color_manual(values = c("#88694B",
                                "#657D94",
                                "#35483F"),
                     guide = guide_legend(reverse = TRUE, ncol = 3)) +
  labs(x = "Coefficient", y = "", color = "Variable", shape = "Type") +
  guides(color = guide_legend(ncol = 1), shape = guide_legend(ncol = 1)) + 
  theme_bw() +
  theme(legend.position = "right", legend.title = element_text(size = 12), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.text = element_text(size = 12))

coefs_bd_plot

ggsave("plots/coefs_bd.png", coefs_bd_plot, width = 10, height = 5, dpi = 600, units = "in")

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
  labs(x = "Overlapping Collaborative Membership", y = "Tie Probability",
       fill = "Credible interval") +
  theme_minimal() +
  theme(legend.position = "none")
plot_collab_memb
ggsave("plots/collab_ame.png", plot_collab_memb, width = 6, height = 4, dpi = 600, units = "in")


re_plots <- cowplot::plot_grid(plot_np_match, plot_collab_memb, labels = "auto")
re_plots

ggsave("plots/re_ame.png", re_plots, width = 10, height = 4, dpi = 600, units = "in")

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

ggsave("plots/imatch_ame.png", plot_i_match, width = 6, height = 4, dpi = 600, units = "in")


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

## unnormalize formula: normalized_d * (max_d - min_d) + min_d
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

ggsave("plots/bd_ame.png", bd_plots, width = 10, height = 6, dpi = 600, units = "in")

# D. Supplemental Information ----
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

ggsave("plots/ego_dist.png", ego_dist, width = 6, height = 4, dpi = 600, units = "in")

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
ggsave("plots/alter_dist.png", alter_dist, width = 6, height = 4, dpi = 600, units = "in")

org_distributions <- cowplot::plot_grid(ego_dist, alter_dist, labels = "auto")

ggsave("plots/org_distributions.png", org_distributions, width = 8, height = 4, dpi = 600, units = "in")



## 2. Model Comparison ----------------------------------------------
### a. Coefficient Table ----
sjPlot::tab_model(m_re, m_bd, m_full_refined, dv.labels = c("Resource Exchange", "Boundary Definition", "Full Model"), file = "outputs/paper_model_table.doc")

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

ggsave("plots/coefs_ejfactor.png", contrast_ejfactor, width = 10, height = 6, dpi = 600, units = "in")

### c. Loo Comparison Table ----
loo(m_re)
loo(m_bd)
loo(m_full_refined)

loo(m_full_c)
loo(m_full_s)

### d. VPC Table ----
sjPlot::tab_model(m_ego, m_ego_alter, m_full_ego, m_full_refined, dv.labels = c("VPC - Ego", "VPC - Ego + Alter", "Full Model - Ego", "Full Model - Ego + Alter"), file = "outputs/vpc_full_model_comp_table.doc")

## 3. Model Diagnostics ----
### a. Posterior Predictive Check ----
ppc <- pp_check(m_full_refined, type = "dens_overlay", ndraws = 100)
ggsave("plots/ppc.png", ppc, width = 6, height = 4, dpi = 600, units = "in")

### b. Trace Plots ----
trace <- plot(m_full_refined)
ggsave("plots/trace_plots.png", trace, width = 6, height = 4, dpi = 600, units = "in")

## 3. Prior Selection ----
loo(m_full_c)
loo(m_full_s)
loo(m_full_refined)

coefs_c <- gather_coefs(m_full_c, "Cauchy")
coefs_s<- gather_coefs(m_full_s, "Horseshoe")
coefs_full <- gather_coefs(m_full_refined, "Normal")

combined_coefs_plot <- combine_coefs_plot3(coefs_c, coefs_s, coefs_full)
combined_coefs_plot
ggsave("plots/coefs_all_priors.png", combined_coefs_plot, width = 9, height = 10, dpi = 600, units = "in")

## 4. Model Robustness Check -----------------------------------------------
### DV Bounding -----
### The model presented in the manuscript bounds potential ego-alter ties by geographic overlap between groups
### How do the results change with different bounding scenarios?
### a. Ego Subsets ----
m_rbcheck_local_egos <- readRDS("outputs/m_rbcheck_local_egos.rds")
m_rbcheck_reg_egos <- readRDS("outputs/m_rbcheck_reg_egos.rds")
m_full_refined <- readRDS("outputs/m_full_refined.rds")

coefs1 <- gather_coefs(m_full_refined, "Full Model")
coefs2 <- gather_coefs(m_rbcheck_local_egos, "Local Egos Only")
coefs3 <- gather_coefs(m_rbcheck_reg_egos, "Regional Egos Only")

combined_coefs_plot <- combine_coefs_plot3(coefs1, coefs2, coefs3)
combined_coefs_plot

ggsave("plots/coefs_rbcheck_egosubsets.png", combined_coefs_plot, width = 9, height = 9, dpi = 600, units = "in")

### b. No ECJW Model Plot ----
m_full_subset <- readRDS("outputs/m_full_subset.rds")
coefs1 <- gather_coefs(m_full_refined, "Full Model")
coefs2 <- gather_coefs(m_full_subset, "Full Model without EJCW")

combined_coefs_plot2 <- combine_coefs_plot2(coefs1, coefs2)
combined_coefs_plot2

ggsave("plots/coefs_full_model_no_ejcw_comp.png", combined_coefs_plot2, width = 9, height = 9, dpi = 600, units = "in")
