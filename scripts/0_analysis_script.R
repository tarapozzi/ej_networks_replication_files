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

# A. Case Study ----
## Legal Delta 
delta.boundary <- st_read("/Users/tarapozzi/Documents/R-Projects/ej_networks/data/raw/i03_Delta_PrimarySecondary_Zones/i03_Delta_PrimarySecondary_Zones.shp") %>%
  st_transform(., crs = "+proj=utm +zone=10 +datum=NAD83 +ellps=GRS80") %>%
  mutate(color = ifelse(Zone == "Primary", "#726186", "brown4"), 
         line = ifelse(Zone == "Primary", "solid", "dashed"))

#let's look at it - it looks good
tmap_mode("plot")
tm_shape(delta.boundary) +
  tm_polygons(col = "blue") 

## Boundaries
ca <- states(year = 2022) %>% filter(NAME == "California")
c <- counties(state = "CA", year = 2022, cb = FALSE) 
pl <- places(state = "CA", year = 2022, cb = FALSE)

## trim places to include only counties that intersect with the legal delta
### Alameda, Sacramento, San Joaquin, Contra Costa, Solano, Yolo
delta.c <- c %>%
  filter(NAME == "Alameda"|NAME == "Sacramento"| NAME == "San Joaquin" |NAME == "Contra Costa"|NAME == "Solano" |NAME == "Yolo") %>%
  st_transform(., crs = "+proj=utm +zone=10 +datum=NAD83 +ellps=GRS80")

st_crs(delta.boundary) == st_crs(delta.c)

## include just the boundaries of the cities
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
glimpse(ces.data) # quick visual check of df to make sure it came it well

### Select Delta counties 
ces.data <- ces.data %>% 
  filter(County == "Alameda"|County == "Sacramento"| County == "San Joaquin" |County == "Contra Costa"|County == "Solano" |County == "Yolo"|County == "Stanislaus") 
## Vulnerability index
### filter out missing data which is marked as "-999" in ces
ces.data <- ces.data %>%
  filter(if_all(c(Ozone,PM2_5, DieselPM, Pesticide, 
                  DrinkWat, Lead, Tox_Rel,Cleanup, 
                  GWThreat, HazWaste, ImpWatBod, SolWaste),
                ~ . != -999))

### select vulnerability indicators
poll_exp <- ces.data %>%
  dplyr::select(Tract,Ozone,PM2_5, DieselPM, Pesticide, DrinkWat, Lead, Tox_Rel,
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

## put it all together
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

# B. Raw Data ----
## Read in attribute data for each group in the analysis
nodelist <- read.csv("data/nodelist.csv")

## Read in edgelist for ego networks
edgelist <- read.csv("data/edgelist.csv")

## Descriptive Plots ----
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

# C. Model Dataset ----
## 1. Missing Data ----
## Two alters do not have capacity information & therefore are removed from the dataset right away because the analysis does not include imputation for missing values
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
### Geography ----
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
### alter are more complicated because we want to consider both ACTUAL alters named in the observed dataset and POTENTIAL alters (egos that were not named as alter originally, but could partner with other orgs)
### can do this by just extracting all the org names
all_potential_alters <- nodelist %>% dplyr::select(ID) %>% pull() 
## 84 potential alters 

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
  mutate(np_match = ifelse(alter_np_501c3 == 1 & ego_np_501c3 == 1, "np_homophily",
                           ifelse(alter_np_501c3 == 0 & ego_np_501c3 == 0, "no_np_homophily", "no_match"))) %>%
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
## As of now, the dataset contains potential ego-alter ties between ALL organizations in the dataset, but that is not realistic. Rather, some egos and alters are highly unlikely to collaborate because their areas of work do not phsyically overlap
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
  select(ego, alter, dv, ego_capacity_n, alter_capacity_n, c_diff_cat, count_ego_collaboratives, count_alter_collaboratives, overlap_collab,  ego_np_501c3, alter_np_501c3, np_match, ego_ej_mission, alter_ej_mission, ej_diff_cat, count_ego_issues, count_alter_issues, i_match, ego_local, alter_local, geo_diff_cat, distance)

## 5. Export model data ----
write.csv(bounded_d, "outputs/model_dataset.csv")

# C. Model -----
m_df <- read.csv("outputs/model_dataset.csv") %>%
  mutate(c_diff_cat = factor(c_diff_cat, levels = c("match", "lower", "higher")), 
         ej_diff_cat= factor(ej_diff_cat, levels = c("match", "lower", "higher")), 
         geo_diff_cat = factor(geo_diff_cat, levels = c("local_match", "regional_match", "smaller", "bigger")),
         np_match = factor(np_match, levels = c("np_homophily", "no_match", "no_np_homophily")), 
         ego_local = factor(ego_local, levels = c("local", "regional")), 
         alter_local = factor(alter_local, levels = c("local", "regional")), 
         distance_n = normalize(distance)) %>%
  select(-X, -distance) # remove ID and distance

## Descriptive Results ----
## Table ----
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
  count()
ego_cat_summary

## 2. Observed Alters ----
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
  count()
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
  count()
# higher: means that alter has higher capacity than ego OR ego has lower capacity than alter
# lower: means that alter has lower cpacity than ego OR ego has higher capacity than alter

m_df %>%
  filter(dv == 0) %>%
  select(ego, alter, np_match) %>%
  unique() %>%
  group_by(np_match) %>%
  count()

m_df %>%
  filter(dv == 0) %>%
  select(ego, alter, ej_diff_cat) %>%
  unique() %>%
  group_by(ej_diff_cat) %>%
  count()

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
  count()
# higher: means that alter has higher capacity than ego OR ego has lower capacity than alter
# lower: means that alter has lower cpacity than ego OR ego has higher capacity than alter

m_df %>%
  filter(dv == 1) %>%
  select(ego, alter, np_match) %>%
  unique() %>%
  group_by(np_match) %>%
  count()

m_df %>%
  filter(dv == 1) %>%
  select(ego, alter, ej_diff_cat) %>%
  unique() %>%
  group_by(ej_diff_cat) %>%
  count()

m_df %>%
  filter(dv == 1) %>%
  select(ego, alter, geo_diff_cat) %>%
  unique() %>%
  group_by(geo_diff_cat) %>%
  count()

## Network figure ----
# Replace anonymous IDs with names for now
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

egos <- edgelist_names %>% select(ego) %>% distinct() %>% pull() 
alters <- edgelist_names %>% select(alter) %>% distinct() %>% pull()
both <- intersect(egos, alters)

net %v% 'position' <- ifelse(net %v% 'vertex.names' %in% egos, 1, 2)

net %v% 'labels' <- ifelse(net %v% 'vertex.names' %in% egos & net %v% 'position' == 1, net %v%  'vertex.names', '')

alter_overlaps <- edgelist_names %>% select(alter) %>% group_by(alter) %>% filter(n() > 1) %>% distinct() %>% pull() # 17 overlapping alters 

net %v% 'overlaps' <- ifelse(net %v% 'vertex.names' %in% both, # 9 overlapping 
                             1, 
                             ifelse(net %v% 'vertex.names' %in% alter_overlaps, 2, 0))  


# Plot
set.seed(1)
net_plot <- ggraph(net, layout = 'fr') +
  geom_edge_link(alpha = 0.5, color = "gray70", show.legend = FALSE) +
  geom_node_point(aes(size = as.numeric(degree), color = as.character(overlaps), shape = as.character(position)), alpha = .9) +
  scale_shape_manual(breaks = c(1, 2), values = c(17, 18), labels = c("Ego", "Alter")) +
  scale_color_manual(breaks = c(0,1,2), values = c("#CBC9E2", "#00BFC4", "#54278F"), labels = c("No Overlaps", "Alter-Alter Overlap", "Ego-Overlap")) +
  theme_void() +
  geom_node_text(aes(label = labels, size = as.numeric(degree)), repel = T, max.overlaps = Inf) +
  theme(legend.position = "bottom", legend.text = element_text(size = 14)) + 
  guides(size = guide_legend(title = "Degree"), shape = guide_legend(title = "Position"), color = guide_legend(title = "Shared Alter")) 

net_plot

#ggsave("plots/full_network.png", net_plot, width = 12, height = 9, units = "in")


## Inferential Results -----
## 1. Correlation ----
## Check correlation between numeric IV
cor_matrix <- cor(m_df[c("ego_capacity_n", "alter_capacity_n", "i_match", "ego_ej_mission", "alter_ej_mission", "distance_n", "count_ego_collaboratives",  "count_alter_collaboratives")])
cor_matrix

## 1. Variance Components Models ----
### a. Ego Only
set.seed(1992)
m_ego <- brm(dv ~ (1|ego), data = m_df, cores = 4, file = "outputs/m_ego.rds", family = bernoulli(link = "logit"))
m_ego


### b. Ego + Alter 
set.seed(1992)
m_ego_alter <- brm(dv ~ (1|ego) + (1|alter),  cores = 4, file = "outputs/m_ego_alter.rds",  data = m_df, family = bernoulli(link = "logit"))
m_ego_alter

## 2. Explanatory Models with REs -----
### a. Resource Exchange Model (ego + alter RE)
set.seed(1992) # set starting seed to make results replicable
m_re <- brm(dv ~ ego_capacity_n + alter_capacity_n + c_diff_cat + count_ego_collaboratives + count_alter_collaboratives + overlap_collab + ego_np_501c3 + alter_np_501c3 + np_match + (1|ego) + (1|alter), 
            data = m_df, 
            prior = c(prior(normal(0,1), class = b),
                      prior(normal(0,10), class = Intercept)), # very weak prior for intercept so we don't influence it
            family = bernoulli(link = "logit"),
            warmup = 1000, #burn in period
            iter = 2000, #actual samples
            chains = 4,
            cores = 4,
            file = "outputs/m_re",
            control = list(adapt_delta = 0.99))


### b. Boundary Definition Model (ego + alter RE)
set.seed(1992)
m_bd <- brm(dv ~ ego_ej_mission + alter_ej_mission + ej_diff_cat + count_ego_issues + count_alter_issues + i_match + ego_local + alter_local + geo_diff_cat + distance_n + (1|ego) + (1|alter), 
            data = m_df, 
            family = bernoulli(link = "logit"), 
            
            prior = c(prior(normal(0,1), class = b),
                      prior(normal(0,10), class = Intercept)), # very weak prior for intercept so we don't influence it
            warmup = 1000, #burn in period
            iter = 2000, #actual samples
            chains = 4,
            cores = 4,
            file = "outputs/m_bd", # save your model output
            control = list(adapt_delta = 0.99)) 

### c. Full Model (ego + alter RE)
set.seed(1992)
m_full <- brm(dv ~ ego_capacity_n + alter_capacity_n + c_diff_cat + count_ego_collaboratives + count_alter_collaboratives + overlap_collab + ego_np_501c3 + alter_np_501c3 + np_match + ego_ej_mission + alter_ej_mission + ej_diff_cat + count_ego_issues + count_alter_issues + i_match + ego_local + alter_local + geo_diff_cat + distance_n + (1|ego) + (1|alter), 
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

m_test <- brm(dv ~ ego_capacity_n + alter_capacity_n + c_diff_cat + count_ego_collaboratives + count_alter_collaboratives + overlap_collab + ego_np_501c3 + alter_np_501c3 + np_match + ego_ej_mission + alter_ej_mission + ej_diff_cat + count_ego_issues + count_alter_issues + i_match + ego_local + alter_local + geo_diff_cat + distance_n + (1|ego) + (1|alter), 
              data = m_df, 
              family = bernoulli(link = "logit"), 
              prior = c(prior(normal(0,1), class = b),
                        prior(normal(0,10), class = Intercept)), # very weak prior for intercept so we don't influence it
              warmup = 1000, #burn in period
              iter = 2000, #actual samples
              chains = 4,
              cores = 2,
              control = list(adapt_delta = 0.99)) 

### d. Full Model (ego only)
set.seed(1992)
m_full_ego <- brm(dv ~ ego_capacity_n + alter_capacity_n + c_diff_cat + count_ego_collaboratives + count_alter_collaboratives + overlap_collab + ego_np_501c3 + alter_np_501c3 + np_match + ego_ej_mission + alter_ej_mission + ej_diff_cat + count_ego_issues + count_alter_issues + i_match + ego_local + alter_local + geo_diff_cat + distance_n + (1|ego), 
                  data = m_df,
                  family = bernoulli(link = "logit"), 
                  prior = c(prior(normal(0,1), class = b),
                            prior(normal(0,10), class = Intercept)), # very weak prior for intercept so we don't influence it
                  warmup = 1000, #burn in period
                  iter = 2000, #actual samples
                  chains = 4,
                  cores = 2,
                  file = "outputs/m_full_ego",
                  control = list(adapt_delta = 0.99))

## 3. Model Selection ----
loo_compare(loo(m_bd), loo(m_re), loo(m_full))

## 4. Prior Selection ----
### a. Shrinkage Priors ----
#The horseshoe prior is a special shrinkage prior initially proposed by Carvalho et al. (2009). It is symmetric around  zero with fat tails and an infinitely large spike at zero. This makes it ideal for sparse models that have many regression coefficients, although only a minority of them is non- zero. The horseshoe prior can be applied on all population-level effects at once (excluding the intercept) by using set_prior("horseshoe(1)"). (p. 70)
set.seed(1992)
m_full_s <- brm(dv ~ ego_capacity_n + alter_capacity_n + c_diff_cat + count_ego_collaboratives + count_alter_collaboratives + overlap_collab + ego_np_501c3 + alter_np_501c3 + np_match + ego_ej_mission + alter_ej_mission + ej_diff_cat + count_ego_issues + count_alter_issues + i_match + ego_local + alter_local + geo_diff_cat + distance_n + (1|ego) + (1|alter), 
              data = m_df, 
              prior = c(prior(horseshoe(1), class = b),
                        prior(normal(0,10), class = Intercept)), # very weak prior for intercept so we don't influence it
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
m_full_c <- brm(dv ~ ego_capacity_n + alter_capacity_n + c_diff_cat + count_ego_collaboratives + count_alter_collaboratives + overlap_collab + ego_np_501c3 + alter_np_501c3 + np_match + ego_ej_mission + alter_ej_mission + ej_diff_cat + count_ego_issues + count_alter_issues + i_match + ego_local + alter_local + geo_diff_cat + distance_n + (1|ego) + (1|alter), 
              data = m_df, 
              prior = c(prior(cauchy(0, 2.5), class = b), # regularizing based on gelman 2008
                        prior(normal(0,10), class = Intercept)), # very weak prior for intercept so we don't influence it
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
### a. Full Model without G43 ----
## G43 is an ego that is ALSO a collaborative & therefore is included in the model in multiple ways. This version of the model runs a version of the model data that does not include G43 to see if the results substantively change. 
m_df_subset <- m_df %>%
  filter(ego != "G43" & alter != "G43")

set.seed(1992)
m_full_subset <- brm(dv ~ ego_capacity_n + alter_capacity_n + c_diff_cat + count_ego_collaboratives + count_alter_collaboratives + overlap_collab + ego_np_501c3 + alter_np_501c3 + np_match + ego_ej_mission + alter_ej_mission + ej_diff_cat + count_ego_issues + count_alter_issues + i_match + ego_local + alter_local + geo_diff_cat + distance_n + (1|ego) + (1|alter), 
                data = m_df_subset, 
                prior = c(prior(normal(0,1), class = b),
                          prior(normal(0,10), class = Intercept)), # very weak prior for intercept so we don't influence it
                family = bernoulli(link = "logit"), 
                warmup = 1000, #burn in period
                iter = 2000, #actual samples
                chains = 4,
                cores = 2,
                file = "outputs/m_full_subset",
                control = list(adapt_delta = 0.99)) 

## 6. Model Plots ----
m_re <- readRDS("outputs/m_re.rds")
m_bd <- readRDS("outputs/m_bd.rds")
m_full <- readRDS("outputs/m_full.rds")
### a. Coefficient Plots ----
#### Resource Exchange Model ----
coefs_re <- gather_coefs_re(m_full, "Resource Exchange")

coefs_re_plot <- coefs_re %>% 
  mutate(ordering = case_when(
    par == "Ego-Alter: Collaborative Membership Overlap" ~ -1.8, # update the value which will be used to order the parameter estimates
    TRUE ~ ordering
  )) %>%
  mutate(par = fct_reorder(par, ordering,  .desc = F)) %>% # reorder the coef names by ordering variable
  ggplot(., aes(value, par, color = variable)) +
  geom_point(position = position_dodge(width=.75)) + 
  geom_pointinterval(aes(xmin = .lower, xmax = .upper), position=position_dodge(width=.75), height=0) + 
  geom_vline(xintercept = 0) +
  scale_color_manual(values = c("#88694B",
                                "#657D94",
                                "#35483F"),
                     guide = guide_legend(reverse = TRUE, ncol = 3)) +
  labs(x = "Coefficient", y = "", color = "Variable") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_text(size = 12), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.text = element_text(size = 12))

coefs_re_plot

#ggsave("plots/coefs_re.png", coefs_re_plot, width = 9, height = 5, dpi = 600, units = "in")

#### Boundary Definition ----
coefs_bd <- gather_coefs_bd(m_full, "Boundary Definition")

coefs_bd_plot <- coefs_bd %>% 
  mutate(ordering = case_when(
    par == "Ego-Alter: Distance Between Home Offices" ~ -10, #put it at the bottom of the plot
    par == "Ego-Alter: No. of Matching Issues" ~ -2.8, 
    TRUE ~ ordering
  )) %>%
  mutate(par = fct_reorder(par, ordering, .desc = F)) %>% # reorder the coef names by ordering variable
  ggplot(., aes(value, par, color = variable, shape = type)) +
  geom_point(position = position_dodge(width=.75)) + 
  geom_pointinterval(aes(xmin = .lower, xmax = .upper), position=position_dodge(width=.75), height=0) + 
  geom_vline(xintercept = 0) +
  scale_color_manual(values = c("#88694B",
                                "#BFAB62",
                                "#657D94",
                                "#35483F"),
                     guide = guide_legend(reverse = TRUE, ncol = 1)) +
  labs(x = "Coefficient", y = "", color = "Variable", shape = "Type") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_text(size = 12), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.text = element_text(size = 12))

coefs_bd_plot

#ggsave("plots/coefs_bd.png", coefs_bd_plot, width = 9, height = 7, dpi = 600, units = "in")

### b. Posterior Prediction Plots --------
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

## 7. VPC Table ----
sjPlot::tab_model(m_ego, m_ego_alter, m_full_ego, m_full, dv.labels = c("VPC - Ego", "VPC - Ego + Alter", "Full Model - Ego", "Full Model - Ego + Alter"), file = "outputs/vpc_full_model_comp_table.doc")

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
## 2. Model Results Comparison ----

### a. Plot ----
coefs_re <- gather_coefs(m_re, "Resource Exchange")
coefs_bd <- gather_coefs(m_bd, "Boundary Definition")
coefs_full <- gather_coefs(m_full, "Full Model")

combined_coefs_plot <- combine_coefs_plot3(coefs_re, coefs_bd, coefs_full)

combined_coefs_plot

#ggsave("plots/coefs_all_models.png", combined_coefs_plot, width = 9, height = 10, dpi = 600, units = "in")

### b. Coefficient Table ----
sjPlot::tab_model(m_re, m_bd, m_full, dv.labels = c("Resource Exchange", "Boundary Definition", "Full Model"), file = "outputs/paper_model_table.doc")

### c. Loo Comparison Table ----
loo(m_re)
loo(m_bd)
loo(m_full)

### d. No ECJW Model Plot ----
coefs_full <- gather_coefs(m_full, "Full Model")
coefs_full_no_ejcw <- gather_coefs(m_full_subset, "Full Model without EJCW")

combined_coefs_plot2 <- combine_coefs_plot2(coefs_full, coefs_full_no_ejcw)

combined_coefs_plot2

ggsave("plots/coefs_full_model_no_ejcw_comp.png", combined_coefs_plot2, width = 9, height = 10, dpi = 600, units = "in")

## 2. Model Diagnostics ----
### a. Posterior Predictive Check ----
ppc <- pp_check(m_full, type = "dens_overlay", ndraws = 100)
ggsave("plots/ppc.png", ppc, width = 6, height = 4, dpi = 600, units = "in")

### b. Trace Plots ----
trace <- plot(m_full)
ggsave("plots/trace_plots.png", trace, width = 6, height = 4, dpi = 600, units = "in")

## 3. Prior Selection ----
loo(m_full_c)
loo(m_full_s)
loo(m_full)

coefs_c <- gather_coefs(m_full_c, "Cauchy")
coefs_s<- gather_coefs(m_full_s, "Horseshoe")
coefs_full <- gather_coefs(m_full, "Normal")

combined_coefs_plot <- combine_coefs_plot3(coefs_c, coefs_s, coefs_full)

combined_coefs_plot

#ggsave("plots/coefs_all_priors.png", combined_coefs_plot, width = 9, height = 10, dpi = 600, units = "in")

