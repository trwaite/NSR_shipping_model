library(tidyverse)

rewrite_subAU_map <- F

# get names of AUs needed
AU_name_map <- read_csv("inputs/oil_production/OilProdn_Ref.csv") %>%
  filter(grepl("rus", AU)) %>%
  select(ASSESSNAME = arcticAU, AU) %>%
  unique()

AU_names <- AU_name_map %>%
  pull(ASSESSNAME)

# read in AU boundaries shapefile
AU_poly <- sf::read_sf(dsn = "inputs/cara_au", layer = "cara_au_offshore_reprojected") %>%
  filter(ASSESSNAME %in% AU_names) %>%
  mutate(ID = 1:nrow(.))

# extract the AU coordinates
ID_map <- data.frame(AU_poly) %>%
  select(ID, ASSESSNAME)

AU_coords <- data.frame(sf::st_coordinates(AU_poly)) %>%
  left_join(ID_map, by = c("L3" = "ID")) %>%
  left_join(AU_name_map, by = "ASSESSNAME") %>%
  select(X,Y,AU)

# get the maximum and minimum longitude of each AU
AU_lon_range <- AU_coords %>%
  select(-Y) %>%
  # remap negative longitudes to over 180 for easier accounting
  mutate(X = case_when(X < 0 ~ 180 + (180 + X),
                       T ~ X)) %>%
  group_by(AU) %>%
  summarize(AU_start = min(X), AU_end = max(X)) %>%
  # calculate length of AU in terms of longitude
  mutate(AU_length =  AU_end - AU_start)

# read in subzone coordinates and get maximum and minimum longitude
sz_coords <- read_csv("inputs/cheaitou_2019_coordinates.csv") %>%
  rowid_to_column("subzone") %>%
  mutate(subzone = paste0("sz", subzone-1)) %>%
  rename(lat = `lat (Y)`, lon = `long (X)`) %>%
  # remap negative longitudes to over 180 for easier accounting
  mutate(lon = case_when(lon < 0 ~ 180 + (180 + lon),
                         T ~ lon)) %>%
  mutate(sz_end = lead(lon)) %>%
  filter(order > 1, order <= 37) %>%
  select(subzone, sz_start = lon, sz_end) %>%
  # calculate length of subzone in terms of longitude
  mutate(sz_length =  sz_end - sz_start) %>%
  # remove the last two subzones that have negative lengths
  filter(sz_length > 0)

# join together subzone and AU longitude ranges
sz_AU_map <- sz_coords %>% mutate(join = T) %>%
  full_join(AU_lon_range %>% mutate(join = T),
            by = "join") %>%
  select(-join) %>%
  mutate(
    # flag whether a subzone is contained fully within an AU, or whether the
    # AU starts or ends within a subzone
    overlap_flag = case_when(sz_start <= AU_start & sz_end >= AU_start ~ "begin",
                             sz_start >= AU_start & sz_end <= AU_end ~ "full",
                             sz_start <= AU_end & sz_end >= AU_end ~ "end",
                             T ~ "none"),
    # calculate the length of the AU that is within each subzone
    overlap_length = case_when(
      # if the subzone is fully within the AU, assign the whole subzone length
      overlap_flag == "full" ~ sz_length,
      # if the AU starts or ends within the subzone, assign just the part of
      # the subzone that overlaps with the AU
      overlap_flag == "begin" ~ sz_end - AU_start,
      overlap_flag == "end" ~ AU_end - sz_start,
      T ~ 0),
    # calculate proportion of the AU within each subzone
    proportion = overlap_length/AU_length) %>%
  filter(proportion > 0) %>%
  select(subzone, AU, proportion) %>%
  mutate(subAU = paste0(AU, "_", subzone))

# for AUs that start or end outside of the NSR, their proportions will add
# up to less than 1. Calculate the proportion of their oil production that would
# be produced outside of the NSR and assign all subzones to the East or West

# identify start and end of NSR
start_NSR <- sz_coords %>%
  filter(subzone == "sz1") %>%
  pull(sz_start)

# for end, use subzone 34 since we removed subzones 35 and 36
end_NSR <- sz_coords %>%
  filter(subzone == "sz34") %>%
  pull(sz_end)

# identify subzones that start or end outside NSR
subAU_outside_NSR <- AU_lon_range %>%
  mutate(outside_NSR = case_when(AU_start < start_NSR ~ "start",
                                 AU_end > end_NSR ~ "end",
                                 T ~ "none")) %>%
  filter(outside_NSR != "none") %>%
  select(AU, outside_NSR)

# assign proportions outside NSR to the first or last subzone
subAU_outside_NSR_prop <- sz_AU_map %>%
  group_by(AU) %>%
  summarize(proportion = sum(proportion)) %>%
  ungroup() %>%
  filter(proportion < 1) %>%
  left_join(subAU_outside_NSR, by = "AU") %>%
  mutate(subzone = case_when(outside_NSR == "start" ~ "sz1",
                             T ~ "sz34"),
         proportion = 1-proportion) %>%
  select(-outside_NSR) %>%
  mutate(subAU = paste0(AU, "_", subzone))

# add outside NSR proportions back into full map
subAU_proportions <- rbind(sz_AU_map, subAU_outside_NSR_prop) %>%
  group_by(subzone, AU, subAU) %>%
  summarize(proportion = sum(proportion)) %>%
  ungroup() %>%
  select(AU, "subAU", proportion)

# now all proportions should sum to 1-- check this
subAU_prop_check <- subAU_proportions %>%
  group_by(AU) %>%
  summarize(proportion = sum(proportion)) %>%
  ungroup()

# write out subAU proportions
if(rewrite_subAU_map){
  write_csv(subAU_proportions, "inputs/oil_production/subAU_oil_proportions.csv")
}


# create mapping file for shipping model

# first make df of all combinations of subzone and subAU
subAU <- unique(subAU_proportions$subAU)
subzone <- paste0("sz", 1:36)
all_AU_sz <- tidyr::expand_grid(subAU, subzone)

# now map subzones to the east and west of each subAU
subAUSubZoneMap <- all_AU_sz %>%
  mutate(subAU_split = subAU) %>%
  separate(subAU_split, into = c("AU", "subzone_map"), sep = "_") %>%
  select(-AU) %>%
  mutate(subzone_map_n = as.numeric(gsub("sz", "", subzone_map)),
         subzone_n = as.numeric(gsub("sz", "", subzone)),
         value = case_when(subzone_n > subzone_map_n ~ 1,
                           subzone_n < subzone_map_n ~ 2)) %>%
  select(sz = subzone, subAU, value)


# write out the mapping file
if(rewrite_subAU_map){
  write_csv(subAUSubZoneMap, "inputs/mappings/subAUSubZoneMap.csv")
}


