library(tidyverse)

out_dir <- "outputs"

fig_dir <- "figures"

conv_mmbtu_GJ <- 1.055
conv_ton_mmbtu_bunker <- 40.2
Conv_BBL_TOE <- 0.1363636
Conv_MMBO_EJ <- 5.712/1000
Conv_EJ_MTOE <- Conv_BBL_TOE/Conv_MMBO_EJ
conv_nm_km <- 1.852

#...............................................................................
# Non-spatial figures ----------------------------------------------------------
#...............................................................................

#...............................................................................
## Figure 2: months per year when NSR transit is possible ======================
#...............................................................................

NSR_transit_months <- read_csv(paste0(out_dir,"/numbMonthsTransitNSR.csv"))

NSR_transit_months$rcp <- factor(NSR_transit_months$rcp,
                                 levels = c("8p5", "2p6", "4p5"),
                                 labels = c("RCP 8.5", "RCP 2.6", "RCP 4.5"))

NSR_transit_months$ens <-
  factor(NSR_transit_months$ens,
         levels = c("ens1", "ens2", "ens3", "ens4", "ens5", "ens6", "ens7",
                    "ens8", "ens9", "ens10", "ens11", "ens12", "ens13"))


# version with ensemble averages, both ship types
NSR_transit_months %>%
  filter(rcp %in% c("RCP 2.6", "RCP 8.5")) %>%
  group_by(year, rcp, ship_type) %>%
  mutate(avg = mean(num_months)) %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(x = year, y = num_months, col = ens)) +
  scale_color_manual(values = rep("grey80", 13), guide = "none") +
  geom_line(aes(x = year, y = avg), col = "blue") +
  scale_y_continuous(limits = c(0, 12), n.breaks = 8) +
  facet_grid(ship_type~rcp) +
  theme_bw() +
  ylab("Months NSR transit possible") + xlab("Year")

ggsave(paste0(fig_dir, "/NSR_transit_months.png"),
       width = 10, height = 6, units = "in")



#...............................................................................
## Figure 3: Total Russian oil production ======================================
#...............................................................................

oil_production <- read_csv(paste0(out_dir, "/RussiaOilProdnTotal.csv"))

oil_production_scen <- oil_production %>%
  filter(rcp %in% c("2p6", "8p5")) %>%
  mutate(scen = case_when(grepl("8", rcp) ~ "BAU|8.5",
                          scen == "Ref" ~ "BAU|2.6",
                          T ~ "LowC|2.6")) %>%
  select(-rcp)

oil_production_scen$scen <-
  factor(oil_production_scen$scen,
         levels = c("BAU|8.5", "BAU|2.6", "LowC|2.6"))

#version showing all ensembles + ensemble average
oil_production_scen %>%
  filter(year >= 2015) %>%
  group_by(year, scen) %>%
  mutate(avg = mean(value)) %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(x = year, y = value, col = ens)) +
  scale_color_manual(values = rep("grey80", 13), guide = "none") +
  geom_line(aes(x = year, y = avg), col = "blue") +
  facet_wrap(~scen) +
  theme_bw() +
  ylab("Oil produced (million tonnes)") + xlab("Year")

ggsave(paste0(fig_dir, "/oil_production.png"), width = 10, height = 4, units = "in")


#...............................................................................
## Figure 4: Percent oil that can be shipped either route ======================
#...............................................................................

# read in route navigability
route_nav <- read_csv(paste0(out_dir, "/NavigableEastWest_AUsubZ.csv"))

# read in oil production by subzone
oil_prod <- read_csv(paste0(out_dir,"/RussiaOilProdn_subAU.csv")) %>%
  group_by(year, subAU, scen, rcp) %>%
  summarize(value = mean(value)) %>%
  ungroup()

# calculate navigability by season
prop_navigable_East_season <- route_nav %>%
  left_join(oil_prod, by = c("year", "subAU", "rcp")) %>%
  filter(value > 0) %>%
  pivot_wider(names_from = "dir", values_from = "navigable") %>%
  mutate(West = if_else(is.na(West), T, West),
         flag = !East,
         season = case_when(month %in% c("jul", "aug", "sep", "oct", "nov") ~ "summer_autumn",
                            T ~ "winter_spring")) %>%
  select(-East, -West) %>%
  group_by(year, season, ens, rcp, ship_type, scen, flag) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  # calculate proportion
  group_by(year, ens, season, rcp, ship_type, scen) %>%
  mutate(prop = value/sum(value)) %>%
  ungroup() %>%
  # remove the "false" rows
  filter(!flag) %>%
  select(-flag)

# calculate navigability by year
prop_navigable_East_total <- route_nav %>%
  left_join(oil_prod, by = c("year", "subAU", "rcp")) %>%
  filter(value > 0) %>%
  pivot_wider(names_from = "dir", values_from = "navigable") %>%
  mutate(West = if_else(is.na(West), T, West),
         flag = !East) %>%
  select(-East, -West) %>%
  group_by(year, ens, rcp, ship_type, scen, flag) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  # calculate proportion
  group_by(year, rcp, ens, ship_type, scen) %>%
  mutate(prop = value/sum(value)) %>%
  ungroup() %>%
  # remove the "false" rows
  filter(!flag) %>%
  select(-flag) %>%
  mutate(season = "combined")

# combine annual with seasonal
prop_navigable_East_all <- rbind(prop_navigable_East_season,
                                 prop_navigable_East_total)

prop_navigable_East_all <- prop_navigable_East_all %>%
  mutate(scen = case_when(grepl("8", rcp) ~ "BAU|8.5",
                          scen == "Ref" ~ "BAU|2.6",
                          T ~ "LowC|2.6"))

prop_navigable_East_all$scen <-
  factor(prop_navigable_East_all$scen,
         levels = c("BAU|8.5", "BAU|2.6", "LowC|2.6"))

prop_navigable_East_all$season <-
  factor(prop_navigable_East_all$season,
         labels = c("Full year", "Summer/ Autumn", "Winter/ Spring"))

# plot seasonal and total
prop_navigable_East_all %>%
  # get ens stats for plotting
  group_by(year, scen, ship_type, season) %>%
  summarize(min = min(prop),
            max = max(prop),
            mean = mean(prop)) %>%
  ungroup() %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = mean*100, color = scen, lty = ship_type)) +
  scale_color_discrete(name="Scenario") +
  scale_linetype_discrete(name = "Vessel type") +
  expand_limits(y = 0) +
  theme_bw() +
  facet_grid(~season) +
  xlab("Year") + ylab("% of oil that can be shipped via either route")

ggsave(paste0(fig_dir,"/prop_navigable_both_routes.png"),
       width = 8, height = 4, units = "in")



#...............................................................................
## Figures 5 and S13: tonnage of oil shipped East vs West ======================
#...............................................................................

# read in oil shipment data
tonnage_oil_shipped <- read_csv(paste0(out_dir, "/RussiaOilProdn_TotalShippedYr.csv"))


# get ensemble averages
tonnage_oil_shipped_ens_avg <- tonnage_oil_shipped %>%
  group_by(scen, rcp, usd_rub, year, dir, ship_type) %>%
  mutate(ens_avg = mean(value)) %>%
  ungroup() %>%
  filter(rcp %in% c("2p6", "8p5")) %>%
  mutate(scen = case_when(grepl("8", rcp) ~ "BAU|8.5",
                          scen == "Ref" ~ "BAU|2.6",
                          T ~ "LowC|2.6"))

tonnage_oil_shipped_ens_avg$scen <-
  factor(tonnage_oil_shipped_ens_avg$scen,
         levels = c("BAU|8.5", "BAU|2.6", "LowC|2.6"))


tonnage_oil_shipped_ens_avg %>%
  filter(usd_rub == "base") %>%
  ggplot(aes(x = year, y = ens_avg, col = dir, lty = ship_type)) +
  geom_line() +
  scale_linetype_discrete(name = "Vessel type") +
  scale_color_manual(values = jgcricolors::jgcricol()$pal_16[c(3,2)],
                     name = "Route") +
  facet_wrap(~scen) +
  theme_bw() +
  ylab("Oil shipped (million tonnes)") + xlab("Year")

ggsave(paste0(fig_dir, "/tonnage_oil_shipped.png"),
       width = 10, height = 4, units = "in")

# version showing individual ensembles, 1A (Figure S13a)
tonnage_oil_shipped_ens_avg %>%
  filter(ship_type == "1A", usd_rub == "base") %>%
  ggplot() +
  geom_line(aes(x = year, y = value, col = ens)) +
  ylim(c(0, 35)) +
  scale_color_manual(values = rep("grey80", 13), guide = "none") +
  geom_line(aes(x = year, y = ens_avg), col = "blue") +
  facet_grid(dir~scen) +
  theme_bw() +
  ylab("Oil shipped (million tonnes)") + xlab("Year")

ggsave(paste0(fig_dir, "/tonnage_oil_shipped_ens_1A.png"),
       width = 10, height = 6, units = "in")

# version showing individual ensembles, 1AS (Figure S13b)
tonnage_oil_shipped_ens_avg %>%
  filter(ship_type == "1AS", usd_rub == "base") %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(x = year, y = value, col = ens)) +
  ylim(c(0, 35)) +
  scale_color_manual(values = rep("grey80", 13), guide = "none") +
  geom_line(aes(x = year, y = ens_avg), col = "blue") +
  facet_grid(dir~scen) +
  theme_bw() +
  ylab("Oil shipped (million tonnes)") + xlab("Year")

ggsave(paste0(fig_dir, "/tonnage_oil_shipped_ens_1AS.png"),
       width = 10, height = 6, units = "in")


#...............................................................................
## Figure 6: Number of trips ===================================================
#...............................................................................

# read in trips data and calculate ensemble averages
trips_ensemble_avg <- read_csv(paste0(out_dir,"/YearlyOutboundTrips.csv")) %>%
  group_by(year, scen, rcp, usd_rub, ship_type, dir) %>%
  summarise(value = mean(value)) %>%
  ungroup()


trips_ensemble_avg_scen <- trips_ensemble_avg %>%
  filter(rcp %in% c("8p5", "2p6")) %>%
  mutate(scen = case_when(grepl("8", rcp) ~ "BAU|8.5",
                          scen == "Ref" ~ "BAU|2.6",
                          T ~ "LowC|2.6"))

trips_ensemble_avg_scen$scen <-
  factor(trips_ensemble_avg_scen$scen,
         levels = c("BAU|8.5", "BAU|2.6", "LowC|2.6"))


# version with both ship types
trips_ensemble_avg_scen %>%
  filter(usd_rub == "base") %>%
  ggplot(aes(x = year, y = value, col = scen, lty = ship_type)) +
  geom_line() +
  scale_color_discrete(name = "Scenario") +
  facet_wrap(~dir) +
  scale_linetype_discrete(name = "Vessel type") +
  theme_bw() +
  ylab("Number of outbound trips") + xlab("Year")

ggsave(paste0(fig_dir, "/outbound_trips.png"), width = 10, height = 4, units = "in")



#...............................................................................
## Figure 7: total shipping traffic on the NSR =================================
#...............................................................................

# read in shipping traffic data
NSR_ship_km_total <- read_csv(paste0(out_dir, "/ship_km_sz.csv")) %>%
  group_by(year, scen, ens, usd_rub, rcp, ship_type) %>%
  summarize(ship_km = sum(ship_km, na.rm = T)) %>%
  ungroup() %>%
  filter(year >= 2020)


NSR_ship_km_total_scen <- NSR_ship_km_total %>%
  mutate(scen = case_when(grepl("8", rcp) ~ "BAU|8.5",
                          scen == "Ref" ~ "BAU|2.6",
                          T ~ "LowC|2.6"))

NSR_ship_km_total_scen$scen <-
  factor(NSR_ship_km_total_scen$scen,
         levels = c("BAU|8.5", "BAU|2.6", "LowC|2.6"))

# version with just ensemble averages
NSR_ship_km_total_scen %>%
  filter(usd_rub == "base") %>%
  group_by(year, scen, ship_type) %>%
  summarize(ship_km = mean(ship_km)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = ship_km, col = scen, lty = ship_type)) +
  geom_line() +
  scale_color_discrete(name = "Scenario") +
  scale_linetype_discrete(name = "Vessel type") +
  theme_bw() +
  ylab("Shipping traffic (ship-km)") + xlab("Year")

ggsave(paste0(fig_dir, "/shipping_traffic_ship_km.png"),
       width = 8, height = 4, units = "in")


#...............................................................................
## Figures 9 and S15: total CO2 emissions ======================================
#...............................................................................

# read in emissions data and get ensemble averages
emiss_ensemble_avg <- read.csv(paste0(out_dir, "/Emissions_Yr_total.csv")) %>%
  group_by(year, rcp, scen, ship_type, dir, section, usd_rub) %>%
  summarize(value = mean(value)) %>%
  ungroup()


emiss_ensemble_avg_scen <- emiss_ensemble_avg %>%
  filter(rcp %in% c("2p6", "8p5")) %>%
  mutate(scen = case_when(grepl("8", rcp) ~ "BAU|8.5",
                          scen == "Ref" ~ "BAU|2.6",
                          T ~ "LowC|2.6"))

emiss_ensemble_avg_scen$scen <-
  factor(emiss_ensemble_avg_scen$scen,
         levels = c("BAU|8.5", "BAU|2.6", "LowC|2.6"))


emiss_ensemble_avg_scen$section <-
  factor(emiss_ensemble_avg_scen$section,
         levels = c("NSR", "non_NSR"),
         labels = c("Inside NSR", "Outside NSR"))

# area chart of total emissions by section, 1A
emiss_ensemble_avg_scen %>%
  filter(ship_type == "1A", year >= 2015, usd_rub == "base") %>%
  mutate(section = paste0(dir, ": ", section)) %>%
  ggplot(aes(x = year, y = value, fill = section)) +
  geom_area() +
  ylim(c(0, 6200)) +
  scale_fill_manual(values = c("#1f78b4", "#a6cee3", "#33a02c", "#b2df8a"),
                    name = "Section") +
  facet_wrap(~scen) +
  theme_bw() +
  ylab("Emissions (ktCO2)") + xlab("Year")

ggsave(paste0(fig_dir, "/total_emissions_1A.png"),
       width = 10, height = 4, units = "in")


# area chart of total emissions by section, 1AS
emiss_ensemble_avg_scen %>%
  filter(ship_type == "1AS", year >= 2015, usd_rub == "base") %>%
  mutate(section = paste0(dir, ": ", section)) %>%
  ggplot(aes(x = year, y = value, fill = section)) +
  geom_area() +
  ylim(c(0, 6200)) +
  scale_fill_manual(values = c("#1f78b4", "#a6cee3", "#33a02c", "#b2df8a"),
                    name = "Section") +
  facet_wrap(~scen) +
  theme_bw() +
  ylab("Emissions (ktCO2)") + xlab("Year")

ggsave(paste0(fig_dir, "/total_emissions_1AS.png"), width = 10, height = 4, units = "in")

#...............................................................................
## Figure S16: CO2 emissions intensity =========================================
#...............................................................................

# calculate emissions intensity (emissions per unit of oil shipped)
emiss_intensity_by_dir <- emiss_ensemble_avg_scen %>%
  group_by(year, scen, ship_type, dir, usd_rub) %>%
  summarize(emiss = sum(value)) %>%
  ungroup() %>%
  left_join(tonnage_oil_shipped_ens_avg %>% filter(ens == 'ens1'),
            by = c("year", "scen", "ship_type", "dir", "usd_rub")) %>%
  mutate(intensity = emiss/ens_avg)

emiss_intensity_total <- emiss_ensemble_avg_scen %>%
  group_by(year, scen, ship_type, dir, usd_rub) %>%
  summarize(emiss = sum(value)) %>%
  ungroup() %>%
  left_join(tonnage_oil_shipped_ens_avg,
            by = c("year", "scen", "ship_type", "dir", "usd_rub")) %>%
  group_by(year, scen, ship_type, usd_rub) %>%
  summarise(emiss = sum(emiss), value = sum(value)) %>%
  ungroup() %>%
  mutate(intensity = emiss/value)

# plot overall average emissions intensity (Figure S16a)
emiss_intensity_total %>%
  filter(usd_rub == "base") %>%
  ggplot(aes(x = year, y = intensity, col = scen, lty = ship_type)) +
  geom_line() +
  scale_color_discrete(name = "Scenario") +
  theme_bw() +
  xlab("") +
  ylab("kg CO2/ million tonnes oil shipped")

ggsave(paste0(fig_dir, "/emiss_intensity_total.png"),
       width = 6, height = 4, units = "in")

# plot emissions intensity by route (Figure S16b)
emiss_intensity_by_dir %>%
  filter(usd_rub == "base") %>%
  ggplot(aes(x = year, y = intensity, col = scen, lty = ship_type)) +
  geom_line() +
  scale_color_discrete(name = "Scenario") +
  theme_bw() +
  facet_wrap(~dir, scales = "free_y") +
  xlab("") +
  ylab("kg CO2/ million tonnes oil shipped")

ggsave(paste0(fig_dir, "/emiss_intensity_dir.png"),
       width = 8, height = 4, units = "in")


emiss_intensity_route_compare <- emiss_intensity_by_dir %>%
  select(-c(emiss, value, ens_avg)) %>%
  pivot_wider(names_from = dir, values_from = intensity) %>%
  mutate(pct_diff = (West-East)/West*100,
         abs_diff = West-East)

#...............................................................................
## Figure 10: sensitivity of NSR traffic to ice breaker escort fee cases =======
#...............................................................................

# calculate differences between sensitivity cases and base case
NSR_ship_km_fee_diff <- NSR_ship_km_total_scen %>%
  pivot_wider(names_from = "usd_rub", values_from = "ship_km") %>%
  mutate(discounted = (discounted - base)/base*100,
         high = (high - base)/base*100,
         low = (low - base)/base*100) %>%
  select(-base) %>%
  pivot_longer(c(discounted, high, low),
               names_to = "fee_sensitivity",
               values_to = "pct_diff")

NSR_ship_km_fee_diff$fee_sensitivity <-
  factor(NSR_ship_km_fee_diff$fee_sensitivity,
         levels = c("discounted", "high", "low"),
         labels = c("Discounted fee", "Low RUB/USD rate", "High RUB/USD rate"))

NSR_ship_km_fee_diff %>%
  group_by(year, scen, ship_type, fee_sensitivity) %>%
  mutate(avg = mean(pct_diff),
         min = min(pct_diff),
         max = max(pct_diff)) %>%
  ungroup() %>%
  ggplot() +
  geom_ribbon(aes(x = year, ymin = min, ymax = max, fill = fee_sensitivity),
              alpha = 0.3) +
  geom_line(aes(x = year, y = avg, color = fee_sensitivity)) +
  scale_color_discrete(name = "Icebreaker escort\nfee sensitivity") +
  scale_fill_discrete(name = "Icebreaker escort\nfee sensitivity") +
  facet_grid(ship_type ~ scen) +
  theme_bw() +
  xlab("Year") + ylab("Percent difference from base")

ggsave(paste0(fig_dir, "/fee_cases_ship_km_pct_diff.png"),
       width = 12, height = 5, units = "in")

#...............................................................................
## Figure S12: stranded oil (share of oil that can be shipped) =================
#...............................................................................

# calculate share of total oil production that is shipped
shipped_shares <- tonnage_oil_shipped_ens_avg %>%
  select(-ens_avg) %>%
  group_by(year, scen, ens, rcp, ship_type, usd_rub) %>%
  summarize(shipped = sum(value)) %>%
  ungroup() %>%
  left_join(oil_production_scen %>% rename(prod = value),
            by = c("year", "scen", "ens")) %>%
  mutate(share = shipped/prod)


shipped_shares$scen <-
  factor(shipped_shares$scen,
         levels = c("BAU|8.5", "BAU|2.6", "LowC|2.6"))


shipped_shares %>%
  filter(usd_rub == "base") %>%
  group_by(year, scen, ship_type) %>%
  summarize(share = mean(share)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = share, col = scen, lty = ship_type)) +
  geom_line() +
  scale_color_discrete(name = "Scenario") +
  scale_linetype_discrete(name = "Ship type") +
  theme_bw() +
  #expand_limits(y = 0) +
  ylab("Share of oil produced that can be shipped") + xlab("Year")

ggsave(paste0(fig_dir, "/share_oil_shipped.png"), width = 8, height = 4, units = "in")


#...............................................................................
## Figure S5: vessel speed equations ===========================================
#...............................................................................

# get a range of sea ice thickness values to plot on the x axis
sit_ranges <- tibble(sit = rep(seq(0, 120, length.out = 500),2),
                     vessel_type = c(rep("1A", 500), rep("1AS", 500)))

# calculate vessel speed as a function of SIT based on the thresholds and
# equations used in the paper
speed_data <- sit_ranges %>%
  mutate(speed = case_when(sit < 10 & vessel_type == "1A" ~ 16,
                           sit < 10 & vessel_type == "1AS" ~ 15.4,
                           sit < 30 & vessel_type == "1A" ~ 3.7427*(sit/100)^-0.6309,
                           sit < 40 & vessel_type == "1AS" ~ 5.1891*(sit/100)^-0.4724,
                           sit < 75 & vessel_type == "1A" ~ 3,
                           sit < 105 & vessel_type == "1AS" ~ 3,
                           T ~ 0),
         # label the different SIT ranges
         phase = case_when(sit < 10 & vessel_type == "1A" ~ "full speed",
                           sit < 10 & vessel_type == "1AS" ~ "full speed",
                           sit < 30 & vessel_type == "1A" ~ "reduced speed",
                           sit < 40 & vessel_type == "1AS" ~ "reduced speed",
                           sit < 75 & vessel_type == "1A" ~ "ice breaker assisted",
                           sit < 105 & vessel_type == "1AS" ~ "ice breaker assisted",
                           T ~ "not navigable"))

# reorder SIT ranges
speed_data$phase <-
  factor(speed_data$phase,
         levels = c("full speed", "reduced speed", "ice breaker assisted",
                    "not navigable"))

# plot speed as a piece-wise function of SIT
speed_data %>%
  ggplot(aes(x = sit, y = speed, color = phase)) +
  geom_line() +
  scale_color_manual(values = c("#1a9641", "#a6d96a", "#fdae61", "#d7191c"),
                     name = "SIT range") +
  scale_y_continuous(breaks = c(0,2,4,6,8,10,12,14,16)) +
  facet_wrap(~vessel_type) +
  theme_bw() +
  xlab("Sea ice thickness (cm)") + ylab("Speed (knots)")

ggsave(paste0(fig_dir, "/speed_function.png"), width = 9, height = 3, units = "in")


#...............................................................................
# Spatial figures --------------------------------------------------------------
#...............................................................................

map_theme <-
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_rect(fill = "grey93"))

## Basemap =====================================================================

# get a map of all countries
world <- ggplot2::map_data("world")

# extract just the lat long data and set its original CRS
world_dat<- world[,1:2]
sp::coordinates(world_dat) = c("long", "lat")
# set the original CRS, which is unprojected (WGS84)
sp::proj4string(world_dat) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs")

# define the desired projection (WGS 84 / Arctic Polar Stereographic)
polar <- "+proj=stere +lat_0=90 +lat_ts=71 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# project the world map data to the above projection
polar_world_dat <- as.data.frame(sp::spTransform(world_dat, polar))

# add back in the other attributes for the world map
polar_world <- data.frame(polar_world_dat,
                          region = world$region,
                          group = world$group,
                          order = world$order)

# extract just countries with Arctic regions for faster plotting
arctic_regions <- c("Russia", "USA", "Canada",
                    "Greenland", "Iceland", "Norway",
                    "Sweden", "Finland")

# plot the basemap (NSR region) in the polar projection
basemap <- ggplot() +
  geom_polygon(data = polar_world %>%
                 filter(region %in% arctic_regions),
               aes(x = long, y = lat, group = group),
               color = "black", fill = "grey") +
  coord_sf(crs = polar,
           xlim = c(-1000000, 3000000),
           ylim = c(-2000000, 3000000)) +
  theme_bw()


## Basemap with the NSR ========================================================

# read in subzone coordinates
sz_coord <- read_csv("inputs/cheaitou_2019_coordinates.csv") %>%
  # add subzone column to match emissions data
  rowid_to_column("subzone") %>%
  mutate(subzone = paste0("sz", subzone-1)) %>%
  rename(lat = `lat (Y)`, long = `long (X)`)

# extract the lat long and set its current CRS (unprojected)
sz_coord_dat<- sz_coord %>% select(long, lat)
sp::coordinates(sz_coord_dat) = c("long", "lat")
sp::proj4string(sz_coord_dat) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs")

# project to the polar CRS
polar_sz_coord_dat = as.data.frame(sp::spTransform(sz_coord_dat, polar))

# add back in the other attributes
polar_sz_coord <- data.frame(polar_sz_coord_dat,
                             subzone = sz_coord$subzone,
                             order = sz_coord$order,
                             group1 = sz_coord$group1,
                             group2 = sz_coord$group2)

# generate a basemap that includes the NSR
# (needed for emissions plots where there are sections without emissions)
basemap_with_NSR <- basemap +
  geom_path(data = filter(arrange(polar_sz_coord, order),
                          order <= 38),
            aes(x = long, y = lat), color = "red", lty = 1)



## Basemap with the NSR and AUs ================================================

# get names of AUs needed and mapping between names
AU_name_map <- read_csv("inputs/oil_production/OilProdn_Ref.csv") %>%
  filter(grepl("rus", AU)) %>%
  select(ASSESSNAME = arcticAU, AU) %>%
  unique()

AU_names <- AU_name_map %>%
  pull(ASSESSNAME)

# read in AU boundaries shapefile
AU_poly <- sf::read_sf(dsn = "inputs/cara_au",
                       layer = "cara_au_offshore_reprojected") %>%
  filter(ASSESSNAME %in% AU_names) %>%
  mutate(ID = 1:nrow(.)) %>%
  left_join(AU_name_map, by = "ASSESSNAME")

# project the AU boundaries
AU_poly_polar <- sf::st_transform(AU_poly, polar)

# get sz segments for ploting
sz_segment_pts <- data.frame(sz_coord_dat) %>% select(lat, long) %>%
  rowid_to_column("sz_n") %>%
  filter(sz_n >1, sz_n <= 36) %>%
  mutate(lat_end = 89, long_end = long, lat = 65) %>%
  pivot_longer(c(lat, lat_end), names_to = "lat_pt", values_to = "lat") %>%
  pivot_longer(c(long, long_end), names_to = "long_pt", values_to = "long") %>%
  filter((lat_pt == "lat" & long_pt == "long") | (lat_pt == "lat_end" & long_pt == "long_end")) %>%
  select(sz_n, lat, long)

sp::coordinates(sz_segment_pts) = c("long", "lat")
sp::proj4string(sz_segment_pts) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs")


sz_segments <- sf::st_as_sf(sz_segment_pts,
                            coords = c("long", "lat"),
                            crs = polar) %>%
  group_by(sz_n) %>%
  summarize() %>%
  sf::st_cast("LINESTRING")

#...............................................................................
## Figure S3: map of labelled AUs and NSR ======================================
#...............................................................................

# make map with labelled AUs and NSR
basemap_with_NSR_and_AUs <- basemap +
  # add AUs
  geom_sf(data = AU_poly_polar, fill = "#56B4E9", alpha = 1) +
  # add NSR on top
  geom_path(data = filter(arrange(polar_sz_coord, order), order <= 38),
            aes(x = long, y = lat), color = "#E69F00", lty = 1, lwd = 1) +
  # label AUs
  geom_sf_label(data = AU_poly_polar, aes(label = AU),
                label.padding = unit(0.15, "lines"), size = 2.5) +
  coord_sf(crs = polar,
           xlim = c(-1000000, 3000000),
           ylim = c(-2000000, 3000000)) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

ggsave(paste0(fig_dir, "/AU_NSR_map.png"), basemap_with_NSR_and_AUs,
       width = 6, height = 7, units = "in")

#...............................................................................
## Figure S4a: map of AUs, NSR, and subzone longitudinal boundaries ============
#...............................................................................

# make map with labelled AUs and NSR plus subzone longitude boundaries
basemap_with_NSR_and_AUs_sz <- basemap +
  # add AUs
  geom_sf(data = AU_poly_polar, fill = "#56B4E9", alpha = 1) +
  # add NSR on top
  geom_path(data = filter(arrange(polar_sz_coord, order), order <= 38),
            aes(x = long, y = lat), color = "#E69F00", lty = 1, lwd = 1) +
  # add nsr longitute lines
  geom_sf(data = sz_segments, color = "#E69F00", lwd = 0.3, lty = 1) +
  # label AUs
  geom_sf_label(data = AU_poly_polar, aes(label = AU),
                label.padding = unit(0.15, "lines"), size = 2.5) +
  coord_sf(crs = polar,
           xlim = c(-1000000, 3000000),
           ylim = c(-2000000, 3000000)) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

ggsave(paste0(fig_dir, "/AU_NSR_map_with_subzone_long.png"),
       basemap_with_NSR_and_AUs_sz,
       width = 6, height = 7, units = "in")

#...............................................................................
## Figure S4b: mapping between AU boundaries and NSR subzones ==================
#...............................................................................
source("map_AU_SZ.R")

# join subzone start and end longitudes with AU start and end longitudes
all_coords <- sz_coords %>% mutate(join = T) %>%
  full_join(AU_lon_range %>% mutate(join = T),
            by = "join") %>%
  select(subzone, sz_start, sz_end, AU, AU_start, AU_end)

# order AUs for plotting
all_coords$AU <- factor(all_coords$AU,
                        levels = c("rus1", "rus2", "rus3", "rus4", "rus6",
                                   "rus9", "rus10", "rus12", "rus13", "rus15",
                                   "rus17", "rus18", "rus19", "rus21", "rus22"))

# plot AUs along longitude range
all_coords %>%
  ggplot() +
  geom_vline(aes(xintercept = sz_start), col = "#E69F00", lwd = 0.5) +
  geom_vline(aes(xintercept = sz_end), col = "#E69F00", lwd = 0.5) +
  geom_segment(aes(x = AU_start, xend = AU_end, y = AU, yend = AU),
               col = "#56B4E9", lwd = 1) +
  theme_bw() +
  xlab("Longitude") + ylab("AU")

ggsave(paste0(fig_dir, "/AU_sz_longitude_ranges.png"),
       width = 8, height = 5, units = "in")

#...............................................................................
## Figures 8 and S14: traffic distribution along NSR ===========================
#...............................................................................

# read in tonne-km by subzone
NSR_ship_km_sz <- read_csv(paste0(out_dir, "/ship_km_sz.csv")) %>%
  # take ensemble average
  group_by(year, scen, rcp, ship_type, dir, subzone) %>%
  summarize(ship_km = mean(ship_km)) %>%
  ungroup() %>%
  # sum over direction
  group_by(year, scen, rcp, ship_type, subzone) %>%
  summarize(ship_km = sum(ship_km)) %>%
  ungroup()

NSR_ship_km_sz_scen <- NSR_ship_km_sz %>%
  mutate(scen = case_when(grepl("8", rcp) ~ "BAU|8.5",
                          scen == "Ref" ~ "BAU|2.6",
                          T ~ "LowC|2.6"))

NSR_ship_km_sz_scen$scen <-
  factor(NSR_ship_km_sz_scen$scen,
         levels = c("BAU|8.5", "BAU|2.6", "LowC|2.6"))


# join coordinates with emissions
NSR_ship_km_coords <-
  NSR_ship_km_sz_scen %>%
  left_join(sz_coord, by = "subzone")

# read subzone lengths
sz_lengths <- read_csv("inputs/distances/SubZoneDist.csv") %>%
  rename(length = dist, subzone = sz)

# join with subzone length and calculate traffic intensity
NSR_ship_km_coords_length <-
  NSR_ship_km_coords %>%
  left_join(sz_lengths, by = "subzone") %>%
  mutate(intensity = ship_km/(length*conv_nm_km))

# join with polar coordinates of subzones
polar_NSR_ship_km <- NSR_ship_km_coords_length %>%
  # replace the lat and long with the polar projected versions
  select(-c(lat, long)) %>%
  left_join(polar_sz_coord, by = c("subzone", "order", "group1", "group2"))


# calculate average traffic intensity by period (2025-2050, 2050-2075, 2075-2100)
polar_NSR_ship_km_period <- polar_NSR_ship_km %>%
  filter(year > 2025) %>%
  mutate(period = case_when(year <= 2050 ~ "2025-2050",
                            year <= 2075 ~ "2050-2075",
                            T ~ "2075-2100")) %>%
  group_by(scen, subzone, ship_type, order, group1, group2, length, long, lat, period) %>%
  summarize(ship_km = sum(ship_km)) %>%
  ungroup() %>%
  # get the average yearly intensity by dividing by 25 (the period length)
  mutate(intensity = ship_km/(length*conv_nm_km)/25)


# make map for 1A results with shared scale for all periods
NSR_ship_km_map_1A <- basemap_with_NSR +
  geom_line(data = polar_NSR_ship_km_period %>% filter(ship_type == "1A"),
            aes(x = long, y = lat, group = group1, color = intensity),
            lwd = 1.75) +
  geom_line(data = polar_NSR_ship_km_period %>% filter(ship_type == "1A"),
            aes(x = long, y = lat, group = group2, color = intensity),
            lwd = 1.75) +
  viridis::scale_color_viridis(guide_legend(title = "Traffic\n(ships/year)"),
                               direction = -1, limits = c(0, 520)) +
  facet_grid(period~scen) +
  map_theme +
  theme(plot.margin = unit(c(-1, 5.5, 0, 5.5), "pt"))

ggsave(paste0(fig_dir, "/traffic_map_1A.png"), NSR_ship_km_map_1A,
       width = 7, height = 7.5, units = "in")

# make map for 1AS results with shared scale for all periods
NSR_ship_km_map_1AS <- basemap_with_NSR +
  geom_line(data = polar_NSR_ship_km_period %>% filter(ship_type == "1AS"),
            aes(x = long, y = lat, group = group1, color = intensity),
            lwd = 1.75) +
  geom_line(data = polar_NSR_ship_km_period %>% filter(ship_type == "1AS"),
            aes(x = long, y = lat, group = group2, color = intensity),
            lwd = 1.75) +
  viridis::scale_color_viridis(guide_legend(title = "Traffic\n(ships/year)"),
                               direction = -1, limits = c(0, 520)) +
  facet_grid(period~scen) +
  map_theme +
  theme(plot.margin = unit(c(-1, 5.5, 0, 5.5), "pt"))

ggsave(paste0(fig_dir, "/traffic_map_1AS.png"), NSR_ship_km_map_1AS,
       width = 7, height = 7.5, units = "in")


#...............................................................................
## Figure S11: oil production map ==============================================
#...............................................................................

# read in oil production by AU
oil_prod_AU <- rbind(read_csv("inputs/oil_production/OilProdn_Ref.csv"),
                  read_csv("inputs/oil_production/OilProdn_Policy.csv")) %>%
  filter(grepl("_e", Scenario)) %>%
  separate(Scenario, into = c("scen", "rcp", "ens"), sep = "_")

# calculate cumulative oil production (ensemble average) within 25-year periods
oil_prod_AU_cumulative <- oil_prod_AU %>%
  filter(grepl("rus", AU),
         year > 2025,
         # only keep ref 8.5 and pol 2.6
         scen == "Ref" | rcp == "2p6") %>%
  # take ensemble averages
  group_by(AU, scen, rcp, year) %>%
  summarize(value = mean(value)) %>%
  ungroup() %>%
  # group into periods and calculate cumulative production
  mutate(period = case_when(year <= 2050 ~ "2025-2050",
                            year <= 2075 ~ "2050-2075",
                            T ~ "2075-2100")) %>%
  group_by(AU, scen, rcp, period) %>%
  # also convert to MTOE
  summarize(value = sum(value)*5*Conv_EJ_MTOE) %>%
  ungroup()

# rename scenarios and fill in NAs/ replace 0s with NAs
oil_prod_AU_cumulative_scen <- oil_prod_AU_cumulative %>%
  mutate(scen = case_when(grepl("8", rcp) ~ "BAU|8.5",
                          scen == "Ref" ~ "BAU|2.6",
                          T ~ "LowC|2.6")) %>%
  select(-rcp) %>%
  complete(AU, scen, period) %>%
  mutate(value = if_else(value == 0, NA_real_, value))

oil_prod_AU_cumulative_scen$scen <-
  factor(oil_prod_AU_cumulative_scen$scen,
         levels = c("BAU|8.5", "BAU|2.6", "LowC|2.6"))

# join with spatial AU data
AU_poly_polar_prod <- AU_poly_polar %>%
  left_join(oil_prod_AU_cumulative_scen, by = "AU") %>%
  select(AU, period, scen, value, geometry)


# map AU cumulative production with zero-production AUs greyed out
AU_prod_map <- basemap +
  # add AUs
  geom_sf(data = AU_poly_polar_prod, aes(fill = value)) +
  viridis::scale_fill_viridis(option = "viridis", na.value = "grey94",
                              name = "Cumulative oil\nproduction (MT)",
                              breaks = c(0, 50, 150, 250),
                              direction = -1) +
  coord_sf(crs = polar,
           xlim = c(-1000000, 3000000),
           ylim = c(-2000000, 3000000)) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "right") +
  facet_grid(period~scen)

ggsave(paste0(fig_dir, "/oil_production_map.png"), AU_prod_map,
       width = 9, height = 10, units = "in")
