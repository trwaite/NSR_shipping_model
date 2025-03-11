library(tidyverse)

out_dir <- "outputs"

fig_dir <- "figures_final"

conv_mmbtu_GJ <- 1.055
conv_ton_mmbtu_bunker <- 40.2
Conv_BBL_TOE <- 0.1363636
Conv_MMBO_EJ <- 5.712/1000
Conv_EJ_MTOE <- Conv_BBL_TOE/Conv_MMBO_EJ
conv_nm_km <- 1.852

# evaluate for only a subset of years to reduce memory usage
eval_subset_years <- F
subset_years <- seq(2020, 2100, 5)


# read in route navigability
route_nav <- read_csv(paste0(out_dir, "/NavigableEastWest_AUsubZ.csv"))

# read in oil production by subzone
oil_prod <- read_csv(paste0(out_dir,"/RussiaOilProdn_1.csv")) %>%
  group_by(year, subAU, scen, rcp) %>%
  summarize(value = mean(value)) %>%
  ungroup()


# get oil production shares by subzone, to use in weighing average costs
oil_prod_sz <- oil_prod %>%
  separate(subAU, into = c("AU", "sz"), sep = "_") %>%
  select(-AU) %>%
  mutate(sz = as.numeric(gsub("sz", "", sz))) %>%
  group_by(year, scen, rcp) %>%
  mutate(share_prod = value/sum(value)) %>%
  ungroup() %>%
  select(-value)

# read costs of all trips separated by cost type
all_costs <- read_csv(paste0(out_dir, "/TotalCosts_AU_sep.csv"))

if(eval_subset_years){
  all_costs <- all_costs %>%
    filter(year %in% subset_years)
}

# filter to 1A and subAUs that have production in a given year
all_costs_1A_prod <- all_costs %>%
  filter(ship_type == "1A") %>%
  left_join(oil_prod, by = c("year", "subAU", "scen", "rcp")) %>%
  filter(!is.na(value), value > 0) %>%
  select(-value)

all_costs_1A_prod_nav <- all_costs_1A_prod %>%
  left_join(route_nav, by = c("year", "month", "ens", "rcp", "ship_type",
                              "subAU", "dir")) %>%
  filter(navigable) %>%
  mutate(season = case_when(month %in% c("jul", "aug", "sep", "oct", "nov") ~ "summer_autumn",
                            T ~ "winter_spring"))

# average by season and sz
all_costs_1A_prod_agg <- all_costs_1A_prod_nav %>%
  mutate(season = case_when(month %in% c("jul", "aug", "sep", "oct", "nov") ~ "summer_autumn",
                            T ~ "winter_spring")) %>%
  separate(subAU, into = c("AU", "sz"), sep = "_") %>%
  group_by(year, season, rcp, ship_type, sz, dir, scen, usd_rub) %>%
  summarize(operate_cost = mean(operate_cost), fuel_cost = mean(fuel_cost),
            canal_fee = mean(canal_fee), escort_cost = mean(escort_cost),
            emiss_cost = mean(emiss_cost)) %>%
  ungroup() %>%
  pivot_longer(c(operate_cost, fuel_cost, canal_fee, escort_cost, emiss_cost),
               names_to = "type", values_to = "cost")


all_costs_1A_prod_agg$sz <- as.numeric(gsub("sz", "", all_costs_1A_prod_agg$sz))


# weigh costs by production
cost_shares_agg_weighted <- all_costs_1A_prod_agg %>%
  left_join(oil_prod_sz, by = c("year", "sz", "scen", "rcp")) %>%
  replace_na(list(share_prod = 0)) %>%
  mutate(cost = cost*share_prod) %>%
  group_by(year, season, rcp, ship_type, dir, scen, type, usd_rub) %>%
  summarize(cost = sum(cost)) %>%
  ungroup()


# plot weighted average costs for each scenario

# BAU|8.5
weighted_costs_8p5 <- cost_shares_agg_weighted %>%
  filter(rcp == "8p5", ship_type == "1A", usd_rub == "base") %>%
  mutate(cost = cost/1000000) %>%
  ggplot(aes(x = year, y = cost, fill = type)) +
  geom_area() +
  #geom_bar(stat = "identity", position = "stack") +
  facet_grid(season~dir) +
  theme_bw() +
  ylim(c(0, 2)) +
  theme(legend.title = element_blank()) +
  xlab("Year") + ylab("Cost (millions 2015$)")

ggsave(paste0(fig_dir, "/cost_breakdown_8p5.png"),
       weighted_costs_8p5, width = 6, height = 8, units = "in")

weighted_costs_2p6 <- cost_shares_agg_weighted %>%
  filter(rcp == "2p6", scen == "Ref", ship_type == "1A", usd_rub == "base") %>%
  mutate(cost = cost/1000000) %>%
  ggplot(aes(x = year, y = cost, fill = type)) +
  geom_area() +
  #geom_bar(stat = "identity", position = "stack") +
  facet_grid(season~dir) +
  theme_bw() +
  ylim(c(0,2)) +
  theme(legend.title = element_blank()) +
  xlab("Year") + ylab("Cost (millions 2015$)")

ggsave(paste0(fig_dir, "/cost_breakdown_2p6.png"), weighted_costs_2p6,
       width = 6, height = 8, units = "in")

weighted_costs_lowc_2p6 <- cost_shares_agg_weighted %>%
  filter(rcp == "2p6", scen == "Pol", ship_type == "1A", usd_rub == "base") %>%
  mutate(cost = cost/1000000) %>%
  ggplot(aes(x = year, y = cost, fill = type)) +
  geom_area() +
  #geom_bar(stat = "identity", position = "stack") +
  facet_grid(season~dir) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  xlab("Year") + ylab("Cost (millions 2015$)")

ggsave(paste0(fig_dir, "/cost_breakdown_2p6_pol.png"), weighted_costs_lowc_2p6,
       width = 6, height = 8, units = "in")

# total average costs (figure S6b)
cost_shares_total_weighted <- cost_shares_agg_weighted %>%
  group_by(year, season, rcp, ship_type, dir, scen, usd_rub) %>%
  summarize(cost = sum(cost)) %>%
  ungroup() %>%
  mutate(scenario = case_when(scen == "Pol" ~ "LowC|2.6",
                              rcp == "2p6" ~ "RCP 2.6",
                              T ~ "RCP 8.5"))

cost_shares_total_weighted$scenario <-
  factor(cost_shares_total_weighted$scenario,
         levels = c("RCP 8.5", "RCP 2.6", "LowC|2.6"))

cost_shares_total_weighted %>%
  filter(ship_type == "1A", usd_rub == "base") %>%
  ggplot(aes(x = year, y = cost/10^6, color = scenario, lty = dir)) +
  geom_line() +
  facet_grid(~season) +
  theme_bw() +
  guides(color = guide_legend(title = "Scenario"),
         lty = guide_legend(title = "Route")) +
  xlab("Year") + ylab("Cost per shipment (millions 2015$)")

ggsave(paste0(fig_dir, "/cost_totals.png"),
       width = 8, height = 4, units = "in")