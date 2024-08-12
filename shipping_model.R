library(tidyverse)

print("STARTING...")

# constants
CostScale <- 1000 # convert from dollars to thousands of dollars
beta_1 <- -0.5 # logit choice function parameter
delta <- 0.000001 # small pertubation for calibration of fuel consumption
Co2emissnFactor <- 3179 # kg CO2 per tonne of fuel consumed

# conversions
Conv_OilBBL_GJ <- 5.712
Conv_BBL_TOE <- 0.1363636
Conv_MMBO_EJ <- 5.712/1000
Conv_EJ_MTOE <- Conv_BBL_TOE/Conv_MMBO_EJ
conv_C_CO2 <- 44/12
Conv_m_cm <- 100
conv_nm_km <- 1.852 # nautical miles to km
conv_rub_usd_2014 <- 0.02649 #https://www.exchange-rates.org/exchange-rate-history/rub-usd-2014-12-19
conv_kg_kt <- 10^-6


# variables
months <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep",
            "oct", "nov", "dec")
ship_types <- c("1A", "1AS")
ensembles <- c("ens1", "ens2", "ens3", "ens4", "ens5", "ens6", "ens7", "ens8",
               "ens9", "ens10", "ens11", "ens12", "ens13")
RCPs <- c("2p6", "8p5")
SIT_years <- seq(from = 2015, to = 2100, by = 1)
subzones <- paste0("sz", 1:36)
RusAUs <- c("rus1", "rus2", "rus3", "rus4", "rus6", "rus9", "rus10", "rus12",
            "rus13", "rus15", "rus17", "rus18", "rus19", "rus21", "rus22")

if(!dir.exists("outputs")){
  dir.create("outputs")
}

if(!dir.exists("figures")){
  dir.create("figures")
}

eval_single_year <- F
single_year <- 2020

#...............................................................................
# read in inputs ---------------------------------------------------------------
#...............................................................................

## Mappings ====================================================================
ZoneMap <- read_csv("inputs/mappings/ZoneMap.csv") %>%
  pivot_longer(-section, names_to = "zone", values_to = "value") %>%
  drop_na() %>%
  select(-value)

MapSubzoneNSRsecs <- read_csv("inputs/mappings/MapSubzoneNSRsecs.csv")

Seasons <- read_csv("inputs/mappings/Seasons.csv") %>%
  pivot_longer(-month, names_to = "season", values_to = "value") %>%
  drop_na() %>% select(-value)

MapProjYrs <- read_csv("inputs/mappings/MapProjYrs.csv")

MapGCAMscenEnsmbl <- read_csv("inputs/mappings/MapGCAMscenEnsmbl.csv")

MapSITdataEnsRCP <- read_csv("inputs/mappings/MapSITdataEnsRCP.csv")

# 1 means subzone is East of the subAU, 2 means subzone is West of the subAU
subAUSubZoneMap <- read_csv("inputs/mappings/subAUSubZoneMap.csv")

## Costs =======================================================================

# price inflator/deflator
price_index <- read_csv("inputs/costs/price_conversions.csv",
                              skip = 1)
price_conversion <- price_index$`price deflator`
names(price_conversion) <- price_index$year

# ice breaker fees
IceBreakCosts_v2 <- read_csv("inputs/costs/IceBreakCosts_v2.csv")

# fuel consumption/ cost parameters
FuelCostParams_pre <- read_csv("inputs/costs/FuelCostParams.csv") %>%
  pivot_longer(-c(param, units), names_to = "ship_type", values_to = "value") %>%
  filter(ship_type %in% ship_types)

OperatingCosts <- read_csv("inputs/costs/OperatingCosts.csv") %>%
  pivot_longer(cols = -units, names_to = "route", values_to = "value")

# suez canal fee
CanalFee <- read_csv("inputs/costs/CanalFee.csv") %>%
  pivot_longer(cols = everything(), names_to = "route", values_to = "value")

# carbon prices from GCAM 2p6 policy scenario (Russia)
CarbonPrice <- read_csv("inputs/costs/carbon_prices_2p6.csv") %>%
  pivot_longer(-c(scenario, Units), names_to = "year", values_to = "cPrice") %>%
  mutate(year = as.numeric(year))

## Ship specs ==================================================================

# speed function parameters
SpeedFcnParams1 <- read_csv("inputs/ship_specs/SpeedFcnParams1.csv") %>%
  filter(ship_type %in% ship_types)

# ship specs (maximum speed, load capacity, SIT thresholds)
ShipSpecs <- read_csv("inputs/ship_specs/ShipSpecs.csv") %>%
  pivot_longer(-c(param, units), names_to = "ship_type", values_to = "value") %>%
  filter(ship_type %in% ship_types)

## distances ===================================================================
DistOutsideNSR <- read_csv("inputs/distances/DistOutsideNSR.csv")

SubZoneDist <- read_csv("inputs/distances/SubZoneDist.csv") %>%
  rename(subzone = sz)

## Oil production ==============================================================
OilProdn_Policy <- read_csv("inputs/oil_production/OilProdn_Policy.csv")
OilProdn_Ref <- read_csv("inputs/oil_production/OilProdn_Ref.csv")
subAU_oil_proportions <- read_csv("inputs/oil_production/subAU_oil_proportions.csv")

## Sea ice thickness ===========================================================
SeaIceThickESM_adj2 <- read_csv("inputs/SIT/SeaIceThickESM_adj2.csv") %>%
  pivot_longer(-c(SIT_ens, year, month, units),
               names_to = "subzone", values_to = "value")

#...............................................................................
# post-process inputs ----------------------------------------------------------
#...............................................................................

## Ship specs ==================================================================

# map speed parameters from seasons to months
SpeedFcnParams2 <- SpeedFcnParams1 %>%
  left_join(Seasons, by = "season") %>%
  select(-season)

# extract ship loading capacity from ship specs
ShipLoadDWT <- ShipSpecs %>%
  filter(param == "ShipLoadCapDWT") %>%
  select(-param)

## Costs =======================================================================

IceBreakCosts_v2_ship <- IceBreakCosts_v2 %>%
  left_join(ShipLoadDWT, by = "ship_type") %>%
  # convert from rub to usd and get total tariff per ship
  # (tariffs are given on a per tonne basis)
  mutate(fee = fee*conv_rub_usd_2014*value*Conv_BBL_TOE*price_conversion["2015"]/price_conversion["2014"],
         units = "USD_2014") %>%
  select(-units.x, -units.y)


# convert costs to thousands of 2015 dollars
FuelCostParams <- FuelCostParams_pre %>%
  mutate(value = case_when(param == "BunkerFuelCost" ~ value/CostScale*price_conversion["2015"]/price_conversion["2013"],
                           T ~ value),
         units = case_when(param == "BunkerFuelCost" ~ "thousand 2015$/ton",
                           T ~ units))

OperatingCosts$value <- OperatingCosts$value/CostScale*price_conversion["2015"]/price_conversion["2016"]
OperatingCosts$units <- "thousand 2015$"

IceBreakCosts_v2_ship$fee <- IceBreakCosts_v2_ship$fee/CostScale
IceBreakCosts_v2_ship$units <- "thousand 2015$"

CanalFee$value <- CanalFee$value/CostScale
CanalFee$units <- "thousand 2015$"

# linearly extrapolate carbon prices to annual,
# convert to thousand 2015$ per tCO2
# map to scenarios, rcps,
CarbonPrice_annual <- CarbonPrice %>%
  complete(nesting(scenario, Units), year = SIT_years) %>%
  mutate(cPrice = gcamdata::approx_fun(year, cPrice, rule = 2)*price_conversion["2015"]/price_conversion["1990"]/conv_C_CO2/CostScale,
         Units = "thousand 2015$/tCO2") %>%
  left_join(MapGCAMscenEnsmbl, by = c("scenario" = "GCAMscen")) %>%
  filter(year %in% SIT_years) %>%
  select(scen, ens, rcp, year, cPrice, Units)

# and add zeros for non-policy scenarios
CarbonPrice_annual_allScen <-
  expand_grid(year = SIT_years, rcp = RCPs, ens = ensembles, scen = "Ref",
              cPrice = 0, Units = "thousand 2015$/tCO2") %>%
  rbind(CarbonPrice_annual)



## Oil production ==============================================================

# combine scenarios
OilProdn <- rbind(OilProdn_Policy, OilProdn_Ref) %>%
  select(AU, GCAMscen = Scenario, GCAM_period = year, units, value)

# get GCAM years
projYrs <- unique(OilProdn$GCAM_period)

# get AUs
AUs <- unique(OilProdn$AU)
RusAUs <- AUs[grepl("rus", AUs)]

# convert GCAM period projections to annual projections using trapezoidal rule
# and convert to MTOW
OilProdn_1 <- OilProdn %>%
  group_by(AU, GCAMscen) %>%
  mutate(value = (value + lag(value))/2*Conv_EJ_MTOE,
         units = "mtoe") %>%
  ungroup()

OilProdn_2 <- OilProdn_1 %>%
  left_join(MapProjYrs, by = "GCAM_period") %>%
  select(AU, GCAMscen, year, units, value) %>%
  drop_na()

# filter to just the Russian AUs and map to subAUs
RussiaOilProdn <- OilProdn_2 %>%
  filter(grepl("rus", AU)) %>%
  left_join(subAU_oil_proportions, by = "AU") %>%
  mutate(value = value*proportion) %>%
  select(GCAMscen, subAU, year, value)

# map to scenarios, RCPs, and ensembles
RussiaOilProdn_1 <- RussiaOilProdn %>%
  left_join(MapGCAMscenEnsmbl, by = "GCAMscen") %>%
  drop_na() %>%
  select(-GCAMscen)

# map to months
RussiaOilProdn_Monthly <- RussiaOilProdn_1 %>%
  gcamdata::repeat_add_columns(tibble(month = months)) %>%
  # assume oil production is spread evenly among months
  mutate(value = value/12)

# calculate ensemble average yearly oil production
RussiaOilProdnTotal <- RussiaOilProdn_1 %>%
  group_by(year, ens, scen, rcp) %>%
  summarize(value = sum(value)) %>%
  ungroup()

RussiaOilProdnEnsAvg <- RussiaOilProdnTotal %>%
  group_by(year, scen, rcp) %>%
  summarize(value = mean(value)) %>%
  ungroup()

# indicator for whether there is nonzero production in a given AU and year
indicProd <- RussiaOilProdn_1 %>%
  complete(subAU, year, ens, scen, rcp) %>%
  filter(!(scen == "Pol" & rcp == "8p5")) %>%
  replace_na(list(value = 0)) %>%
  mutate(nonzero_production = value > 0) %>%
  select(-value)


## Subzone positions ===========================================================

# indicate the subzones to the east and west of each AU
AUSubZoneEastWest <- subAUSubZoneMap %>%
  mutate(East = case_when(value == 1 ~ T, T ~ F),
         West = case_when(value == 2 ~ T, T ~ F)) %>%
  select(-value) %>%
  pivot_longer(-c(sz, subAU), names_to = "dir", values_to = "traversed") %>%
  rename(subzone = sz)


## Sea ice thickness ===========================================================

# map to ensembles and RCPs
SeaIceThickESM_EnsRCP_subzone_pre <- SeaIceThickESM_adj2 %>%
  left_join(MapSITdataEnsRCP, by = "SIT_ens") %>%
  select(-SIT_ens) %>%
  filter(rcp %in% RCPs) %>%
  mutate(value = value*Conv_m_cm,
         units = "cm")

# replace NAs with 0 and fill in implicit missing values with 0
SeaIceThickESM_EnsRCP_subzone <- SeaIceThickESM_EnsRCP_subzone_pre %>%
  # add implicit missing values
  full_join(expand_grid(year = SIT_years, month = months, subzone = subzones,
                        ens = ensembles, rcp = RCPs),
            by = c('year', "month", "subzone", "ens", "rcp")) %>%
  # replace missing values with 0
  replace_na(list(value = 0))

if(eval_single_year){
  SeaIceThickESM_EnsRCP_subzone <- SeaIceThickESM_EnsRCP_subzone %>%
    filter(year == single_year)
}


## Travel outside NSR ==========================================================

# extract speed traveled outside the NSR from ship specs
# (assume this is the maximum speed since there is no sea ice)
SpeedOutsideNSR <- ShipSpecs %>%
  filter(param == "MaxSpeed") %>%
  rename(speed = value) %>%
  select(-param) %>%
  # same speed for each direction
  gcamdata::repeat_add_columns(select(DistOutsideNSR, dir))

# calculate days traveled outside the NSR
DaysTravelOutsideNSR <- DistOutsideNSR %>%
  left_join(SpeedOutsideNSR, by = "dir") %>%
  mutate(value = dist/(24*speed)) %>%
  select(dir, ship_type, days = value)

# calculate fuel consumed outside the NSR
# per day
FuelConsOutsideNSR <- FuelCostParams %>%
  filter(param %in% c("FuelConsDS", "Dspeed", "FuelConsexp")) %>%
  select(-units) %>%
  pivot_wider(names_from = param, values_from = value) %>%
  left_join(SpeedOutsideNSR, by = "ship_type") %>%
  mutate(value = FuelConsDS*(speed/Dspeed + delta)^FuelConsexp,
         units = "tons/day") %>%
  select(ship_type, dir, units, value)

# total fuel consumed outside the NSR for each direction and ship type
TotFuelConsOutsideNSR <- FuelConsOutsideNSR %>%
  left_join(DaysTravelOutsideNSR, by = c("dir", "ship_type")) %>%
  mutate(value = value*days,
         units = "tons") %>%
  select(ship_type, dir, units, value)

# calculate total fuel costs of travel outside the NSR
TotFuelCostOutsideNSR <- FuelCostParams %>%
  filter(param == "BunkerFuelCost") %>%
  rename(BunkerFuelCost = value) %>%
  left_join(TotFuelConsOutsideNSR, by = "ship_type") %>%
  mutate(value = value*BunkerFuelCost,
         units = "thousand 2015$") %>%
  select(dir, ship_type, units, value)

#...............................................................................
# Main model -------------------------------------------------------------------
#...............................................................................

## Navigability, ice breaker need, and speed ===================================

# indicate if ice breaker escort is needed in a given subzone
indicIceBreak_subz <- SeaIceThickESM_EnsRCP_subzone %>%
  rename(sit = value) %>%
  # calculate for each ship type
  gcamdata::repeat_add_columns(tibble(ship_type = ship_types)) %>%
  left_join(ShipSpecs %>%
              filter(param %in% c("MaxSIT_IBneed", "MaxSIT_NoGo")) %>%
              pivot_wider(names_from = param, values_from = value),
            by = "ship_type") %>%
  mutate(IB_needed = sit >= MaxSIT_IBneed & sit < MaxSIT_NoGo) %>%
  select(year, month, subzone, ens, rcp, ship_type, IB_needed)


# calculate maximum speeds along subzones
# (full speed, reduced speed with ice breaker, or zero speed-- not navigable)
speed_specs <- ShipSpecs %>%
  select(-units) %>%
  filter(param %in% c("MaxSIT_SpeedRedn", "MaxSpeed", "MaxSIT_NoGo",
                      "MaxSIT_IBneed", "MaxIceBreakSpeed")) %>%
  pivot_wider(names_from = "param", values_from = "value")

MaxSpeed_subz <- SeaIceThickESM_EnsRCP_subzone %>%
  rename(sit = value) %>%
  # add rows for each ship type
  gcamdata::repeat_add_columns(tibble(ship_type = ship_types)) %>%
  left_join(speed_specs, by = "ship_type") %>%
  left_join(SpeedFcnParams2, by = c("ship_type", "month")) %>%
  # calculate the maximum speeds
  mutate(max_speed = case_when(
    # if sit is above the "no go" threshold, speed is 0 (not navigable)
    sit > MaxSIT_NoGo ~ 0,
    # if an ice breaker is needed, speed is the ice breaker's maximum speed
    sit >= MaxSIT_IBneed ~ MaxIceBreakSpeed,
    # if sit is above the "speed reduction" threshold but an ice breaker
    # is not needed, apply the speed reduction function
    sit > MaxSIT_SpeedRedn ~ intercept*(sit/100)^exponent,
    # for all other cases (sit below the "speed reduction" threshold),
    # speed is the ship's maximum speed
    T ~ MaxSpeed)) %>%
  select(year, month, subzone, ens, rcp, ship_type, max_speed)

# save data on which subzones are navigable
subz_navigable <- MaxSpeed_subz %>%
  mutate(navigable = max_speed != 0) %>%
  select(year, month, ens, rcp, ship_type, subzone, navigable)


# indicate whether the east and west routes are navigable (each subzone must
# be navigable) for a given AU
# note: this step is very computationally expensive (may run out of memory)
NavigableEastWest_AUsubZ <- MaxSpeed_subz %>%
  mutate(zero_speed = max_speed == 0) %>%
  full_join(AUSubZoneEastWest %>% filter(traversed), by = "subzone") %>%
  select(-traversed) %>%
  group_by(year, month, ens, rcp, ship_type, subAU, dir) %>%
  # assign navigability if none of the subzones have a speed of 0 for a given
  # AU and direction
  summarize(navigable = sum(zero_speed)==0) %>%
  ungroup()


# calculate number of months in which East route from rus1 AU is navigable
numbMonthsTransitNSR <- NavigableEastWest_AUsubZ %>%
  filter(subAU == "rus2_sz1", dir == "East") %>%
  group_by(year, ens, rcp, ship_type) %>%
  summarize(num_months = sum(navigable)) %>%
  ungroup()


# identify cases when neither route is passable from a given subAU
# but the subAU is producing (calculate resulting amount of "stranded' oil)
stranded_oil <- NavigableEastWest_AUsubZ %>%
  pivot_wider(names_from = dir, values_from = navigable) %>%
  # filter for cases where neither route is navigable
  filter(!East, !West) %>%
  select(-c(East, West)) %>%
  inner_join(RussiaOilProdn_Monthly,
             by = c("year", "month", "ens", "rcp", "subAU"))


## Fuel consumption ============================================================

# assume speed traveled will always be maximum speed, and filter out
# subzones that aren't navigable
Speed_subz <- MaxSpeed_subz %>%
  filter(max_speed > 0) %>%
  rename(speed = max_speed)

# calculate fuel consumption per day in each subzone
FuelCons_subz <- Speed_subz %>%
  left_join(FuelCostParams %>%
              select(-units) %>%
              pivot_wider(names_from = "param", values_from = "value"),
            by = "ship_type") %>%
  mutate(fuel_cons = FuelConsDS*(speed/Dspeed + delta)^FuelConsexp) %>%
  select(year, month, ens, rcp, ship_type, subzone, fuel_cons, BunkerFuelCost)

## Emissions ===================================================================

# calculate time traveled in each navigable subzone
Days_subz <- Speed_subz %>%
  left_join(SubZoneDist, by = "subzone") %>%
  mutate(days = dist/(24*speed)) %>%
  select(-c(speed, dist, units))

# calculate emissions per subzone by fuel consumption
Emissions_subz_vessel <- FuelCons_subz %>%
  left_join(Days_subz, by = c("year", "month", "ens", "rcp", "subzone",
                              "ship_type")) %>%
  left_join(indicIceBreak_subz, by = c("year", "month", "ens", "rcp", "subzone",
                                       "ship_type")) %>%
  left_join(rename(MapSubzoneNSRsecs, subzone = sz), by = "subzone") %>%
  # if an ice breaker is needed in any subzone within a given zone, turn on
  # ice breaker indicator for all subzones in that zone
  group_by(year, month, ens, rcp, ship_type, section) %>%
  mutate(IB_needed_new = sum(IB_needed) > 0) %>%
  ungroup() %>%
  # calculate emissions and convert to thousand tonnes co2
  # (orig units are kg as CO2emissnFactor is kg/ton of fuel)
  mutate(emiss = fuel_cons*days*Co2emissnFactor,
         emiss = emiss*conv_kg_kt,
         units = "ktCO2") %>%
  select(year, month, ens, rcp, ship_type, subzone, emiss, units)

# calculate emissions from ice breakers
Emissions_subz_IB <- Days_subz %>%
  left_join(indicIceBreak_subz, by = c("year", "month", "ens", "rcp", "subzone",
                                       "ship_type")) %>%
  filter(IB_needed) %>%
  left_join(SeaIceThickESM_EnsRCP_subzone %>% rename(SIT = value),
            by = c("year", "month", "subzone", "ens", "rcp")) %>%
  mutate(SIT = SIT/100,
         fuel_cons_IB = 13.3*(.375+.625*(3/(15.479-1.914*SIT-10.541*SIT^2))),
         emiss_IB = fuel_cons_IB*days*Co2emissnFactor,
         emiss_IB = emiss_IB*conv_kg_kt) %>%
  select(year, month, ens, rcp, ship_type, subzone, emiss_IB)

# add up total emissions from vessel and ice breakers
Emissions_subz <- Emissions_subz_vessel %>%
  left_join(Emissions_subz_IB, by = c("year", "month", "ens", "rcp",
                                      "ship_type", "subzone")) %>%
  mutate(emiss = if_else(is.na(emiss_IB), emiss, emiss + emiss_IB)) %>%
  select(-emiss_IB)

# calculate non-NSR emissions per trip
Emiss_nonNSR_perTrip <- FuelConsOutsideNSR %>%
  left_join(DaysTravelOutsideNSR, by = c("dir", "ship_type")) %>%
  mutate(emiss = Co2emissnFactor*days*value*conv_kg_kt,
         units = "ktCO2") %>%
  select(dir, ship_type, emiss, units)

# calculate total emissions per trip
Emiss_total_perTrip <- Emissions_subz %>%
  left_join(filter(AUSubZoneEastWest, traversed), by = "subzone") %>%
  select(-traversed) %>%
  # sum emissions from each subzone
  group_by(year, month, ens, rcp, ship_type, subAU, dir, units) %>%
  summarize(emiss = sum(emiss)) %>%
  left_join(Emiss_nonNSR_perTrip,
            by = c("dir", "ship_type", "units")) %>%
  mutate(emiss = emiss.x + emiss.y) %>%
  select(year, month, ens, rcp, ship_type, subAU, dir, units, emiss)


## calculate costs of each route ===============================================

### fuel costs #################################################################

# calculate total fuel cost to traverse each subzone
FuelCost_subz <- FuelCons_subz %>%
  left_join(Days_subz, by = c("year", "month", "ens", "rcp", "ship_type",
                              "subzone")) %>%
  mutate(cost = fuel_cons*days*BunkerFuelCost,
         units = "thousand 2015$") %>%
  select(-c(fuel_cons, days, BunkerFuelCost))

# calculate total fuel cost to traverse whole route from each AU
FuelCost_M_AU <- FuelCost_subz %>%
  left_join(filter(AUSubZoneEastWest, traversed), by = "subzone") %>%
  select(-traversed) %>%
  group_by(year, month, ens, rcp, ship_type, subAU, dir, units) %>%
  summarize(cost = sum(cost)) %>%
  ungroup() %>%
  left_join(TotFuelCostOutsideNSR, by = c("ship_type", "dir", "units")) %>%
  mutate(cost = cost + value) %>%
  select(-value)

### emissions costs ############################################################

# use 2p6 carbon price to calculate cost of CO2 emissions (for policy scenario)
emiss_costs <- Emiss_total_perTrip %>%
  left_join(CarbonPrice_annual_allScen, by = c("ens", "rcp", "year")) %>%
  # multiplying by 1000 here because emissions are in ktCO2 but carbon prices
  # are in $/tCO2
  mutate(emiss_cost = emiss*1000*cPrice,
         units = "thousand 2015$")


### other costs ################################################################

# escort costs
# calculate number of zones for which escort is needed
# and select the relevant fee
EscortCost_AU2 <- filter(AUSubZoneEastWest, traversed) %>%
  select(-traversed) %>%
  # get just subzones needing ice breaker
  left_join(filter(indicIceBreak_subz, IB_needed), by = "subzone") %>%
  drop_na() %>%
  left_join(rename(MapSubzoneNSRsecs, subzone = sz), by = "subzone") %>%
  select(-subzone) %>%
  unique() %>%
  # for each trip, count how many zones require an icebreaker
  group_by(year, month, ens, rcp, ship_type, dir, subAU) %>%
  summarize(n_zones = sum(IB_needed)) %>%
  ungroup() %>%
  # join with ice breaker costs and apply the relevant fee
  left_join(IceBreakCosts_v2_ship, by = c("month", "ship_type", "n_zones")) %>%
  select(year, month, ens, rcp, ship_type, dir, subAU, units, escort_cost = fee)

# operating costs

# calculate days traveled on the NSR for each route from each AU
DaysRoute_totNSR <- Days_subz %>%
  left_join(filter(AUSubZoneEastWest, traversed), by = "subzone") %>%
  select(-traversed) %>%
  group_by(year, month, ens, rcp, ship_type, subAU, dir) %>%
  summarize(days = sum(days)) %>%
  ungroup()

# add non-NSR days for each route
DaysRoute_tot <- DaysRoute_totNSR %>%
  left_join(DaysTravelOutsideNSR, by = c("dir", "ship_type")) %>%
  mutate(days = days.x + days.y) %>%
  select(-c(days.x, days.y))

OperateCosts_m_AU <- DaysRoute_tot %>%
  # using rteA1 costs for both east and West routes
  # TODO: check with Siwa on whether we need to fix this
  mutate(route = "rteA1") %>%
  left_join(OperatingCosts, by = "route") %>%
  mutate(operate_cost = value*days) %>%
  select(-c(days, value, route))

# canal fee costs
CanalFeeCosts_AU <- OperateCosts_m_AU %>%
  select(year, month, ens, rcp, ship_type, dir, subAU) %>%
  filter(dir == "West") %>%
  mutate(route = "rteNA1") %>%
  left_join(CanalFee, by = "route") %>%
  rename(canal_fee = value) %>%
  select(-route)

# add up all costs (without emissions cost)
TotalCosts_AU_noC_ref <- OperateCosts_m_AU %>%
  left_join(rename(FuelCost_M_AU, fuel_cost = cost),
            by = c('year', "month", "ens", "rcp", "ship_type",
                   "subAU", "dir", 'units')) %>%
  left_join(CanalFeeCosts_AU,
            by = c('year', "month", "ens", "rcp", "ship_type",
                   "subAU", "dir", "units")) %>%
  left_join(EscortCost_AU2,
            by = c('year', "month", "ens", "rcp", "ship_type",
                   "subAU", "dir", "units")) %>%
  replace_na(list(canal_fee = 0, escort_cost = 0)) %>%
  # mutate(cost = operate_cost + canal_fee + escort_cost + fuel_cost) %>%
  # select(-c(operate_cost, canal_fee, escort_cost, fuel_cost))
  mutate(scen = "Ref")

# add in the 2p6 policy scenario (above costs don't vary by scenario, but
# emissions costs below do)
TotalCosts_AU_prop <- TotalCosts_AU_noC_ref %>%
  filter(rcp == "2p6") %>%
  mutate(scen = "Pol") %>%
  rbind(TotalCosts_AU_noC_ref) %>%
  left_join(emiss_costs,
            by = c("year", "month", "scen", "rcp", "ens",
                   "ship_type", "subAU", "dir", "units")) %>%
  mutate(cost = operate_cost + canal_fee + escort_cost + fuel_cost + emiss_cost,
         emiss_cost_prop = emiss_cost/cost) %>%
  select(-c(Units, operate_cost, canal_fee, escort_cost, fuel_cost, emiss_cost))

TotalCosts_AU <- TotalCosts_AU_prop %>%
  select(-c(emiss_cost_prop, emiss, cPrice))

# costs split up by source
TotalCosts_AU_sep <- TotalCosts_AU_noC_ref %>%
  filter(rcp == "2p6") %>%
  mutate(scen = "Pol") %>%
  rbind(TotalCosts_AU_noC_ref) %>%
  left_join(emiss_costs,
            by = c("year", "month", "scen", "rcp", "ens",
                   "ship_type", "subAU", "dir"))



## choice function =============================================================

# calculate proportion of oil shipped in each direction from a given AU
route_choice_AU <- TotalCosts_AU %>%
  # filter out routes that aren't passable
  left_join(NavigableEastWest_AUsubZ, by = c("year", "month", "ens", "rcp",
                                             "ship_type", "subAU", "dir")) %>%
  filter(navigable) %>%
  # convert cost to millions USD
  mutate(cost = cost/1000, units = "millions 2015$") %>%
  pivot_wider(names_from = dir, values_from = cost) %>%
  mutate(East_choice = case_when(
    # when one route is passable and the other is not, always choose
    # the passable one
    is.na(East) & !is.na(West) ~ 0,
    is.na(West) & !is.na(East) ~ 1,
    # when both routes are passable, apply the logit choice function
    T ~ exp(beta_1*East)/(exp(beta_1*East) + exp(beta_1*West))
    #East > West ~ 0,
    #T ~ 1
    ),
  West_choice = 1-East_choice) #%>%
  #select(-c(East,West))

# check for NaNs
if(nrow(route_choice_AU %>% filter(is.nan(West_choice)|is.nan(East_choice))) > 0){
  print("WARNING: logit choice function resulted in NaNs")
} else{
  print("LOGIT CHOICE FUNCTION OKAY")
}

# calculate total oil shipped in each direction from each AU, by month
RussiaOilProdn_ShippedMonthly <- RussiaOilProdn_Monthly %>%
  full_join(route_choice_AU %>% select(-c(East, West)),
            by = c("year", "month", "ens", "rcp", "subAU", "scen")) %>%
  # NAs occur when oil can't be shipped in either direction
  drop_na() %>%
  mutate(East = value*East_choice, West = value*West_choice) %>%
  select(-c(East_choice, West_choice, value)) %>%
  pivot_longer(c(East, West), names_to = "dir", values_to = "value")

# calculate total oil shipped in each direction from each AU, by year
RussiaOilProdn_ShippedYr <- RussiaOilProdn_ShippedMonthly %>%
  group_by(year, scen, ens, rcp, ship_type, subAU, dir) %>%
  summarize(value = sum(value)) %>%
  ungroup()

# calculate total oil shipped in each direction by year (all AUs combined)
RussiaOilProdn_TotalShippedYr <- RussiaOilProdn_ShippedYr %>%
  group_by(year, scen, ens, rcp, ship_type, dir) %>%
  summarize(value = sum(value)) %>%
  ungroup()

# calculate shares of total oil produced that are shipped in either direction
RussiaOilShipped_shares <- RussiaOilProdn_TotalShippedYr %>%
  group_by(year, scen, ens, rcp, ship_type) %>%
  summarize(shipped = sum(value)) %>%
  ungroup() %>%
  left_join(RussiaOilProdnTotal %>% rename(prod = value),
            by = c("year", "ens", "scen", "rcp")) %>%
  mutate(share = shipped/prod)


## number of trips =============================================================

NumbTripsNeeded_Month <- RussiaOilProdn_ShippedMonthly %>%
  left_join(rename(ShipLoadDWT, load = value), by = "ship_type") %>%
  mutate(n_trips = 2*round(value/(Conv_BBL_TOE*load/1000000), digits = 0)) %>%
  select(-c(load, navigable, value))

YearlyOutboundTrips <- NumbTripsNeeded_Month %>%
  group_by(year, ens, scen, rcp, ship_type, dir) %>%
  # divide by 2 for just outbound trips (don't include returns)
  summarize(value = sum(n_trips/2)) %>%
  ungroup()


## tonne-km on the NSR =========================================================

# calculate total distance traveled on the NSR when shipping in a given
# direction from a given subAU
NSR_dist_traveled_sz <- AUSubZoneEastWest %>%
  filter(traversed) %>%
  left_join(SubZoneDist, by = "subzone") %>%
  select(subAU, dir, subzone, dist, units)

NSR_dist_traveled_total <- NSR_dist_traveled_sz %>%
  group_by(subAU, dir, units) %>%
  summarize(dist = sum(dist)) %>%
  ungroup()

# multiply by amount of oil shipped from each subAU to get tonne-km
NSR_tonne_km_AU <- RussiaOilProdn_ShippedYr %>%
  left_join(NSR_dist_traveled_total, by = c("subAU", "dir")) %>%
  mutate(tonne_km = value*dist*conv_nm_km) %>%
  select(year, scen, ens, rcp, ship_type, subAU, dir, tonne_km)

NSR_tonne_km_total <- NSR_tonne_km_AU %>%
  group_by(year, scen, ens, rcp, ship_type) %>%
  summarize(tonne_km = sum(tonne_km, na.rm = T)) %>%
  ungroup()

# calculate tonne-km by subzone
NSR_tonne_km_sz <- RussiaOilProdn_ShippedYr %>%
  left_join(NSR_dist_traveled_sz, by = c("subAU", "dir")) %>%
  mutate(tonne_km = value*dist*conv_nm_km) %>%
  group_by(year, scen, ens, rcp, ship_type, dir, subzone) %>%
  summarize(tonne_km = sum(tonne_km)) %>%
  ungroup()


## ship-km by subzone ==========================================================
ship_km_sz <- NumbTripsNeeded_Month %>%
  # get number of trips per year by subAU
  group_by(year, ens, scen, rcp, ship_type, dir, subAU) %>%
  summarise(n_trips = sum(n_trips)) %>%
  ungroup() %>%
  # join with dist traveled per subzone
  left_join(NSR_dist_traveled_sz, by = c("subAU", "dir")) %>%
  # multiply distance by number of trips to get ship-km
  mutate(ship_km = n_trips*dist*conv_nm_km) %>%
  # sum over subAUs
  group_by(scen, ens, rcp, year, ship_type, dir, subzone) %>%
  summarize(n_trips = sum(n_trips), ship_km = sum(ship_km)) %>%
  ungroup()


## emissions ===================================================================


# get annual emissions by subzone
Emissions_Yr_sz <- NumbTripsNeeded_Month %>%
  filter(n_trips > 0) %>%
  left_join(AUSubZoneEastWest %>% filter(traversed), by = c("subAU", "dir")) %>%
  left_join(Emissions_subz, by = c("year", "month", "ens", "rcp", "ship_type",
                                   "subzone")) %>%
  mutate(emiss = emiss*n_trips) %>%
  # sum over AUs and months
  group_by(year, ens, rcp, scen, ship_type, dir, subzone) %>%
  summarize(emiss = sum(emiss)) %>%
  ungroup()

# total NSR emissions by year (sum over subzones)
Emissions_Yr_NSR <- Emissions_Yr_sz %>%
  group_by(year, ens, rcp, scen, ship_type, dir) %>%
  summarize(emiss = sum(emiss)) %>%
  ungroup()

# calculate non-NSR emissions per trip
Emiss_nonNSR_perTrip <- FuelConsOutsideNSR %>%
  left_join(DaysTravelOutsideNSR, by = c("dir", "ship_type")) %>%
  mutate(emiss = Co2emissnFactor*days*value/1000000, units = "ktCO2") %>%
  select(dir, ship_type, emiss, units)

# calculate total non_NSR emissions per year
Emiss_nonNSR_yr <- NumbTripsNeeded_Month %>%
  filter(n_trips > 0) %>%
  left_join(Emiss_nonNSR_perTrip, by = c("ship_type", "dir")) %>%
  mutate(emiss = emiss*n_trips) %>%
  group_by(year, ens, rcp, scen, ship_type, dir) %>%
  summarize(emiss = sum(emiss)) %>%
  ungroup()

# total NSR + non-NSR emissions by year
Emissions_Yr_total <- Emissions_Yr_NSR %>%
  rename(NSR = emiss) %>%
  left_join(Emiss_nonNSR_yr %>% rename(non_NSR = emiss),
            by = c("year", "ens", "rcp", "scen", "dir", "ship_type")) %>%
  pivot_longer(c(NSR, non_NSR), names_to = "section", values_to = "value")


#...............................................................................
# Write out outputs ------------------------------------------------------------
#...............................................................................

# oil production by year, scenario, RCP, ensemble, and subAU
write_csv(RussiaOilProdn_1, "outputs/RussiaOilProdn_subAU.csv")

# oil production by year, scenario, RCP, ensemble
write_csv(RussiaOilProdnTotal, "outputs/RussiaOilProdnTotal.csv")

# number of months whole NSR is navigable by year, RCP, ensemble
write_csv(numbMonthsTransitNSR, "outputs/numbMonthsTransitNSR.csv")

# navigability of each subzone by month, year, RCP, ensemble
write_csv(subz_navigable, "outputs/subz_navigable.csv")

# navigability of East and West routes from each AU by month, year, RCP, ensemble
write_csv(NavigableEastWest_AUsubZ, "outputs/NavigableEastWest_AUsubZ.csv")

# total oil shipped in each direction by year, scenario, RCP, ensemble
write_csv(RussiaOilProdn_TotalShippedYr, "outputs/RussiaOilProdn_TotalShippedYr.csv")

# oil that cannot be shipped in either direction
write_csv(stranded_oil, "outputs/stranded_oil.csv")

# total operating costs per trip
write_csv(TotalCosts_AU, "outputs/TotalCosts_AU.csv")

# tonne-km of oil shipped within the NSR (regardless of direction)
write_csv(NSR_tonne_km_total, "outputs/NSR_tonne_km_total.csv")

# tonne-km of oil shipped by subzone
write_csv(NSR_tonne_km_sz, "outputs/NSR_tonne_km_sz.csv")

# ship-km by subzone (including return trips)
write_csv(ship_km_sz, "outputs/ship_km_sz.csv")

# share of total oil produced that is shipped in either direction
write_csv(RussiaOilShipped_shares, "outputs/RussiaOilShipped_shares.csv")

# ensemble average number of outbound trips
write_csv(YearlyOutboundTrips, "outputs/YearlyOutboundTrips.csv")

# emissions by year, scenario, RCP, ensemble, subzone
write_csv(Emissions_Yr_sz, "outputs/Emissions_Yr_sz.csv")

# total NSR and non-NSR emissions by year, scenario, RCP, ensemble
write_csv(Emissions_Yr_total, "outputs/Emissions_Yr_total.csv")


# route choice data (proportion going each direction from a given AU)
write_csv(route_choice_AU, "outputs/route_choice_AU.csv")


# total operating costs per trip
write_csv(TotalCosts_AU, "test_costs/TotalCosts_AU.csv")

# costs per trip split up by source
write_csv(TotalCosts_AU_sep, "test_costs/TotalCosts_AU_sep.csv")

