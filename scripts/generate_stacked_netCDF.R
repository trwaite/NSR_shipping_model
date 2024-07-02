# ..............................................................................
# load libraries ---------------------------------------------------------------
# ..............................................................................

library(tidyverse)
library(sf)
library(terra)

# ..............................................................................
# set directories --------------------------------------------------------------
# ..............................................................................

# working directory
wd <- getwd()

# input data (subzone coordinates/ emissions) directory
input_dir <- paste0(wd, "/data")

# output file directory
output_dir <- paste0(wd, '/stacked_outputs')

if(!exists(output_dir)){
  dir.create(output_dir)
}

# ..............................................................................
# calculate shares of each sz crossing each grid cell --------------------------
# ..............................................................................

# read in subzone coordinates
sz_coords <- read_csv(paste0(input_dir, "/subzone_coordinates.csv")) %>%
  pivot_longer(-subzone, names_to = "var", values_to = "value") %>%
  separate(var, into = c("latlong", "order"),sep = "_") %>%
  pivot_wider(names_from = "latlong", values_from = "value") %>%
  select(subzone, lat, long)

# create line objects from subzones
sz_lines <- sz_coords %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = "WGS84") %>%
  group_by(subzone) %>%
  dplyr::summarise(do_union = FALSE) %>%
  sf::st_cast("LINESTRING")


# make a global grid
grid_global <- sf::st_make_grid(sf::st_polygon(list(rbind(c(-180, 90), c(180, 90),
                                                          c(180, -90), c(-180, -90),
                                                          c(-180, 90))),
                                               sf::st_crs),
                                cellsize = c(0.5, 0.5))

# convert grid to sf object and set crs to match the subzone lines
grid_sf_global <- sf::st_sf(grid_global)
sf::st_crs(grid_sf_global) <- sf::st_crs(sz_lines)

# add ID columns to global grid to keep track of order (order is row by row
# and ascending lat/long values)
grid_sf_global$grid_ID <- seq(1, 259200, 1)

# find which global grid cells intersect with subzones
grid_intersect <- sf::st_intersects(sz_lines,
                                    grid_sf_global, sparse = F)

# extract only the intersecting grid cells from the global grid
# otherwise, st_intersection step below will be very slow
sub_grid <- grid_sf_global[which(colSums(grid_intersect) > 0),]

# set crs of the subset grid
sf::st_crs(sub_grid) <- sf::st_crs(sz_lines)

# generate intersections between subzones and the subset grid
intersections <- sf::st_intersection(sz_lines, sub_grid)

# calculate lengths of intersecting segments
intersection_lengths <- intersections %>%
  mutate(length = sf::st_length(.))

# calculate shares of total subzone length within each grid cell
intersection_shares <- as_tibble(intersection_lengths %>%
                                   group_by(subzone) %>%
                                   mutate(share = as.numeric(length/sum(length))) %>%
                                   ungroup() %>%
                                   sf::st_drop_geometry() %>%
                                   select(subzone, grid_ID, share)) %>%
  filter(share > 0)

# ..............................................................................
# calculate total emissions per grid cell and convert to rasters ---------------
# ..............................................................................

# read in emissions by subzone data
sz_emiss <- read_csv(paste0(input_dir, "/subzone_emissions.csv"))

# combine scenario and rcp columns to simplify
sz_emiss <- sz_emiss %>% mutate(scenario = paste0(rcp, "_", scen),
                                gas = "CO2") %>%
  select(-c(rcp, scen))

# initialize list to save rasters
netCDF_list <- list()

# LOOP THROUGH EVERY COMBINATION OF GAS, SHIP TYPE, SCENARIO, AND YEAR
for (gas in unique(sz_emiss$gas)){
  for (st in unique(sz_emiss$ship_type)){
    for (scen in unique(sz_emiss$scenario)) {
      for (yr in unique(sz_emiss$year)){

        ## calculate total emissions per grid cell =============================

        # get emissions data
        sz_emiss_subset <- sz_emiss %>%
          # filter to current ship type, scenario, and year
          filter(ship_type == st, scenario == scen, year == yr) %>%
          # add up East and West emissions
          group_by(ens, subzone) %>%
          summarize(emiss = sum(emiss)) %>%
          ungroup() %>%
          # average over ensembles
          group_by(subzone) %>%
          summarize(emiss = mean(emiss)) %>%
          ungroup()

        # join emissions data with sz/grid intersection shares
        sz_emiss_grid <- intersection_shares %>%
          filter(subzone != 0) %>%
          left_join(sz_emiss_subset, by = "subzone") %>%
          mutate(emiss = emiss*share) %>%
          # add up emissions per grid cell
          group_by(grid_ID) %>%
          summarize(emiss = sum(emiss))

        # fill in empty cells to make a global grid
        all_grid_cells <- tibble(grid_ID = seq(1, 259200, 1))

        sz_emiss_grid_global <- sz_emiss_grid %>%
          full_join(all_grid_cells, by = "grid_ID")


        ## generate emissions matrix ===========================================

        # arrange data by grid_ID to prepare for filling the matrix
        emiss_data_ordered <- arrange(sz_emiss_grid_global, grid_ID)

        # convert full global grid to matrix
        # taking transpose because matrix fills in column by column
        # but our data is in the order of row by row
        global_emiss_matrix <- t(matrix(data = emiss_data_ordered$emiss,
                                        nrow = 720, ncol = 360))

        # reverse the order of the rows since terra::rast() expects the opposite
        # order of latitudes (descending rather than ascending)
        global_emiss_matrix_rev <- global_emiss_matrix[nrow(global_emiss_matrix):1,]


        ## convert matrix to raster ============================================

        # make a combined scenario name for list indexing
        agg_scen <- paste(scen, st, sep = "_")

        # Convert matrix to Spatial Raster Object
        global_emiss_matrix_rev2 <- global_emiss_matrix_rev
        # convert 0 to NA
        global_emiss_matrix_rev2[global_emiss_matrix_rev2 == 0] <- NA
        emissions_rast <- terra::rast(global_emiss_matrix_rev2)

        # set lat/lon extent
        terra::ext(emissions_rast) <- c(-180, 180, -90, 90)

        # Set Time
        time_set <- as.Date(paste0('1/1/', yr), '%m/%d/%Y')
        terra::time(emissions_rast) <- time_set

        ## add raster to list ==================================================

        # list is indexed by scenario, gas, and year
        netCDF_list[[agg_scen]][[gas]][[as.character(yr)]] <- emissions_rast

      } # end year
    } # end scenario
  } # end ship_type
} # end gas



# ..............................................................................
# Stack rasters by year and save as netCDF files -------------------------------
# ..............................................................................

# Loop through each stack, stack by year, and save as a new netCDF
for (scenario in names(netCDF_list)) {
  for(gas in names(netCDF_list[[scenario]])){

    # stack the netCDF files into an array
    stack <- simplify2array(netCDF_list[[scenario]][[gas]])

    # write stacked matrices to raster
    stacked_rast <- terra::rast(stack)

    # Write to output file
    output_file <- paste0(gas, "_", scenario, "_stacked.nc")
    writeCDF(stacked_rast,
             file.path(output_dir, output_file),
             varname = gas,
             longname = paste0("NSR Shipping ", gas),
             unit = "kt",
             overwrite = TRUE)
  }
}

