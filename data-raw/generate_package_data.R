## This script generates the full lat and lon grid for the netcdf output from
## example example Demeter output. This script should be run from the
## data-raw directory. However the landcover csv file is too large to be
## committed to the repo.
# 0. Set Up ----------------------------------------------------------------------------------------------------
library(dplyr)
library(gcamdata)
library(rgcam)
library(tidyr)
devtools::load_all()

INPUT_DIR     <- here::here('data-raw')


# 1. Import & Formate Data -------------------------------------------------------------------------------------
#base_layer <- importDemeter(file.path(INPUT_DIR, 'gcam_reg32_basin235_modis_v6_2010_mirca_2000_5arcmin_sqdeg_wgs84_11Jul2019.csv'))
base_layer <- importDemeter(file.path(INPUT_DIR, 'landcover_2020_fraction.csv'))

iso_region <- read.csv(system.file('extdata/common/iso_GCAM_regID.csv', package = 'gcamdata'), comment.char = '#', stringsAsFactors = FALSE)
prj        <- get(load(file.path(INPUT_DIR, 'gcam.dat')))

# Import the basin names from the GCAM data system.
read.csv(system.file('extdata/water/basin_to_country_mapping.csv', package = 'gcamdata'),
         comment.char = '#', stringsAsFactors = FALSE) %>%
  select(basin_id = GCAM_basin_ID, basin = GLU_name, iso = ISO) %>%
  mutate(iso = tolower(iso)) %>%
  left_join(iso_region, by = 'iso') %>%
  select(basin_id, basin, GCAM_region_ID) ->
  basin_df


# Import the demter to gcam classification
list.files(INPUT_DIR, 'gcam_regbasin_modis_v6_type5_mirca_5arcmin_projected_alloc.csv', full.names = TRUE) %>%
  read.csv(stringsAsFactors = FALSE) ->
  gcam_demter_mapping

# 2. Generate the Full Grid -------------------------------------------------------------------------------------
# Select the unique lat and lon values.
lat <- unique(round(base_layer$latitude, 4))
lon <- unique(round(base_layer$longitude, 4))

# Expand the grid and arrrange so that the longitude increases the
# fastest.
full_lat_lon <- expand.grid(latitude = lat, longitude = lon)
FullGrid     <- dplyr::arrange(full_lat_lon, longitude, latitude)

usethis::use_data(FullGrid, overwrite = TRUE)

# 3. Save Land Allocation Information ----------------------------------------------------------------------------
# Now save a copy of the land allocation name.
LandAllocation <- names(base_layer)
LandAllocation <- LandAllocation[!LandAllocation %in% c('latitude', 'longitude',
                                                        'basin_id', 'region_id',
                                                        'target_fid')]

# 4. Basin Coordinates ----------------------------------------------------------------------------------------

base_layer %>%
  select(latitude, longitude, basin_id) %>%
  mutate(latitude = round(latitude, 4),
         longitude = round(longitude, 4)) %>%
  full_join(basin_df, by = "basin_id") %>%
  full_join(FullGrid, by = c("latitude", "longitude")) %>%
  select(-basin) %>%
  rename(gcam_region_id = GCAM_region_ID) ->
  BasinCoordinates

usethis::use_data(BasinCoordinates, overwrite = TRUE)



# 5. Land Allocation Mapping ----------------------------------------------------------------------------------------


gcam_demter_mapping %>%
  rename(gcam = category) %>%
  gather(demeter, value, -gcam)  %>%
  filter(value == 1) %>%
  select(-value) ->
  gcam_detmer_long

commodity_data <-  getQuery(prj, "Profit_Rate" )

commodity<- tibble(lc_basin_h20_fert = unique(commodity_data$commodity))

# Format a data frame of non areable land cover types.
land_types <- tibble(lc_type = c('Forest', 'Grassland', 'OtherArableLand', 'Pasture', 'RockIceDesert',
                                 'Shrubland', 'UnmanagedForest', 'UnmanagedPasture',
                                 'UrbanLand', 'Tundra'))
basins <- tibble(basin = unique(basin_df$basin))

land_types %>%
  gcamdata::repeat_add_columns(basins) %>%
  mutate(lc_basin_h20_fert = paste0(lc_type, '_', basin)) ->
  extra_land_types

# Format a data frame of areable land cover types and combine it
# with the non areable land cover data frame.
crop_types          <- tibble(lc_type =  c('Corn', 'Wheat', 'Rice', 'OilCrop', 'MiscCrop', 'FiberCrop', 'FodderGrass',
                                           'FodderHerb', 'OtherGrain', 'PalmFruit', 'Root_Tuber', 'biomass_grass',
                                           'biomass_tree', 'SugarCrop'))
h20                 <- tibble(h20 = c('IRR', 'RFD'))
fert                <- tibble(fert = c('hi', 'lo'))

crop_types %>%
  gcamdata::repeat_add_columns(basins) %>%
  gcamdata::repeat_add_columns(h20) %>%
  gcamdata::repeat_add_columns(fert) %>%
  mutate(lc_basin_h20_fert = paste0(lc_type, '_', basin, '_', h20, '_', fert)) %>%
  bind_rows(extra_land_types)  %>%
  left_join(basin_df,  by = "basin") %>%
  mutate(new_gcam = if_else(is.na(h20), lc_type, paste0(lc_type, '_', h20))) %>%
  left_join(gcam_detmer_long, by = c('new_gcam' = 'gcam')) %>%
  select(gcam_commodity = lc_basin_h20_fert, demeter, basin_id, gcam_region_id = GCAM_region_ID) ->
  LandAllocationMapping

usethis::use_data(LandAllocationMapping, overwrite = TRUE)

# 5.
