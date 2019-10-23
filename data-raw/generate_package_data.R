## This script generates the full lat and lon grid for the netcdf output from
## example example Demeter output. This script should be run from the
## data-raw directory. However the landcover csv file is too large to be
## committed to the repo.

# Import the Demeter data
fraction_data <-importDemeter('./landcover_2040_fraction.csv')

# Select the unique lat and lon values.
lat <- unique(fraction_data$latitude)
lon <- unique(fraction_data$longitude)

# Expand the grid and arrrange so that the longitude increases the
# fastest.
full_lat_lon <- expand.grid(latitude = lat, longitude = lon)
FullGrid     <- dplyr::arrange(full_lat_lon, longitude, latitude)

# Now save a copy of the land allocation name.
LandAllocation <- names(fraction_data)
LandAllocation <- LandAllocation[!LandAllocation %in% c('latitude', 'longitude',
                                                        'basin_id', 'region_id')]

# Save data
usethis::use_data(FullGrid, overwrite = TRUE)
usethis::use_data(LandAllocation, overwrite = TRUE)
