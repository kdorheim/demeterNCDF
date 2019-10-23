context('Test the gridding functions.')

testthat::test_that('generate_FullGrid works',{

  geo_data <- data.frame(lat = c(-55.7916, -30.8749, 27.4584, -5.04162, 75.4583),
                         lon = c(-179.958, -179.958, -179.958, -179.542, -179.292),
                         Value = c(4, 5, 6, 7, 5))

  testthat::expect_error(generate_FullGrid(input = geo_data, variable = 'Value'))

  geo_data$longitude <- geo_data$lon
  geo_data$latitude  <- geo_data$lat
  geo_data <- geo_data[ , names(geo_data) %in% c('longitude', 'latitude', 'Value')]

  # Expand the fake geo data.
  expanded_grid <- generate_FullGrid(input = geo_data, variable = 'Value')

  # Check to make sure that the correct grid was expanded.
  testthat::expect_true(is.numeric(expanded_grid$value))
  testthat::expect_equal(nrow(na.omit(expanded_grid)), nrow(geo_data))

  # Make sure that it throws an error if incorrect coordinates are read in.
  geo_data <- rbind(geo_data, c(1, 1, 9))
  testthat::expect_error(generate_FullGrid(input = geo_data, variable = 'Value'))

})


# Create the fake geo data that will be used in the next two tests.
geo_data <- data.frame(lat = c(-55.7916, -30.8749, 27.4584, -5.04162, 75.4583),
                       lon = c(-179.958, -179.958, -179.958, -179.542, -179.292),
                       Value = c(4, 5, 6, 7, 5))

geo_data$longitude <- geo_data$lon
geo_data$latitude  <- geo_data$lat
geo_data <- geo_data[ , names(geo_data) %in% c('longitude', 'latitude', 'Value')]

# Expand the fake geo data.
expanded_grid <- generate_FullGrid(input = geo_data, variable = 'Value')

testthat::test_that('generate_nc works',{

  # Make the netcdf file
  nc_path <- generate_nc(dataFrame_list = list(expanded_grid), years = 2010, var_name = 'test', var_units = 'moles', nc_name = './test1.nc')

  testthat::expect_true(file.exists(nc_path))

  nc <- ncdf4::nc_open(nc_path)
  x  <- ncdf4::ncvar_get(nc, 'test')
  x_lat <- ncdf4::ncvar_get(nc, 'lat')
  x_lon <- ncdf4::ncvar_get(nc, 'lon')


  testthat::expect_true(prod(dim(x)) == nrow(FullGrid))
  testthat::expect_equal(sum(geo_data$Value == 5) , length(which(x == 5)))


  # Test to see if the values were entered correctly.
  selected_values <- geo_data[geo_data$Value == 7, ]
  x_id <- which(x == 7, arr.ind = TRUE)
  testthat::expect(all(c(selected_values - c(Value = 7, longitude = x_lon[[x_id[1]]], latitude = x_lat[[x_id[[2]]]])) == 0 ))

file.remove(nc_path)

})

testthat::test_that('generate_nc works with mulitple time slices', {

  # Let's say that we are going to use three years of data.
  data_list <- list(expanded_grid, expanded_grid, expanded_grid)
  years     <- c(2010, 2011, 2012)
  nc_path <- generate_nc(dataFrame_list = data_list, years = years, var_name = 'test', var_units = 'moles', nc_name = './test1.nc')

  nc <- ncdf4::nc_open(nc_path)
  x  <- ncdf4::ncvar_get(nc, 'test')
  x_lat  <- ncdf4::ncvar_get(nc, 'lat')
  x_lon  <- ncdf4::ncvar_get(nc, 'lon')
  x_time <- ncdf4::ncvar_get(nc, 'time')

  testthat::expect_true(all(years == x_time))
  testthat::expect_true(all(dim(x) == c(length(x_lon), length(x_lat), length(years))))

  file.remove(nc_path)
})
