
#' Import Demeter csv output
#'
#' Read in the csv input for Demeter land allocation.
#'
#' @param input A data frame of longitude, latitude, and some variable that needs to be expanded to the full NCDF grid.
#' @param variable A string of the variable name in the input data frame that needs to be expanded before we can grid it.
#' @return A data frame of the Demeter output.
#' @importFrom dplyr %>%
#' @export
generate_FullGrid <- function(input, variable){

  # First check the input data frame.
  assertthat::assert_that(length(variable) == 1 && is.character(variable))
  assertthat::assert_that(variable != 'value') # TODO a possible enhancement would be to make this possible.
  assertthat::assert_that(all(c('longitude', 'latitude', variable) %in% names(input)))

  input$value       <- input[[variable]]
  input[[variable]] <- variable

  tibble::as_tibble(demeterNCDF::FullGrid) %>%
    dplyr::full_join(tibble::as_tibble(input[ , names(input) %in% c('longitude', 'latitude', 'value')]),
                                       by = c('latitude', 'longitude')) %>%
    dplyr::arrange(latitude, longitude) ->
    out

  # Make sure no extra coordinate values were added to the expanded gird.
  assertthat::assert_that(nrow(out) == nrow(demeterNCDF::FullGrid))

  out

}

generate_nc <- function(dataFrame_list, years, var_name, var_units, nc_name){

  # Check the inpus
  assertthat::assert_that(is.list(dataFrame_list))
  assertthat::assert_that(length(dataFrame_list) == length(years))

  dataFrame_rows  <- unique(sapply(dataFrame_list, nrow))
  dataFarme_names <- unlist(unique(lapply(dataFrame_list, names)))

  assertthat::assert_that(dataFrame_rows == nrow(demeterNCDF::FullGrid))
  assertthat::assert_that(all(c('longitude', 'latitude', 'value') %in% dataFarme_names))

  lon_dim  <- ncdf4::ncdim_def('lon', 'degrees_east', unique(as.double(demeterNCDF::FullGrid$longitude)))
  lat_dim  <- ncdf4::ncdim_def('lat', 'degrees_north', unique(as.double(demeterNCDF::FullGrid$latitude)))
  time_dim <- ncdf4::ncdim_def('time', 'year', as.double(years))
  def      <- ncdf4::ncvar_def('fake', 'deg C', list(lon_dim, lat_dim, time_dim), NA)

  ncout <- ncdf4::nc_create(nc_name, def)

  for (i in 1:length(years)) {

    ncdf4::ncvar_put(ncout, def, dataFrame_list[[i]][['value']],
                     start = c(1, 1, i),
                     count = c(-1, -1, 1))
  }

  ncdf4::ncatt_put(ncout,"lon","axis","X")
  ncdf4::ncatt_put(ncout,"lat","axis","Y")
  ncdf4::ncatt_put(ncout,"time","axis","T")

  ncdf4::nc_close(ncout)

  # Return the nc file name
  nc_name

}





