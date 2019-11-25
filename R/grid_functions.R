#' Convert from a partial Demeter grid to a full grid
#'
#' Need to add this
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

  input$longitude <- round(x = input$longitude, digits = 4)
  input$latitude  <- round(x = input$latitude, digits = 4)

  tibble::as_tibble(demeterNCDF::FullGrid) %>%
    dplyr::mutate(longitude = round(longitude, digits = 4),
                  latitude = round(latitude, digits = 4)) %>%
    dplyr::full_join(tibble::as_tibble(input[ , names(input) %in% c('longitude', 'latitude', 'value')]),
                                       by = c('latitude', 'longitude')) %>%
    dplyr::arrange(latitude, longitude) ->
    out

  # Make sure no extra coordinate values were added to the expanded gird.
  assertthat::assert_that(nrow(out) == nrow(demeterNCDF::FullGrid))

  out

}

#' Wrtie data as a netcdf file.
#'
#' Using a list of data frames containing values for all of the input variables write netcdf files.
#'
#' @param dataFrame_list A list of data frames that were produced by the \code{generate_FullGrid}, each element in the data frmae list should
#' contain data for a single time slice.
#' @param years A vector of the name of the years or the time slices.
#' @param var_name A string vector of the variable name
#' @param var_units A string vector of the variable units
#' @param nc_name A string file path for the name of the file to save the netcdf at.
#' @return A netcdf saved as nc_name
#' @importFrom dplyr %>%
#' @export
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
  def      <- ncdf4::ncvar_def(var_name, var_units, list(lon_dim, lat_dim, time_dim), NA)

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





