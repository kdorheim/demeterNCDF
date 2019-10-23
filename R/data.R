#' The full grid of lat and lon values expected for the netcdf
#'
#' This dataset contains all of the combinations of the lat and
#' lon coordinates that are expected for the netcdf.
#'
#' @section Notes:
#'
#' Demeter only returns data that covers land but in order to convert
#' a data frame to a netcdf we need all of the possible combinations of
#' the lat and lon coordinates. This constains the full grid of expected
#' lat and lon values.
#'
#' @format Data frame with 2 columns
#' \describe{
#' \item{latitude}{The latitdue values.}
#' \item{longitude}{The longitude values.}
#' }
#' @family DemeterData
'FullGrid'

#' A name of the different GCAM land cover types
#'
#' This vector contains all of the names of the GCAM land cover types downscaled
#' by the Demeter.
#'
#' @format vector of characters
#' @family DemeterData
'FullGrid'
