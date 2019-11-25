# There are a number of specific ways that rgcam products must be processed in order to be able
# to downscale the economic data. Here are a set of helper functions that will do this processing
# in a reporducible automated manner.

#' Agricultural commodity production by technology to the Demeter relevant commodities.
#'
#' Using rgcam output calculate and a mapping file aggragate the rgcam output to Demeter
#' categories. This function will also convert to the appropriate units.
#'
#' @param prjdata A rgcam output containing resutls for the AgProduction_Tech and LandAllocation queries.
#' @return A data frame of the agricultural commodity production by technology in Demeter relevant categories
#' and units.
#' @importFrom dplyr %>%
process_AgProductionTech <- function(prjdata){

  # Query the agriculutral production by technology results.
  rgcam::getQuery(prjdata, 'AgProduction_Tech') %>%
    dplyr::filter(sector == output) %>%
    # Convert from billions of m3 of product to m3 of product.
    dplyr::mutate(value = value * 1e9,
                  Units = 'm3') %>%
  # Aggregate to the demeter output level.
    dplyr::left_join(LandAllocationMapping, by = c('commodity' = 'gcam_commodity')) %>%
    # Prevent any basin / land use areas that are someimtes classified as  from being
    # duplicated.
    dplyr::select(scenario, Units, year, demeter, basin_id, gcam_region_id, commodity, value) %>%
    dplyr::distinct() %>%
    dplyr::group_by(scenario, Units, year, demeter, basin_id, gcam_region_id) %>%
    dplyr::summarise(production = sum(value), n = n_distinct(commodity)) %>%
    dplyr::ungroup() %>%
    dplyr::select(scenario, year, demeter, basin_id, gcam_region_id, production, production_units = Units)

}

#' Land allocation technology to the Demeter relevant commodities.
#'
#' Using rgcam output calculate and a mapping file aggragate the rgcam output to Demeter
#' categories. This function will also convert to the appropriate units.
#'
#' @param prjdata A rgcam output containing resutls for the AgProduction_Tech and LandAllocation queries.
#' @return A data frame of the land allocation by technology in Demeter relevant categories
#' and units.
#' @importFrom dplyr %>%
process_LandAllocation <- function(prjdata){

  rgcam::getQuery(prjdata, 'LandAllocation') %>%
    # Convert from thousands of km2 to  km2 (this is the samme scale as the demeter output).
    dplyr::mutate(value = value * 1000,
                  Units = 'km2') %>%
    # Aggregate to the demeter output level.
    dplyr::left_join(LandAllocationMapping, by = c(`land-allocation` = 'gcam_commodity')) %>%
    dplyr::select(scenario, Units, year, demeter, basin_id, gcam_region_id, `land-allocation`, value) %>%
    dplyr::distinct() %>%
    dplyr::group_by(Units, scenario, year, demeter, basin_id, gcam_region_id) %>%
    dplyr::summarise(area = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::select(scenario, year, demeter, basin_id, gcam_region_id, area, area_units = Units)
}

#' Caluclate the demeter commoidity yeild per gri cell area.
#'
#' Using rgcam output calculate and a mapping file aggragate the rgcam output to Demeter
#' categories. This is an  internal function used by \code{generate_AgYield_grid}
#'
#' @param land_use A dataframe of production information returned by \code{process_AgProductionTech}.
#' @param land_area A dataframe of land use area returned b y \code{process_LandAllocation}.
#' @return A data frame of the yeild per grid cell for each Demeter commodity.
#' @importFrom dplyr %>%
calucalte_GridCellYeild <- function(production, land_area){

  # Use the production and land allocation results to calculate the yield per basin / region.
  production %>%
    dplyr::left_join(land_area, by = c("scenario", "year", "demeter", "basin_id", "gcam_region_id")) %>%
    na.omit() %>%
    dplyr::mutate(yield = production / area,
                  units = paste0(production_units, '/', area_units)) %>%
    dplyr::select(scenario, year, basin_id, gcam_region_id, demeter, yield, units) %>%
    tidyr::spread(demeter, yield) %>%
    # Interpolate the yeild rate over the grid cells in the basin / region. This
    # product can be used with the spatial land area information to calculate the
    # total production per of commodity per grid cell.
    dplyr::left_join(BasinCoordinates, by = c('basin_id', 'gcam_region_id'))

}


#' Calculate the grid cell yield
#'
#' Using rgcam output calculate the ag commodiity yield on the grid cell level. Because the GCAM
#' and demeter land classes are slightly different from one another the land area allocation and
#' commodity production will need to be aggregated from the GCAM to Demeter nomenclature.
#'
#' @param prjdata A rgcam output containing resutls for the AgProduction_Tech and LandAllocation queries.
#' @return A data frame of the profit rate in each grid cell for the demeter land classes.
#' @importFrom dplyr %>%
#' @export
generate_AgYield_grid <- function(prjdata){

  # It would be ideal if there was som test to make sure that this is actually a gcam data strucutre.
  # Then make sure that the required query results are in the gcam data output.
  queries <- rgcam::listQueries(prjdata)
  assertthat::assert_that(all(c('AgProduction_Tech', 'LandAllocation') %in% queries), msg = 'Missing required query output')

  # The gcamdata results will have to be aggregated to the demeter relevant categroies by basin / gcam region. Then
  # some unit conversions will need to take place to prepare to downscale the production and area from a basin / region
  # level to a grid cell scale.
  land_use_region_production <- process_AgProductionTech(prjdata)
  land_use_region_area       <- process_LandAllocation(prjdata)

  # Divide the land use region proudction by the area to calculate the yeild. Assume
  # the yeild is constant within all of the gridcells of a land use region.
  calucalte_GridCellYeild(production = land_use_region_production, land_area = land_use_region_area)

}



