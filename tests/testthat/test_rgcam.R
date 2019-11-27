context('Test rgcam processing functions')


# Import the rgcam data output.
load(here::here('tests', 'testthat', "gcam.dat"))

testthat::test_that('process_AgProductionTech', {

  # Expected values for root tuber rfd is going
  # to be 2 x 10^9 since the demeter category is
  # the aggregate of the high and low fertilizer.
  prjdata$BAU$AgProduction_Tech$value <- 1
  process_AgProductionTech(prjdata = prjdata) %>%
    dplyr::filter(demeter == 'root_tuber_rfd') %>%
    na.omit() %>%
    dplyr::pull(production) %>%
    unique() ->
    out

  testthat::expect_equal(out, 2 * 1e9)

})

testthat::test_that('process_LandAllocation', {

  # Expected values for root tuber rfd is going
  # to be 2 x 10^3 since the demeter category is
  # the aggregate of the high and low fertilizer.
  prjdata$BAU$LandAllocation$value <- 1
  process_LandAllocation(prjdata = prjdata) %>%
    dplyr::filter(demeter == 'root_tuber_rfd') %>%
    na.omit() %>%
    dplyr::pull(area) %>%
    unique() ->
    out

  testthat::expect_equal(out, 2 * 1e3)
})

testthat::test_that('calucalte_GridCellYeild',{

  # Check to make sure the function works.
  production <- process_AgProductionTech(prjdata = prjdata)
  area       <- process_LandAllocation(prjdata = prjdata)

  production <- dplyr::filter(production, basin_id == 13)
  area       <- dplyr::filter(area, basin_id == 13)

  out <- calucalte_GridCellYeild(production = production, land_area = area)
  testthat::expect_true(!is.null(out))

  # Are the yeild values returned as expected?
  production$production <- 100
  area$area             <- 10
  out <- calucalte_GridCellYeild(production = production, land_area = area)
  testthat::expect_true(all(unique(out$biomass_grass_irr) %in% c(10, NA)))

})

testthat::test_that('generate_AgYield_grid', {

  # Make sure an error is thrown.
  fake <- prjdata
  fake$BAU$LandAllocation <- NULL
  testthat::expect_error(generate_AgYield_grid(prjdata = fake), regexp = 'Missing required query output')

})
