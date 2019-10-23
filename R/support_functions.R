

#' Import Demeter csv output
#'
#' Read in the csv input for Demeter land allocation.
#'
#' @param path The path to a Demeter csv file
#' @return A data frame of the Demeter output
importDemeter <- function(path){

  # Check to make sure that the file exsits
  assertthat::assert_that(file.exists(path))

  # Read in the demeter data.
  read.csv(path, comment.char = '#')

}


