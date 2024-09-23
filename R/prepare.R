#' Prepare input data sets for PACTA for Supervisors analysis
#'
#' @description
#' `prepare()` runs the necessary steps to prepare the input data sets for the
#' PACTA for Supervisors analysis. Specifically it prepares the abcd_final data
#' set removing inactive companies if desired. And it allows preparing the ratios
#' by which the exposures to counterparties are split along the sectors.
#' Parameters for both steps are read from a `config.yml` file. The function is
#' called for its side effects and writes the prepared data sets to the directory
#' `output/prepare`, where `output` is the output directory specified in the
#' `config.yml`.
#'
#' @param config either a path to a config.yml file or a list of parameters
#'
#' @return NULL
#' @export
#'
#' @examples
#' # TODO
prepare <- function(config) {
  prepare_abcd(config)
  prepare_sector_split(config)
}
