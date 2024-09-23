#' Prioritise and diagnose the loan book data sets used in the PACTA for Supervisors analysis
#'
#' @description
#' `prioritise_and_diagnose()` runs the necessary steps to prioritise the matched
#' loan books and diagnose both the match success rate and the coverage of the
#' real economy assets by the matched loan books.
#' Parameters for all steps are read from a `config.yml` file. The function is
#' called for its side effects and writes the prepared and diagnosed data sets
#' to the directory `output/prioritise_and_diagnose`, where `output` is the
#' output directory  specified in the `config.yml`.
#'
#' `prioritise_and_diagnose()` and `prioritize_and_diagnose()` are synonyms.
#'
#' @param config either a path to a config.yml file or a list of parameters
#'
#' @return NULL
#' @export
#'
#' @examples
#' # TODO
prioritise_and_diagnose <- function(config) {
  run_match_prioritize(config)
  run_calculate_match_success_rate(config)
  run_calculate_loanbook_coverage(config)
}

#' @rdname prioritise_and_diagnose
#' @export
prioritize_and_diagnose <- prioritise_and_diagnose
