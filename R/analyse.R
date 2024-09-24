#' Analyse the loan book data sets used in the PACTA for Supervisors analysis
#'
#' @description
#' `analyse()` runs the necessary steps to analyse the matched loan books,
#' producing both the outputs of the standard PACTA analysis and the outputs of
#' the net aggregated alignment metric, including tables and plots.
#' Parameters for all steps are read from a `config.yml` file. The function is
#' called for its side effects and writes the prepared and diagnosed data sets
#' to the directory `output/analysis`, where `output` is the
#' output directory  specified in the `config.yml`.
#'
#' `analyse()` and `analyze()` are synonyms.
#'
#' @param config either a path to a config.yml file or a list of parameters
#'
#' @return NULL
#' @export
#'
#' @examples
#' # TODO
analyse <- function(config) {
  run_pacta(config)
  run_aggregate_alignment_metric(config)
  plot_aggregate_loanbooks(config)
}

#' @rdname analyse
#' @export
analyze <- analyse
