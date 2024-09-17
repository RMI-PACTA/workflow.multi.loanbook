#' Validate that a data frame contains expected columns
#'
#' Validate that all expected columns for an operation are given in a data frame.
#'
#' @param data data frame that is to be validated
#' @param expected_columns Character vector listing the expected columns
#'
#' @return NULL
#' @export
validate_data_has_expected_cols <- function(data,
                                            expected_columns) {
  stopifnot(rlang::is_named(data))
  stopifnot(is.character(expected_columns))

  data_has_expected_columns <-
    all(expected_columns %in% colnames(data))

  if (!data_has_expected_columns) {
    affected_cols <- glue::glue_collapse(sort(setdiff(expected_columns, names(data))), sep = ", ")
    rlang::abort(c(
      "Must include expected columns in input data set.",
      x = glue::glue("Missing columns: {affected_cols}."),
      i = "Please check that data has expected columns."
    ))
  }
  invisible()
}


beautify_scenario_label <- function(label) {
  out <- toupper(label)
  out <- r2dii.plot::to_title(out)
  out
}


#' Check if a named object contains expected names
#'
#' Based on fgeo.tool::abort_if_missing_names()
#'
#' @param x A named object.
#' @param expected_names String; expected names of `x`.
#'
#' @return Invisible `x`, or an error with informative message.
#'
#' @examples
#' x <- c(a = 1)
#' abort_if_missing_names(x, "a")
#' try(abort_if_missing_names(x, "bad"))
#'
#' @noRd

abort_if_missing_names <- function(data, expected_names) {
  stopifnot(rlang::is_named(data))
  stopifnot(is.character(expected_names))

  if (!all(unique(expected_names) %in% names(data))) {
    missing_names <- sort(setdiff(expected_names, names(data)))
    rlang::abort(
      c(
        "`data` must have all the expected names.",
        x = glue::glue("Missing names: {toString(missing_names)}.")
      ),
      class = "missing_names"
    )
  }

  invisible(data)
}


abort_if_unknown_values <- function(value, data, column) {
  if (is.null(value)) {
    return(invisible(value))
  }

  .value <- deparse1(substitute(value))
  .data <- deparse1(substitute(data))

  valid <- unique(data[[column]])
  if (!all(value %in% valid)) {
    msg <- c(
      glue::glue("Each value of `{.value}` must be one of these:\n{toString(valid)}."),
      x = glue::glue("You passed: {toString(value)}."),
      i = glue::glue("Do you need to see valid values in this dataset?:\n{.data}")
    )
    rlang::abort(msg, class = "unknown_value")
  }

  invisible(value)
}
