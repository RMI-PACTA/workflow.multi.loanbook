#' stop_if_not_inherits
#'
#' @param x an object to be checked
#' @param cls a string defining the expected object class
#'
#' @return `NULL` invisibly or an error

stop_if_not_inherits <- function(x, cls) {
  if (isFALSE(inherits(x, cls))) {
    arg <- deparse(substitute(x))
    cli::cli_abort(
      message = paste0(
        "x" = "Argument {.arg {arg}} must inherit class {.cls {cls}}, ",
        "not {.cls {class(x)}}."
      ),
      call = rlang::caller_env()
    )
  }
}


#' stop_if_not_length
#'
#' @param x an object to be checked
#' @param len an integer defining the expected length of the object
#'
#' @return `NULL` invisibly or an error

stop_if_not_length <- function(x, len) {
  if (length(x) != len) {
    arg <- deparse(substitute(x))
    cli::cli_abort(
      message = paste0(
        "x" = "Argument {.arg {arg}} must be of length {.strong {len}}, ",
        " not {.strong {length(x)}}."
      ),
      call = rlang::caller_env()
    )
  }
}


#' stop_if_dir_not_found
#'
#' @param path a string defining the path to a directory
#' @param desc a string describing the type of directory to be added to the
#'   error msg
#'
#' @return `NULL` invisibly or an error

stop_if_dir_not_found <- function(path, desc = NULL) {
  if (isFALSE(dir.exists(path))) {
    if (is.null(desc)) {
      msg <- "Directory not found at path:"
    } else {
      msg <- "{desc} directory not found at path:"
    }
    cli::cli_abort(
      message = c(
        "x" = msg,
        " " = "{.path {path}}",
        "i" = "Check the path set in your {.file config.yml}."
      ),
      call = rlang::caller_env()
    )
  }
}


#' stop_if_file_not_found
#'
#' @param path a string defining the path to a file
#' @param desc a string describing the type of file to be added to the error msg
#'
#' @return `NULL` invisibly or an error

stop_if_file_not_found <- function(path, desc = NULL) {
  if (isFALSE(file.exists(path))) {
    if (is.null(desc)) {
      msg <- "File not found at path:"
    } else {
      msg <- "{desc} file not found at path:"
    }
    cli::cli_abort(
      message = c(
        "x" = msg,
        " " = "{.file {path}}",
        "i" = "Check the path and filename set in your {.file config.yml}."
      ),
      call = rlang::caller_env()
    )
  }
}


#' stop_if_sheet_not_found
#'
#' @param sheet a string defining the name of a sheet
#' @param path a string defining the path to a XLS/X file
#'
#' @return `NULL` invisibly or an error

stop_if_sheet_not_found <- function(sheet, path) {
  if (isFALSE(sheet %in% readxl::excel_sheets(path))) {
    cli::cli_abort(
      message = c(
        "x" = "Sheet {.val {sheet}} is not found in file:",
        " " = "{.file {path}}",
        "i" = "Check the sheet name set in your {.file config.yml}."
      ),
      call = rlang::caller_env()
    )
  }
}


#' stop_if_not_expected_columns
#'
#' @param data a data frame to be checked
#' @param cols a vector of expected column names
#' @param desc a string describing the type of data to be added to the error msg
#'
#' @return `NULL` invisibly or an error

stop_if_not_expected_columns <- function(data, cols, desc = NULL) {
  if (isFALSE(all(cols %in% names(data)))) {
    if (is.null(desc)) {
      msg <- "Data does not contain all of the expected columns."
    } else {
      msg <- "{desc} data does not contain all of the expected columns."
    }
    missing_cols <- setdiff(cols, names(data))
    cli::cli_abort(
      message = c(
        "x" = msg,
        "i" = "missing columns: {.var {missing_cols}}."
      ),
      call = rlang::caller_env()
    )
  }
}
