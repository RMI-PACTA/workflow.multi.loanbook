#' Match raw input loan books with ABCD for PACTA for Supervisors analysis
#'
#' @description
#' `match_loanbooks()` runs the necessary steps to match the raw input loan books with the
#' asset based company data (ABCD) used in the PACTA for Supervisors analysis.
#' Specifically, it prepares matched loan books based on name matching or direct
#' identifiers, depending on the configuration. The output matched loan books
#' need to be manually validated for further processing.
#' Parameters the matching step are read from a `config.yml` file and follow the
#' options available in `r2dii.match::match_name`. The function is called for its
#' side effects and writes the prepared data sets to the directory `output/match`,
#' where `output` is the output directory specified in the `config.yml`.
#'
#' @param config either a path to a config.yml file or a list of parameters
#'
#' @return NULL
#' @export
#'
#' @examples
#' # TODO
match_loanbooks <- function(config) {
  config <- load_config(config)

  input_loanbooks_dir <- get_loanbook_dir(config)
  output_prepare_dir <- get_output_prepare_dir(config)
  output_matched_loanbooks_dir <- get_output_matched_loanbooks_dir(config)
  dir.create(output_matched_loanbooks_dir, recursive = TRUE, showWarnings = FALSE)

  matching_by_sector <- get_match_by_sector(config)
  matching_min_score <- get_match_min_score(config)
  matching_method <- get_match_method(config)
  matching_p <- get_match_p(config)
  matching_overwrite <- get_match_overwrite(config)
  matching_join_id <- get_match_join_id(config)

  matching_use_manual_sector_classification <- get_use_manual_sector_classification(config)
  if (matching_use_manual_sector_classification) {
    path_manual_sector_classification <- get_manual_sector_classification_path(config)
  }

  # validate config values----
  stop_if_not_length(input_loanbooks_dir, 1L)
  stop_if_not_inherits(input_loanbooks_dir, "character")
  stop_if_dir_not_found(input_loanbooks_dir, desc = "Input - loanbooks")

  stop_if_not_length(output_prepare_dir, 1L)
  stop_if_not_inherits(output_prepare_dir, "character")
  stop_if_dir_not_found(output_prepare_dir, desc = "Output - prepare ABCD")
  stop_if_file_not_found(file.path(output_prepare_dir, "abcd_final.csv"), desc = "ABCD final")

  stop_if_not_length(output_matched_loanbooks_dir, 1L)
  stop_if_not_inherits(output_matched_loanbooks_dir, "character")
  stop_if_dir_not_found(output_matched_loanbooks_dir, desc = "Output - Matched loanbooks")

  stop_if_not_length(matching_by_sector, 1L)
  stop_if_not_inherits(matching_by_sector, "logical")

  stop_if_not_length(matching_min_score, 1L)
  stop_if_not_inherits(matching_min_score, "numeric")

  stop_if_not_length(matching_method, 1L)
  stop_if_not_inherits(matching_method, "character")

  stop_if_not_length(matching_p, 1L)
  stop_if_not_inherits(matching_p, "numeric")

  # TODO: check for data.frame
  # stop_if_not_length(matching_overwrite, 1L)
  # stop_if_not_inherits(matching_overwrite, "numeric")
  #
  # TODO: check for join_object
  # stop_if_not_length(matching_join_id, 1L)
  # stop_if_not_inherits(matching_join_id, "numeric")

  stop_if_not_length(matching_use_manual_sector_classification, 1L)
  stop_if_not_inherits(matching_use_manual_sector_classification, "logical")

  # path to manual sector classification only required if boolean TRUE
  if (matching_use_manual_sector_classification) {
    stop_if_not_length(path_manual_sector_classification, 1L)
    stop_if_not_inherits(path_manual_sector_classification, "character")
    stop_if_file_not_found(path_manual_sector_classification, desc = "Manual sector classification")
  }

  # load data----

  ## load abcd----
  abcd <- readr::read_csv(
    file.path(output_prepare_dir, "abcd_final.csv"),
    col_select = dplyr::all_of(cols_abcd),
    col_types = col_types_abcd_final
  )

  ## optionally load manual classification system----
  if (matching_use_manual_sector_classification) {
    sector_classification_system <- readr::read_csv(
      file = path_manual_sector_classification,
      col_types = col_types_sector_classification,
      col_select = dplyr::all_of(col_select_sector_classification)
    )
  }

  ## load raw loan books----
  list_raw <- list.files(path = input_loanbooks_dir, pattern = "[.]csv$")
  stop_if_no_files_found(list_raw, input_loanbooks_dir, "dir_input", "raw loan book CSVs")

  raw_lbk <- readr::read_csv(
    file = file.path(input_loanbooks_dir, list_raw),
    col_types = col_types_raw,
    id = "group_id"
  ) %>%
    dplyr::mutate(group_id = tools::file_path_sans_ext(basename(.data[["group_id"]]))) %>%
    dplyr::group_split(.data[["group_id"]])

  # match and save loan books----
  cli::cli_progress_bar(
    total = length(raw_lbk),
    format = "{cli::pb_spin} Matching loanbooks {cli::pb_current}/{cli::pb_total} | ETA: {cli::pb_eta}"
  )
  for (i in seq_along(raw_lbk)) {
    group_name <- unique(raw_lbk[[i]][["group_id"]])

    ## match data----
    if (matching_use_manual_sector_classification) {
      withr::with_options(
        new = list(r2dii.match.sector_classifications = sector_classification_system),
        code = {
          getOption("r2dii.match.sector_classifications")
          matched_lbk_i <- r2dii.match::match_name(
            loanbook = raw_lbk[[i]],
            abcd = abcd,
            by_sector = matching_by_sector,
            min_score = matching_min_score,
            method = matching_method,
            p = matching_p,
            overwrite = matching_overwrite,
            join_id = matching_join_id
            # TODO: allow surfacing the other match_name args
          )
        }
      )
    } else {
      matched_lbk_i <- r2dii.match::match_name(
        loanbook = raw_lbk[[i]],
        abcd = abcd,
        by_sector = matching_by_sector,
        min_score = matching_min_score,
        method = matching_method,
        p = matching_p,
        overwrite = matching_overwrite,
        join_id = matching_join_id
      )
    }

    ## write matched data to file----
    matched_lbk_i %>%
      readr::write_csv(
        file = file.path(output_matched_loanbooks_dir, glue::glue("matched_lbk_{group_name}.csv")),
        na = ""
      )
    cli::cli_progress_update()
  }
  cli::cli_progress_done()
}
