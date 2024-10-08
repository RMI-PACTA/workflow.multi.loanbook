# TODO: move to `r2dii.plot` and export
plot_match_success_rate <- function(data,
                                    metric_type = c("absolute", "relative"),
                                    match_success_type = c("n", "outstanding", "credit_limit"),
                                    currency,
                                    by_group) {
  # validate inputs
  stop_if_not_inherits(data, "data.frame")
  expected_cols <- c(
    by_group,
    "sector",
    "matched",
    "match_success_type",
    "match_success_rate",
    "metric_type"
  )
  stop_if_not_expected_columns(data, expected_cols, desc = "Input")

  stop_if_not_inherits(metric_type, "character")

  stop_if_not_inherits(match_success_type, "character")

  stop_if_not_length(currency, 1L)
  stop_if_not_inherits(currency, "character")

  data <- data %>%
    dplyr::filter(.data[["sector"]] != "not in scope") %>%
    dplyr::filter(.data[["metric_type"]] == .env[["metric_type"]]) %>%
    dplyr::filter(.data[["match_success_type"]] == .env[["match_success_type"]])

  # plot design
  fill_scale <- c(
    "Matched" = "#00c082",
    "Not matched" = "#a63d57"
  )

  theme_match_success <- ggplot2::theme(
    legend.position = "top",
    legend.title = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)
  )

  # plot description

  title <- r2dii.plot::to_title(glue::glue("{metric_type} Match Success Rate"))

  if (match_success_type == "n") {
    subtitle <- "number of loans by sector"
  } else if (match_success_type == "outstanding") {
    subtitle <- "loan size outstanding by sector"
  } else {
    subtitle <- "credit limit by sector"
  }

  subtitle <- r2dii.plot::to_title(glue::glue("{subtitle} and {by_group}"))

  if (match_success_type == "n") {
    y_label <- "Match success rate (n)"
  } else if (match_success_type == "outstanding") {
    y_label <- "Match success rate: loan size outstanding"
  } else {
    y_label <- "Match success rate: credit limit"
  }

  if (metric_type == "absolute") {
    y_label <- glue::glue("{y_label} (in {currency})")
  } else {
    y_label <- glue::glue("{y_label} (share of total)")
  }

  # plot
  plot <- data %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = r2dii.plot::to_title(.data[["sector"]]),
        y = .data[["match_success_rate"]],
        fill = .data[["matched"]]
      )
    ) +
    ggplot2::geom_col(
      position = ggplot2::position_stack(reverse = TRUE)
    ) +
    ggplot2::scale_fill_manual(values = fill_scale) +
    ggplot2::labs(
      x = "Sector",
      y = y_label,
      title = title,
      subtitle = subtitle
    ) +
    ggplot2::theme_bw() +
    theme_match_success +
    ggplot2::facet_wrap(ggplot2::vars(r2dii.plot::to_title(.data[[by_group]])))

  plot
}


generate_individual_outputs <- function(data,
                                        matched_prioritized,
                                        output_directory,
                                        target_type = c("tms", "sda"),
                                        by_group,
                                        by_group_value,
                                        scenario_source,
                                        scenario,
                                        region = "global",
                                        sector,
                                        start_year,
                                        time_horizon) {

  # match input values
  target_type <- match.arg(target_type)

  target_scenario <- paste0("target_", scenario)

  # validate input values
  validate_input_args_generate_individual_outputs(
    output_directory = output_directory,
    by_group = by_group,
    by_group_value = by_group_value,
    scenario_source = scenario_source,
    target_scenario = target_scenario,
    region = region,
    sector = sector,
    start_year = start_year,
    time_horizon = time_horizon
  )

  # validate input data
  validate_input_data_generate_individual_outputs(
    data = data,
    matched_prioritized = matched_prioritized,
    target_type = target_type,
    by_group = by_group
  )

  # create sub directory for the selected institute
  dir.create(file.path(output_directory, by_group_value), showWarnings = FALSE)

  data <- data %>%
    dplyr::filter(
      .data[[by_group]] == .env[["by_group_value"]],
      .data[["scenario_source"]] == .env[["scenario_source"]],
      .data[["region"]] == .env[["region"]],
      .data[["sector"]] %in% .env[["sector"]]
    )

  matched_prioritized <- matched_prioritized %>%
    dplyr::filter(
      .data[[by_group]] == .env[["by_group_value"]],
      .data[["sector"]] %in% .env[["sector"]]
    )

  if (target_type == "tms") {
    # plot tech mix for given sector
    data_techmix <- data %>%
      dplyr::filter(
        .data[["metric"]] %in% c("projected", "corporate_economy", .env[["target_scenario"]]),
        dplyr::between(.data[["year"]], .env[["start_year"]], .env[["start_year"]] + .env[["time_horizon"]])
      ) %>%
      dplyr::mutate(
        label = dplyr::case_when(
          .data[["metric"]] == "projected" ~ "Portfolio",
          .data[["metric"]] == "corporate_economy" ~ "Corporate Economy",
          .data[["metric"]] == .env[["target_scenario"]] ~ glue::glue("{r2dii.plot::to_title(toupper(.env$scenario))} Scenario")
        )
      ) %>%
      r2dii.plot::prep_techmix(
        span_5yr = TRUE
      )

    plot_techmix <- data_techmix %>%
      r2dii.plot::plot_techmix()

    # colors in tech mix plot set to make technologies more distinguishable
    if (sector == "automotive") {
      plot_techmix <- plot_techmix +
        ggplot2::scale_fill_manual(
          values = c("#4a5e54", "#d0d7e1", "#1b324f", "#00c082"),
          labels = c("ICE", "Hybrid", "Fuelcell","Electric")
        )
    } else if (sector == "power") {
      plot_techmix <- plot_techmix +
        ggplot2::scale_fill_manual(
          values = c("#4a5e54", "#d0d7e1", "#a63d57", "#f2e06e", "#1b324f", "#00c082"),
          labels = paste(c("Coal", "Oil", "Gas", "Nuclear", "Hydro", "Renewables"), "Cap.")
        )
    }

    # export tech mix
    data_techmix %>%
      readr::write_csv(
        file = file.path(
          output_directory,
          by_group_value,
          glue::glue("data_tech_mix_{sector}_{by_group_value}.csv")
        ),
        na = ""
      )

    ggplot2::ggsave(
      filename = glue::glue("plot_tech_mix_{sector}_{by_group_value}.png"),
      plot = plot_techmix,
      device = "png",
      path = file.path(output_directory, by_group_value)
    )

    # plot trajectory charts for all available techs in given sector
    technologies_in_sector <- r2dii.data::increasing_or_decreasing %>%
      dplyr::filter(.data[["sector"]] == .env[["sector"]]) %>%
      dplyr::pull(.data[["technology"]])

    technologies_to_plot <- data %>%
      dplyr::filter(
        .data[["metric"]] == .env[["target_scenario"]],
        .data[["technology"]] %in% .env[["technologies_in_sector"]]
      ) %>%
      dplyr::distinct(.data[["technology"]]) %>%
      dplyr::arrange(.data[["technology"]]) %>%
      dplyr::pull()

    for (i in 1:length(technologies_to_plot)) {
      data_trajectory <- data %>%
        dplyr::filter(
          .data[["technology"]] == .env[["technologies_to_plot"]][i],
          dplyr::between(.data[["year"]], .env[["start_year"]], .env[["start_year"]] + .env[["time_horizon"]])
        ) %>%
        r2dii.plot::prep_trajectory(
          convert_label = r2dii.plot::recode_metric_trajectory,
          span_5yr = TRUE,
          value_col = "percentage_of_initial_production_by_scope"
        )

      if (sector == "power") {
        y_lab_trajectory <- "Normalized Capacity"
      } else {
        y_lab_trajectory <- "Normalized Production"
      }

      plot_trajectory <- data_trajectory %>%
        r2dii.plot::plot_trajectory(
          center_y = TRUE,
          perc_y_scale = TRUE
        ) +
        ggplot2::xlab("Year") +
        ggplot2::ylab(y_lab_trajectory)

      # export trajectory chart
      data_trajectory %>%
        readr::write_csv(
          file = file.path(
            output_directory,
            by_group_value,
            glue::glue("data_trajectory_{sector}_{technologies_to_plot[i]}_{by_group_value}.csv")
          ),
          na = ""
        )

      ggplot2::ggsave(
        filename = glue::glue("plot_trajectory_{sector}_{technologies_to_plot[i]}_{by_group_value}.png"),
        plot = plot_trajectory,
        device = "png",
        path = file.path(output_directory, by_group_value)
      )
    }
  } else {
    # plot convergence chart for given sector
    adjusted_scenario <- paste0("adjusted_scenario_", scenario)

    data_emission_intensity <- data %>%
      dplyr::filter(
        dplyr::between(
          .data[["year"]],
          .env[["start_year"]],
          .env[["start_year"]] + .env[["time_horizon"]])
      ) %>%
      dplyr::filter(
        .data[["emission_factor_metric"]] %in% c(
          "projected",
          "corporate_economy",
          .env[["target_scenario"]],
          .env[["adjusted_scenario"]]
        )
      ) %>%
      dplyr::mutate(
        emission_factor_metric = factor(
          .data[["emission_factor_metric"]],
          levels = c(
            "projected",
            "corporate_economy",
            .env[["target_scenario"]],
            .env[["adjusted_scenario"]]
          )
        )
      ) %>%
      r2dii.plot::prep_emission_intensity(
        span_5yr = TRUE
      )

    colours_scale <- c(
      "dark_blue",
      "green",
      "orange",
      "grey"
    )

    if (sector == "cement") {
      y_lab_emissions_intensity <- "Tons of CO2 per Ton of Cement Produced"
    } else if (sector == "steel") {
      y_lab_emissions_intensity <- "Tons of CO2 per Ton of Steel Produced"
    } else if (sector == "aviation") {
      y_lab_emissions_intensity <- "Tons of CO2 per Passenger Kilometer"
    } else {
      y_lab_emissions_intensity <- "Emission Factor Value"
    }

    plot_emission_intensity <- data_emission_intensity  %>%
      r2dii.plot::plot_emission_intensity() +
      r2dii.plot::scale_colour_r2dii(
        colour_labels = colours_scale,
        labels = c(
          "Portfolio",
          "Corporate Economy",
          glue::glue("Target {r2dii.plot::to_title(toupper(scenario))}"),
          glue::glue("Adjusted Scenario {r2dii.plot::to_title(toupper(scenario))}")
        )
      ) +
      ggplot2::xlab("Year") +
      ggplot2::ylab(y_lab_emissions_intensity)

    # export convergence chart
    data_emission_intensity %>%
      readr::write_csv(
        file = file.path(
          output_directory,
          by_group_value,
          glue::glue("data_emission_intensity_{sector}_{by_group_value}.csv")
        ),
        na = ""
      )

    ggplot2::ggsave(
      filename = glue::glue("plot_emission_intensity_{sector}_{by_group_value}.png"),
      plot = plot_emission_intensity,
      device = "png",
      path = file.path(output_directory, by_group_value)
    )
  }
  companies_included <- matched_prioritized %>%
    dplyr::select(
      dplyr::all_of(
        c(
          .env[["by_group"]], "name_abcd", "sector_abcd", "loan_size_outstanding",
          "loan_size_outstanding_currency", "loan_size_credit_limit",
          "loan_size_credit_limit_currency"
        )
      )
    )

  companies_included %>%
    readr::write_csv(
      file = file.path(
        output_directory,
        by_group_value,
        glue::glue("companies_included_{sector}_{by_group_value}.csv")
      ),
      na = ""
    )

}


validate_input_args_generate_individual_outputs <- function(output_directory,
                                                            by_group,
                                                            by_group_value,
                                                            scenario_source,
                                                            target_scenario,
                                                            region,
                                                            sector,
                                                            start_year,
                                                            time_horizon) {
  stop_if_not_length(output_directory, 1L)
  stop_if_not_inherits(output_directory, "character")

  stop_if_not_length(by_group, 1L)

  stop_if_not_length(by_group_value, 1L)

  stop_if_not_length(scenario_source, 1L)
  stop_if_not_inherits(scenario_source, "character")

  stop_if_not_length(target_scenario, 1L)
  stop_if_not_inherits(target_scenario, "character")

  stop_if_not_length(region, 1L)
  stop_if_not_inherits(region, "character")

  stop_if_not_length(sector, 1L)
  stop_if_not_inherits(sector, "character")

  stop_if_not_length(start_year, 1L)
  stop_if_not_inherits(start_year, "integer")

  stop_if_not_length(time_horizon, 1L)
  stop_if_not_inherits(time_horizon, "integer")

  invisible()
}


validate_input_data_generate_individual_outputs <- function(data,
                                                            matched_prioritized,
                                                            target_type,
                                                            by_group) {
  if (target_type == "sda") {
    stop_if_not_expected_columns(
      data = data,
      cols = c(
        "sector", "year", "region", "scenario_source", "emission_factor_metric",
        "emission_factor_value", by_group
      )
    )
  } else if (target_type == "tms") {
    stop_if_not_expected_columns(
      data = data,
      cols = c(
        "sector", "technology", "year", "region", "scenario_source", "metric",
        "production", "technology_share", "scope",
        "percentage_of_initial_production_by_scope", by_group
      )
    )
  }

  stop_if_not_expected_columns(
    data = matched_prioritized,
    cols = c(
      by_group, "name_abcd", "sector", "sector_abcd", "loan_size_outstanding",
      "loan_size_outstanding_currency", "loan_size_credit_limit",
      "loan_size_credit_limit_currency"
    )
  )

  invisible()
}
