# Data dictionary for the output table company_technology_deviation_tms
dd_company_technology_deviation_tms <- dplyr::tribble(
  ~dataset, ~column, ~typeof, ~definition, ~value,
  "company_technology_deviation_tms", "sector", "character", "The sector of the technology", "One of the following: 'power', 'automotive', 'coal', 'oil and gas'",
  "company_technology_deviation_tms", "technology", "character", "The technology", "One of the in-scope PACTA technologies that belong to the sector indicated in 'sector'",
  "company_technology_deviation_tms", "year", "integer", "The year of the data", "A year between the 'start_year' of the analyis and the 'start_year' plus the 'time_frame'",
  "company_technology_deviation_tms", "region", "character", "The region for which the analysis has been run. Indicates which production assets have been considered and which scenario region is used", "Must be a value available in the input scenario data",
  "company_technology_deviation_tms", "scenario_source", "character", "The publication the scenario data is based on", "Must be available in the input scenario data. Usually, available sources are: 'weo', 'geco', 'isf'. Usually follows the pattern '<source>_<publication_year>'",
  "company_technology_deviation_tms", "name_abcd", "character", "The name of the company", "The name of the company",
  "company_technology_deviation_tms", "projected", "double", "The projected activity level of the technology in the year for the given company", "Numerical value greater or equal to 0",
  "company_technology_deviation_tms", "target_<scenario>", "double", "The target activity level of the technology in the year for the given company", "Numerical value greater or equal to 0",
  "company_technology_deviation_tms", "total_tech_deviation", "double", "The total difference between 'target_<scenario>' and 'projected'", "Numerical value. Can be negative or positive",
  "company_technology_deviation_tms", "direction", "character", "The direction the technology follows long term in the scenario", "One of the following: 'buildout', 'phaseout'",
  "company_technology_deviation_tms", "activity_unit", "character", "The unit the activity is measured in for a given sector", "The unit corresponding to the sector. For example 'MW' of capacity for power"
)

dd_company_alignment_net_tms <- dplyr::tribble(
  ~dataset, ~column, ~typeof, ~definition, ~value,
  "company_alignment_net_tms", "name_abcd", "character", "The name of the company", "The name of the company",
  "company_alignment_net_tms", "sector", "character", "The sector of the technology", "One of the following: 'power', 'automotive', 'coal', 'oil and gas'",
  "company_alignment_net_tms", "activity_unit", "character", "The unit the activity is measured in for a given sector", "The unit corresponding to the sector. For example 'MW' of capacity for power",
  "company_alignment_net_tms", "region", "character", "The region for which the analysis has been run. Indicates which production assets have been considered and which scenario region is used", "Must be a value available in the input scenario data",
  "company_alignment_net_tms", "scenario_source", "character", "The publication the scenario data is based on", "Must be available in the input scenario data. Usually, available sources are: 'weo', 'geco', 'isf'. Usually follows the pattern '<source>_<publication_year>'",
  "company_alignment_net_tms", "scenario", "character", "The name of the scenario against which alignment is measured", "Must be available in the input scenario data. Must be a scenario provided in the indicated 'scenario_source'",
  "company_alignment_net_tms", "year", "integer", "The year of the data", "A year between the 'start_year' of the analyis and the 'start_year' plus the 'time_frame'",
  "company_alignment_net_tms", "direction", "character", "At the sector level, 'direction' indicates if the alignment value is aggregated across buildout or phaseout technologies, or if it is the net aggregate of the sector.", "In this case, must be: 'net'",
  "company_alignment_net_tms", "total_deviation", "double", "Net aggregate deviation of all underlying technologies, accounting for directionality. A more positive number means that the combination of underlying technologies is more aligned, a more negative one implies higher misalignment", "Numerical value. Can be negative or positive",
  "company_alignment_net_tms", "alignment_metric", "double", "Net aggregate alignment value by sector. A ratio that is calculated as the total deviation divided by the net scenario value (accounting for directionality). A positive value shows the company plans are ahead of target. A negative value means they are behind target", "Numerical value. Can be negative or positive"
)

dd_company_alignment_bo_po_tms <- dplyr::tribble(
  ~dataset, ~column, ~typeof, ~definition, ~value,
  "company_alignment_bo_po_tms", "name_abcd", "character", "The name of the company", "The name of the company",
  "company_alignment_bo_po_tms", "sector", "character", "The sector of the technology", "One of the following: 'power', 'automotive', 'coal', 'oil and gas'",
  "company_alignment_bo_po_tms", "activity_unit", "character", "The unit the activity is measured in for a given sector", "The unit corresponding to the sector. For example 'MW' of capacity for power",
  "company_alignment_bo_po_tms", "region", "character", "The region for which the analysis has been run. Indicates which production assets have been considered and which scenario region is used", "Must be a value available in the input scenario data",
  "company_alignment_bo_po_tms", "scenario_source", "character", "The publication the scenario data is based on", "Must be available in the input scenario data. Usually, available sources are: 'weo', 'geco', 'isf'. Usually follows the pattern '<source>_<publication_year>'",
  "company_alignment_bo_po_tms", "scenario", "character", "The name of the scenario against which alignment is measured", "Must be available in the input scenario data. Must be a scenario provided in the indicated 'scenario_source'",
  "company_alignment_bo_po_tms", "year", "integer", "The year of the data", "A year between the 'start_year' of the analyis and the 'start_year' plus the 'time_frame'",
  "company_alignment_bo_po_tms", "direction", "character", "At the sector level, 'direction' indicates if the alignment value is aggregated across buildout or phaseout technologies, or if it is the net aggregate of the sector.", "In this case, must be: 'buildout' or 'phaseout'",
  "company_alignment_bo_po_tms", "total_deviation", "double", "Aggregate deviation of all underlying technologies of the same directionality. A more positive number means that the combination of underlying technologies is more aligned, a more negative one implies higher misalignment", "Numerical value. Can be negative or positive",
  "company_alignment_bo_po_tms", "alignment_metric", "double", "Net aggregate alignment value by sector, disaggregated by direction of the underlying technlogies. A ratio that is calculated as the total deviation divided by the net scenario value (accounting for directionality). A positive value shows the company plans are ahead of target. A negative value means they are behind target", "Numerical value. Can be negative or positive"
)

dd_company_alignment_net_sda <- dplyr::tribble(
  ~dataset, ~column, ~typeof, ~definition, ~value,
  "company_alignment_net_sda", "name_abcd", "character", "The name of the company", "The name of the company",
  "company_alignment_net_sda", "sector", "character", "The sector of the technology", "One of the following: 'aviation', 'cement', 'steel'",
  "company_alignment_net_sda", "activity_unit", "character", "The unit the physical emission intensity is measured in for a given sector", "The unit corresponding to the sector. For example 'tCO2 / t cement'",
  "company_alignment_net_sda", "region", "character", "The region for which the analysis has been run. Indicates which production assets have been considered and which scenario region is used", "Must be a value available in the input scenario data",
  "company_alignment_net_sda", "scenario_source", "character", "The publication the scenario data is based on", "Must be available in the input scenario data. Usually, available sources are: 'weo', 'geco', 'isf'. Usually follows the pattern '<source>_<publication_year>'",
  "company_alignment_net_sda", "scenario", "character", "The name of the scenario against which alignment is measured", "Must be available in the input scenario data. Must be a scenario provided in the indicated 'scenario_source'",
  "company_alignment_net_sda", "year", "integer", "The year of the data", "A year between the 'start_year' of the analyis and the 'start_year' plus the 'time_frame'",
  "company_alignment_net_sda", "direction", "character", "At the sector level, 'direction' indicates if the alignment value is aggregated across buildout or phaseout technologies, or if it is the net aggregate of the sector.", "In this case, must be: 'net'",
  "company_alignment_net_sda", "total_deviation", "double", "Net aggregate deviation of all underlying technologies, accounting for directionality. A more positive number means that the combination of underlying technologies is more aligned, a more negative one implies higher misalignment", "Numerical value. Can be negative or positive",
  "company_alignment_net_sda", "alignment_metric", "double", "Net aggregate alignment value by sector. A ratio that is calculated as the total deviation divided by the net scenario value (accounting for directionality). A positive value shows the company plans are ahead of target. A negative value means they are behind target", "Numerical value. Can be negative or positive"
)

dd_company_exposure_bo_po_aggregate_alignment <- dplyr::tribble(
  ~dataset, ~column, ~typeof, ~definition, ~value,
  "company_exposure_bo_po_aggregate_alignment", "<by_group>", "character", "Any additional descriptor either at the loan level or at the banking book level. This is used to calculate grouped results by additional dimensions of interest, such as types of FIs or types of loans", "Any variable name is permissible, that is not already used otherwise. All entries in the banking book should have a corresponding value. NULL is permissible and implies no grouping",
  "company_exposure_bo_po_aggregate_alignment", "name_abcd", "character", "The name of the company", "The name of the company",
  "company_exposure_bo_po_aggregate_alignment", "sector", "character", "The sector of the technology", "One of the following: 'power', 'automotive', 'coal', 'oil and gas'",
  "company_exposure_bo_po_aggregate_alignment", "activity_unit", "character", "The unit the activity is measured in for a given sector", "The unit corresponding to the sector. For example 'MW' of capacity for power",
  "company_exposure_bo_po_aggregate_alignment", "region", "character", "The region for which the analysis has been run. Indicates which production assets have been considered and which scenario region is used", "Must be a value available in the input scenario data",
  "company_exposure_bo_po_aggregate_alignment", "scenario_source", "character", "The publication the scenario data is based on", "Must be available in the input scenario data. Usually, available sources are: 'weo', 'geco', 'isf'. Usually follows the pattern '<source>_<publication_year>'",
  "company_exposure_bo_po_aggregate_alignment", "scenario", "character", "The name of the scenario against which alignment is measured", "Must be available in the input scenario data. Must be a scenario provided in the indicated 'scenario_source'",
  "company_exposure_bo_po_aggregate_alignment", "year", "integer", "The year of the data", "A year between the 'start_year' of the analyis and the 'start_year' plus the 'time_frame'",
  "company_exposure_bo_po_aggregate_alignment", "direction", "character", "At the sector level, 'direction' indicates if the alignment value is aggregated across buildout or phaseout technologies, or if it is the net aggregate of the sector.", "In this case, must be: 'buildout' or 'phaseout'",
  "company_exposure_bo_po_aggregate_alignment", "total_deviation", "double", "Aggregate deviation of all underlying technologies of the same directionality. A more positive number means that the combination of underlying technologies is more aligned, a more negative one implies higher misalignment", "Numerical value. Can be negative or positive",
  "company_exposure_bo_po_aggregate_alignment", "alignment_metric", "double", "Net aggregate alignment value by sector, disaggregated by direction of the underlying technlogies. A ratio that is calculated as the total deviation divided by the net scenario value (accounting for directionality). A positive value shows the company plans are ahead of target. A negative value means they are behind target", "Numerical value. Can be negative or positive",
  "company_exposure_bo_po_aggregate_alignment", "loan_size_outstanding_currency", "character", "Denomination of the loans listed in the given banking book", "Three letter currency code following the ISO 4217 standard. Only one currency allowed per banking book",
  "company_exposure_bo_po_aggregate_alignment", "loan_size_outstanding", "double", "Remaining outstanding loan value to the given counterparty", "Numerical value greater or equal to 0",
  "company_exposure_bo_po_aggregate_alignment", "exposure_weight", "double", "Relative size of the loan compared to the overall size of the analysed banking book", "Numerical value greater or equal to 0"
)

dd_company_exposure_net_aggregate_alignment <- dplyr::tribble(
  ~dataset, ~column, ~typeof, ~definition, ~value,
  "company_exposure_net_aggregate_alignment", "<by_group>", "character", "Any additional descriptor either at the loan level or at the banking book level. This is used to calculate grouped results by additional dimensions of interest, such as types of FIs or types of loans", "Any variable name is permissible, that is not already used otherwise. All entries in the banking book should have a corresponding value. NULL is permissible and implies no grouping",
  "company_exposure_net_aggregate_alignment", "name_abcd", "character", "The name of the company", "The name of the company",
  "company_exposure_net_aggregate_alignment", "sector", "character", "The sector of the technology", "One of the following: 'power', 'automotive', 'coal', 'oil and gas', 'aviation', 'cement', 'steel'",
  "company_exposure_net_aggregate_alignment", "activity_unit", "character", "The unit the activity is measured in for a given sector", "The unit corresponding to the sector. For example 'MW' of capacity for power",
  "company_exposure_net_aggregate_alignment", "region", "character", "The region for which the analysis has been run. Indicates which production assets have been considered and which scenario region is used", "Must be a value available in the input scenario data",
  "company_exposure_net_aggregate_alignment", "scenario_source", "character", "The publication the scenario data is based on", "Must be available in the input scenario data. Usually, available sources are: 'weo', 'geco', 'isf'. Usually follows the pattern '<source>_<publication_year>'",
  "company_exposure_net_aggregate_alignment", "scenario", "character", "The name of the scenario against which alignment is measured", "Must be available in the input scenario data. Must be a scenario provided in the indicated 'scenario_source'",
  "company_exposure_net_aggregate_alignment", "year", "integer", "The year of the data", "A year between the 'start_year' of the analyis and the 'start_year' plus the 'time_frame'",
  "company_exposure_net_aggregate_alignment", "direction", "character", "At the sector level, 'direction' indicates if the alignment value is aggregated across buildout or phaseout technologies, or if it is the net aggregate of the sector.", "In this case, must be: 'net'",
  "company_exposure_net_aggregate_alignment", "total_deviation", "double", "Aggregate deviation of all underlying technologies of the same directionality. A more positive number means that the combination of underlying technologies is more aligned, a more negative one implies higher misalignment", "Numerical value. Can be negative or positive",
  "company_exposure_net_aggregate_alignment", "alignment_metric", "double", "Net aggregate alignment value by sector. A ratio that is calculated as the total deviation divided by the net scenario value (accounting for directionality). A positive value shows the company plans are ahead of target. A negative value means they are behind target", "Numerical value. Can be negative or positive",
  "company_exposure_net_aggregate_alignment", "loan_size_outstanding_currency", "character", "Denomination of the loans listed in the given banking book", "Three letter currency code following the ISO 4217 standard. Only one currency allowed per banking book",
  "company_exposure_net_aggregate_alignment", "loan_size_outstanding", "double", "Remaining outstanding loan value to the given counterparty", "Numerical value greater or equal to 0",
  "company_exposure_net_aggregate_alignment", "exposure_weight", "double", "Relative size of the loan compared to the overall size of the analysed banking book", "Numerical value greater or equal to 0"
)

dd_loanbook_exposure_bo_po_aggregate_alignment <- dplyr::tribble(
  ~dataset, ~column, ~typeof, ~definition, ~value,
  "loanbook_exposure_bo_po_aggregate_alignment", "<by_group>", "character", "Any additional descriptor either at the loan level or at the banking book level. This is used to calculate grouped results by additional dimensions of interest, such as types of FIs or types of loans", "Any variable name is permissible, that is not already used otherwise. All entries in the banking book should have a corresponding value. NULL is permissible and implies no grouping",
  "loanbook_exposure_bo_po_aggregate_alignment", "scenario", "character", "The name of the scenario against which alignment is measured", "Must be available in the input scenario data. Must be a scenario provided in the indicated 'scenario_source'",
  "loanbook_exposure_bo_po_aggregate_alignment", "region", "character", "The region for which the analysis has been run. Indicates which production assets have been considered and which scenario region is used", "Must be a value available in the input scenario data",
  "loanbook_exposure_bo_po_aggregate_alignment", "sector", "character", "The sector of the technology", "One of the following: 'power', 'automotive', 'coal', 'oil and gas'",
  "loanbook_exposure_bo_po_aggregate_alignment", "year", "integer", "The year of the data", "A year between the 'start_year' of the analyis and the 'start_year' plus the 'time_frame'",
  "loanbook_exposure_bo_po_aggregate_alignment", "direction", "character", "At the sector level, 'direction' indicates if the alignment value is aggregated across buildout or phaseout technologies, or if it is the net aggregate of the sector.", "In this case, must be: 'buildout' or 'phaseout'",
  "loanbook_exposure_bo_po_aggregate_alignment", "n_companies", "double", "Number of individual company-by-sector combinations in the given sector analysed within this (group of) banking book(s)", "Numerical value greater or equal to 0",
  "loanbook_exposure_bo_po_aggregate_alignment", "n_companies_aligned", "double", "Number of individual company-by-sector combinations within the given sector that have an alignment metric greater or equal to 0", "Numerical value greater or equal to 0 and smaller or equal to 'n_companies'",
  "loanbook_exposure_bo_po_aggregate_alignment", "share_companies_aligned", "double", "Share of 'n_companies_aligned' relative to 'n_companies'", "Numerical value. Must be between 0 and 1",
  "loanbook_exposure_bo_po_aggregate_alignment", "exposure_weighted_net_alignment", "double", "Net aggregate alignment value aggregated to the banking book-by-sector level, disaggregated into 'buildout' and 'phaseout' components. Individual company alignment metrics are allocated based on financial exposure, using the 'exposure_weight'", "Numerical value. Can be negative or positive"
)

dd_loanbook_exposure_net_aggregate_alignment <- dplyr::tribble(
  ~dataset, ~column, ~typeof, ~definition, ~value,
  "loanbook_exposure_net_aggregate_alignment", "<by_group>", "character", "Any additional descriptor either at the loan level or at the banking book level. This is used to calculate grouped results by additional dimensions of interest, such as types of FIs or types of loans", "Any variable name is permissible, that is not already used otherwise. All entries in the banking book should have a corresponding value. NULL is permissible and implies no grouping",
  "loanbook_exposure_net_aggregate_alignment", "scenario", "character", "The name of the scenario against which alignment is measured", "Must be available in the input scenario data. Must be a scenario provided in the indicated 'scenario_source'",
  "loanbook_exposure_net_aggregate_alignment", "region", "character", "The region for which the analysis has been run. Indicates which production assets have been considered and which scenario region is used", "Must be a value available in the input scenario data",
  "loanbook_exposure_net_aggregate_alignment", "sector", "character", "The sector of the technology", "One of the following: 'power', 'automotive', 'coal', 'oil and gas', 'aviation', 'cement', 'steel'",
  "loanbook_exposure_net_aggregate_alignment", "year", "integer", "The year of the data", "A year between the 'start_year' of the analyis and the 'start_year' plus the 'time_frame'",
  "loanbook_exposure_net_aggregate_alignment", "direction", "character", "At the sector level, 'direction' indicates if the alignment value is aggregated across buildout or phaseout technologies, or if it is the net aggregate of the sector.", "In this case, must be: 'net'",
  "loanbook_exposure_net_aggregate_alignment", "n_companies", "double", "Number of individual company-by-sector combinations in the given sector analysed within this (group of) banking book(s)", "Numerical value greater or equal to 0",
  "loanbook_exposure_net_aggregate_alignment", "n_companies_aligned", "double", "Number of individual company-by-sector combinations within the given sector that have an alignment metric greater or equal to 0", "Numerical value greater or equal to 0 and smaller or equal to 'n_companies'",
  "loanbook_exposure_net_aggregate_alignment", "share_companies_aligned", "double", "Share of 'n_companies_aligned' relative to 'n_companies'", "Numerical value. Must be between 0 and 1",
  "loanbook_exposure_net_aggregate_alignment", "exposure_weighted_net_alignment", "double", "Net aggregate alignment value aggregated to the banking book-by-sector level. Individual company alignment metrics are allocated based on financial exposure, using the 'exposure_weight'", "Numerical value. Can be negative or positive",
  "loanbook_exposure_net_aggregate_alignment", "sum_loan_size_outstanding", "double", "Sum of outstanding loan size at the banking book-by-sector level of all loans analysed within this (group of) banking book(s)", "Numerical value greater or equal to 0",
  "loanbook_exposure_net_aggregate_alignment", "sum_exposure_companies_aligned", "double", "Sum of outstanding loan size at the banking book-by-sector level of all loans to aligned companies within this (group of) banking book(s)", "Numerical value greater or equal to 0 and smaller or equal to 'sum_loan_size_outstanding'",
  "loanbook_exposure_net_aggregate_alignment", "share_exposure_aligned", "double", "Share of 'sum_exposure_companies_aligned' relative to 'sum_loan_size_outstanding'", "Numerical value. Must be between 0 and 1"
)

dd_data_sankey <- dplyr::tribble(
  ~dataset, ~column, ~typeof, ~definition, ~value,
  "data_sankey", "<by_group>", "character", "Leftmost node in the sankey plot. Defines the split of the banking books into groups. Any additional descriptor either at the loan level or at the banking book level", "Any variable name is permissible, that is not already used otherwise. All entries in the banking book should have a corresponding value. NULL is permissible and implies no grouping",
  "data_sankey", "middle_node", "character", "Middle node in the sankey plot. A different grouping of interest. E.g. 'sector' would split the financial exposures according to sectoral exposure", "Must be available in the exposure alignment results used as input for the graph",
  "data_sankey", "middle_node2", "character", "Optional: An additional middle node can be added for further disaggregation of exposures in the sankey plot", "Must be available in the exposure alignment results used as input for the graph",
  "data_sankey", "is_aligned", "logical", "End node and color indicator. A dummy variable that indicates if the underlying counterparty related to the exposure is aligned based on the net alignment metric or not", "Value is either 'Aligned' or 'Not aligned'",
  "data_sankey", "loan_size_outstanding", "double", "Remaining outstanding loan value to the underlying counterparty", "Numerical value greater or equal to 0"
)

dd_data_scatter_alignment_exposure <- dplyr::tribble(
  ~dataset, ~column, ~typeof, ~definition, ~value,
  "data_scatter_alignment_exposure", "<by_group>", "character", "Any additional descriptor either at the loan level or at the banking book level. This is used to calculate grouped results by additional dimensions of interest, such as types of FIs or types of loans", "Any variable name is permissible, that is not already used otherwise. All entries in the banking book should have a corresponding value. NULL is permissible and implies no grouping",
  "data_scatter_alignment_exposure", "scenario", "character", "The name of the scenario against which alignment is measured", "Must be available in the input scenario data. Must be a scenario provided in the indicated 'scenario_source'",
  "data_scatter_alignment_exposure", "region", "character", "The region for which the analysis has been run. Indicates which production assets have been considered and which scenario region is used", "Must be a value available in the input scenario data",
  "data_scatter_alignment_exposure", "sector", "character", "The sector of the technology", "One of the following: 'power', 'automotive', 'coal', 'oil and gas', 'aviation', 'cement', 'steel'",
  "data_scatter_alignment_exposure", "year", "integer", "The year of the data", "A year between the 'start_year' of the analyis and the 'start_year' plus the 'time_frame'",
  "data_scatter_alignment_exposure", "exposure_weighted_net_alignment", "double", "Net aggregate alignment value aggregated to the banking book-by-sector level. Individual company alignment metrics are allocated based on financial exposure, using the 'exposure_weight'", "Numerical value. Can be negative or positive",
  "data_scatter_alignment_exposure", "sum_loan_size_outstanding", "double", "Sum of outstanding loan size at the banking book-by-sector level of all loans analysed within this (group of) banking book(s)", "Numerical value greater or equal to 0"
)

# TODO: currently the sector is only indicated by the file name, not by a column in the data
dd_data_scatter_sector <- dplyr::tribble(
  ~dataset, ~column, ~typeof, ~definition, ~value,
  "data_scatter_sector", "name", "character", "Name of the entity to analyse. If analysed at group level, this variable contains the values of <by_group>. If analysed at company level, it contains the values of 'name_abcd'", "An identifying name of the entity",
  "data_scatter_sector", "buildout", "character", "Net aggregate alignment value by sector, disaggregated to buildout technologies only", "Numerical value. Can be negative or positive",
  "data_scatter_sector", "phaseout", "character", "Net aggregate alignment value by sector, disaggregated to phaseout technologies only", "Numerical value. Can be negative or positive",
  "data_scatter_sector", "net", "character", "Net aggregate alignment value by sector of the entity analysed", "Numerical value. Can be negative or positive. Sum of the disaggregated components of 'buildout' and 'phaseout'",
  "data_scatter_sector", "datapoint", "character", "The level of the analysis, either group level or company level", "Must be one of: 'Group' or 'company'"
)

# TODO: currently the sector is only indicated by the file name, not by a column in the data
dd_data_scatter_sector_animated <- dplyr::tribble(
  ~dataset, ~column, ~typeof, ~definition, ~value,
  "data_scatter_sector_animated", "name", "character", "Name of the entity to analyse. If analysed at group level, this variable contains the values of <by_group>. If analysed at company level, it contains the values of 'name_abcd'", "An identifying name of the entity",
  "data_scatter_sector_animated", "year", "integer", "The year of the data", "A year between the 'start_year' of the analyis and the 'start_year' plus the 'time_frame'",
  "data_scatter_sector_animated", "buildout", "character", "Net aggregate alignment value by sector, disaggregated to buildout technologies only", "Numerical value. Can be negative or positive",
  "data_scatter_sector_animated", "phaseout", "character", "Net aggregate alignment value by sector, disaggregated to phaseout technologies only", "Numerical value. Can be negative or positive",
  "data_scatter_sector_animated", "net", "character", "Net aggregate alignment value by sector of the entity analysed", "Numerical value. Can be negative or positive. Sum of the disaggregated components of 'buildout' and 'phaseout'",
  "data_scatter_sector_animated", "datapoint", "character", "The level of the analysis, either group level or company level", "Must be one of: 'Group' or 'company'"
)

# same as loanbook_exposure_net_aggregate_alignment, but filtered for a specific sector
dd_data_timeline_net <- dplyr::tribble(
  ~dataset, ~column, ~typeof, ~definition, ~value,
  "data_timeline_net", "<by_group>", "character", "Any additional descriptor either at the loan level or at the banking book level. This is used to calculate grouped results by additional dimensions of interest, such as types of FIs or types of loans", "Any variable name is permissible, that is not already used otherwise. All entries in the banking book should have a corresponding value. NULL is permissible and implies no grouping",
  "data_timeline_net", "scenario", "character", "The name of the scenario against which alignment is measured", "Must be available in the input scenario data. Must be a scenario provided in the indicated 'scenario_source'",
  "data_timeline_net", "region", "character", "The region for which the analysis has been run. Indicates which production assets have been considered and which scenario region is used", "Must be a value available in the input scenario data",
  "data_timeline_net", "sector", "character", "The sector of the technology", "One of the following: 'power', 'automotive', 'coal', 'oil and gas', 'aviation', 'cement', 'steel'",
  "data_timeline_net", "year", "integer", "The year of the data", "A year between the 'start_year' of the analyis and the 'start_year' plus the 'time_frame'",
  "data_timeline_net", "direction", "character", "At the sector level, 'direction' indicates if the alignment value is aggregated across buildout or phaseout technologies, or if it is the net aggregate of the sector.", "In this case, must be: 'net'",
  "data_timeline_net", "n_companies", "double", "Number of individual company-by-sector combinations in the given sector analysed within this (group of) banking book(s)", "Numerical value greater or equal to 0",
  "data_timeline_net", "n_companies_aligned", "double", "Number of individual company-by-sector combinations within the given sector that have an alignment metric greater or equal to 0", "Numerical value greater or equal to 0 and smaller or equal to 'n_companies'",
  "data_timeline_net", "share_companies_aligned", "double", "Share of 'n_companies_aligned' relative to 'n_companies'", "Numerical value. Must be between 0 and 1",
  "data_timeline_net", "exposure_weighted_net_alignment", "double", "Net aggregate alignment value aggregated to the banking book-by-sector level. Individual company alignment metrics are allocated based on financial exposure, using the 'exposure_weight'", "Numerical value. Can be negative or positive",
  "data_timeline_net", "sum_loan_size_outstanding", "double", "Sum of outstanding loan size at the banking book-by-sector level of all loans analysed within this (group of) banking book(s)", "Numerical value greater or equal to 0",
  "data_timeline_net", "sum_exposure_companies_aligned", "double", "Sum of outstanding loan size at the banking book-by-sector level of all loans to aligned companies within this (group of) banking book(s)", "Numerical value greater or equal to 0 and smaller or equal to 'sum_loan_size_outstanding'",
  "data_timeline_net", "share_exposure_aligned", "double", "Share of 'sum_exposure_companies_aligned' relative to 'sum_loan_size_outstanding'", "Numerical value. Must be between 0 and 1"
)

# same as loanbook_exposure_bo_po_aggregate_alignment, but filtered for a specific sector
dd_data_timeline_bo_po <- dplyr::tribble(
  ~dataset, ~column, ~typeof, ~definition, ~value,
  "data_timeline_bo_po", "<by_group>", "character", "Any additional descriptor either at the loan level or at the banking book level. This is used to calculate grouped results by additional dimensions of interest, such as types of FIs or types of loans", "Any variable name is permissible, that is not already used otherwise. All entries in the banking book should have a corresponding value. NULL is permissible and implies no grouping",
  "data_timeline_bo_po", "scenario", "character", "The name of the scenario against which alignment is measured", "Must be available in the input scenario data. Must be a scenario provided in the indicated 'scenario_source'",
  "data_timeline_bo_po", "region", "character", "The region for which the analysis has been run. Indicates which production assets have been considered and which scenario region is used", "Must be a value available in the input scenario data",
  "data_timeline_bo_po", "sector", "character", "The sector of the technology", "One of the following: 'power', 'automotive', 'coal', 'oil and gas'",
  "data_timeline_bo_po", "year", "integer", "The year of the data", "A year between the 'start_year' of the analyis and the 'start_year' plus the 'time_frame'",
  "data_timeline_bo_po", "direction", "character", "At the sector level, 'direction' indicates if the alignment value is aggregated across buildout or phaseout technologies, or if it is the net aggregate of the sector.", "In this case, must be: 'buildout' or 'phaseout'",
  "data_timeline_bo_po", "n_companies", "double", "Number of individual company-by-sector combinations in the given sector analysed within this (group of) banking book(s)", "Numerical value greater or equal to 0",
  "data_timeline_bo_po", "n_companies_aligned", "double", "Number of individual company-by-sector combinations within the given sector that have an alignment metric greater or equal to 0", "Numerical value greater or equal to 0 and smaller or equal to 'n_companies'",
  "data_timeline_bo_po", "share_companies_aligned", "double", "Share of 'n_companies_aligned' relative to 'n_companies'", "Numerical value. Must be between 0 and 1",
  "data_timeline_bo_po", "exposure_weighted_net_alignment", "double", "Net aggregate alignment value aggregated to the banking book-by-sector level, disaggregated into 'buildout' and 'phaseout' components. Individual company alignment metrics are allocated based on financial exposure, using the 'exposure_weight'", "Numerical value. Can be negative or positive"
)

# TODO: extend variable grouping to standard PACTA and update
dd_tms_results_all_groups <- dplyr::tribble(
  ~dataset, ~column, ~typeof, ~definition, ~value,
  "tms_results_all_groups", "group_id", "character", "Identification of the banking book analysed", "The group_id is automatically generated from the file name of the corresponding raw banking book",
  # "tms_results_all_groups", "<by_group>", "character", "Any additional descriptor either at the loan level or at the banking book level. This is used to calculate grouped results by additional dimensions of interest, such as types of FIs or types of loans", "Any variable name is permissible, that is not already used otherwise. All entries in the banking book should have a corresponding value. NULL is permissible and implies no grouping",
  "tms_results_all_groups", "sector", "character", "The sector of the technology", "One of the following: 'power', 'automotive', 'coal', 'oil and gas'",
  "tms_results_all_groups", "technology", "character", "The technology", "One of the in-scope PACTA technologies that belong to the sector indicated in 'sector'",
  "tms_results_all_groups", "year", "integer", "The year of the data", "A year greater or equal to the 'start_year' of the analyis",
  "tms_results_all_groups", "region", "character", "The region for which the analysis has been run. Indicates which production assets have been considered and which scenario region is used", "Must be a value available in the input scenario data",
  "tms_results_all_groups", "scenario_source", "character", "The publication the scenario data is based on", "Must be available in the input scenario data. Usually, available sources are: 'weo', 'geco', 'isf'. Usually follows the pattern '<source>_<publication_year>'",
  "tms_results_all_groups", "metric", "character", "Indicates if the production related values refer to the projected activities of the underlying counterparty, to the economy wide benchmark, or to allocated levels of activity based on the scenarios", "Must be one of the following: 'projected', 'corporate_economy', or 'target_<scenario>'",
  "tms_results_all_groups", "production", "double", "The production level of the given 'metric'", "Numerical value greater or equal to 0",
  "tms_results_all_groups", "technology_share", "double", "The share of the 'production' the given 'technology' relative to all technologies of the corresponding 'sector' for the gien combination of 'group_id', 'region', 'year' and 'metric'", "Numerical value between 0 and 1",
  "tms_results_all_groups", "scope", "character", "Indicates if the targets for the given technology have been calculated based on the TMSR (technology) or the SMSP (sector). High-carbon technologies that need to decrease have their targets calculated on the technology level, whereas low-carbon technologies that need to increase have them calculated on the sector level", "Must be one of: 'technology' or 'sector'",
  "tms_results_all_groups", "percentage_of_initial_production_by_scope", "double", "Relative change compared to the start value (by scope). Used for displaying the change in activity over time on a common scale", "Numerical value. Can be negative or positive"
)

# TODO: extend variable grouping to standard PACTA and update
dd_sda_results_all_groups <- dplyr::tribble(
  ~dataset, ~column, ~typeof, ~definition, ~value,
  "sda_results_all_groups", "group_id", "character", "Identification of the banking book analysed", "The group_id is automatically generated from the file name of the corresponding raw banking book",
  # "sda_results_all_groups", "<by_group>", "character", "Any additional descriptor either at the loan level or at the banking book level. This is used to calculate grouped results by additional dimensions of interest, such as types of FIs or types of loans", "Any variable name is permissible, that is not already used otherwise. All entries in the banking book should have a corresponding value. NULL is permissible and implies no grouping",
  "sda_results_all_groups", "sector", "character", "The sector of the technology", "One of the following: 'aviation', 'cement', 'steel'",
  "sda_results_all_groups", "year", "integer", "The year of the data", "A year greater or equal to the 'start_year' of the analyis",
  "sda_results_all_groups", "region", "character", "The region for which the analysis has been run. Indicates which production assets have been considered and which scenario region is used", "Must be a value available in the input scenario data",
  "sda_results_all_groups", "scenario_source", "character", "The publication the scenario data is based on", "Must be available in the input scenario data. Usually, available sources are: 'weo', 'geco', 'isf'. Usually follows the pattern '<source>_<publication_year>'",
  "sda_results_all_groups", "emission_factor_metric", "character", "Indicates if the emission intensity related values refer to the projected activities of the underlying counterparty, to the economy wide benchmark, or to allocated levels of activity based on the scenarios", "Must be one of the following: 'projected', 'corporate_economy', or 'target_<scenario>'",
  "sda_results_all_groups", "emission_factor_value", "double", "The physical emission intensity level of the given 'emission_factor_metric'", "Numerical value greater or equal to 0"
)

# TODO: extend variable grouping to standard PACTA and update
dd_data_tech_mix <- dplyr::tribble(
  ~dataset, ~column, ~typeof, ~definition, ~value,
  "data_tech_mix", "group_id", "character", "Identification of the banking book analysed", "The group_id is automatically generated from the file name of the corresponding raw banking book",
  # "data_tech_mix", "<by_group>", "character", "Any additional descriptor either at the loan level or at the banking book level. This is used to calculate grouped results by additional dimensions of interest, such as types of FIs or types of loans", "Any variable name is permissible, that is not already used otherwise. All entries in the banking book should have a corresponding value. NULL is permissible and implies no grouping",
  "data_tech_mix", "sector", "character", "The sector of the technology", "One of the following: 'power', 'automotive', 'coal', 'oil and gas'",
  "data_tech_mix", "technology", "character", "The technology", "One of the in-scope PACTA technologies that belong to the sector indicated in 'sector'",
  "data_tech_mix", "year", "integer", "The year of the data", "A year greater or equal to the 'start_year' of the analyis",
  "data_tech_mix", "region", "character", "The region for which the analysis has been run. Indicates which production assets have been considered and which scenario region is used", "Must be a value available in the input scenario data",
  "data_tech_mix", "scenario_source", "character", "The publication the scenario data is based on", "Must be available in the input scenario data. Usually, available sources are: 'weo', 'geco', 'isf'. Usually follows the pattern '<source>_<publication_year>'",
  "data_tech_mix", "metric", "character", "Indicates if the production related values refer to the projected activities of the underlying counterparty, to the economy wide benchmark, or to allocated levels of activity based on the scenarios", "Must be one of the following: 'projected', 'corporate_economy', or 'target_<scenario>'",
  "data_tech_mix", "production", "double", "The production level of the given 'metric'", "Numerical value greater or equal to 0",
  "data_tech_mix", "technology_share", "double", "The share of the 'production' the given 'technology' relative to all technologies of the corresponding 'sector' for the gien combination of 'group_id', 'region', 'year' and 'metric'", "Numerical value between 0 and 1",
  "data_tech_mix", "scope", "character", "Indicates if the targets for the given technology have been calculated based on the TMSR (technology) or the SMSP (sector). High-carbon technologies that need to decrease have their targets calculated on the technology level, whereas low-carbon technologies that need to increase have them calculated on the sector level", "Must be one of: 'technology' or 'sector'",
  "data_tech_mix", "percentage_of_initial_production_by_scope", "double", "Relative change compared to the start value (by scope). Used for displaying the change in activity over time on a common scale", "Numerical value. Can be negative or positive",
  "data_tech_mix", "label", "character", "Same as 'metric', formatted for display in plot", "Must be one of the following: 'projected', 'corporate_economy', or 'target_<scenario>', but formatted for display",
  "data_tech_mix", "label_tech", "character", "Same as 'technology', formatted for display in plot", "One of the in-scope PACTA technologies that belong to the sector indicated in 'sector'",
  "data_tech_mix", "value", "double", "Same as 'technology_share', for display in plot", "Numerical value between 0 and 1"
)

# TODO: extend variable grouping to standard PACTA and update
dd_data_trajectory <- dplyr::tribble(
  ~dataset, ~column, ~typeof, ~definition, ~value,
  "data_trajectory", "group_id", "character", "Identification of the banking book analysed", "The group_id is automatically generated from the file name of the corresponding raw banking book",
  # "data_trajectory", "<by_group>", "character", "Any additional descriptor either at the loan level or at the banking book level. This is used to calculate grouped results by additional dimensions of interest, such as types of FIs or types of loans", "Any variable name is permissible, that is not already used otherwise. All entries in the banking book should have a corresponding value. NULL is permissible and implies no grouping",
  "data_trajectory", "sector", "character", "The sector of the technology", "One of the following: 'power', 'automotive', 'coal', 'oil and gas'",
  "data_trajectory", "technology", "character", "The technology", "One of the in-scope PACTA technologies that belong to the sector indicated in 'sector'",
  "data_trajectory", "year", "integer", "The year of the data", "A year greater or equal to the 'start_year' of the analyis",
  "data_trajectory", "region", "character", "The region for which the analysis has been run. Indicates which production assets have been considered and which scenario region is used", "Must be a value available in the input scenario data",
  "data_trajectory", "scenario_source", "character", "The publication the scenario data is based on", "Must be available in the input scenario data. Usually, available sources are: 'weo', 'geco', 'isf'. Usually follows the pattern '<source>_<publication_year>'",
  "data_trajectory", "metric", "character", "Indicates if the production related values refer to the projected activities of the underlying counterparty, to the economy wide benchmark, or to allocated levels of activity based on the scenarios", "Must be one of the following: 'projected', 'corporate_economy', or 'target_<scenario>'",
  "data_trajectory", "production", "double", "The production level of the given 'metric'", "Numerical value greater or equal to 0",
  "data_trajectory", "technology_share", "double", "The share of the 'production' the given 'technology' relative to all technologies of the corresponding 'sector' for the gien combination of 'group_id', 'region', 'year' and 'metric'", "Numerical value between 0 and 1",
  "data_trajectory", "scope", "character", "Indicates if the targets for the given technology have been calculated based on the TMSR (technology) or the SMSP (sector). High-carbon technologies that need to decrease have their targets calculated on the technology level, whereas low-carbon technologies that need to increase have them calculated on the sector level", "Must be one of: 'technology' or 'sector'",
  "data_trajectory", "percentage_of_initial_production_by_scope", "double", "Relative change compared to the start value (by scope). Used for displaying the change in activity over time on a common scale", "Numerical value. Can be negative or positive",
  "data_trajectory", "label", "character", "Same as 'metric', formatted for display in plot", "Must be one of the following: 'projected', 'corporate_economy', or 'target_<scenario>', but formatted for display",
  "data_trajectory", "value", "double", "Same as 'percentage_of_initial_production_by_scope', for display in plot", "Numerical value. Can be negative or positive"
)

# TODO: extend variable grouping to standard PACTA and update
dd_data_emission_intensity <- dplyr::tribble(
  ~dataset, ~column, ~typeof, ~definition, ~value,
  "data_emission_intensity", "group_id", "character", "Identification of the banking book analysed", "The group_id is automatically generated from the file name of the corresponding raw banking book",
  # "data_emission_intensity", "<by_group>", "character", "Any additional descriptor either at the loan level or at the banking book level. This is used to calculate grouped results by additional dimensions of interest, such as types of FIs or types of loans", "Any variable name is permissible, that is not already used otherwise. All entries in the banking book should have a corresponding value. NULL is permissible and implies no grouping",
  "data_emission_intensity", "sector", "character", "The sector of the technology", "One of the following: 'aviation', 'cement', 'steel'",
  "data_emission_intensity", "year", "integer", "The year of the data", "A year greater or equal to the 'start_year' of the analyis",
  "data_emission_intensity", "region", "character", "The region for which the analysis has been run. Indicates which production assets have been considered and which scenario region is used", "Must be a value available in the input scenario data",
  "data_emission_intensity", "scenario_source", "character", "The publication the scenario data is based on", "Must be available in the input scenario data. Usually, available sources are: 'weo', 'geco', 'isf'. Usually follows the pattern '<source>_<publication_year>'",
  "data_emission_intensity", "emission_factor_metric", "character", "Indicates if the emission intensity related values refer to the projected activities of the underlying counterparty, to the economy wide benchmark, or to allocated levels of activity based on the scenarios", "Must be one of the following: 'projected', 'corporate_economy', or 'target_<scenario>'",
  "data_emission_intensity", "emission_factor_value", "double", "The physical emission intensity level of the given 'emission_factor_metric'", "Numerical value greater or equal to 0",
  "data_emission_intensity", "label", "character", "Same as 'emission_factor_metric', formatted for display in plot", "Must be one of the following: 'projected', 'corporate_economy', or 'target_<scenario>', but formatted for display"
)

# TODO: extend variable grouping to standard PACTA and update
# TODO: possibly deprecate this output
dd_companies_included <- dplyr::tribble(
  ~dataset, ~column, ~typeof, ~definition, ~value,
  "companies_included", "group_id", "character", "Identification of the banking book analysed", "The group_id is automatically generated from the file name of the corresponding raw banking book",
  # "companies_included", "<by_group>", "character", "Any additional descriptor either at the loan level or at the banking book level. This is used to calculate grouped results by additional dimensions of interest, such as types of FIs or types of loans", "Any variable name is permissible, that is not already used otherwise. All entries in the banking book should have a corresponding value. NULL is permissible and implies no grouping",
  "companies_included", "name_abcd", "character", "The name of the company", "The name of the company",
  "companies_included", "sector_abcd", "character", "The sector of the technology", "One of the following: 'power', 'automotive', 'coal', 'oil and gas', 'aviation', 'cement', 'steel'",
  "companies_included", "loan_size_outstanding", "double", "Remaining outstanding loan value to the given counterparty", "Numerical value greater or equal to 0",
  "companies_included", "loan_size_outstanding_currency", "character", "Denomination of the outstanding loans listed in the given banking book", "Three letter currency code following the ISO 4217 standard. Only one currency allowed per banking book",
  "companies_included", "loan_size_credit_limit", "double", "Maximum value provided to the a counterparty under the given credit line", "Numerical value greater or equal to 0",
  "companies_included", "loan_size_credit_limit_currency", "character", "Denomination of the credit lines of loans listed in the given banking book", "Three letter currency code following the ISO 4217 standard. Only one currency allowed per banking book"
)

# TODO: extend variable grouping to standard PACTA and update
dd_summary_statistics_loanbook_coverage <- dplyr::tribble(
  ~dataset, ~column, ~typeof, ~definition, ~value,
  "summary_statistics_loanbook_coverage", "group_id", "character", "Identification of the banking book analysed", "The group_id is automatically generated from the file name of the corresponding raw banking book",
  # "summary_statistics_loanbook_coverage", "<by_group>", "character", "Any additional descriptor either at the loan level or at the banking book level. This is used to calculate grouped results by additional dimensions of interest, such as types of FIs or types of loans", "Any variable name is permissible, that is not already used otherwise. All entries in the banking book should have a corresponding value. NULL is permissible and implies no grouping",
  "summary_statistics_loanbook_coverage", "region", "character", "The region for which the analysis has been run. Indicates which production assets have been considered and which scenario region is used", "Must be a value available in the input scenario data",
  "summary_statistics_loanbook_coverage", "sector", "character", "The sector of the technology", "One of the following: 'power', 'automotive', 'coal', 'oil and gas', 'aviation', 'cement', 'steel'",
  "summary_statistics_loanbook_coverage", "total_exposure", "double", "Remaining outstanding loan value to the all companies in the sector that have some operations within the given region", "Numerical value greater or equal to 0",
  "summary_statistics_loanbook_coverage", "n_companies_matched", "integer", "Number of companies identified for analysis in the given region and sector. Any matched company is counted regardless of size of exposure", "Integer value greater or equal to 0",
  "summary_statistics_loanbook_coverage", "n_companies_total", "integer", "Total number of companies in the reference dataset in the given region and sector", "Integer value greater or equal to 0",
  "summary_statistics_loanbook_coverage", "share_companies_matched", "double", "Share of companies identified for analysis relative to total number of companies in the reference dataset. This is a proxy for which part of the economy of a region is covered by the analysis", "Numerical value between 0 and 1",
  "summary_statistics_loanbook_coverage", "production_financed", "double", "Sum of production in a sector and region by all companies identfied for analysis. The amount of production is not weighted by exposure", "Numerical value greater or equal to 0",
  "summary_statistics_loanbook_coverage", "production_total", "double", "Sum of production in a sector and region by all companies in the reference dataset", "Numerical value greater or equal to 0",
  "summary_statistics_loanbook_coverage", "share_production_financed", "double", "Share of production of companies identified for analysis relative to production by all companies in the reference dataset. This is a proxy for how much of the output of a region is covered by the analysis", "Numerical value between 0 and 1"
)

# TODO: extend variable grouping to standard PACTA and update
# TODO: probably better to export data_lbk_match_success_rate, which is actual format used in plots
dd_lbk_match_success_rate <- dplyr::tribble(
  ~dataset, ~column, ~typeof, ~definition, ~value,
  "lbk_match_success_rate", "group_id", "character", "Identification of the banking book analysed", "The group_id is automatically generated from the file name of the corresponding raw banking book",
  # "lbk_match_success_rate", "<by_group>", "character", "Any additional descriptor either at the loan level or at the banking book level. This is used to calculate grouped results by additional dimensions of interest, such as types of FIs or types of loans", "Any variable name is permissible, that is not already used otherwise. All entries in the banking book should have a corresponding value. NULL is permissible and implies no grouping",
  "lbk_match_success_rate", "sector", "character", "The sector of the technology", "One of the following: 'power', 'automotive', 'coal', 'oil and gas', 'aviation', 'cement', 'steel'",
  "lbk_match_success_rate", "matched", "character", "Indicates if the matching values are shown for matched or unmatched loans", "Must be one of the following: 'Matched' or 'Not matched'",
  "lbk_match_success_rate", "match_n", "integer", "Number of loans identified for analysis in a given sector in the given banking book that were successfully matched with companies in the production data", "Integer value greater or equal to 0",
  "lbk_match_success_rate", "total_n", "integer", "Total number of loans in the banking book for a given sector", "Integer value greater or equal to 0",
  "lbk_match_success_rate", "match_success_rate_rel", "double", "Share of matched loans in a secftor relative to total number of loans in the sector in the raw input banking book", "Numerical value between 0 and 1",
  "lbk_match_success_rate", "match_outstanding", "double", "Remaining outstanding loan value in the banking book to the all matched companies in the sector", "Numerical value greater or equal to 0",
  "lbk_match_success_rate", "total_outstanding", "double", "Total outstanding loan value in the banking book in a given sector for matched and unmatched loans", "Numerical value greater or equal to 0",
  "lbk_match_success_rate", "match_success_outstanding_rel", "double", "Share of the matched outstanding loan amount in a sector relative to the total outstanding loan amount in that sector", "Numerical value between 0 and 1",
  "lbk_match_success_rate", "match_credit_limit", "double", "Sum value of the credit limit of all matched loans within a sector", "Numerical value greater or equal to 0",
  "lbk_match_success_rate", "total_credit_limit", "double", "Total value of the credit limit of all loans matched or unmatched within a sector", "Numerical value greater or equal to 0",
  "lbk_match_success_rate", "match_success_credit_limit_rel", "double", "Share of the matched amount of credit limit in a sector relative to the total credit limit in that sector", "Numerical value between 0 and 1"
)


data_dictionary <- dplyr::bind_rows(
  dd_company_technology_deviation_tms,
  dd_company_alignment_net_tms,
  dd_company_alignment_bo_po_tms,
  dd_company_alignment_net_sda,
  dd_company_exposure_bo_po_aggregate_alignment,
  dd_company_exposure_net_aggregate_alignment,
  dd_loanbook_exposure_bo_po_aggregate_alignment,
  dd_loanbook_exposure_net_aggregate_alignment,
  dd_data_sankey,
  dd_data_scatter_alignment_exposure,
  dd_data_scatter_sector,
  dd_data_scatter_sector_animated,
  dd_data_timeline_net,
  dd_data_timeline_bo_po,
  dd_tms_results_all_groups,
  dd_sda_results_all_groups,
  dd_data_tech_mix,
  dd_data_trajectory,
  dd_data_emission_intensity,
  dd_companies_included,
  dd_summary_statistics_loanbook_coverage,
  dd_lbk_match_success_rate
)

usethis::use_data(
  data_dictionary,
  overwrite = TRUE
)
