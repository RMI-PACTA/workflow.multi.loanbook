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

# TODO: tms_results_all_groups
# TODO: sda_results_all_groups
# TODO: data_tech_mix_<sector>_<...>
# TODO: data_trajectory_<sector>_<technology>_<...>
# TODO: data_emission_intensity_<sector>_<...>
# TODO: companies_included_<sector>_<...>

# TODO: summary_statistics_loanbook_coverage

# TODO: add tabular output for match_success_rate


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
  dd_data_timeline_bo_po
)

usethis::use_data(data_dictionary, overwrite = TRUE)
