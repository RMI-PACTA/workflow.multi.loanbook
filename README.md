# workflow.multi.loanbook

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/RMI-PACTA/workflow.multi.loanbook/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RMI-PACTA/workflow.multi.loanbook/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `workflow.multi.loanbook` is to provide an easy-to-use
interface for anyone who wants to run PACTA on multiple loan books. It
allows tailoring the analysis to the individual needs of a project in a
simple and straight-forward manner.

## Run in RStudio

### R package dependencies

Running `workflow.multi.loanbook` has a number of R package dependencies that are listed in the `DESCRIPTION` file. These can be installed manually or by using something like `pak::local_install_deps()`.

### Setting config options

You will need to set options in a config.yml file that should be placed at the root level of the repository. You can use `example.config.yml` as a template to set options in your `config.yml`. The config file is used to set up project parameters, such as paths and file names, and to decide which modules to run with which individual settings. This allows tailoring the calculations to individual research questions while ensuring that project-wide parameters are used consistently across all modules.

### Detailed documentation of parameters in config.yml

to be added...

### Running the multi loan book analysis

The analysis is run in two steps. The first step is to run the process of matching the raw input loan books with the asset/based company data (ABCD), using the `run_matching.R` script. Initial matching results will be saved in the `matched/` directory specified in the `config.yml`. These files need to be validated manually. Once the manual validation of the matched loan books is completed, the checked files are further processed, using the script `run_pacta_multi_loanbook.R`, which calculates standard PACTA results for the loan books and additional related metrics and analytics.

Both these scripts can either be run line-by-line, or by calling `source("run_matching.R")` and `source("run_pacta_multi_loanbook.R")`.
