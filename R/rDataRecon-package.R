#' rDataRecon: Dataset Comparison and Reconciliation Utility
#'
#' @description
#' The \pkg{rDataRecon} package provides a comprehensive data frame
#' comparison and reconciliation tool. The main function [recon()] compares a
#' BASE data frame against a COMPARE data frame and produces a structured
#' four-section report covering variable structure, type mismatches,
#' row-level value differences, and a summary count table.
#'
#' @section Main function:
#' * [recon()]  run a full comparison and print/return the report.
#'
#' @section Helper functions:
#' * [get_diffs()]  extract the differences `data.frame` from a result.
#' * [summary_df()]  convert the summary list to a one-row `data.frame`.
#' * [compare_stats()]  side-by-side numeric summary statistics.
#' * [is_structure_equal()]  quick structural equality check.
#'
#' @section Key features:
#' * **Zero hard dependencies**  base R only; no additional packages required.
#' * **Key-based or positional matching**  match rows by one or more ID
#'   columns (`id` argument), or fall back to row position when `id = NULL`.
#' * **Cross-variable name mapping**  compare columns with different names
#'   across datasets using `base_var` / `compare_var`.
#' * **Absolute and relative numeric tolerances**  `criterion` controls the
#'   acceptable difference; set `relative = TRUE` for proportional comparison.
#' * **Plain-text report export**  `out = "file.txt"` writes the full report.
#' * **Programmatic result**  returns a structured `rDataRecon_result` list
#'   for use in automated QC pipelines.
#'
#' @section Option reference:
#'
#' | `recon()` argument | Behaviour |
#' |--------------------|-----------|
#' | `base`             | Reference dataset |
#' | `compare`          | Dataset to validate against BASE |
#' | `id`               | Key column(s) for row matching |
#' | `var`              | Restrict comparison to named variables |
#' | `base_var` / `compare_var` | Map differently-named columns |
#' | `criterion`        | Numeric equality tolerance |
#' | `relative`         | Apply tolerance proportionally |
#' | `noequal`          | Suppress equal-variable output |
#' | `listall`          | Print all variable names in header |
#' | `brief`            | Summary section only |
#' | `out`              | Write report to text file |
#'
#' @author Your Name \email{your.email@@example.com}
#'
#' @keywords internal
"_PACKAGE"
