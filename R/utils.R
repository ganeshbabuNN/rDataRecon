#' Print method for rDataRecon_result objects
#'
#' Prints a concise summary of a [recon()] result.
#'
#' @param x   An object of class `"rDataRecon_result"`.
#' @param ... Ignored.
#' @return `x`, invisibly.
#'
#' @examples
#' base_df <- data.frame(id = 1:3, x = c(1, 2, 3))
#' comp_df <- data.frame(id = 1:3, x = c(1, 9, 3))
#' r <- recon(base_df, comp_df, id = "id")
#' print(r)
#'
#' @export
print.rDataRecon_result <- function(x, ...) {
  cat("rDataRecon result\n")
  cat("  Verdict            :", x$verdict, "\n")
  cat("  Total differences  :", x$summary$total_value_diffs, "\n")
  cat("  Variables with diffs:", x$summary$vars_with_diffs, "\n")
  invisible(x)
}


#' Extract the value-differences data frame
#'
#' Pulls `$value_diffs` from a [recon()] result, with an informative error
#' when the data were not retained.
#'
#' @param result An object of class `"rDataRecon_result"`.
#' @return A `data.frame` with columns `obs_id`, `variable`, `base_value`,
#'   `compare_value`, and `diff`.
#'
#' @examples
#' base_df <- data.frame(id = 1:3, score = c(10, 20, 30))
#' comp_df <- data.frame(id = 1:3, score = c(10, 99, 30))
#' r <- recon(base_df, comp_df, id = "id")
#' get_diffs(r)
#'
#' @export
get_diffs <- function(result) {
  if (!inherits(result, "rDataRecon_result"))
    stop("`result` must be an rDataRecon_result object returned by recon().")
  if (is.null(result$value_diffs))
    stop("No differences data available. Re-run recon() with out_data = TRUE.")
  result$value_diffs
}


#' Summarise a recon result as a one-row data frame
#'
#' Converts the `$summary` list from a [recon()] result into a single-row
#' `data.frame`. Useful for logging comparison outcomes in batch QC pipelines.
#'
#' @param result An object of class `"rDataRecon_result"`.
#' @return A one-row `data.frame` of all summary counts plus a `verdict`
#'   column.
#'
#' @examples
#' base_df <- data.frame(id = 1:3, x = c(1, 2, 3))
#' comp_df <- data.frame(id = 1:3, x = c(1, 5, 3))
#' r <- recon(base_df, comp_df, id = "id")
#' summary_df(r)
#'
#' @export
summary_df <- function(result) {
  if (!inherits(result, "rDataRecon_result"))
    stop("`result` must be an rDataRecon_result object returned by recon().")
  as.data.frame(c(result$summary, list(verdict = result$verdict)),
                stringsAsFactors = FALSE)
}


#' Side-by-side numeric summary statistics for two data frames
#'
#' For every numeric variable common to both datasets, computes mean, SD,
#' min, and max and returns them side-by-side for easy inspection.
#'
#' @param base    A `data.frame` (BASE dataset).
#' @param compare A `data.frame` (COMPARE dataset).
#' @param vars    Character vector of variables to summarise. Defaults to all
#'   common numeric variables.
#' @return A `data.frame` with one row per variable and columns
#'   `base_mean`, `compare_mean`, `base_sd`, `compare_sd`,
#'   `base_min`, `compare_min`, `base_max`, `compare_max`, and `mean_diff`.
#'
#' @examples
#' base_df <- data.frame(id = 1:4, age = c(25, 30, 35, 40),
#'                        score = c(80, 90, 85, 95))
#' comp_df <- data.frame(id = 1:4, age = c(25, 31, 35, 40),
#'                        score = c(80, 92, 85, 95))
#' compare_stats(base_df, comp_df)
#'
#' @export
compare_stats <- function(base, compare, vars = NULL) {
  if (!is.data.frame(base))    stop("`base` must be a data.frame.")
  if (!is.data.frame(compare)) stop("`compare` must be a data.frame.")
  num_b  <- names(base)[vapply(base,    is.numeric, logical(1L))]
  num_c  <- names(compare)[vapply(compare, is.numeric, logical(1L))]
  common <- intersect(num_b, num_c)
  if (!is.null(vars)) common <- intersect(vars, common)
  if (length(common) == 0L) {
    message("No common numeric variables found.")
    return(invisible(NULL))
  }
  do.call(rbind, lapply(common, function(v) {
    data.frame(
      variable     = v,
      base_mean    = mean(base[[v]],    na.rm = TRUE),
      compare_mean = mean(compare[[v]], na.rm = TRUE),
      base_sd      = stats::sd(base[[v]],    na.rm = TRUE),
      compare_sd   = stats::sd(compare[[v]], na.rm = TRUE),
      base_min     = min(base[[v]],     na.rm = TRUE),
      compare_min  = min(compare[[v]],  na.rm = TRUE),
      base_max     = max(base[[v]],     na.rm = TRUE),
      compare_max  = max(compare[[v]],  na.rm = TRUE),
      mean_diff    = mean(base[[v]], na.rm=TRUE) - mean(compare[[v]], na.rm=TRUE),
      stringsAsFactors = FALSE)
  }))
}


#' Test whether two data frames have identical structure
#'
#' Returns `TRUE` when both data frames share exactly the same variable names
#' (regardless of column order) with identical classes.
#'
#' @param base    A `data.frame`.
#' @param compare A `data.frame`.
#' @return `TRUE` or `FALSE`.
#'
#' @examples
#' df1 <- data.frame(x = 1L,  y = "a", stringsAsFactors = FALSE)
#' df2 <- data.frame(y = "b", x = 2L,  stringsAsFactors = FALSE)
#' is_structure_equal(df1, df2)   # TRUE
#'
#' df3 <- data.frame(x = 1.0,  y = "a", stringsAsFactors = FALSE)
#' is_structure_equal(df1, df3)   # FALSE  x is integer vs numeric
#'
#' @export
is_structure_equal <- function(base, compare) {
  if (!identical(sort(names(base)), sort(names(compare)))) return(FALSE)
  for (v in names(base))
    if (!identical(class(base[[v]]), class(compare[[v]]))) return(FALSE)
  TRUE
}
