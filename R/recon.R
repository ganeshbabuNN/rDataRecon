#' Compare two data frames
#'
#' @description
#' `recon()` is the core function of the \pkg{rDataRecon} package.
#' It compares a **BASE** data frame against a **COMPARE** data frame and
#' produces a structured four-section report covering:
#'
#' * Dataset-level summary (observation counts, variable counts)
#' * Variables present in one dataset but not the other
#' * Variable class / type mismatches
#' * Row-level value differences with absolute or relative numeric deltas
#' * A final **IDENTICAL / HAS DIFFERENCES** verdict
#'
#' The function is inspired by traditional dataset-comparison procedures
#' available in statistical programming environments, and exposes an
#' equivalent feature set entirely within base R.
#'
#' @param base A `data.frame`  the reference (BASE) dataset.
#' @param compare A `data.frame`  the dataset to compare against BASE.
#' @param id Character vector of one or more key variable name(s) used to
#'   match rows across datasets. If `NULL` (default), rows are matched
#'   positionally (row 1 vs row 1, row 2 vs row 2, etc.).
#' @param var Character vector of variable names to include in the value
#'   comparison. Defaults to all common variables excluding `id` variables.
#'   Equivalent to specifying a variable list in the comparison procedure.
#' @param base_var Character vector of variable names in BASE to map to the
#'   corresponding entries in `compare_var`. Allows comparison of
#'   differently-named columns that represent the same concept. Must be the
#'   same length as `compare_var`.
#' @param compare_var Character vector of variable names in COMPARE to map to
#'   the corresponding entries in `base_var`. Must be the same length as
#'   `base_var`.
#' @param criterion Numeric tolerance for comparing numeric variables.
#'   Differences `<= criterion` are treated as equal. Default `1e-8`.
#' @param relative Logical. If `TRUE`, `criterion` is applied as a relative
#'   (proportional) difference: `|a - b| / max(|a|, eps)`. Default `FALSE`
#'   (absolute difference).
#' @param noequal Logical. If `TRUE`, suppress printing of variables with
#'   no differences in the output report. Default `FALSE`.
#' @param listall Logical. If `TRUE`, list all variable names in each dataset
#'   in the header section of the report. Default `FALSE`.
#' @param brief Logical. If `TRUE`, print only the summary section and
#'   suppress the row-level difference table. Default `FALSE`.
#' @param out Optional file path (character string). When supplied, the
#'   complete report is written to that file in addition to being printed.
#' @param out_data Logical. If `TRUE` (default), the differences data frame
#'   is included in the returned list as `$value_diffs`.
#'
#' @return A named list of class `"rDataRecon_result"` (returned
#'   **invisibly**) with elements:
#'
#' \describe{
#'   \item{`summary`}{Named list of dataset-level counts.}
#'   \item{`vars_base_only`}{Character vector  variables in BASE only.}
#'   \item{`vars_compare_only`}{Character vector  variables in COMPARE only.}
#'   \item{`type_mismatches`}{`data.frame` with columns `variable`,
#'     `base_class`, `compare_class`.}
#'   \item{`value_diffs`}{`data.frame` with columns `obs_id`, `variable`,
#'     `base_value`, `compare_value`, `diff` (or `NULL` if
#'     `out_data = FALSE`).}
#'   \item{`verdict`}{Either `"DATASETS ARE IDENTICAL"` or
#'     `"DATASETS HAVE DIFFERENCES"`.}
#' }
#'
#' @section Argument quick-reference:
#'
#' | Argument           | Purpose                                    |
#' |--------------------|--------------------------------------------|
#' | `base`             | Reference dataset                          |
#' | `compare`          | Dataset to validate against BASE           |
#' | `id`               | Key column(s) for row matching             |
#' | `var`              | Restrict comparison to named variables     |
#' | `base_var`         | Rename BASE column(s) before comparison    |
#' | `compare_var`      | Rename COMPARE column(s) before comparison |
#' | `criterion`        | Numeric equality tolerance (default 1e-8) |
#' | `relative`         | Apply tolerance proportionally             |
#' | `noequal`          | Suppress equal-variable output             |
#' | `listall`          | Print all variable names in header         |
#' | `brief`            | Summary section only                       |
#' | `out`              | Write report to a text file                |
#'
#' @examples
#' base_df <- data.frame(
#'   id   = 1:4,
#'   age  = c(25L, 30L, 35L, 40L),
#'   name = c("Alice", "Bob", "Carol", "Dave"),
#'   stringsAsFactors = FALSE
#' )
#' comp_df <- data.frame(
#'   id   = 1:4,
#'   age  = c(25L, 31L, 35L, 40L),
#'   name = c("Alice", "bob", "Carol", "Dave"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Basic comparison with a key variable
#' result <- recon(base_df, comp_df, id = "id")
#'
#' # Absolute tolerance of 0.01
#' recon(base_df, comp_df, id = "id", criterion = 0.01)
#'
#' # Relative 5 % tolerance
#' recon(base_df, comp_df, id = "id", criterion = 0.05, relative = TRUE)
#'
#' # Cross-variable name mapping
#' base2 <- data.frame(id = 1:3, revenue = c(100, 200, 300))
#' comp2 <- data.frame(id = 1:3, sales   = c(100, 250, 300))
#' recon(base2, comp2, id = "id",
#'       base_var = "revenue", compare_var = "sales")
#'
#' @export
recon <- function(base,
                  compare,
                  id          = NULL,
                  var         = NULL,
                  base_var    = NULL,
                  compare_var = NULL,
                  criterion   = 1e-8,
                  relative    = FALSE,
                  noequal     = FALSE,
                  listall     = FALSE,
                  brief       = FALSE,
                  out         = NULL,
                  out_data    = TRUE) {

  ##  Input validation 
  if (!is.data.frame(base))    stop("`base` must be a data.frame.")
  if (!is.data.frame(compare)) stop("`compare` must be a data.frame.")
  if (!is.numeric(criterion) || length(criterion) != 1L || criterion < 0)
    stop("`criterion` must be a single non-negative number.")
  if (!is.logical(relative) || length(relative) != 1L)
    stop("`relative` must be TRUE or FALSE.")

  ##  Cross-variable name mapping 
  if (!is.null(base_var) || !is.null(compare_var)) {
    if (is.null(base_var) || is.null(compare_var))
      stop("`base_var` and `compare_var` must both be supplied together.")
    if (length(base_var) != length(compare_var))
      stop("`base_var` and `compare_var` must have the same length.")
    idx <- match(compare_var, names(compare))
    if (anyNA(idx))
      stop("Names in `compare_var` not found in `compare`: ",
           paste(compare_var[is.na(idx)], collapse = ", "))
    names(compare)[idx] <- base_var
  }

  ##  Output capture 
  lines_out <- character(0L)
  .emit <- function(...) {
    ln <- paste0(...)
    lines_out <<- c(lines_out, ln)
    cat(ln, "\n", sep = "")
  }

  ##  Header 
  .emit(strrep("=", 72))
  .emit("               rDataRecon  -  Dataset Comparison Report")
  .emit(strrep("=", 72))
  .emit(sprintf("  BASE    : %-20s  %d obs  x  %d vars",
                deparse(substitute(base)),    nrow(base),    ncol(base)))
  .emit(sprintf("  COMPARE : %-20s  %d obs  x  %d vars",
                deparse(substitute(compare)), nrow(compare), ncol(compare)))
  .emit(sprintf("  Criterion : %g  (%s)",
                criterion, if (relative) "relative" else "absolute"))
  .emit(sprintf("  Run time  : %s",
                format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  .emit(strrep("-", 72))

  ##  Section 1  Variable structure 
  base_names        <- names(base)
  compare_names     <- names(compare)
  vars_base_only    <- setdiff(base_names,    compare_names)
  vars_compare_only <- setdiff(compare_names, base_names)
  common_vars       <- intersect(base_names,  compare_names)

  .emit("\n  SECTION 1  -  Variable Structure")
  .emit(strrep("-", 72))
  .emit(sprintf("  In BASE only    (%2d) : %s",
                length(vars_base_only),
                if (length(vars_base_only))
                  paste(vars_base_only, collapse = ", ") else "none"))
  .emit(sprintf("  In COMPARE only (%2d) : %s",
                length(vars_compare_only),
                if (length(vars_compare_only))
                  paste(vars_compare_only, collapse = ", ") else "none"))
  if (listall) {
    .emit(sprintf("  All BASE vars    : %s", paste(base_names,    collapse = ", ")))
    .emit(sprintf("  All COMPARE vars : %s", paste(compare_names, collapse = ", ")))
  }

  ##  Section 2  Type mismatches 
  ## integer and numeric are both numeric families and are value-comparable.
  ## Only flag mismatches that make comparison meaningless (e.g. character vs numeric).
  .num_family <- function(x) is.numeric(x) || is.integer(x)

  mm_list <- lapply(common_vars, function(v) {
    bc <- paste(class(base[[v]]),    collapse = "/")
    cc <- paste(class(compare[[v]]), collapse = "/")
    if (bc == cc) return(NULL)
    ## Both numeric-family (integer/numeric mix)  comparable, not a mismatch
    if (.num_family(base[[v]]) && .num_family(compare[[v]])) return(NULL)
    data.frame(variable = v, base_class = bc, compare_class = cc,
               stringsAsFactors = FALSE)
  })
  type_mm <- do.call(rbind, mm_list[!vapply(mm_list, is.null, logical(1L))])
  if (is.null(type_mm))
    type_mm <- data.frame(variable = character(), base_class = character(),
                          compare_class = character(), stringsAsFactors = FALSE)

  .emit("\n  SECTION 2  -  Variable Type Mismatches")
  .emit(strrep("-", 72))
  if (nrow(type_mm) > 0L) {
    .emit(sprintf("    %-22s  %-18s  %-18s",
                  "Variable", "BASE class", "COMPARE class"))
    .emit(sprintf("    %-22s  %-18s  %-18s",
                  strrep("-", 22), strrep("-", 18), strrep("-", 18)))
    for (i in seq_len(nrow(type_mm)))
      .emit(sprintf("    %-22s  %-18s  %-18s",
                    type_mm$variable[i],
                    type_mm$base_class[i],
                    type_mm$compare_class[i]))
  } else {
    .emit("  All common variables have matching types.")
  }

  ##  Key / ID setup 
  positional <- is.null(id)
  if (positional) {
    id    <- ".row_num_"
    n_max <- max(nrow(base), nrow(compare))
    base[seq_len(n_max),    ".row_num_"] <- seq_len(n_max)
    compare[seq_len(n_max), ".row_num_"] <- seq_len(n_max)
  } else {
    miss_b <- setdiff(id, names(base))
    miss_c <- setdiff(id, names(compare))
    if (length(miss_b))
      stop("Key variable(s) missing from BASE: ", paste(miss_b, collapse = ", "))
    if (length(miss_c))
      stop("Key variable(s) missing from COMPARE: ", paste(miss_c, collapse = ", "))
  }

  ##  Variables to compare 
  if (!is.null(var)) {
    compare_these <- intersect(var, common_vars)
    not_found     <- setdiff(var, common_vars)
    if (length(not_found))
      warning("Variables not found in both datasets (skipped): ",
              paste(not_found, collapse = ", "))
  } else {
    compare_these <- setdiff(common_vars, id)
  }
  compare_these <- setdiff(compare_these, type_mm$variable)

  ##  Merge 
  merged <- merge(base, compare, by = id, all = TRUE,
                  suffixes = c("._B_", "._C_"))

  ##  Observation counts 
  if (positional) {
    obs_b_only <- max(0L, nrow(base)    - nrow(compare))
    obs_c_only <- max(0L, nrow(compare) - nrow(base))
  } else {

    #bk <- do.call(paste, c(base[,    id, drop = FALSE], sep = "\x00"))
	bk <- do.call(paste, c(base[, id, drop = FALSE], sep = "\x1f"))
    #ck <- do.call(paste, c(compare[, id, drop = FALSE], sep = "\x00"))
	ck <- do.call(paste, c(compare[, id, drop = FALSE], sep = "\x1f"))
    obs_b_only <- length(setdiff(bk, ck))
    obs_c_only <- length(setdiff(ck, bk))
  }

  ##  Value comparison 
  value_diffs <- data.frame(obs_id = character(), variable = character(),
                             base_value = character(), compare_value = character(),
                             diff = character(), stringsAsFactors = FALSE)
  equal_vars <- character()
  diff_vars  <- character()

  for (v in compare_these) {
    b_col <- paste0(v, "._B_")
    c_col <- paste0(v, "._C_")
    if (!b_col %in% names(merged) || !c_col %in% names(merged)) next

    bv     <- merged[[b_col]]
    cv     <- merged[[c_col]]
    is_num <- is.numeric(bv) || is.numeric(cv)

    id_lbl <- if (identical(id, ".row_num_")) {
      as.character(merged[[".row_num_"]])
    } else {
      apply(merged[, id, drop = FALSE], 1L,
            function(r) paste(id, "=", r, collapse = " | "))
    }

    var_diff <- FALSE
    for (i in seq_len(nrow(merged))) {
      bval <- bv[i]; cval <- cv[i]
      if (is.na(bval) && is.na(cval)) next
      if (is_num) {
        bn <- suppressWarnings(as.numeric(bval))
        cn <- suppressWarnings(as.numeric(cval))
        ad <- abs(bn - cn)
        if (relative) {
          den      <- max(abs(bn), .Machine$double.eps)
          rd       <- ad / den
          is_diff  <- is.na(rd) || rd > criterion
          diff_str <- if (!is.na(rd)) sprintf("%.6g (rel)", rd) else "NA vs value"
        } else {
          is_diff  <- is.na(ad) || ad > criterion
          diff_str <- if (!is.na(ad)) sprintf("%.6g", ad) else "NA vs value"
        }
      } else {
        is_diff  <- !identical(as.character(bval), as.character(cval))
        diff_str <- NA_character_
      }
      if (is_diff) {
        var_diff    <- TRUE
        value_diffs <- rbind(value_diffs,
          data.frame(obs_id = id_lbl[i], variable = v,
                     base_value = as.character(bval),
                     compare_value = as.character(cval),
                     diff = diff_str, stringsAsFactors = FALSE))
      }
    }
    if (var_diff) diff_vars  <- c(diff_vars,  v)
    else          equal_vars <- c(equal_vars, v)
  }

  ##  Section 3  Value differences 
  if (!brief) {
    .emit("\n  SECTION 3  -  Value Comparison")
    .emit(strrep("-", 72))
    if (!noequal && length(equal_vars))
      .emit(sprintf("  Variables with no differences (%d): %s",
                    length(equal_vars), paste(equal_vars, collapse = ", ")))
    else if (noequal)
      .emit(sprintf("  Variables with no differences (%d): [suppressed]",
                    length(equal_vars)))
    else
      .emit("  Variables with no differences (0): none")

    if (length(diff_vars)) {
      .emit(sprintf("\n  Variables WITH differences (%d): %s",
                    length(diff_vars), paste(diff_vars, collapse = ", ")))
      .emit(sprintf("\n  %-28s  %-18s  %-18s  %-18s  %s",
                    "Obs ID", "Variable", "BASE value",
                    "COMPARE value", "Difference"))
      .emit(sprintf("  %-28s  %-18s  %-18s  %-18s  %s",
                    strrep("-",28), strrep("-",18), strrep("-",18),
                    strrep("-",18), strrep("-",12)))
      for (i in seq_len(nrow(value_diffs))) {
        r <- value_diffs[i, ]
        .emit(sprintf("  %-28s  %-18s  %-18s  %-18s  %s",
                      substr(r$obs_id,1L,28L), substr(r$variable,1L,18L),
                      substr(r$base_value,1L,18L), substr(r$compare_value,1L,18L),
                      if (is.na(r$diff)) "" else r$diff))
      }
    } else {
      .emit("\n  Variables WITH differences (0): none")
    }
  }

  ##  Section 4  Summary 
  .emit("\n  SECTION 4  -  Summary Statistics")
  .emit(strrep("-", 72))
  tbl <- list(
    "Obs in BASE"                     = nrow(base),
    "Obs in COMPARE"                  = nrow(compare),
    "Obs in BASE only"                = obs_b_only,
    "Obs in COMPARE only"             = obs_c_only,
    "Variables in BASE"               = ncol(base),
    "Variables in COMPARE"            = ncol(compare),
    "Variables in BASE only"          = length(vars_base_only),
    "Variables in COMPARE only"       = length(vars_compare_only),
    "Variables with type mismatches"  = nrow(type_mm),
    "Variables compared"              = length(compare_these),
    "Variables with all values equal" = length(equal_vars),
    "Variables with any differences"  = length(diff_vars),
    "Total value differences"         = nrow(value_diffs)
  )
  for (nm in names(tbl))
    .emit(sprintf("  %-44s  %d", nm, tbl[[nm]]))

  ##  Verdict 
  identical_flag <-
    length(vars_base_only) == 0L && length(vars_compare_only) == 0L &&
    nrow(type_mm) == 0L && nrow(value_diffs) == 0L &&
    obs_b_only == 0L && obs_c_only == 0L

  verdict <- if (identical_flag) "DATASETS ARE IDENTICAL" else "DATASETS HAVE DIFFERENCES"
  .emit(strrep("=", 72))
  .emit(sprintf("  VERDICT :  %s", verdict))
  .emit(strrep("=", 72))

  if (!is.null(out)) {
    writeLines(lines_out, con = out)
    message("Report written to: ", out)
  }

  invisible(structure(
    list(
      summary = list(
        nrow_base = nrow(base), nrow_compare = nrow(compare),
        ncol_base = ncol(base), ncol_compare = ncol(compare),
        obs_base_only = obs_b_only, obs_compare_only = obs_c_only,
        vars_compared = length(compare_these),
        vars_equal = length(equal_vars),
        vars_with_diffs = length(diff_vars),
        total_value_diffs = nrow(value_diffs)),
      vars_base_only    = vars_base_only,
      vars_compare_only = vars_compare_only,
      type_mismatches   = type_mm,
      value_diffs       = if (out_data) value_diffs else NULL,
      verdict           = verdict),
    class = "rDataRecon_result"
  ))
}
