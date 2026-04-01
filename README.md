<img src="https://img.shields.io/badge/version-1.0.0-blue" align="right"/> <img src="https://img.shields.io/badge/license-GPL--3.0-green" align="right"/>
# rDataRecon

**An R package for dataset comparison and reconciliation for SDTM/ADAM data frames and Others .**

Compare two data frames BASE (reference) vs COMPARE (new version) and get
a detailed, structured report of every structural and value-level difference.
Designed for clinical data programming, ETL validation, and general
data-quality workflows.
<!--
[![R-CMD-check](https://github.com/yourgithub/rDataRecon/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yourgithub/rDataRecon/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/rDataRecon)](https://CRAN.R-project.org/package=rDataRecon)
-->
---

## Installation

```r
# From CRAN (once published)
install.packages("rDataRecon")

# Stable version:
remotes::install_github("ganeshbabunn/rDataRecon")

# Development version:
remotes::install_github("ganeshbabunn/rDataRecon@dev")

# From local source
install.packages("../rDataRecon_X.X.tar.gz", repos = NULL, type = "source")
X indicates the version of the release.
```

---

## Quick start

```r
library(rDataRecon)

base_df <- data.frame(
  id   = 1:5,
  age  = c(25L, 30L, 35L, 40L, 45L),
  name = c("Sarang", "Ganesh", "Swetha", "Jyothi", "Rudresh"),
  stringsAsFactors = FALSE
)

comp_df <- data.frame(
  id   = 1:5,
  age  = c(25L, 31L, 35L, 40L, 45L),
  name = c("Sarang", "ganesh", "Swetha", "Jyothi", "Rudresh"),
  stringsAsFactors = FALSE
)

result <- recon(base_df, comp_df, id = "id")
```

Extract differences as a tidy data frame:

```r
get_diffs(result)
#   obs_id variable base_value compare_value  diff
# 1  id = 2      age         30            31     1
# 2  id = 2     name        Ganesh         ganesh  NA
```

---

## Features

| Feature | Details |
|---|---|
| Zero dependencies | Base R only no additional packages required |
| Key-based matching | Match rows by one or more ID columns |
| Positional matching | Row-by-row when no ID is specified |
| Cross-variable mapping | Compare differently-named columns via `base_var` / `compare_var` |
| Absolute tolerance | `criterion = 0.01`  ignore differences below threshold |
| Relative tolerance | `criterion = 0.05, relative = TRUE`  proportional comparison |
| Focused comparison | `var = c("age","sbp")`  compare specific variables only |
| Report export | `out = "report.txt"`  save full report to file |
| S3 result object | Class `rDataRecon_result` with `print()`, `get_diffs()`, `summary_df()` |

---

## Argument reference

| `recon()` argument | Behaviour |
|---|---|
| `base` | Reference dataset |
| `compare` | Dataset to validate against BASE |
| `id` | Key column(s) for row matching |
| `var` | Restrict comparison to named variables |
| `base_var` + `compare_var` | Map differently-named columns across datasets |
| `criterion` | Numeric equality tolerance (default `1e-8`) |
| `relative = TRUE` | Apply tolerance proportionally |
| `noequal = TRUE` | Suppress equal-variable output |
| `listall = TRUE` | Print all variable names in header |
| `brief = TRUE` | Summary section only |
| `out = "file.txt"` | Write report to text file |

---

## Examples

### Cross-variable name mapping

```r
base2 <- data.frame(id = 1:3, revenue = c(100, 200, 300))
comp2 <- data.frame(id = 1:3, sales   = c(100, 250, 300))
recon(base2, comp2, id = "id", base_var = "revenue", compare_var = "sales")
```

### Relative tolerance (5 %)

```r
recon(base_df, comp_df, id = "id", criterion = 0.05, relative = TRUE)
```

### Multi-column key

```r
recon(base_df, comp_df, id = c("site", "visit"))
```

### Save report to file

```r
recon(base_df, comp_df, id = "id", out = "rDataRecon_report.txt")
```

### Programmatic use in QC pipelines

```r
r <- recon(base_df, comp_df, id = "id", brief = TRUE)

summary_df(r)          # one-row log entry
compare_stats(base_df, comp_df)   # side-by-side numeric stats
is_structure_equal(base_df, comp_df)  # structural check
```

---

## Report structure

`recon()` prints a four-section plain-text report:

```
========================================================================
               rDataRecon  -  Dataset Comparison Report
========================================================================
  BASE    : base_df              5 obs  x  3 vars
  COMPARE : comp_df              5 obs  x  3 vars
  Criterion : 1e-08  (absolute)
  Run time  : 2025-01-01 12:00:00
------------------------------------------------------------------------
  SECTION 1  -  Variable Structure
  SECTION 2  -  Variable Type Mismatches
  SECTION 3  -  Value Comparison
  SECTION 4  -  Summary Statistics
========================================================================
  VERDICT :  DATASETS HAVE DIFFERENCES
========================================================================
```

---

## Return value

`recon()` returns an `"rDataRecon_result"` list invisibly:

```
$summary             list of counts (obs, vars, diffs)
$vars_base_only      character vector
$vars_compare_only   character vector
$type_mismatches     data.frame (variable, base_class, compare_class)
$value_diffs         data.frame (obs_id, variable, base_value,
                                  compare_value, diff)
$verdict             character string
```

---

## Bug Reports & Feature Requests

Found a bug or have an idea to improve `rDataRecon`?
Please raise it via the **[GitHub Issues](https://github.com/ganeshbabunn/rDataRecon/issues)** tab.

When opening a new issue, use the appropriate label:

|Type     |Label   |When to use|
|-------- |--------|------------|
| Bug     | `bug`         | Something is broken or behaving unexpectedly   |
| Feature | `enhancement` | A new feature or improvement you'd like to see |

### Steps to raise an issue

1. Go to the **[Issues tab](https://github.com/ganeshbabunn/rDataRecon/issues)**
2. Click **"New Issue"**
3. Choose the relevant template — **Bug Report** or **Feature Request**
4. Fill in the details and apply the correct label (`bug` or `enhancement`)
5. Click **"Submit new issue"**

> **Tip:** Before opening a new issue, please search existing issues to avoid duplicates.

---

## License

GPL-3.0
Copyright (C) 2026 Ganesh Babu G
