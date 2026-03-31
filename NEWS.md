# rDataRecon 1.0.0
## Bug fixes

* `recon()` now correctly compares columns where one side is `integer` and
  the other is `numeric` (double). Previously, the type-mismatch check treated
  `integer` and `numeric` as incompatible classes and silently excluded such
  columns from value comparison, returning zero differences when differences
  existed. Both types are now recognized as members of the numeric family and
  are compared normally (#1).
* Test 12 (positional matching) updated to use consistent `numeric` types.
* Test 12b added as an explicit regression test for the `integer`/`numeric`
  mix behavior.

# rDataRecon 0.1.0

## Initial CRAN release

* Core function `recon()`  full dataset comparison with structured reporting.
* Helper functions: `get_diffs()`, `summary_df()`, `compare_stats()`,
  `is_structure_equal()`.
* Zero hard dependencies  base R only.
* Key-based and positional row matching (via `id` argument).
* Absolute and relative numeric tolerances (via `criterion` and `relative`).
* Cross-variable name mapping via `base_var` / `compare_var`
  (compare columns with different names across datasets).
* Plain-text report export via `out` argument.
* S3 class `"rDataRecon_result"` with `print` method.
* Full `testthat` test suite (23 tests).
* Introductory vignette with clinical dataset examples.
