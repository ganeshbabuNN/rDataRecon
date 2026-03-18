## tests/testthat/test-recon.R
## Run with: devtools::test()  or  testthat::test_local()

base_df <- data.frame(
  id   = 1:5,
  age  = c(25L, 30L, 35L, 40L, 45L),
  name = c("Alice", "Bob", "Carol", "Dave", "Eve"),
  stringsAsFactors = FALSE
)

# ── 1. Identical datasets ─────────────────────────────────────────────────────
test_that("identical datasets return IDENTICAL verdict", {
  r <- recon(base_df, base_df, id = "id")
  expect_equal(r$verdict, "DATASETS ARE IDENTICAL")
  expect_equal(r$summary$total_value_diffs, 0L)
})

# ── 2. Result class ───────────────────────────────────────────────────────────
test_that("recon() returns a rDataRecon_result object", {
  r <- recon(base_df, base_df, id = "id")
  expect_s3_class(r, "rDataRecon_result")
})

# ── 3. Numeric difference detected ───────────────────────────────────────────
test_that("numeric difference is detected", {
  comp <- base_df; comp$age[2] <- 99L
  r <- recon(base_df, comp, id = "id")
  expect_equal(r$verdict, "DATASETS HAVE DIFFERENCES")
  expect_equal(r$summary$total_value_diffs, 1L)
  expect_equal(get_diffs(r)$variable, "age")
})

# ── 4. Within absolute criterion treated as equal ─────────────────────────────
test_that("difference within criterion is treated as equal", {
  comp <- base_df; comp$age[2] <- 30L + 1L
  r <- recon(base_df, comp, id = "id", criterion = 2)
  expect_equal(r$verdict, "DATASETS ARE IDENTICAL")
})

# ── 5. Character difference ───────────────────────────────────────────────────
test_that("character difference is detected", {
  comp <- base_df; comp$name[1] <- "alice"
  r <- recon(base_df, comp, id = "id")
  expect_equal(r$summary$total_value_diffs, 1L)
  expect_equal(get_diffs(r)$base_value,    "Alice")
  expect_equal(get_diffs(r)$compare_value, "alice")
})

# ── 6. Relative criterion ─────────────────────────────────────────────────────
test_that("relative criterion filters small proportional differences", {
  b  <- data.frame(id = 1:2, val = c(1000, 2000))
  c1 <- data.frame(id = 1:2, val = c(1001, 2000))   # 0.1% — within 1%
  r  <- recon(b, c1, id = "id", criterion = 0.01, relative = TRUE)
  expect_equal(r$verdict, "DATASETS ARE IDENTICAL")

  c2 <- data.frame(id = 1:2, val = c(1000, 2200))   # 10% — exceeds 1%
  r2 <- recon(b, c2, id = "id", criterion = 0.01, relative = TRUE)
  expect_equal(r2$verdict, "DATASETS HAVE DIFFERENCES")
})

# ── 7. Variable renaming (base_var / compare_var) ─────────────────────────────
test_that("base_var / compare_var renaming works", {
  b <- data.frame(id = 1:3, revenue = c(100, 200, 300))
  c <- data.frame(id = 1:3, sales   = c(100, 250, 300))
  r <- recon(b, c, id = "id", base_var = "revenue", compare_var = "sales")
  expect_equal(r$summary$total_value_diffs, 1L)
})

# ── 8. Variable in BASE only ──────────────────────────────────────────────────
test_that("variable in BASE only is reported", {
  comp <- base_df[, c("id", "name")]
  r    <- recon(base_df, comp, id = "id")
  expect_true("age" %in% r$vars_base_only)
})

# ── 9. Variable in COMPARE only ──────────────────────────────────────────────
test_that("variable in COMPARE only is reported", {
  comp       <- base_df; comp$extra <- 1:5
  r          <- recon(base_df, comp, id = "id")
  expect_true("extra" %in% r$vars_compare_only)
})

# ── 10. Type mismatch ─────────────────────────────────────────────────────────
test_that("type mismatch is detected and excluded from value comparison", {
  comp     <- base_df; comp$age <- as.character(comp$age)
  r        <- recon(base_df, comp, id = "id")
  expect_equal(nrow(r$type_mismatches), 1L)
  expect_equal(r$type_mismatches$variable, "age")
})

# ── 11. Multi-column ID ───────────────────────────────────────────────────────
test_that("multi-column ID matching works", {
  b <- data.frame(site = c("A","A","B"), period = c(1,2,1),
                  score = c(10,20,30), stringsAsFactors = FALSE)
  c <- b; c$score[2] <- 99
  r <- recon(b, c, id = c("site","period"))
  expect_equal(r$summary$total_value_diffs, 1L)
})

# ── 12. Positional matching (no id) ──────────────────────────────────────────
test_that("positional matching works when id is NULL", {
  b <- data.frame(x = c(1.0, 2.0, 3.0))
  c <- data.frame(x = c(1.0, 99.0, 3.0))
  r <- recon(b, c)
  expect_equal(r$summary$total_value_diffs, 1L)
})

# ── 12b. Integer vs numeric mix is treated as comparable (not a type mismatch) ──
test_that("integer vs numeric columns are compared, not skipped as type mismatch", {
  b <- data.frame(id = 1:3, score = c(10L, 20L, 30L))   # integer
  c <- data.frame(id = 1:3, score = c(10.0, 99.0, 30.0)) # numeric/double
  r <- recon(b, c, id = "id")
  # integer vs numeric must NOT be flagged as a type mismatch
  expect_equal(nrow(r$type_mismatches), 0L)
  # the value difference must be detected
  expect_equal(r$summary$total_value_diffs, 1L)
})

# ── 13. Different row counts ──────────────────────────────────────────────────
test_that("extra observations in BASE are reported", {
  comp <- base_df[1:3, ]
  r    <- recon(base_df, comp, id = "id")
  expect_equal(r$summary$obs_base_only, 2L)
  expect_equal(r$verdict, "DATASETS HAVE DIFFERENCES")
})

# ── 14. out_data = FALSE ──────────────────────────────────────────────────────
test_that("out_data = FALSE suppresses value_diffs in result", {
  comp <- base_df; comp$age[1] <- 99L
  r <- recon(base_df, comp, id = "id", out_data = FALSE)
  expect_null(r$value_diffs)
  expect_error(get_diffs(r))
})

# ── 15. summary_df returns one row ───────────────────────────────────────────
test_that("summary_df returns a one-row data frame", {
  r   <- recon(base_df, base_df, id = "id")
  sdf <- summary_df(r)
  expect_s3_class(sdf, "data.frame")
  expect_equal(nrow(sdf), 1L)
  expect_true("verdict" %in% names(sdf))
})

# ── 16. compare_stats ─────────────────────────────────────────────────────────
test_that("compare_stats returns correct columns", {
  cs <- compare_stats(base_df, base_df)
  expect_true(all(c("variable","base_mean","compare_mean","mean_diff") %in% names(cs)))
  expect_equal(cs$mean_diff, rep(0, nrow(cs)))
})

# ── 17. is_structure_equal ────────────────────────────────────────────────────
test_that("is_structure_equal returns TRUE for matching structures", {
  df2 <- base_df[, rev(names(base_df))]
  expect_true(is_structure_equal(base_df, df2))
})

test_that("is_structure_equal returns FALSE for type mismatch", {
  df2     <- base_df; df2$age <- as.numeric(df2$age)
  expect_false(is_structure_equal(base_df, df2))
})

# ── 18. print method ──────────────────────────────────────────────────────────
test_that("print.rDataRecon_result works without error", {
  r <- recon(base_df, base_df, id = "id")
  expect_output(print(r), "rDataRecon result")
})

# ── 19. Input validation errors ───────────────────────────────────────────────
test_that("non-data-frame input raises error", {
  expect_error(recon(list(a = 1), base_df))
  expect_error(recon(base_df, matrix(1:4, 2, 2)))
})

test_that("invalid criterion raises error", {
  expect_error(recon(base_df, base_df, criterion = -1))
  expect_error(recon(base_df, base_df, criterion = "big"))
})

test_that("mismatched base_var / compare_var raises error", {
  expect_error(
    recon(base_df, base_df,
          base_var = c("age","name"), compare_var = "age")
  )
})

test_that("missing ID variable raises error", {
  expect_error(recon(base_df, base_df, id = "nonexistent"))
})

# ── 20. noequal suppresses equal vars in output ───────────────────────────────
test_that("noequal = TRUE suppresses equal-variable output", {
  comp <- base_df; comp$age[1] <- 99L
  expect_output(recon(base_df, comp, id = "id", noequal = TRUE), "suppressed")
})

# ── 21. brief mode ────────────────────────────────────────────────────────────
test_that("brief = TRUE skips Section 3 output", {
  comp <- base_df; comp$age[1] <- 99L
  out  <- capture.output(recon(base_df, comp, id = "id", brief = TRUE))
  expect_false(any(grepl("SECTION 3", out)))
  expect_true(any(grepl("SECTION 4", out)))
})

# ── 22. out writes to file ────────────────────────────────────────────────────
test_that("out argument writes report to file", {
  tmp  <- tempfile(fileext = ".txt")
  comp <- base_df; comp$age[1] <- 99L
  recon(base_df, comp, id = "id", out = tmp)
  expect_true(file.exists(tmp))
  lines <- readLines(tmp)
  expect_true(any(grepl("rDataRecon", lines)))
  unlink(tmp)
})

# ── 23. var argument restricts comparison ─────────────────────────────────────
test_that("var argument restricts which variables are compared", {
  comp <- base_df; comp$age[1] <- 99L; comp$name[2] <- "bob"
  r <- recon(base_df, comp, id = "id", var = "age")
  expect_equal(r$summary$vars_compared, 1L)
  expect_equal(r$summary$total_value_diffs, 1L)
})
