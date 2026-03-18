# =============================================================================
#  rDataRecon — Safe file installer
#  Run this script ONCE from RStudio after downloading the rDataRecon_cran/
#  folder. It copies every source file into the correct package location
#  with guaranteed UTF-8 encoding, preventing the "embedded nul" error
#  caused by Windows saving files as UTF-16.
#
#  Usage:
#    1. Set PKG_ROOT below to the folder where you want the package to live.
#    2. Set DOWNLOAD_DIR to where you unzipped/saved rDataRecon_cran/.
#    3. Run: source("install_files.R")
# =============================================================================

PKG_ROOT     <- "C:/Users/ganes/Documents/Kriyababa/GitHub/rDataRecon"           # change this to your desired path
DOWNLOAD_DIR <- "C:/Users/ganes/Documents/Kriyababa/GitHub/MyPractice/R_Packages/rDataRecon/rDataRecon_cran"  # change to where you saved the files

# ── Helper: read + re-encode + write ─────────────────────────────────────────
copy_utf8 <- function(src, dst) {
  # readLines with encoding="UTF-8" strips any BOM and handles UTF-16 safely
  lines <- readLines(src, encoding = "UTF-8", warn = FALSE)
  dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, con = dst, useBytes = FALSE)
  message("  Copied: ", basename(dst))
}

# ── Create package directory structure ───────────────────────────────────────
dirs <- c(
  file.path(PKG_ROOT, "R"),
  file.path(PKG_ROOT, "man"),
  file.path(PKG_ROOT, "tests", "testthat"),
  file.path(PKG_ROOT, "vignettes"),
  file.path(PKG_ROOT, "inst", "doc"),
  file.path(PKG_ROOT, ".github", "workflows")
)
invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
message("Package directories created under: ", PKG_ROOT)

# ── Root files ────────────────────────────────────────────────────────────────
root_files <- c("DESCRIPTION", "NAMESPACE", "LICENSE", "LICENSE.md",
                "NEWS.md", "README.md", ".Rbuildignore", "cran-comments.md")
for (f in root_files) {
  src <- file.path(DOWNLOAD_DIR, f)
  if (file.exists(src))
    copy_utf8(src, file.path(PKG_ROOT, f))
  else
    message("  SKIPPED (not found): ", f)
}

# ── R/ source files ───────────────────────────────────────────────────────────
r_files <- c("recon.R", "utils.R", "rDataRecon-package.R")
for (f in r_files) {
  src <- file.path(DOWNLOAD_DIR, "R", f)
  if (file.exists(src))
    copy_utf8(src, file.path(PKG_ROOT, "R", f))
  else
    message("  SKIPPED (not found): R/", f)
}

# ── Test files ────────────────────────────────────────────────────────────────
copy_utf8(file.path(DOWNLOAD_DIR, "tests", "testthat.R"),
          file.path(PKG_ROOT, "tests", "testthat.R"))
copy_utf8(file.path(DOWNLOAD_DIR, "tests", "test_all.R"),
          file.path(PKG_ROOT, "tests", "test_all.R"))
copy_utf8(file.path(DOWNLOAD_DIR, "tests", "testthat", "test-recon.R"),
          file.path(PKG_ROOT, "tests", "testthat", "test-recon.R"))

# ── Vignette ──────────────────────────────────────────────────────────────────
copy_utf8(file.path(DOWNLOAD_DIR, "vignettes", "introduction.Rmd"),
          file.path(PKG_ROOT, "vignettes", "introduction.Rmd"))

# ── CI workflow ───────────────────────────────────────────────────────────────
copy_utf8(file.path(DOWNLOAD_DIR, ".github", "workflows", "R-CMD-check.yaml"),
          file.path(PKG_ROOT, ".github", "workflows", "R-CMD-check.yaml"))

message("\n=== All files installed. Now run in RStudio: ===")
message("  setwd('", PKG_ROOT, "')")
message("  devtools::document()")
message("  devtools::test()")
message("  devtools::check()")
