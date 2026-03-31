# =============================================================================
#  rDataRecon — ADaM Clinical Data Scenarios
#  Covers real-world pharma/biotech submission-grade use cases
#  Aligned with CDISC ADaM v1.3 standards
# =============================================================================


# =============================================================================
# SCENARIO 1:ADaM ADSL (Subject-Level Analysis Dataset)
# Use case: Programmer's ADSL v1.0 vs Biostatistics-reviewed ADSL v2.0
# Key: derivations for SAFFL, ITTFL, PPROTFL, TRT01PN, DTHFL flags
# ID: USUBJID
# =============================================================================
library(rDataRecon)
cat(
  "\n\n--- SCENARIO 1: ADaM ADSL — Subject Level: Programmer vs Biostatistics Review ---\n\n"
)

adsl_v1 <- data.frame(
  STUDYID  = rep("STUDY001", 6),
  USUBJID  = c(
    "STUDY001-001",
    "STUDY001-002",
    "STUDY001-003",
    "STUDY001-004",
    "STUDY001-005",
    "STUDY001-006"
  ),
  SUBJID   = c("001", "002", "003", "004", "005", "006"),
  TRT01P   = c(
    "Drug A 10mg",
    "Placebo",
    "Drug A 10mg",
    "Drug A 20mg",
    "Placebo",
    "Drug A 20mg"
  ),
  TRT01PN  = c(1L, 0L, 1L, 2L, 0L, 2L),
  TRT01A   = c(
    "Drug A 10mg",
    "Placebo",
    "Drug A 10mg",
    "Drug A 10mg",
    "Placebo",
    "Drug A 20mg"
  ),
  # subject 004 actual differs
  TRT01AN  = c(1L, 0L, 1L, 1L, 0L, 2L),
  ITTFL    = c("Y", "Y", "Y", "Y", "Y", "Y"),
  SAFFL    = c("Y", "Y", "Y", "Y", "Y", "Y"),
  PPROTFL  = c("Y", "Y", "N", "Y", "Y", "Y"),
  # subject 003 PP deviation
  DTHFL    = c("N", "N", "N", "N", "N", "N"),
  AGE      = c(45L, 62L, 55L, 48L, 71L, 39L),
  AGEGR1   = c("18-64", ">=65", "18-64", "18-64", ">=65", "18-64"),
  SEX      = c("M", "F", "F", "M", "F", "M"),
  WEIGHTBL = c(72.4, 65.1, 80.2, 91.5, 58.3, 77.0),
  HEIGHTBL = c(175, 162, 168, 182, 155, 178),
  BMIBL    = c(23.6, 24.8, 28.4, 27.6, 24.3, 24.3),
  stringsAsFactors = FALSE
)

# Biostatistics review corrections
adsl_v2 <- adsl_v1
adsl_v2$SAFFL[3]    <- "N"      # subject 003 never received study drug
adsl_v2$PPROTFL[4]  <- "N"      # missed visit identified in audit
adsl_v2$DTHFL[5]    <- "Y"      # post-study death reported
adsl_v2$AGEGR1[1]   <- "18-64"  # unchanged — verify
adsl_v2$BMIBL[2]    <- 24.9     # weight re-measured at baseline visit

recon(
  base     = adsl_v1,
  compare  = adsl_v2,
  id       = "USUBJID",
  noequal  = TRUE,
  listall  = FALSE
)

r6 <- recon(adsl_v1,
            adsl_v2,
            id = "USUBJID",
            brief = TRUE,
            noequal = TRUE)
cat(
  sprintf(
    "\n  [ADSL QC] %d flag(s) changed — SAFFL/PPROTFL/DTHFL impact population counts!\n",
    r6$summary$total_value_diffs
  )
)

# =============================================================================
# SCENARIO 2:ADaM ADAE (Adverse Event Analysis Dataset)
# Use case: ADAE v1 (pre-medical review) vs ADAE v2 (post-medical review)
# Critical: TRTEMFL, AESEVN, AREL (treatment-emergent, severity grade, relatedness)
# ID: USUBJID + AESEQ
# =============================================================================
cat("\n\n--- SCENARIO 2: ADaM ADAE — AE Analysis Dataset: Pre vs Post Medical Review ---\n\n")

adae_v1 <- data.frame(
  STUDYID  = rep("STUDY001", 7),
  USUBJID  = c(
    "STUDY001-001",
    "STUDY001-001",
    "STUDY001-002",
    "STUDY001-003",
    "STUDY001-004",
    "STUDY001-005",
    "STUDY001-006"
  ),
  AESEQ    = c(1L, 2L, 1L, 1L, 1L, 1L, 1L),
  AEDECOD  = c(
    "HEADACHE",
    "NAUSEA",
    "FATIGUE",
    "DIZZINESS",
    "RASH",
    "VOMITING",
    "BACK PAIN"
  ),
  AEBODSYS = c(
    "NERVOUS SYSTEM DISORDERS",
    "GASTROINTESTINAL DISORDERS",
    "GENERAL DISORDERS",
    "NERVOUS SYSTEM DISORDERS",
    "SKIN DISORDERS",
    "GASTROINTESTINAL DISORDERS",
    "MUSCULOSKELETAL DISORDERS"
  ),
  AESEV    = c("MILD", "MODERATE", "MILD", "SEVERE", "MILD", "MODERATE", "MILD"),
  AESEVN   = c(1L, 2L, 1L, 3L, 1L, 2L, 1L),
  AREL     = c("Y", "Y", "N", "Y", "Y", "Y", "N"),
  TRTEMFL  = c("Y", "Y", "Y", "Y", "Y", "Y", "Y"),
  AESER    = c("N", "N", "N", "Y", "N", "N", "N"),
  AESDTH   = c("N", "N", "N", "N", "N", "N", "N"),
  ANL01FL  = c("Y", "Y", "Y", "Y", "Y", "Y", "Y"),
  stringsAsFactors = FALSE
)

adae_v2 <- adae_v1
adae_v2$AESEV[3]   <- "MODERATE"   # medical monitor upgraded severity
adae_v2$AESEVN[3]  <- 2L           # numeric grade must match
adae_v2$AREL[5]    <- "N"          # relatedness revised after unblinding
adae_v2$AESDTH[4]  <- "Y"          # subject 003 died — SAE update
adae_v2$AESER[4]   <- "Y"          # already Y — confirm no change
adae_v2$ANL01FL[6] <- "N"          # excluded from analysis population

recon(
  base    = adae_v1,
  compare = adae_v2,
  id      = c("USUBJID", "AESEQ"),
  noequal = TRUE
)

r7 <- recon(
  adae_v1,
  adae_v2,
  id = c("USUBJID", "AESEQ"),
  noequal = TRUE,
  brief = TRUE
)
diffs7 <- get_diffs(r7)
death_rows <- diffs7[diffs7$variable == "AESDTH" &
                       diffs7$compare_value == "Y", ]
if (nrow(death_rows) > 0) {
  cat("\n  *** SAE DEATH flag changed — regulatory notification may be required! ***\n")
}

# =============================================================================
# SCENARIO 3:ADaM ADLB (Laboratory Analysis Dataset)
# Use case: Validate ADLB derivations — BASE, CHG, PCHG, BNRIND, ANRIND
# Tolerance: 1e-4 for percentage change (floating-point precision)
# ID: USUBJID + PARAMCD + VISITNUM + ADT
# =============================================================================
cat("\n\n--- SCENARIO 3: ADaM ADLB — Lab Analysis: Derivation QC (CHG, PCHG, Flags) ---\n\n")

adlb_derived <- data.frame(
  USUBJID  = rep(c("STUDY001-001", "STUDY001-002"), each = 4),
  PARAMCD  = rep(c("HGB", "WBC"), 4),
  ADT      = rep(c(
    "2023-03-01", "2023-03-01", "2023-06-01", "2023-06-01"
  ), 2),
  VISITNUM = rep(c(1, 1, 3, 3), 2),
  AVAL     = c(13.5, 6.2, 12.8, 7.0, 12.1, 8.4, 11.5, 9.2),
  BASE     = c(13.5, 6.2, 13.5, 6.2, 12.1, 8.4, 12.1, 8.4),
  CHG      = c(0.0, 0.0, -0.7, 0.8, 0.0, 0.0, -0.6, 0.8),
  PCHG     = c(0.00, 0.00, -5.19, 12.90, 0.00, 0.00, -4.96, 9.52),
  ABLFL    = c("Y", "Y", "N", "N", "Y", "Y", "N", "N"),
  ANRLO    = rep(c(12.0, 4.0), 4),
  ANRHI    = rep(c(17.5, 11.0), 4),
  ANRIND   = c(
    "NORMAL",
    "NORMAL",
    "NORMAL",
    "NORMAL",
    "NORMAL",
    "NORMAL",
    "LOW",
    "NORMAL"
  ),
  BNRIND   = c(
    "NORMAL",
    "NORMAL",
    "NORMAL",
    "NORMAL",
    "NORMAL",
    "NORMAL",
    "NORMAL",
    "NORMAL"
  ),
  stringsAsFactors = FALSE
)

# QC re-derivation: programmer recalculated with corrected formula
adlb_qc <- adlb_derived
adlb_qc$PCHG[3]   <- -5.185        # more precise calculation
adlb_qc$PCHG[8]   <- 9.524         # corrected rounding
adlb_qc$ANRIND[7] <- "NORMAL"      # correction: 11.5 g/dL is within normal range
adlb_qc$CHG[5]    <- 0.0           # baseline record: CHG should be 0 not recalculated

recon(
  base      = adlb_derived,
  compare   = adlb_qc,
  id        = c("USUBJID", "PARAMCD", "VISITNUM"),
  criterion = 1e-4,
  # floating-point tolerance for % change
  noequal   = TRUE
)


# =============================================================================
# SCENARIO 4:ADaM ADTTE (Time-to-Event Analysis Dataset)
# Use case: Primary endpoint — ADTTE for PFS (Progression-Free Survival)
# Validate AVAL (event time), CNSR (censoring), EVNTDESC between analysis runs
# ID: USUBJID + PARAMCD
# =============================================================================
cat("\n\n--- SCENARIO 4: ADaM ADTTE — Time-to-Event: PFS Endpoint QC ---\n\n")

adtte_v1 <- data.frame(
  STUDYID  = rep("STUDY001", 8),
  USUBJID  = rep(
    c(
      "STUDY001-001",
      "STUDY001-002",
      "STUDY001-003",
      "STUDY001-004"
    ),
    each = 2
  ),
  PARAMCD  = rep(c("PFS", "OS"), 4),
  PARAM    = rep(c(
    "Progression-Free Survival", "Overall Survival"
  ), 4),
  AVAL     = c(185, 220, 92, 310, 241, 241, 160, 188),
  AVALU    = rep("DAYS", 8),
  CNSR     = c(0L, 1L, 1L, 1L, 0L, 0L, 1L, 0L),
  EVNTDESC = c(
    "PD",
    "CENSORED-LAST CONTACT",
    "CENSORED-LAST CONTACT",
    "CENSORED-LAST CONTACT",
    "PD",
    "DEATH",
    "CENSORED-LAST CONTACT",
    "DEATH"
  ),
  STARTDT  = rep("2023-01-10", 8),
  ADT      = c(
    "2023-07-14",
    "2023-08-17",
    "2023-04-12",
    "2023-11-15",
    "2023-09-08",
    "2023-09-08",
    "2023-06-18",
    "2023-07-16"
  ),
  stringsAsFactors = FALSE
)

# Updated analysis: progression date confirmed from scan reads
adtte_v2 <- adtte_v1
adtte_v2$AVAL[1]     <- 190         # PFS extended by 5 days after central read confirmation
adtte_v2$CNSR[3]     <- 0L          # censored → event: progression confirmed
adtte_v2$EVNTDESC[3] <- "PD"        # consistent with CNSR change
adtte_v2$AVAL[3]     <- 95          # updated event time

recon(
  base    = adtte_v1,
  compare = adtte_v2,
  id      = c("USUBJID", "PARAMCD"),
  noequal = TRUE
)

r9 <- recon(
  adtte_v1,
  adtte_v2,
  id = c("USUBJID", "PARAMCD"),
  noequal = TRUE,
  brief = TRUE
)
cat(
  sprintf(
    "\n  [ADTTE QC] Primary endpoint (PFS) changes: %d — impact KM analysis!\n",
    r9$summary$total_value_diffs
  )
)


# =============================================================================
# SCENARIO 5:ADaM BDS / ADCFB (Change from Baseline — Custom BDS)
# Use case: QoL / PRO endpoint — ADCFB comparing scoring algorithm v1 vs v2
# Variable rename: old score name 'EQ5D_SCR' vs new 'EQ5DSCR' after SAP update
# Tolerance: 0.01 (scoring algorithm rounding)
# ID: USUBJID + PARAMCD + VISITNUM
# =============================================================================
cat("\n\n--- SCENARIO 5: ADaM BDS — QoL/PRO: Scoring Algorithm v1 vs v2 (SAP update) ---\n\n")

adcfb_v1 <- data.frame(
  USUBJID  = rep(c(
    "STUDY001-001", "STUDY001-002", "STUDY001-003"
  ), each = 3),
  PARAMCD  = rep(c("EQ5DVAS", "EQ5DIND", "PGIC"), 3),
  VISITNUM = rep(c(1, 3, 5), 3),
  AVAL     = c(65.0, 70.0, 75.0, 0.72, 0.78, 0.81, 3.0, 2.0, 1.0),
  BASE     = c(65.0, 65.0, 65.0, 0.72, 0.72, 0.72, 3.0, 3.0, 3.0),
  CHG      = c(0.0, 5.0, 10.0, 0.00, 0.06, 0.09, 0.0, -1.0, -2.0),
  PCHG     = c(0.00, 7.69, 15.38, 0.00, 8.33, 12.50, 0.00, -33.33, -66.67),
  ABLFL    = c("Y", "N", "N", "Y", "N", "N", "Y", "N", "N"),
  ANL01FL  = rep("Y", 9),
  stringsAsFactors = FALSE
)

# v2: SAP updated EQ-5D index scoring to crosswalk algorithm
adcfb_v2 <- adcfb_v1
adcfb_v2$AVAL[c(4, 5, 6)]  <- c(0.731, 0.792, 0.823)   # new crosswalk values
adcfb_v2$CHG[c(5, 6)]     <- c(0.061, 0.092)
adcfb_v2$PCHG[c(5, 6)]    <- c(8.48, 12.76)
adcfb_v2$ABLFL[7]        <- NA              # PGIC baseline not applicable per v2 SAP

recon(
  base      = adcfb_v1,
  compare   = adcfb_v2,
  id        = c("USUBJID", "PARAMCD", "VISITNUM"),
  criterion = 0.01,
  noequal   = TRUE
)

# ── Numeric stats comparison for PRO scores ──────────────────────────────────
cat("\n  --- Side-by-side numeric summary (AVAL, CHG, PCHG) ---\n\n")
stats10 <- compare_stats(adcfb_v1, adcfb_v2, vars = c("AVAL", "CHG", "PCHG"))
print(stats10)

# =============================================================================
# FINAL SUMMARY TABLE
# =============================================================================
cat("\n\n", strrep("=", 72), "\n")
cat("  ADAM SCENARIOS — SUMMARY\n")
cat(strrep("=", 72), "\n")
cat(sprintf("  %-4s  %-12s  %-32s  %s\n",
            "No.", "Domain", "Use Case", "Result"))
cat(sprintf("  %-4s  %-12s  %-32s  %s\n",
            strrep("-",4), strrep("-",12), strrep("-",32), strrep("-",22)))

scenarios <- list(
  list("01","ADaM ADSL","Programmer vs Biostatistics Review ",
       adsl_v1,adsl_v2,c("USUBJID")),
  list("02","ADaM ADAE","Pre vs Post Medical Review",
       adae_v1,adae_v2,c("USUBJID", "AESEQ")),
  list("03","ADaM ADLB","Derivation QC (CHG, PCHG, Flags)",
       adlb_derived,adlb_qc,c("USUBJID", "PARAMCD", "VISITNUM")),
  list("04","ADaM ADTTE","Time-to-Event: PFS Endpoint QC",
       adtte_v1,adtte_v2,c("USUBJID", "PARAMCD"))
)

for (s in scenarios) {
  r <- recon(s[[4]], s[[5]], id = s[[6]], brief = TRUE, noequal = TRUE)
  verdict_short <- ifelse(r$verdict == "DATASETS ARE IDENTICAL",
                          "IDENTICAL", "HAS DIFFERENCES")
  cat(sprintf("  %-4s  %-12s  %-32s  %s (%d diff)\n",
              s[[1]], s[[2]], s[[3]],
              verdict_short, r$summary$total_value_diffs))
}
cat(strrep("=", 72), "\n\n")