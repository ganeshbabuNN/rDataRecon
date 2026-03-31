# =============================================================================
#  rDataRecon — SDTM Clinical Data Scenarios
#  Covers real-world pharma/biotech submission-grade use cases
#  Aligned with CDISC SDTM v1.8
# =============================================================================
library(rDataRecon)

cat(strrep("=", 72), "\n")
cat("  rDataRecon: SDTM & ADaM Dataset Comparison Scenarios\n")
cat("  CDISC-aligned | Submission-grade | FDA/EMA best practices\n")
cat(strrep("=", 72), "\n")

# =============================================================================
# SCENARIO 1 — SDTM DM (Demographics)
# Use case: Compare DM dataset between data cut 1 and data cut 2
# ID: USUBJID
# Key check: RFSTDTC, ARMCD, ACTARMCD changes after cut
# =============================================================================
cat("\n\n--- SCENARIO 1: SDTM DM — Demographics: Data Cut 1 vs Data Cut 2 ---\n\n")

dm_cut1 <- data.frame(
  STUDYID  = rep("STUDY001", 6),
  DOMAIN   = rep("DM", 6),
  USUBJID  = c(
    "STUDY001-001",
    "STUDY001-002",
    "STUDY001-003",
    "STUDY001-004",
    "STUDY001-005",
    "STUDY001-006"
  ),
  SUBJID   = c("001", "002", "003", "004", "005", "006"),
  RFSTDTC  = c(
    "2023-01-10",
    "2023-01-15",
    "2023-01-20",
    "2023-02-01",
    "2023-02-10",
    "2023-02-15"
  ),
  RFENDTC  = c(
    "2023-07-10",
    "2023-07-15",
    NA,
    "2023-08-01",
    "2023-08-10",
    "2023-08-15"
  ),
  ARMCD    = c("TRT A", "PLBO", "TRT A", "TRT B", "PLBO", "TRT B"),
  ACTARMCD = c("TRT A", "PLBO", "TRT A", "TRT B", "PLBO", "TRT B"),
  AGE      = c(45L, 62L, 55L, 48L, 71L, 39L),
  SEX      = c("M", "F", "F", "M", "F", "M"),
  RACE     = c("WHITE", "ASIAN", "BLACK", "WHITE", "WHITE", "HISPANIC"),
  COUNTRY  = rep("USA", 6),
  stringsAsFactors = FALSE
)

# Cut 2: RFENDTC populated for subject 003, one ACTARMCD corrected
dm_cut2 <- dm_cut1
dm_cut2$RFENDTC[3]  <- "2023-07-22"   # study completion date populated
dm_cut2$ACTARMCD[4] <- "TRT A"        # protocol deviation — actual arm corrected
dm_cut2$AGE[6]      <- 40L            # age at screening corrected via query

recon(
  base     = dm_cut1,
  compare  = dm_cut2,
  id       = "USUBJID",
  noequal  = TRUE,
  out      = "sdtm_dm_comparison.txt"
)

r1 <- recon(dm_cut1,
            dm_cut2,
            id = "USUBJID",
            noequal = TRUE,
            brief = TRUE)
cat(
  sprintf(
    "\n  [QC RESULT] DM changes: %d value difference(s) across %d variable(s)\n",
    r1$summary$total_value_diffs,
    r1$summary$vars_with_diffs
  )
)

# =============================================================================
# SCENARIO 2 — SDTM AE (Adverse Events)
# Use case: Sponsor database vs CRO database reconciliation
# ID: USUBJID + AESEQ
# Key check: AESEV, AEREL, AEOUT grade changes after medical review
# =============================================================================
cat("\n\n--- SCENARIO 2: SDTM AE — Adverse Events: Sponsor vs CRO Reconciliation ---\n\n")

ae_sponsor <- data.frame(
  STUDYID  = rep("STUDY001", 7),
  DOMAIN   = rep("AE", 7),
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
  AETERM   = c(
    "Headache",
    "Nausea",
    "Fatigue",
    "Dizziness",
    "Rash",
    "Vomiting",
    "Back Pain"
  ),
  AEDECOD  = c(
    "HEADACHE",
    "NAUSEA",
    "FATIGUE",
    "DIZZINESS",
    "RASH",
    "VOMITING",
    "BACK PAIN"
  ),
  AESEV    = c("MILD", "MODERATE", "MILD", "SEVERE", "MILD", "MODERATE", "MILD"),
  AEREL    = c(
    "RELATED",
    "RELATED",
    "NOT RELATED",
    "RELATED",
    "POSSIBLY RELATED",
    "RELATED",
    "NOT RELATED"
  ),
  AEOUT    = c(
    "RECOVERED",
    "RECOVERING",
    "RECOVERED",
    "NOT RECOVERED",
    "RECOVERED",
    "RECOVERED",
    "RECOVERED"
  ),
  AESLIFE  = c("N", "N", "N", "Y", "N", "N", "N"),
  AEACN    = c(
    "DOSE REDUCED",
    "NONE",
    "NONE",
    "DRUG WITHDRAWN",
    "NONE",
    "NONE",
    "NONE"
  ),
  stringsAsFactors = FALSE
)

ae_cro <- ae_sponsor
ae_cro$AESEV[3]   <- "MODERATE"       # CRO graded higher
ae_cro$AEREL[5]   <- "RELATED"        # CRO medical reviewer changed relatedness
ae_cro$AEOUT[4]   <- "FATAL"          # SAE outcome update — critical discrepancy!
ae_cro$AESLIFE[4] <- "Y"              # consistent with FATAL update

recon(
  base    = ae_sponsor,
  compare = ae_cro,
  id      = c("USUBJID", "AESEQ"),
  noequal = TRUE,
  out      = "sdtm_ae_comparison.txt"
)

r2 <- recon(
  ae_sponsor,
  ae_cro,
  id = c("USUBJID", "AESEQ"),
  noequal = TRUE,
  brief = TRUE
)
diffs2 <- get_diffs(r2)
fatal_chg <- diffs2[diffs2$compare_value == "FATAL", ]
if (nrow(fatal_chg) > 0) {
  cat(
    "\n  *** CRITICAL: FATAL outcome discrepancy detected — expedited review required! ***\n"
  )
  print(fatal_chg)
}

# =============================================================================
# SCENARIO 3 — SDTM LB (Laboratory)
# Use case: Raw lab data (central lab) vs SDTM-mapped lab dataset
# ID: USUBJID + LBSEQ
# Tolerance: 0.001 for numeric results (instrument precision)
# =============================================================================
cat("\n\n--- SCENARIO 3: SDTM LB — Lab Data: Central Lab Extract vs SDTM Mapped ---\n\n")

lb_raw <- data.frame(
  STUDYID  = rep("STUDY001", 8),
  DOMAIN   = rep("LB", 8),
  USUBJID  = c(
    "STUDY001-001",
    "STUDY001-001",
    "STUDY001-002",
    "STUDY001-002",
    "STUDY001-003",
    "STUDY001-003",
    "STUDY001-004",
    "STUDY001-004"
  ),
  LBSEQ    = c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L),
  LBTESTCD = c("HGB", "WBC", "HGB", "WBC", "HGB", "WBC", "HGB", "WBC"),
  LBTEST   = c(
    "Hemoglobin",
    "White Blood Cell",
    "Hemoglobin",
    "White Blood Cell",
    "Hemoglobin",
    "White Blood Cell",
    "Hemoglobin",
    "White Blood Cell"
  ),
  LBORRES  = c("13.5", "6.2", "12.1", "8.4", "14.0", "5.9", "11.8", "9.1"),
  LBORRESU = c(
    "g/dL",
    "10^9/L",
    "g/dL",
    "10^9/L",
    "g/dL",
    "10^9/L",
    "g/dL",
    "10^9/L"
  ),
  LBSTRESN = c(13.5, 6.2, 12.1, 8.4, 14.0, 5.9, 11.8, 9.1),
  LBSTRESU = c(
    "g/dL",
    "10^9/L",
    "g/dL",
    "10^9/L",
    "g/dL",
    "10^9/L",
    "g/dL",
    "10^9/L"
  ),
  LBNRLO   = c(12.0, 4.0, 12.0, 4.0, 12.0, 4.0, 12.0, 4.0),
  LBNRHI   = c(17.5, 11.0, 17.5, 11.0, 17.5, 11.0, 17.5, 11.0),
  LBNRIND  = c(
    "NORMAL",
    "NORMAL",
    "NORMAL",
    "NORMAL",
    "NORMAL",
    "NORMAL",
    "LOW",
    "NORMAL"
  ),
  stringsAsFactors = FALSE
)

lb_sdtm <- lb_raw
lb_sdtm$LBSTRESN[2] <- 6.201         # instrument rounding — within tolerance
lb_sdtm$LBSTRESN[6] <- 6.0           # ETL mapping error — outside tolerance
lb_sdtm$LBNRIND[7]  <- "NORMAL"      # incorrect normal range flag after mapping
lb_sdtm$LBORRES[8]  <- "9.10"        # trailing zero — string difference

recon(
  base      = lb_raw,
  compare   = lb_sdtm,
  id        = c("USUBJID", "LBSEQ"),
  criterion = 0.001,
  # lab instrument precision tolerance
  noequal   = TRUE
)


# =============================================================================
# SCENARIO 4 — SDTM CM (Concomitant Medications)
# Use case: Site-level eCRF data vs centrally coded CM dataset (WHODrug)
# ID: USUBJID + CMSEQ
# Key check: CMDECOD, CMCLAS after coding dictionary application
# =============================================================================
cat("\n\n--- SCENARIO 4: SDTM CM — Conmed: eCRF vs WHODrug Coded Dataset ---\n\n")

cm_ecrf <- data.frame(
  STUDYID = rep("STUDY001", 6),
  DOMAIN  = rep("CM", 6),
  USUBJID = c(
    "STUDY001-001",
    "STUDY001-002",
    "STUDY001-003",
    "STUDY001-004",
    "STUDY001-005",
    "STUDY001-006"
  ),
  CMSEQ   = rep(1L, 6),
  CMTRT   = c(
    "ASPIRIN",
    "LISINOPRIL",
    "METFORMIN",
    "ATORVASTATIN",
    "OMEPRAZOLE",
    "IBUPROFEN"
  ),
  CMDECOD = c(
    "ASPIRIN",
    "LISINOPRIL",
    "METFORMIN",
    "ATORVASTATIN",
    "OMEPRAZOLE",
    "IBUPROFEN"
  ),
  CMCLAS  = c(
    "ANALGESICS",
    "ANTIHYPERTENSIVES",
    "ANTIDIABETICS",
    "LIPID MODIFYING",
    "ANTACIDS",
    "ANALGESICS"
  ),
  CMCLASCD = c("N02BA", "C09AA", "A10BA", "C10AA", "A02BC", "M01AE"),
  CMDOSE  = c(100, 10, 500, 20, 20, 400),
  CMDOSU  = rep("mg", 6),
  CMROUTE = rep("ORAL", 6),
  stringsAsFactors = FALSE
)

cm_coded <- cm_ecrf
cm_coded$CMDECOD[1] <- "ACETYLSALICYLIC ACID"   # WHODrug preferred name
cm_coded$CMCLAS[5]  <- "PROTON PUMP INHIBITORS"  # more specific class after coding
cm_coded$CMCLASCD[5] <- "A02BC01"                 # ATC code corrected

recon(
  base    = cm_ecrf,
  compare = cm_coded,
  id      = c("USUBJID", "CMSEQ"),
  var     = c("CMDECOD", "CMCLAS", "CMCLASCD"),
  # focus on coding vars only
  noequal = TRUE
)

# =============================================================================
# SCENARIO 5 — SDTM VS (Vital Signs)
# Use case: Visit 1 baseline vs Visit 6 (Week 24) within-study change
#           Validate derivation of VSBLFL and EPOCH
# ID: USUBJID + VSTESTCD + VISITNUM
# =============================================================================
cat("\n\n--- SCENARIO 5: SDTM VS — Vital Signs: Baseline vs Week 24 Derivation QC ---\n\n")

vs_v1 <- data.frame(
  STUDYID  = rep("STUDY001", 8),
  DOMAIN   = rep("VS", 8),
  USUBJID  = rep(c("STUDY001-001", "STUDY001-002"), each = 4),
  VSTESTCD = rep(c("SYSBP", "DIABP", "PULSE", "TEMP"), 2),
  VSTEST   = rep(
    c(
      "Systolic Blood Pressure",
      "Diastolic Blood Pressure",
      "Pulse Rate",
      "Temperature"
    ),
    2
  ),
  VISITNUM = rep(1, 8),
  VISIT    = rep("WEEK 1 - BASELINE", 8),
  VSSTRESN = c(128, 82, 72, 36.8, 135, 88, 68, 37.0),
  VSSTRESU = c(
    "mmHg",
    "mmHg",
    "beats/min",
    "C",
    "mmHg",
    "mmHg",
    "beats/min",
    "C"
  ),
  VSBLFL   = rep("Y", 8),
  EPOCH    = rep("TREATMENT", 8),
  stringsAsFactors = FALSE
)

# QC team found EPOCH should be SCREENING for VISITNUM=1
vs_v1_qc <- vs_v1
vs_v1_qc$EPOCH <- rep("SCREENING", 8)    # correction: Visit 1 is SCREENING not TREATMENT
vs_v1_qc$VSBLFL[c(5, 6)] <- NA            # subjects 002 missing VSBLFL — query issued

recon(
  base    = vs_v1,
  compare = vs_v1_qc,
  id      = c("USUBJID", "VSTESTCD"),
  noequal = TRUE
)

# =============================================================================
# SCENARIO 6 —SDTM EX (Exposure)
# Use case: Planned dose vs actual dose administered
# Real-world problem: Protocol deviations where subjects received wrong dose
# ID: USUBJID + EXSEQ
# Key vars: EXDOSE, EXDOSU, EXROUTE, EXSTDTC, EXENDTC
# =============================================================================
cat("\n\n--- SCENARIO 6: SDTM EX — Exposure: Planned vs Actual Dose ---\n\n")

ex_planned <- data.frame(
  STUDYID = rep("DR001", 6),
  DOMAIN  = rep("EX", 6),
  USUBJID = c("DR001-001","DR001-002","DR001-003",
              "DR001-004","DR001-005","DR001-006"),
  EXSEQ   = rep(1L, 6),
  EXTRT   = rep("DRUG A", 6),
  EXDOSE  = c(10, 10, 10, 20, 20, 20),
  EXDOSU  = rep("mg", 6),
  EXDOSFRM= rep("TABLET", 6),
  EXROUTE = rep("ORAL", 6),
  EXSTDTC = c("2024-01-10","2024-01-11","2024-01-12",
              "2024-01-10","2024-01-11","2024-01-12"),
  EXENDTC = c("2024-03-10","2024-03-11","2024-03-12",
              "2024-03-10","2024-03-11","2024-03-12"),
  EPOCH   = rep("TREATMENT", 6),
  stringsAsFactors = FALSE
)

ex_actual <- ex_planned
ex_actual$EXDOSE[2]   <- 5           # subject 002 received half dose — protocol deviation
ex_actual$EXROUTE[4]  <- "INTRAVENOUS" # wrong route of administration
ex_actual$EXENDTC[6]  <- "2024-02-28"  # early discontinuation
ex_actual$EPOCH[3]    <- "FOLLOW-UP"   # incorrectly recorded epoch

recon(
  base    = ex_planned,
  compare = ex_actual,
  id      = c("USUBJID", "EXSEQ"),
  noequal = TRUE,
  out     = "sdtm_ex_comparison.txt"
)

r1 <- recon(ex_planned, ex_actual, id = c("USUBJID","EXSEQ"),
            noequal = TRUE, brief = TRUE)
cat(sprintf("\n  [EX QC] %d protocol deviation(s) detected in exposure data\n",
            r1$summary$total_value_diffs))


# =============================================================================
# SCENARIO 7—SDTM MH (Medical History)
# Use case: Baseline medical history — eCRF entry vs medical coding (MedDRA)
# ID: USUBJID + MHSEQ
# Key vars: MHDECOD, MHBODSYS, MHSTDTC, MHONGO
# =============================================================================
cat("\n\n--- SCENARIO 7: SDTM MH — Medical History: eCRF vs MedDRA Coded ---\n\n")

mh_ecrf <- data.frame(
  STUDYID = rep("DR001", 6),
  DOMAIN  = rep("MH", 6),
  USUBJID = c("DR001-001","DR001-002","DR001-003",
              "DR001-004","DR001-005","DR001-006"),
  MHSEQ   = rep(1L, 6),
  MHTERM  = c("High blood pressure","Type 2 diabetes",
              "Asthma","Heart attack","Kidney stones","Depression"),
  MHDECOD = c("High blood pressure","Type 2 diabetes",
              "Asthma","Heart attack","Kidney stones","Depression"),
  MHBODSYS= c("Cardiac","Endocrine","Respiratory","Cardiac",
              "Renal","Psychiatric"),
  MHSTDTC = c("2020-03","2018-01","2015-06",
              "2022-11","2019-08","2021-04"),
  MHONGO  = c("Y","Y","Y","N","N","Y"),
  stringsAsFactors = FALSE
)

mh_coded <- mh_ecrf
# MedDRA preferred terms applied after coding
mh_coded$MHDECOD[1] <- "HYPERTENSION"
mh_coded$MHDECOD[2] <- "TYPE 2 DIABETES MELLITUS"
mh_coded$MHDECOD[4] <- "MYOCARDIAL INFARCTION"
mh_coded$MHDECOD[6] <- "MAJOR DEPRESSIVE DISORDER"
# Body system corrected by medical coder
mh_coded$MHBODSYS[1] <- "Vascular disorders"
mh_coded$MHBODSYS[4] <- "Cardiac disorders"
mh_coded$MHBODSYS[6] <- "Psychiatric disorders"

recon(
  base    = mh_ecrf,
  compare = mh_coded,
  id      = c("USUBJID", "MHSEQ"),
  var     = c("MHDECOD", "MHBODSYS"),
  noequal = TRUE
)


# =============================================================================
# SCENARIO 8—SDTM DS (Disposition)
# Use case: Subject disposition — interim analysis cut vs final analysis cut
# ID: USUBJID + DSSEQ
# Key vars: DSDECOD, DSTERM, DSSTDTC, EPOCH
# Critical: DSDECOD = "COMPLETED" vs "WITHDRAWAL BY SUBJECT" changes ITT
# =============================================================================
cat("\n\n--- SCENARIO 8: SDTM DS — Disposition: Interim vs Final Cut ---\n\n")

ds_interim <- data.frame(
  STUDYID = rep("DR001", 7),
  DOMAIN  = rep("DS", 7),
  USUBJID = c("DR001-001","DR001-002","DR001-003","DR001-004",
              "DR001-005","DR001-006","DR001-007"),
  DSSEQ   = rep(1L, 7),
  DSTERM  = c("COMPLETED STUDY","ADVERSE EVENT","COMPLETED STUDY",
              "WITHDRAWAL BY SUBJECT","COMPLETED STUDY",
              "LOST TO FOLLOW-UP","COMPLETED STUDY"),
  DSDECOD = c("COMPLETED","ADVERSE EVENT","COMPLETED",
              "WITHDRAWAL BY SUBJECT","COMPLETED",
              "LOST TO FOLLOW-UP","COMPLETED"),
  DSSTDTC = c("2024-06-01","2024-02-14","2024-06-01",
              "2024-03-20","2024-06-01","2024-04-10","2024-06-01"),
  EPOCH   = rep("TREATMENT", 7),
  stringsAsFactors = FALSE
)

ds_final <- ds_interim
# Post-interim updates
ds_final$DSDECOD[6]  <- "COMPLETED"           # subject found — not lost
ds_final$DSTERM[6]   <- "COMPLETED STUDY"
ds_final$DSSTDTC[6]  <- "2024-06-01"
ds_final$DSDECOD[2]  <- "DEATH"               # subject 002 died post-interim
ds_final$DSTERM[2]   <- "DEATH"
ds_final$DSSTDTC[2]  <- "2024-04-02"

recon(
  base    = ds_interim,
  compare = ds_final,
  id      = c("USUBJID", "DSSEQ"),
  noequal = TRUE
)

r3 <- recon(ds_interim, ds_final, id = c("USUBJID","DSSEQ"),
            noequal = TRUE, brief = TRUE)
diffs3 <- get_diffs(r3)
death_flag <- diffs3[diffs3$compare_value == "DEATH", ]
if (nrow(death_flag) > 0) {
  cat("\n  *** DEATH disposition flag changed — regulatory safety review required ***\n")
}


# =============================================================================
# SCENARIO 9-SDTM SV (Subject Visits)
# Use case: Protocol-scheduled visits vs actual visit dates
# ID: USUBJID + VISITNUM
# Key vars: SVSTDTC, SVENDTC, VISITDY — window checking for protocol compliance
# =============================================================================
cat("\n\n--- SCENARIO 9: SDTM SV — Subject Visits: Scheduled vs Actual Dates ---\n\n")

sv_scheduled <- data.frame(
  STUDYID = rep("DR001", 10),
  DOMAIN  = rep("SV", 10),
  USUBJID = rep(c("DR001-001","DR001-002"), each = 5),
  VISITNUM= rep(c(1,2,3,4,5), 2),
  VISIT   = rep(c("SCREENING","WEEK 2","WEEK 4","WEEK 8","WEEK 12"), 2),
  SVSTDTC = c("2024-01-08","2024-01-22","2024-02-05","2024-03-04","2024-04-01",
              "2024-01-10","2024-01-24","2024-02-07","2024-03-06","2024-04-03"),
  VISITDY = rep(c(-2, 14, 28, 56, 84), 2),
  stringsAsFactors = FALSE
)

sv_actual <- sv_scheduled
# Visit window violations (>3 days off scheduled date = protocol deviation)
sv_actual$SVSTDTC[3]  <- "2024-02-12"  # subject 001 V3: 7 days late — deviation
sv_actual$SVSTDTC[8]  <- "2024-02-14"  # subject 002 V3: 7 days late — deviation
sv_actual$SVSTDTC[10] <- "2024-04-15"  # subject 002 V5: 12 days late — deviation
sv_actual$VISITDY[3]  <- 35            # study day updated accordingly
sv_actual$VISITDY[8]  <- 35
sv_actual$VISITDY[10] <- 96

recon(
  base    = sv_scheduled,
  compare = sv_actual,
  id      = c("USUBJID", "VISITNUM"),
  noequal = TRUE,
  criterion = 0   # zero tolerance for visit date compliance
)


# =============================================================================
# SCENARIO 10-SDTM TU (Tumour Identification)
# Use case: Oncology study — baseline tumour lesions
#           Central radiologist re-read vs investigator-reported measurements
# ID: USUBJID + TUSEQ
# Key vars: TULOC, TUORRES, TUSTRESC, TUMETHOD
# =============================================================================
cat("\n\n--- SCENARIO 9: SDTM TU — Tumour ID: Investigator vs Central Read ---\n\n")

tu_investigator <- data.frame(
  STUDYID  = rep("DR001", 8),
  DOMAIN   = rep("TU", 8),
  USUBJID  = rep(c("DR001-001","DR001-002"), each = 4),
  TUSEQ    = rep(1:4, 2),
  TULOC    = c("LIVER","LUNG","LYMPH NODE","LIVER",
               "SPLEEN","LUNG","KIDNEY","SPLEEN"),
  TUORRES  = c("TARGET","TARGET","NON-TARGET","TARGET",
               "TARGET","TARGET","NON-TARGET","TARGET"),
  TUSTRESC = c("TARGET","TARGET","NON-TARGET","TARGET",
               "TARGET","TARGET","NON-TARGET","TARGET"),
  TUMETHOD = rep("MRI", 8),
  stringsAsFactors = FALSE
)

tu_central <- tu_investigator
# Central radiology re-read findings differ
tu_central$TULOC[3]    <- "MEDIASTINAL LYMPH NODE"  # more precise location
tu_central$TUORRES[4]  <- "NON-TARGET"              # lesion reclassified
tu_central$TUSTRESC[4] <- "NON-TARGET"
tu_central$TUORRES[6]  <- "NON-TARGET"              # too small for target criteria
tu_central$TUSTRESC[6] <- "NON-TARGET"
tu_central$TUMETHOD[5] <- "CT"                      # different imaging modality

recon(
  base    = tu_investigator,
  compare = tu_central,
  id      = c("USUBJID", "TUSEQ"),
  noequal = TRUE
)

r5 <- recon(tu_investigator, tu_central, id = c("USUBJID","TUSEQ"),
            noequal = TRUE, brief = TRUE)
cat(sprintf("\n  [TU QC] %d tumour classification differences — impacts RECIST 1.1 response\n",
            r5$summary$total_value_diffs))


# =============================================================================
# SCENARIO 11-SDTM TR (Tumour Results)
# Use case: Tumour measurement values — investigator vs central radiology
# ID: USUBJID + TRSEQ
# Tolerance: 1mm (measurement precision)
# Key vars: TRORRES, TRORRESU, TRSTRESN — longest diameter measurements
# =============================================================================
cat("\n\n--- SCENARIO 11: SDTM TR — Tumour Results: Measurement QC (1mm tolerance) ---\n\n")

tr_inv <- data.frame(
  STUDYID  = rep("DR001", 8),
  DOMAIN   = rep("TR", 8),
  USUBJID  = rep(c("DR001-001","DR001-002"), each = 4),
  TRSEQ    = rep(1:4, 2),
  TRTESTCD = rep(c("LDIAM","LDIAM","SDIAM","LDIAM"), 2),
  TRORRES  = c("35","22","18","41","28","15","31","19"),
  TRORRESU = rep("mm", 8),
  TRSTRESN = c(35, 22, 18, 41, 28, 15, 31, 19),
  VISITNUM = rep(c(1,1,1,3), 2),
  stringsAsFactors = FALSE
)

tr_central <- tr_inv
# Central measurements differ (mm-level precision differences expected)
tr_central$TRSTRESN[1] <- 36          # 1mm diff — within tolerance
tr_central$TRSTRESN[2] <- 19          # 3mm diff — exceeds tolerance
tr_central$TRORRES[2]  <- "19"
tr_central$TRSTRESN[5] <- 31          # 3mm diff — exceeds tolerance
tr_central$TRORRES[5]  <- "31"
tr_central$TRSTRESN[7] <- 31.5        # 0.5mm — within tolerance

cat("  -- With 1mm absolute tolerance for measurement precision --\n\n")
recon(
  base      = tr_inv,
  compare   = tr_central,
  id        = c("USUBJID", "TRSEQ"),
  criterion = 1,                  # 1mm tolerance
  noequal   = TRUE
)


# =============================================================================
# SCENARIO 12-SDTM RS (Response Assessment)
# Use case: Overall response per RECIST 1.1 — investigator vs BICR
# (Blinded Independent Central Review)
# ID: USUBJID + RSSEQ
# Critical: RSORRES change from PR/SD to PD affects PFS endpoint
# =============================================================================
cat("\n\n--- SCENARIO 12: SDTM RS — Response: Investigator vs BICR Assessment ---\n\n")

rs_investigator <- data.frame(
  STUDYID  = rep("DR001", 8),
  DOMAIN   = rep("RS", 8),
  USUBJID  = rep(c("DR001-001","DR001-002",
                   "DR001-003","DR001-004"), each = 2),
  RSSEQ    = rep(1:2, 4),
  RSTESTCD = rep(c("OVRLRESP","OVRLRESP"), 4),
  RSORRES  = c("PR","PR","SD","PD","CR","PR","SD","SD"),
  RSSTRESC = c("PR","PR","SD","PD","CR","PR","SD","SD"),
  VISITNUM = rep(c(3, 5), 4),
  EPOCH    = rep("TREATMENT", 8),
  stringsAsFactors = FALSE
)

rs_bicr <- rs_investigator
# BICR assessment differs from investigator
rs_bicr$RSORRES[2]  <- "SD"    # subject 001 V5: PR → SD (less favourable)
rs_bicr$RSSTRESC[2] <- "SD"
rs_bicr$RSORRES[3]  <- "PD"    # subject 002 V3: SD → PD (progression earlier!)
rs_bicr$RSSTRESC[3] <- "PD"
rs_bicr$RSORRES[7]  <- "PD"    # subject 003 V3: SD → PD
rs_bicr$RSSTRESC[7] <- "PD"
rs_bicr$RSORRES[8]  <- "PD"    # subject 003 V5: SD → PD

recon(
  base    = rs_investigator,
  compare = rs_bicr,
  id      = c("USUBJID", "RSSEQ"),
  noequal = TRUE
)

r7 <- recon(rs_investigator, rs_bicr, id = c("USUBJID","RSSEQ"),
            noequal = TRUE, brief = TRUE)
diffs7 <- get_diffs(r7)
pd_changes <- diffs7[diffs7$compare_value == "PD" & diffs7$base_value != "PD", ]
cat(sprintf("\n  [RS QC] %d response upgraded to PD by BICR — recalculate PFS!\n",
            nrow(pd_changes)))


# =============================================================================
# SCENARIO 13-SDTM EG (ECG Test Results)
# Use case: Central ECG lab data vs site-recorded ECG values
# ID: USUBJID + EGSEQ
# Tolerance: 5ms for interval measurements (QTc, PR, QRS)
# Key vars: EGSTRESN, EGSTRESC, EGNRIND — QT prolongation safety flag
# =============================================================================
cat("\n\n--- SCENARIO 13: SDTM EG — ECG: Central Lab vs Site ECG Values ---\n\n")

eg_site <- data.frame(
  STUDYID  = rep("DR001", 10),
  DOMAIN   = rep("EG", 10),
  USUBJID  = rep(c("DR001-001","DR001-002"), each = 5),
  EGSEQ    = rep(1:5, 2),
  EGTESTCD = rep(c("QT","QTcF","PR","QRS","RR"), 2),
  EGTEST   = rep(c("QT Interval","QTcF Interval","PR Interval",
                   "QRS Duration","RR Interval"), 2),
  EGSTRESN = c(380, 415, 162, 88, 920,
               410, 430, 155, 92, 880),
  EGSTRESU = rep("ms", 10),
  EGNRIND  = c("NORMAL","NORMAL","NORMAL","NORMAL","NORMAL",
               "NORMAL","NORMAL","NORMAL","NORMAL","NORMAL"),
  stringsAsFactors = FALSE
)

eg_central <- eg_site
# Central lab measurements differ
eg_central$EGSTRESN[2]  <- 420    # 5ms — on boundary
eg_central$EGSTRESN[7]  <- 448    # QTcF now >450ms — safety threshold crossed!
eg_central$EGNRIND[7]   <- "HIGH" # EGNRIND updated
eg_central$EGSTRESN[3]  <- 168    # 6ms PR difference — exceeds 5ms tolerance
eg_central$EGSTRESN[9]  <- 96     # 4ms QRS difference — within tolerance

cat("  -- With 5ms tolerance for ECG measurement precision --\n\n")
recon(
  base      = eg_site,
  compare   = eg_central,
  id        = c("USUBJID", "EGSEQ"),
  criterion = 5,                  # 5ms ECG tolerance
  noequal   = TRUE
)

r8 <- recon(eg_site, eg_central, id = c("USUBJID","EGSEQ"),
            criterion = 5, noequal = TRUE, brief = TRUE)
diffs8 <- get_diffs(r8)
qtc_flag <- diffs8[diffs8$variable == "EGNRIND" & diffs8$compare_value == "HIGH", ]
if (nrow(qtc_flag) > 0) {
  cat("\n  *** QTcF > 450ms flagged by central lab — safety review required! ***\n")
}


# =============================================================================
# SCENARIO 14-SDTM PE (Physical Examination)
# Use case: Baseline PE — site eCRF vs medical monitor review
# ID: USUBJID + PESEQ
# Key vars: PEORRES, PENORIND — normal/abnormal flags critical for eligibility
# =============================================================================
cat("\n\n--- SCENARIO 14: SDTM PE — Physical Exam: Site vs Medical Monitor Review ---\n\n")

pe_site <- data.frame(
  STUDYID  = rep("DR001", 12),
  DOMAIN   = rep("PE", 12),
  USUBJID  = rep(c("DR001-001","DR001-002","DR001-003"), each = 4),
  PESEQ    = rep(1:4, 3),
  PETESTCD = rep(c("APPEAR","HEENT","CHEST","ABDOMEN"), 3),
  PETEST   = rep(c("General Appearance","HEENT","Chest/Respiratory",
                   "Abdomen"), 3),
  PEORRES  = c("NORMAL","NORMAL","NORMAL","NORMAL",
               "NORMAL","NORMAL","ABNORMAL","NORMAL",
               "NORMAL","NORMAL","NORMAL","NORMAL"),
  PENORIND = c("NORMAL","NORMAL","NORMAL","NORMAL",
               "NORMAL","NORMAL","ABNORMAL","NORMAL",
               "NORMAL","NORMAL","NORMAL","NORMAL"),
  stringsAsFactors = FALSE
)

pe_monitor <- pe_site
# Medical monitor review changes some findings
pe_monitor$PEORRES[3]  <- "ABNORMAL"   # subject 001 chest finding missed
pe_monitor$PENORIND[3] <- "ABNORMAL"
pe_monitor$PEORRES[7]  <- "NORMAL"     # subject 002 query resolved — normal
pe_monitor$PENORIND[7] <- "NORMAL"
pe_monitor$PEORRES[11] <- "ABNORMAL"   # subject 003 new finding documented
pe_monitor$PENORIND[11]<- "ABNORMAL"

recon(
  base    = pe_site,
  compare = pe_monitor,
  id      = c("USUBJID", "PESEQ"),
  noequal = TRUE
)


# =============================================================================
# SCENARIO 15-SDTM QS (Questionnaire / PRO)
# Use case: Patient-reported outcomes — paper CRF transcription vs eDiary
# ID: USUBJID + QSSEQ
# Tolerance: 0 (integer scores — any difference is a transcription error)
# Key vars: QSORRES, QSSTRESN — EQ-5D item scores
# =============================================================================
cat("\n\n--- SCENARIO 15: SDTM QS — Questionnaire: Paper CRF vs eDiary ---\n\n")

qs_paper <- data.frame(
  STUDYID  = rep("DR001", 15),
  DOMAIN   = rep("QS", 15),
  USUBJID  = rep(c("DR001-001","DR001-002","DR001-003"), each = 5),
  QSSEQ    = rep(1:5, 3),
  QSTESTCD = rep(c("EQ5D1","EQ5D2","EQ5D3","EQ5D4","EQ5D5"), 3),
  QSTEST   = rep(c("Mobility","Self-Care","Usual Activities",
                   "Pain/Discomfort","Anxiety/Depression"), 3),
  QSORRES  = c("1","2","1","2","3",
               "2","1","2","3","2",
               "1","1","2","1","2"),
  QSSTRESN = c(1L,2L,1L,2L,3L,
               2L,1L,2L,3L,2L,
               1L,1L,2L,1L,2L),
  VISITNUM = rep(1, 15),
  stringsAsFactors = FALSE
)

qs_ediary <- qs_paper
# Transcription errors from paper to eDiary
qs_ediary$QSORRES[2]   <- "3"    # misread 2 as 3
qs_ediary$QSSTRESN[2]  <- 3L
qs_ediary$QSORRES[8]   <- "3"    # 1 → 3 transcription error
qs_ediary$QSSTRESN[8]  <- 3L
qs_ediary$QSORRES[12]  <- "2"    # 1 → 2 transcription error
qs_ediary$QSSTRESN[12] <- 2L

cat("  -- Zero tolerance: every integer difference is a transcription error --\n\n")
recon(
  base      = qs_paper,
  compare   = qs_ediary,
  id        = c("USUBJID", "QSSEQ"),
  criterion = 0,
  noequal   = TRUE
)

r10 <- recon(qs_paper, qs_ediary, id = c("USUBJID","QSSEQ"),
             criterion = 0, noequal = TRUE, brief = TRUE)
cat(sprintf("\n  [QS QC] %d PRO transcription errors detected — source data verification required\n",
            r10$summary$total_value_diffs))


# =============================================================================
# SCENARIO 16-SDTM DM (Demographics) — Race & Ethnicity Coding
# Use case: Self-reported race/ethnicity eCRF vs CIOMS coding
# ID: USUBJID
# Key vars: RACE, ETHNIC, RACEOTH — regulatory submission coding standards
# =============================================================================
cat("\n\n--- SCENARIO 16: SDTM DM — Demographics: Race/Ethnicity Coding QC ---\n\n")

dm_ecrf <- data.frame(
  STUDYID  = rep("DR001", 8),
  DOMAIN   = rep("DM", 8),
  USUBJID  = paste0("DR001-00", 1:8),
  SUBJID   = paste0("00", 1:8),
  RACE     = c("WHITE","ASIAN","AMERICAN INDIAN OR ALASKA NATIVE",
               "BLACK OR AFRICAN AMERICAN","WHITE","OTHER",
               "ASIAN","WHITE"),
  ETHNIC   = c("NOT HISPANIC OR LATINO","NOT HISPANIC OR LATINO",
               "NOT HISPANIC OR LATINO","NOT HISPANIC OR LATINO",
               "HISPANIC OR LATINO","HISPANIC OR LATINO",
               "NOT HISPANIC OR LATINO","NOT HISPANIC OR LATINO"),
  SEX      = c("M","F","M","F","M","F","M","F"),
  AGE      = c(45L,52L,38L,61L,44L,57L,33L,48L),
  COUNTRY  = rep("USA", 8),
  stringsAsFactors = FALSE
)

dm_coded <- dm_ecrf
# CIOMS/FDA compliant coding corrections
dm_coded$RACE[3]   <- "AMERICAN INDIAN OR ALASKA NATIVE"  # unchanged — verify
dm_coded$RACE[6]   <- "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER"  # re-coded
dm_coded$ETHNIC[5] <- "HISPANIC OR LATINO"  # unchanged — verify
dm_coded$AGE[7]    <- 34L                   # age corrected via query

recon(
  base    = dm_ecrf,
  compare = dm_coded,
  id      = "USUBJID",
  noequal = TRUE
)


# =============================================================================
# SCENARIO 17-SDTM LB — Urinalysis and Coagulation Panel
# Use case: Validate multiple lab panels together in one comparison
# ID: USUBJID + LBSEQ
# Focus on: LBSTRESN, LBNRIND, LBSTNRLO, LBSTNRHI
# Tolerance: 0.01 (lab result precision)
# =============================================================================
cat("\n\n--- SCENARIO 17: SDTM LB — Urinalysis & Coagulation: Reference Range QC ---\n\n")

lb_panel <- data.frame(
  STUDYID  = rep("DR001", 12),
  DOMAIN   = rep("LB", 12),
  USUBJID  = rep(c("DR001-001","DR001-002"), each = 6),
  LBSEQ    = rep(1:6, 2),
  LBTESTCD = rep(c("PT","APTT","INR","URINE_PH","URINE_PRO","URINE_GLU"), 2),
  LBTEST   = rep(c("Prothrombin Time","APTT","INR",
                   "Urine pH","Urine Protein","Urine Glucose"), 2),
  LBSTRESN = c(11.5, 28.0, 1.05, 6.0, 0, 0,
               13.2, 35.0, 1.20, 5.5, 1, 0),
  LBSTRESU = c("sec","sec","ratio","","mg/dL","mg/dL",
               "sec","sec","ratio","","mg/dL","mg/dL"),
  LBSTNRLO = c(9.5, 25.0, 0.80, 4.5, 0, 0,
               9.5, 25.0, 0.80, 4.5, 0, 0),
  LBSTNRHI = c(13.5, 35.0, 1.20, 8.0, 0, 0,
               13.5, 35.0, 1.20, 8.0, 0, 0),
  LBNRIND  = c("NORMAL","NORMAL","NORMAL","NORMAL","NORMAL","NORMAL",
               "HIGH","NORMAL","NORMAL","NORMAL","HIGH","NORMAL"),
  stringsAsFactors = FALSE
)

lb_panel_qc <- lb_panel
lb_panel_qc$LBSTRESN[7]  <- 13.1   # corrected value after re-run
lb_panel_qc$LBNRIND[7]   <- "NORMAL"  # now within range
lb_panel_qc$LBSTNRHI[2]  <- 36.0   # reference range updated for lab site
lb_panel_qc$LBSTNRHI[8]  <- 36.0
lb_panel_qc$LBSTRESN[11] <- 0      # trace protein corrected to 0
lb_panel_qc$LBNRIND[11]  <- "NORMAL"

recon(
  base      = lb_panel,
  compare   = lb_panel_qc,
  id        = c("USUBJID", "LBSEQ"),
  criterion = 0.01,
  noequal   = TRUE
)


# =============================================================================
# SCENARIO 18-SDTM AE — Serious Adverse Events (SAE) Narrative Reconciliation
# Use case: Compare SAE fields between safety database and SDTM AE
# Focuses on: AESER, AESDTH, AESHOSP, AESDISAB, AESCONG, AESMIE
# ID: USUBJID + AESEQ
# =============================================================================
cat("\n\n--- SCENARIO 18: SDTM AE — SAE Flags: Safety Database vs SDTM ---\n\n")

ae_safety_db <- data.frame(
  STUDYID  = rep("DR001", 6),
  DOMAIN   = rep("AE", 6),
  USUBJID  = c("DR001-001","DR001-002","DR001-003",
               "DR001-004","DR001-005","DR001-006"),
  AESEQ    = rep(1L, 6),
  AEDECOD  = c("PNEUMONIA","HEPATOTOXICITY","ANAPHYLAXIS",
               "PULMONARY EMBOLISM","SEPSIS","CEREBROVASCULAR ACCIDENT"),
  AESER    = rep("Y", 6),
  AESDTH   = c("N","N","N","N","N","N"),
  AESHOSP  = c("Y","Y","Y","Y","Y","Y"),
  AESDISAB = c("N","N","N","N","N","N"),
  AESCONG  = rep("N", 6),
  AESMIE   = c("N","N","N","Y","Y","N"),
  AEACN    = c("DRUG WITHDRAWN","DOSE REDUCED","DRUG WITHDRAWN",
               "DRUG WITHDRAWN","DRUG WITHDRAWN","DRUG WITHDRAWN"),
  stringsAsFactors = FALSE
)

ae_sdtm <- ae_safety_db
# Reconciliation findings
ae_sdtm$AESDTH[6]    <- "Y"    # patient died — missed in SDTM
ae_sdtm$AESDISAB[3]  <- "Y"    # permanent disability after anaphylaxis
ae_sdtm$AESMIE[2]    <- "Y"    # hepatotoxicity meets medically important criteria
ae_sdtm$AEACN[4]     <- "DRUG WITHDRAWN"  # unchanged — confirm

recon(
  base    = ae_safety_db,
  compare = ae_sdtm,
  id      = c("USUBJID", "AESEQ"),
  var     = c("AESDTH","AESDISAB","AESCONG","AESMIE","AEACN"),
  noequal = TRUE
)

r13 <- recon(ae_safety_db, ae_sdtm, id = c("USUBJID","AESEQ"),
             var = c("AESDTH","AESDISAB","AESCONG","AESMIE","AEACN"),
             noequal = TRUE, brief = TRUE)
diffs13 <- get_diffs(r13)
dth_rows <- diffs13[diffs13$variable == "AESDTH" & diffs13$compare_value == "Y", ]
if (nrow(dth_rows) > 0) {
  cat("\n  *** AESDTH=Y added — expedited 7/15-day safety report required ***\n")
}


# =============================================================================
# SCENARIO 19-SDTM SU (Substance Use)
# Use case: Tobacco / alcohol screening — eligibility check
#           Compare eCRF vs IRT (Interactive Response Technology) system
# ID: USUBJID + SUSEQ
# Key vars: SUOCCUR, SUDOSE, SUSCAT
# =============================================================================
cat("\n\n--- SCENARIO 19: SDTM SU — Substance Use: eCRF vs IRT System ---\n\n")

su_ecrf <- data.frame(
  STUDYID  = rep("DR001", 10),
  DOMAIN   = rep("SU", 10),
  USUBJID  = rep(c("DR001-001","DR001-002",
                   "DR001-003","DR001-004","DR001-005"), each = 2),
  SUSEQ    = rep(1:2, 5),
  SUTRT    = rep(c("TOBACCO","ALCOHOL"), 5),
  SUSCAT   = rep(c("TOBACCO","ALCOHOL"), 5),
  SUOCCUR  = c("Y","N","N","Y","Y","Y","N","N","Y","Y"),
  SUDOSE   = c(10, 0, 0, 14, 5, 3, 0, 0, 20, 7),
  SUDOSU   = c("cig/day","units/wk","cig/day","units/wk",
               "cig/day","units/wk","cig/day","units/wk",
               "cig/day","units/wk"),
  stringsAsFactors = FALSE
)

su_irt <- su_ecrf
# IRT system discrepancies
su_irt$SUOCCUR[3]  <- "Y"    # subject 002 denied tobacco but biomarker positive
su_irt$SUDOSE[3]   <- 5      # cotinine level suggests 5 cig/day
su_irt$SUOCCUR[7]  <- "Y"    # subject 004 undisclosed alcohol use
su_irt$SUDOSE[7]   <- 8
su_irt$SUDOSE[9]   <- 25     # heavier smoker than reported

recon(
  base    = su_ecrf,
  compare = su_irt,
  id      = c("USUBJID", "SUSEQ"),
  noequal = TRUE
)


# =============================================================================
# SCENARIO 20-SDTM RELREC (Related Records)
# Use case: Validate relational links between SDTM domains
#           AE linked to CM (concomitant medication given for AE)
# ID: USUBJID + IDVAR + IDVARVAL
# Key: Ensures every SAE has a corresponding treatment record
# =============================================================================
cat("\n\n--- SCENARIO 20: SDTM RELREC — Related Records: AE-CM Linkage QC ---\n\n")

relrec_base <- data.frame(
  STUDYID  = rep("DR001", 8),
  RDOMAIN  = c("AE","CM","AE","CM","AE","CM","AE","CM"),
  USUBJID  = c("DR001-001","DR001-001","DR001-002","DR001-002",
               "DR001-003","DR001-003","DR001-004","DR001-004"),
  IDVAR    = rep("SEQ", 8),
  IDVARVAL = c("1","1","1","1","1","1","1","1"),
  RELTYPE  = rep("MANY", 8),
  RELID    = c("AE01","AE01","AE02","AE02","AE03","AE03","AE04","AE04"),
  stringsAsFactors = FALSE
)

relrec_compare <- relrec_base
# Linkage errors detected during review
relrec_compare$IDVARVAL[5] <- "2"    # AE linked to wrong CM record
relrec_compare$RELID[6]    <- "AE03B"# CM split into two related records
relrec_compare$RELTYPE[7]  <- "ONE"  # cardinality corrected

recon(
  base    = relrec_base,
  compare = relrec_compare,
  id      = c("USUBJID", "RDOMAIN", "IDVARVAL"),
  noequal = TRUE
)


# =============================================================================
# FINAL SUMMARY TABLE
# =============================================================================
cat("\n\n", strrep("=", 72), "\n")
cat("  SDTM SCENARIOS — SUMMARY\n")
cat(strrep("=", 72), "\n")
cat(sprintf("  %-4s  %-12s  %-32s  %s\n",
            "No.", "Domain", "Use Case", "Result"))
cat(sprintf("  %-4s  %-12s  %-32s  %s\n",
            strrep("-",4), strrep("-",12), strrep("-",32), strrep("-",22)))

scenarios <- list(
  list("01","SDTM DM","Data Cut 1 vs Data Cut 2",
       dm_cut1,dm_cut2,c("USUBJID")),
  list("02","SDTM AE","Sponsor vs CRO Reconciliation",
       ae_sponsor,ae_cro,c("USUBJID", "AESEQ")),
  list("03","SDTM LB","Central Lab Extract vs SDTM Mapped",
       lb_raw,lb_sdtm,c("USUBJID", "LBSEQ")),
  list("04","SDTM CM","eCRF vs WHODrug Coded Dataset",
       cm_ecrf,cm_coded,c("USUBJID", "CMSEQ")),
  list("05","SDTM CM","Baseline vs Week 24 Derivation QC",
       vs_v1,vs_v1_qc,c("USUBJID", "VSTESTCD")),
  list("06","SDTM EX","Planned vs actual dose",
       ex_planned,ex_actual,c("USUBJID","EXSEQ")),
  list("07","SDTM MH","eCRF vs MedDRA coded",
       mh_ecrf,mh_coded,c("USUBJID","MHSEQ")),
  list("09","SDTM DS","Interim vs final cut",
       ds_interim,ds_final,c("USUBJID","DSSEQ")),
  list("10","SDTM SV","Scheduled vs actual visits",
       sv_scheduled,sv_actual,c("USUBJID","VISITNUM")),
  list("11","SDTM TU","Investigator vs central read",
       tu_investigator,tu_central,c("USUBJID","TUSEQ")),
  list("12","SDTM TR","Tumour measurement QC (1mm)",
       tr_inv,tr_central,c("USUBJID","TRSEQ")),
  list("13","SDTM RS","Investigator vs BICR",
       rs_investigator,rs_bicr,c("USUBJID","RSSEQ")),
  list("14","SDTM EG","Site vs central ECG (5ms)",
       eg_site,eg_central,c("USUBJID","EGSEQ")),
  list("15","SDTM PE","Site vs medical monitor",
       pe_site,pe_monitor,c("USUBJID","PESEQ")),
  list("16","SDTM QS","Paper CRF vs eDiary",
       qs_paper,qs_ediary,c("USUBJID","QSSEQ")),
  list("17","SDTM DM","Race/ethnicity coding QC",
       dm_ecrf,dm_coded,"USUBJID"),
  list("18","SDTM LB","Urinalysis & coagulation",
       lb_panel,lb_panel_qc,c("USUBJID","LBSEQ")),
  list("19","SDTM AE","SAE flags reconciliation",
       ae_safety_db,ae_sdtm,c("USUBJID","AESEQ")),
  list("20","SDTM SU","eCRF vs IRT system",
       su_ecrf,su_irt,c("USUBJID","SUSEQ")),
  list("21","SDTM RELREC","AE-CM linkage QC",
       relrec_base,relrec_compare,c("USUBJID","RDOMAIN","IDVARVAL"))
)

#archive for above
scenarios2 <- list(
  list("01","SDTM EX","Planned vs actual dose",
       ex_planned,ex_actual,c("USUBJID","EXSEQ")),
  list("02","SDTM MH","eCRF vs MedDRA coded",
       mh_ecrf,mh_coded,c("USUBJID","MHSEQ")),
  list("03","SDTM DS","Interim vs final cut",
       ds_interim,ds_final,c("USUBJID","DSSEQ")),
  list("04","SDTM SV","Scheduled vs actual visits",
       sv_scheduled,sv_actual,c("USUBJID","VISITNUM")),
  list("05","SDTM TU","Investigator vs central read",
       tu_investigator,tu_central,c("USUBJID","TUSEQ")),
  list("06","SDTM TR","Tumour measurement QC (1mm)",
       tr_inv,tr_central,c("USUBJID","TRSEQ")),
  list("07","SDTM RS","Investigator vs BICR",
       rs_investigator,rs_bicr,c("USUBJID","RSSEQ")),
  list("08","SDTM EG","Site vs central ECG (5ms)",
       eg_site,eg_central,c("USUBJID","EGSEQ")),
  list("09","SDTM PE","Site vs medical monitor",
       pe_site,pe_monitor,c("USUBJID","PESEQ")),
  list("10","SDTM QS","Paper CRF vs eDiary",
       qs_paper,qs_ediary,c("USUBJID","QSSEQ")),
  list("11","SDTM DM","Race/ethnicity coding QC",
       dm_ecrf,dm_coded,"USUBJID"),
  list("12","SDTM LB","Urinalysis & coagulation",
       lb_panel,lb_panel_qc,c("USUBJID","LBSEQ")),
  list("13","SDTM AE","SAE flags reconciliation",
       ae_safety_db,ae_sdtm,c("USUBJID","AESEQ")),
  list("14","SDTM SU","eCRF vs IRT system",
       su_ecrf,su_irt,c("USUBJID","SUSEQ")),
  list("15","SDTM RELREC","AE-CM linkage QC",
       relrec_base,relrec_compare,c("USUBJID","RDOMAIN","IDVARVAL"))
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