install.packages("remotes")
remotes::install_github("ganeshbabunn/rDataRecon")
library(tidyverse)
library(rDataRecon)

dm <- read.csv("https://raw.githubusercontent.com/ganeshbabuNN/datasets/refs/heads/master/clinical_datasets/sdtm/daibetes/csv/dm.csv")
dm

dm_1 <- dm |> select(SUBJID,SITEID,AGE,SEX)
dm_2 <- dm_1
dm_2[3,3] <-58  #changed the age of SUBJ0003 to 58 from 22
dm_2[9,2] <- 32 #changed the Site of SUBJ0009 to 32 from 44
dm_2[100,4] <- "F" #changed the Site of SUBJ0100 to M from F

recon(
  base = dm_1,
  compare = dm_2,
  id="SUBJID",
  noequal = FALSE,
  listall = FALSE
)

recon

compare_stats(dm_1,dm_2)
is_structure_equal(dm_1,dm_2)
