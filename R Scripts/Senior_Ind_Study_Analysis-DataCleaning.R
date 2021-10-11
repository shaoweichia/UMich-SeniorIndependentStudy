##### SENIOR INDEPENDENT STUDY ANALYSIS - DATA CLEANING #####
#### PREP ####
### Load libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(dplyr)
  library(readr)
  library(data.table)
  library(psych)
})

### Set directory
setwd("C:/Users/Shao Wei/Documents/Chia Shao Wei/U of M/2014-2015/Fall 2014/PSYCH 422/Data")

#### DATA CLEANING ####
### Load data
## Technical issues  + programming error in the first survey (College_Study_2)
## Was able to resolve technical issue when running with first group of participants
## In College_Study_2, item CAS_5 ("I am working in.") was included
## Item CAS_5 was removed in College_Study_3, so before merging both dataset,
## remove CAS_5 from College_Study_2

## Importing all data as character due to Qualtrics export having item text in Row 2
raw.data.1 <- read.csv(paste0(getwd(),
                              "/Data from Qualtrics/College_Study_2.csv"),
                       fileEncoding = "UTF-8-BOM",
                       colClasses = "character") %>%
  as.data.table()

raw.data.2 <-read.csv(paste0(getwd(),
                             "/Data from Qualtrics/College_Study_3.csv"),
                      fileEncoding = "UTF-8-BOM",
                      colClasses = "character") %>%
  as.data.table()

# QA
nrow(raw.data.1); ncol(raw.data.1) # 48 rows; 314 cols - extra column (CAS_5)
nrow(raw.data.2); ncol(raw.data.2) # 330 rows; 313 cols

### Merge raw.data.1 and raw.data.2
## From Qualtrics export, item text are in Row 2 - remove them
raw.data.1.interim <- raw.data.1[-1,]
raw.data.2.interim <- raw.data.2[-1,]

# QA
head(raw.data.1.interim)
head(raw.data.2.interim)

#### Data cleaning and wrangling ####
#### Clean Data
### Remove "CAS_5" from raw.data.1 as mentioned in Line 25-26
raw.data.1.interim[, CAS_5 := NULL]
str(raw.data.1.interim) # 47 rows; 313 cols

### Check if the column names for both raw.data.1.interim and raw.data.2.interim
### are the same
colnames.check <- (names(raw.data.1.interim) == names(raw.data.2.interim)) %>%
  sum() # 313 TRUE

### Combine the 2 raw data tables
raw.data <- list(raw.data.1.interim,
                 raw.data.2.interim) %>%
  rbindlist()

### Recode data into appropriate data type
raw.data.parse <- raw.data %>%
  mutate_all(parse_guess) %>%
  as.data.table()

## QA
str(raw.data.parse)
summary(raw.data.parse) # LGTM

### Recode respondents demographic into factors
raw.data.recode <- copy(raw.data.parse) %>%
  mutate_at(., "SEX", ~recode(.,
                             "1" = "male",
                             "2" = "female",
                             .default = NA_character_)) %>%
  mutate_at(., "SEX", ~factor(., levels = c("male", "female"))) %>%
  mutate_at(., "RACE", ~recode(.,
                               "1" = "caucasian american",
                               "2" = "african american",
                               "3" = "hispanic/latino",
                               "4" = "asian american/pacific islander",
                               "5" = "bi-racial/mixed",
                               "6" = "other",
                               .default = NA_character_)) %>%
  mutate_at(., "RACE", ~factor(.,
                               levels = c("caucasian american",
                                          "african american",
                                          "hispanic/latino",
                                          "asian american/pacific islander",
                                          "bi-racial/mixed",
                                          "other"))) %>%
  mutate_at(., "EDU", ~recode(.,
                              "1" = "high school graduate or below",
                              "2" = "some college or technical school",
                              "3" = "college graduate or beyond",
                              .default = NA_character_)) %>%
  mutate_at(., "EDU", ~factor(.,
                              levels = c("high school graduate or below",
                                         "some college or technical school",
                                         "college graduate or beyond"))) %>%
  mutate_at(., "CLASS", ~recode(.,
                                "1" = "freshman",
                                "2" = "sophomore",
                                "3" = "junior",
                                "4" = "senior",
                                "5" = "other",
                                .default = NA_character_)) %>%
  mutate_at(., "CLASS", ~factor(.,
                                levels = c("freshman",
                                           "sophomore",
                                           "junior",
                                           "senior",
                                           "other"))) %>%
  mutate_at(., "SES", ~recode(.,
                              "1" = "lower class",
                              "2" = "lower-middle class",
                              "3" = "middle class",
                              "4" = "upper-middle class",
                              "5" = "pper class",
                              .default = NA_character_)) %>%
  mutate_at(., "SES", ~factor(.,
                              levels = c("lower class",
                                         "lower-middle class",
                                         "middle class",
                                         "upper-middle class",
                                         "upper class"))) %>%
  mutate_at(., "ORDER", ~recode(.,
                                "1" = "first child",
                                "2" = "middle child",
                                "3" = "youngest child",
                                .default = NA_character_)) %>%
  mutate_at(., "ORDER", ~factor(.,
                                levels = c("first child",
                                           "middle child",
                                           "youngest child"))) %>%
  mutate_at(., "PAREDU", ~recode(.,
                                 "1" = "high school graduate or below",
                                 "2" = "some college or technical school",
                                 "3" = "college graduate or beyond",
                                 .default = NA_character_)) %>%
  mutate_at(., "PAREDU", ~factor(.,
                              levels = c("high school graduate or below",
                                         "some college or technical school",
                                         "college graduate or beyond"))) %>%
  mutate_at(., "Sib.SexAge_1_TEXT", ~gsub("^(f|F|w).*", "female", .,
                                         perl = TRUE)) %>%
  mutate_at(., "Sib.SexAge_1_TEXT", ~gsub("^(m|M).*", "male", .,
                                         perl = TRUE)) %>%
  mutate_at(., "Sib.SexAge_1_TEXT", ~factor(., levels = c("male",
                                                          "female"))) %>%
  mutate_at(., "Sib.SexAge_2_TEXT", ~gsub("^[A-z]+", NA, .,
                                          perl = TRUE)) %>%
  mutate_at(., "Sib.SexAge_2_TEXT", ~gsub("22 or 23", 22, .,
                                         perl = TRUE)) %>%
  mutate_at(., "Sib.SexAge_2_TEXT", ~as.numeric(.))

# QA
demog.var <- c("SEX", "RACE", "ORDER",
               "EDU", "CLASS", "SES",
               "PAREDU", "Sib.SexAge_1_TEXT")

descibe(raw.data.recode[, ..demog.var])

sapply(demog.var, function(x) {
  raw.data.parse[, table(get(x))]
}) # counts match!

#### Data wrangling and manipulation
### Rescale variables given inconsistent output from Qualtrics
raw.data.rescale <- copy(raw.data.recode) %>%
  ## CAS
  mutate_at(., grep("^CAS",
                    colnames(raw.data.recode),
                    value = TRUE),
            ~recode(.,
                    "1" = as.integer("0"),
                    "2" = as.integer("1"),
                    "3" = as.integer("2"),
                    "4" = as.integer("3"),
                    "5" = as.integer("4"))) %>%
  ## BDI and BAI
  mutate_at(., grep("^B[AD]I",
                    colnames(raw.data.recode),
                    value = TRUE),
            ~recode(.,
                    "1" = as.integer("0"),
                    "2" = as.integer("1"),
                    "3" = as.integer("2"),
                    "4" = as.integer("3")))

## QA
# CAS
raw.data.rescale[, .SD, .SDcols =
                   grep("^CAS",
                        colnames(raw.data.rescale))] %>%
  describe() # minimum is 0, and maximum is 4

sapply(grep("^CAS", colnames(raw.data.recode), value = TRUE),
      function(x) {
        raw.data.recode[, table(get(x))]
      })

sapply(grep("^CAS", colnames(raw.data.rescale), value = TRUE),
      function(x) {
        raw.data.rescale[, table(get(x))]
      }) # numbers match!

# BDI and BAI
raw.data.rescale[, .SD, .SDcols =
                   grep("^B[AD]I",
                        colnames(raw.data.rescale))] %>%
  describe() # minimum is 0, and maximum is 4

sapply(grep("^B[AD]I", colnames(raw.data.recode), value = TRUE),
      function(x) {
        raw.data.recode[, table(get(x))]
      })

sapply(grep("^B[AD]I", colnames(raw.data.rescale), value = TRUE),
      function(x) {
        raw.data.rescale[, table(get(x))]
      }) # numbers match!

### Reverse code variables
raw.data.reverse <- copy(raw.data.rescale)

## MLQ_9 (1-7 points)
raw.data.reverse[, MLQ_9r :=
                   sapply(MLQ_9, recode,
                          "1" = as.integer("7"),
                          "2" = as.integer("6"),
                          "3" = as.integer("5"),
                          "4" = as.integer("4"),
                          "5" = as.integer("3"),
                          "6" = as.integer("2"),
                          "7" = as.integer("1"))]

# QA
raw.data.reverse[, c("MLQ_9", "MLQ_9r")] %>% table() # LGTM

## PLAN_2, PLAN_3, PLAN_4, PLAN_15, PLAN_17, 
## PLAN_19, PLAN_20, PLAN_21, PLAN_23 (1-4 points)
# Initialize variable to recode
plan.recode <- c("PLAN_2", "PLAN_3", "PLAN_4",
                 "PLAN_15", "PLAN_17", "PLAN_19",
                 "PLAN_20", "PLAN_21", "PLAN_23")

# Recode
raw.data.reverse[, sapply(plan.recode, paste0, "r") :=
                     lapply(.SD, recode,
                            "1" = as.integer("4"),
                            "2" = as.integer("3"),
                            "3" = as.integer("2"),
                            "4" = as.integer("1")),
                   .SDcols = plan.recode]

# QA
lapply(plan.recode, function(x) {
  raw.data.reverse[, .SD, .SDcols = c(x, paste0(x, "r"))] %>% table()
}) # LGTM

## CAS_3, CAS_4, CAS_8, CAS_11 (0-4 points)
# Initialize variable to recode
cas.recode <- c("CAS_3", "CAS_4",
                "CAS_8", "CAS_11")

# Recode
raw.data.reverse[, sapply(cas.recode, paste0, "r") :=
                   lapply(.SD, recode,
                          "0" = as.integer("4"),
                          "1" = as.integer("3"),
                          "2" = as.integer("2"),
                          "3" = as.integer("1"),
                          "4" = as.integer("0")),
                 .SDcols = cas.recode]

# QA
lapply(cas.recode, function(x) {
  raw.data.reverse[, .SD, .SDcols = c(x, paste0(x, "r"))] %>% table()
}) # LGTM

## FNE_2, FNE_4, FNE_7, FNE_10 (1-5 points)
# Initialize variable to recode
fne.recode <- c("FNE_2", "FNE_4",
                "FNE_7", "FNE_10")

# Recode
raw.data.reverse[, sapply(fne.recode, paste0, "r") :=
                   lapply(.SD, recode,
                          "1" = as.integer("5"),
                          "2" = as.integer("4"),
                          "3" = as.integer("3"),
                          "4" = as.integer("2"),
                          "5" = as.integer("1")),
                 .SDcols = fne.recode]

# QA
lapply(fne.recode, function(x) {
  raw.data.reverse[, .SD, .SDcols = c(x, paste0(x, "r"))] %>% table()
}) # LGTM

## INQ_7, INQ_8, INQ_10, INQ_13,
## INQ_14, INQ_15 (1-7 points)
# Initialize variable to recode
inq.recode <- c("INQ_7", "INQ_8", "INQ_10",
                "INQ_13", "INQ_14", "INQ_15")

# Recode
raw.data.reverse[, sapply(inq.recode, paste0, "r") :=
                     lapply(.SD, recode,
                            "1" = as.integer("7"),
                            "2" = as.integer("6"),
                            "3" = as.integer("5"),
                            "4" = as.integer("4"),
                            "5" = as.integer("3"),
                            "6" = as.integer("2"),
                            "7" = as.integer("1")),
                   .SDcols = inq.recode]

# QA
lapply(inq.recode, function(x) {
  raw.data.reverse[, .SD, .SDcols = c(x, paste0(x, "r"))] %>% table()
}) # LGTM

## RSES_3, RSES_5, RSES_8, RSES_9,
## RSES_10 (1-4 points)
# Initialize variable to recode
rses.recode <- c("RSES_3", "RSES_5", "RSES_8",
                 "RSES_9", "RSES_10")

# Recode
raw.data.reverse[, sapply(rses.recode, paste0, "r") :=
                     lapply(.SD, recode,
                            "1" = as.integer("4"),
                            "2" = as.integer("3"),
                            "3" = as.integer("2"),
                            "4" = as.integer("1")),
                   .SDcols = rses.recode]

# QA
lapply(rses.recode, function(x) {
  raw.data.reverse[, .SD, .SDcols = c(x, paste0(x, "r"))] %>% table()
}) # LGTM

## RIII_6, RIII_8 (1-7 points)
# Initialize variable to recode
riii.recode <- c("RIII_6", "RIII_8")

# Recode
raw.data.reverse[, sapply(riii.recode, paste0, "r") :=
                   lapply(.SD, recode,
                          "1" = as.integer("7"),
                          "2" = as.integer("6"),
                          "3" = as.integer("5"),
                          "4" = as.integer("4"),
                          "5" = as.integer("3"),
                          "6" = as.integer("2"),
                          "7" = as.integer("1")),
                 .SDcols = riii.recode]

# QA
lapply(riii.recode, function(x) {
  raw.data.reverse[, .SD, .SDcols = c(x, paste0(x, "r"))] %>% table()
}) # LGTM

### Create composite scale scores
raw.data.composite <- copy(raw.data.reverse)

## SCS
# Initialize vector to store composite score names
composite.var <- as.character()

# Appearance
raw.data.composite[, scs_appear := apply(.SD, 1, sum),
                     .SDcols = c("SCS_3", "SCS_14",
                                 "SCS_25", "SCS_36")]

summary(raw.data.composite$scs_appear)

composite.var[1] <- "scs_appear"

# Athletic competence
raw.data.composite[, scs_athletic := apply(.SD, 1, sum),
                     .SDcols = c("SCS_11", "SCS_22",
                                 "SCS_33", "SCS_44")]

summary(raw.data.composite$scs_athletic)

composite.var[2] <- "scs_athletic"

# Close friendship
raw.data.composite[, scs_friend := apply(.SD, 1, sum),
                     .SDcols = c("SCS_5", "SCS_16",
                                 "SCS_27", "SCS_38")]

summary(raw.data.composite$scs_friend)

composite.var[3] <- "scs_friend"

# Creativity
raw.data.composite[, scs_create := apply(.SD, 1, sum),
                     .SDcols = c("SCS_10", "SCS_21",
                                 "SCS_32", "SCS_43")]

summary(raw.data.composite$scs_create)

composite.var[4] <- "scs_create"

# Humor
raw.data.composite[, scs_humor := apply(.SD, 1, sum),
                     .SDcols = c("SCS_9", "SCS_20",
                                 "SCS_31", "SCS_42")]

summary(raw.data.composite$scs_humor)

composite.var[5] <- "scs_humor"

# Intellectual ability
raw.data.composite[, scs_intellect := apply(.SD, 1, sum),
                     .SDcols = c("SCS_6", "SCS_17",
                                 "SCS_28", "SCS_39")]

summary(raw.data.composite$scs_intellect)

composite.var[6] <- "scs_intellect"

# Morality
raw.data.composite[, scs_moral := apply(.SD, 1, sum),
                     .SDcols = c("SCS_7", "SCS_18",
                                 "SCS_29", "SCS_40")]

summary(raw.data.composite$scs_moral)

composite.var[7] <- "scs_moral"

# Parent relationship
raw.data.composite[, scs_parent := apply(.SD, 1, sum),
                     .SDcols = c("SCS_4", "SCS_26",
                                 "SCS_15", "SCS_37")]

summary(raw.data.composite$scs_parent)

composite.var[8] <- "scs_parent"

# Romantic relationship
raw.data.composite[, scs_romantic := apply(.SD, 1, sum),
                     .SDcols = c("SCS_8", "SCS_19",
                                 "SCS_30", "SCS_41")]

summary(raw.data.composite$scs_romantic)

composite.var[9] <- "scs_romantic"

# Scholastic competence
raw.data.composite[, scs_scholastic := apply(.SD, 1, sum),
                     .SDcols = c("SCS_1", "SCS_12",
                                 "SCS_23", "SCS_34")]

summary(raw.data.composite$scs_scholastic)

composite.var[10] <- "scs_scholastic"

# Social acceptance
raw.data.composite[, scs_social := apply(.SD, 1, sum),
                     .SDcols = c("SCS_2", "SCS_13",
                                 "SCS_24", "SCS_35")]

summary(raw.data.composite$scs_social)

composite.var[11] <- "scs_social"

## PPECS
raw.data.composite[, ppecs_composite := apply(.SD, 1, sum),
                     .SDcols = c("PPECS_1", "PPECS_2",
                                 "PPECS_3", "PPECS_4",
                                 "PPECS_5", "PPECS_6",
                                 "PPECS_7")]

summary(raw.data.composite$ppecs_composite)

composite.var[12] <- "ppecs_composite"

## MLQ
# Presence
raw.data.composite[, mlq_presence := apply(.SD, 1, sum),
                     .SDcols = c("MLQ_1", "MLQ_4",
                                 "MLQ_5", "MLQ_6",
                                 "MLQ_9r")]

summary(raw.data.composite$mlq_presence)

composite.var[13] <- "mlq_presence"

# Search
raw.data.composite[, mlq_search := apply(.SD, 1, sum),
                     .SDcols = c("MLQ_2", "MLQ_3",
                                 "MLQ_7", "MLQ_8",
                                 "MLQ_10")]

summary(raw.data.composite$mlq_search)

composite.var[14] <- "mlq_search"

## SWLS
raw.data.composite[, swls_composite := apply(.SD, 1, sum),
                     .SDcols = c("SWLS_1", "SWLS_2",
                                 "SWLS_3", "SWLS_4",
                                 "SWLS_5")]

summary(raw.data.composite$swls_composite)

composite.var[15] <- "swls_composite"

## PLAN
# Consider children
raw.data.composite[, plan_child := apply(.SD, 1, sum),
                     .SDcols = c("PLAN_1", "PLAN_8",
                                 "PLAN_10", "PLAN_11",
                                 "PLAN_12", "PLAN_13",
                                 "PLAN_14", "PLAN_16",
                                 "PLAN_18", "PLAN_20r",
                                 "PLAN_22", "PLAN_24")]

summary(raw.data.composite$plan_child)

composite.var[16] <- "plan_child"

# Prioritize / Compromise for Partner
raw.data.composite[, plan_partner := apply(.SD, 1, sum),
                     .SDcols = c("PLAN_2r", "PLAN_3r",
                                 "PLAN_4r", "PLAN_5",
                                 "PLAN_6", "PLAN_7",
                                 "PLAN_9", "PLAN_15r",
                                 "PLAN_17r", "PLAN_19r",
                                 "PLAN_21r", "PLAN_23r")]

summary(raw.data.composite$plan_partner)

composite.var[17] <- "plan_partner"

## CAS
# Educational Aspirations
raw.data.composite[, cas_edu := apply(.SD, 1, sum),
                     .SDcols = c("CAS_8", "CAS_10")]

summary(raw.data.composite$cas_edu)

composite.var[18] <- "cas_edu"

# Leadership and Achievement Aspirations
raw.data.composite[, cas_leader := apply(.SD, 1, sum),
                     .SDcols = c("CAS_1", "CAS_2",
                                 "CAS_4r", "CAS_6",
                                 "CAS_7", "CAS_11r")]

summary(raw.data.composite$cas_leader)

composite.var[19] <- "cas_leader"

## SWFLS
raw.data.composite[, swfls_composite := apply(.SD, 1, sum),
                     .SDcols = c("SWFLS_1", "SWFLS_2",
                                 "SWFLS_3", "SWFLS_4",
                                 "SWFLS_5")]

summary(raw.data.composite$swfls_composite)

composite.var[20] <- "swfls_composite"

## APCCS
# Complementary congruence
raw.data.composite[, apccs_complement := apply(.SD, 1, sum),
                     .SDcols = c("APCCS_1", "APCCS_2",
                                 "APCCS_3", "APCCS_4",
                                 "APCCS_5", "APCCS_6",
                                 "APCCS_7")]

summary(raw.data.composite$apccs_complement)

composite.var[21] <- "apccs_complement"

# Supplementary congruence
raw.data.composite[, apccs_supplement := apply(.SD, 1, sum),
                     .SDcols = c("APCCS_8", "APCCS_9",
                                 "APCCS_10", "APCCS_11",
                                 "APCCS_12")]

summary(raw.data.composite$apccs_supplement)

composite.var[22] <- "apccs_supplement"

## PSS
raw.data.composite[, pss_composite := apply(.SD, 1, sum),
                     .SDcols = c("PSS_1", "PSS_2",
                                 "PSS_3", "PSS_4")]

summary(raw.data.composite$pss_composite)

composite.var[23] <- "pss_composite"

## PANAS
# Positive affect
raw.data.composite[, panas_positive := apply(.SD, 1, sum),
                     .SDcols = c("PANAS_1", "PANAS_3",
                                 "PANAS_5", "PANAS_9",
                                 "PANAS_10", "PANAS_12",
                                 "PANAS_14", "PANAS_16",
                                 "PANAS_17", "PANAS_19")]

summary(raw.data.composite$panas_positive)

composite.var[24] <- "panas_positive"

# Negative affect
raw.data.composite[, panas_negative := apply(.SD, 1, sum),
                     .SDcols = c("PANAS_2", "PANAS_4",
                                 "PANAS_6", "PANAS_7",
                                 "PANAS_8", "PANAS_11",
                                 "PANAS_13", "PANAS_15",
                                 "PANAS_18", "PANAS_20")]

summary(raw.data.composite$panas_negative)

composite.var[25] <- "panas_negative"

## BDI
raw.data.composite[, bdi_composite := apply(.SD, 1, sum),
                     .SDcols = c("BDI1", "BDI2", "BDI3",
                                 "BDI4", "BDI5", "BDI6",
                                 "BDI7", "BDI8", "BDI9",
                                 "BDI10", "BDI11", "BDI12",
                                 "BDI13", "BDI14", "BDI15",
                                 "BDI16", "BDI17", "BDI18",
                                 "BDI19", "BDI20", "BDI21")]

summary(raw.data.composite$bdi_composite)

composite.var[26] <- "bdi_composite"

## HPS-M
raw.data.composite[, hpsm_composite := apply(.SD, 1, sum),
                     .SDcols = c("HPS.M_1", "HPS.M_2",
                                 "HPS.M_3", "HPS.M_4",
                                 "HPS.M_5")]

summary(raw.data.composite$hpsm_composite)

composite.var[27] <- "hpsm_composite"

## BSRI
# Feminine traits
raw.data.composite[, bsri_fem := apply(.SD, 1, sum),
                     .SDcols = c("BSRI_7", "BSRI_8",
                                 "BSRI_9", "BSRI_13",
                                 "BSRI_14", "BSRI_16",
                                 "BSRI_17", "BSRI_18",
                                 "BSRI_19", "BSRI_20")]

summary(raw.data.composite$bsri_fem)

composite.var[28] <- "bsri_fem"

# Masculine traits
raw.data.composite[, bsri_masc := apply(.SD, 1, sum),
                     .SDcols = c("BSRI_1", "BSRI_2",
                                 "BSRI_3", "BSRI_4",
                                 "BSRI_5", "BSRI_6",
                                 "BSRI_10", "BSRI_11",
                                 "BSRI_12", "BSRI_15")]

summary(raw.data.composite$bsri_masc)

composite.var[29] <- "bsri_masc"

## AESI
# Parents / teachers expectations
raw.data.composite[, aesi_par_teach := apply(.SD, 1, sum),
                     .SDcols = c("AESI_1", "AESI_2",
                                 "AESI_3", "AESI_4",
                                 "AESI_5")]

summary(raw.data.composite$aesi_par_teach)

composite.var[30] <- "aesi_par_teach"

# Self-expectations
raw.data.composite[, aesi_self := apply(.SD, 1, sum),
                     .SDcols = c("AESI_6", "AESI_7",
                                 "AESI_8", "AESI_9")]

summary(raw.data.composite$aesi_self)

composite.var[31] <- "aesi_self"

## HPS-F
raw.data.composite[, hpsf_composite := apply(.SD, 1, sum),
                     .SDcols = c("HPS.F_1", "HPS.F_2",
                                 "HPS.F_3", "HPS.F_4",
                                 "HPS.F_5")]

summary(raw.data.composite$hpsf_composite)

composite.var[32] <- "hpsf_composite"

## FNE
raw.data.composite[, fne_composite := apply(.SD, 1, sum),
                     .SDcols = c("FNE_1", "FNE_2r",
                                 "FNE_3", "FNE_4r",
                                 "FNE_5", "FNE_6",
                                 "FNE_7r", "FNE_8",
                                 "FNE_9", "FNE_10r",
                                 "FNE_11", "FNE_12")]

summary(raw.data.composite$fne_composite)

composite.var[33] <- "fne_composite"

## INQ
# Perceived burdensomeness
raw.data.composite[, inq_burden := apply(.SD, 1, sum),
                     .SDcols = c("INQ_1", "INQ_2",
                                 "INQ_3", "INQ_4",
                                 "INQ_5", "INQ_6")]

summary(raw.data.composite$inq_burden)

composite.var[34] <- "inq_burden"

# Thwarted belongingness
raw.data.composite[, inq_belong := apply(.SD, 1, sum),
                     .SDcols = c("INQ_7r", "INQ_8r",
                                 "INQ_9", "INQ_10r",
                                 "INQ_11", "INQ_12",
                                 "INQ_13r", "INQ_14r",
                                 "INQ_15r")]

summary(raw.data.composite$inq_belong)

composite.var[35] <- "inq_belong"

## RSES
raw.data.composite[, rses_composite := apply(.SD, 1, sum),
                     .SDcols = c("RSES_1", "RSES_2",
                                 "RSES_3r", "RSES_4",
                                 "RSES_5r", "RSES_6",
                                 "RSES_7", "RSES_8r",
                                 "RSES_9r", "RSES_10r")]

summary(raw.data.composite$rses_composite)

composite.var[36] <- "rses_composite"

## BAI
raw.data.composite[, bai_composite := apply(.SD, 1, sum),
                     .SDcols = c("BAI_1", "BAI_2", "BAI_3",
                                 "BAI_4", "BAI_5", "BAI_6",
                                 "BAI_7", "BAI_8", "BAI_9",
                                 "BAI_10", "BAI_11", "BAI_12",
                                 "BAI_13", "BAI_14", "BAI_15",
                                 "BAI_16", "BAI_17", "BAI_18",
                                 "BAI_19", "BAI_20", "BAI_21")]

summary(raw.data.composite$bai_composite)

composite.var[37] <- "bai_composite"

## HS
# Hope agency
raw.data.composite[, hs_agency := apply(.SD, 1, sum),
                     .SDcols = c("HS_2", "HS_9",
                                 "HS_10", "HS_12")]

summary(raw.data.composite$hs_agency)

composite.var[38] <- "hs_agency"

# Hope pathways
raw.data.composite[, hs_pathways := apply(.SD, 1, sum),
                     .SDcols = c("HS_1", "HS_4",
                                 "HS_6", "HS_8")]

summary(raw.data.composite$hs_pathways)

composite.var[39] <- "hs_pathways"

## RIII
raw.data.composite[, riii_composite := apply(.SD, 1, sum),
                     .SDcols = c("RIII_1", "RIII_2",
                                 "RIII_3", "RIII_4",
                                 "RIII_5", "RIII_6r",
                                 "RIII_7", "RIII_8r",
                                 "RIII_9")]

summary(raw.data.composite$riii_composite)

composite.var[40] <- "riii_composite"

### Export final cleaned data to csv
final.data <- copy(raw.data.composite)

write_csv(final.data, file = "College Study Data-Cleaned.csv")
