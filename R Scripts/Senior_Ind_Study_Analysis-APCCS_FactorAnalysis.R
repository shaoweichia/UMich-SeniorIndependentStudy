##### SENIOR INDEPENDENT STUDY ANALYSIS - ANALYSIS #####
#### Prep ####
### Load libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(dplyr)
  library(readr)
  library(data.table)
  library(broom)
  library(foreign)
  library(psych)
  library(nFactors)
  library(FactoMineR)
  library(semTools)
  library(lavaan)
  library(semPlot)
  library(GPArotation)
  library(MASS)
})

options(scipen = 999)

### Set directory
setwd("C:/Users/Shao Wei/Documents/Chia Shao Wei/U of M/2014-2015/Fall 2014/PSYCH 422/Data")

### Load cleaned data
data <- read.csv("College Study Data-Cleaned.csv") %>%
  as.data.table()

final.data <- copy(data) %>%
  mutate_at(., "SEX", ~factor(., levels = c("male", "female"))) %>%
  mutate_at(., "RACE", ~factor(.,
                               levels = c("caucasian american",
                                          "african american",
                                          "hispanic/latino",
                                          "asian american/pacific islander",
                                          "bi-racial/mixed",
                                          "other"))) %>%
  mutate_at(., "EDU", ~factor(.,
                              levels = c("high school graduate or below",
                                         "some college or technical school",
                                         "college graduate or beyond"))) %>%
  mutate_at(., "CLASS", ~factor(.,
                                levels = c("freshman",
                                           "sophomore",
                                           "junior",
                                           "senior",
                                           "other"))) %>%
  mutate_at(., "SES", ~factor(.,
                              levels = c("lower class",
                                         "lower-middle class",
                                         "middle class",
                                         "upper-middle class",
                                         "upper class"))) %>%
  mutate_at(., "ORDER", ~factor(.,
                                levels = c("first child",
                                           "middle child",
                                           "youngest child"))) %>%
  mutate_at(., "PAREDU", ~factor(.,
                                 levels = c("high school graduate or below",
                                            "some college or technical school",
                                            "college graduate or beyond"))) %>%
  mutate_at(., "Sib.SexAge_1_TEXT", ~factor(., levels = c("male",
                                                          "female")))

# QA
demog.var <- c("SEX", "RACE", "ORDER",
               "EDU", "CLASS", "SES",
               "PAREDU", "Sib.SexAge_1_TEXT")

str(final.data[, ..demog.var]) # LGTM

#### ANALYSIS ####
### Factor Analysis ###
## APCCS
## CFA
# Define model for Complementary congruence
apccs.model <-
  "comp.congruence =~ APCCS_1 + APCCS_2 + APCCS_3 + APCCS_4 + APCCS_5 + APCCS_6 + APCCS_7
  supp.congruence =~ APCCS_8 + APCCS_9 + APCCS_10 + APCCS_11 + APCCS_12
  comp.congruence ~ supp.congruence"

# Overall fit
apccs.fit <- sem(apccs.model, data = final.data)

summary(apccs.fit, fit.measures = TRUE) # bad fit

modindices(apccs.fit, sort = T, op = "=~")
# Mod indices suggest loading APCCS_8 to Complementary Congruence

apccs.cfa.fit.loadings <- tidy(apccs.fit)
apccs.cfa.fit.stats <- glance(apccs.fit, simplify = TRUE)

# Export fit statistics
write_csv(apccs.cfa.fit.stats,
          "APCCS CFA Fit Stats.csv")

## EFA
# Intialize data for EFA
apccs.efa.data <-
  na.omit(data[, c("APCCS_1", "APCCS_2", "APCCS_3", 
                   "APCCS_4", "APCCS_5", "APCCS_6",
                   "APCCS_7", "APCCS_8", "APCCS_9",
                   "APCCS_10", "APCCS_11", "APCCS_12")])

# Run diagnostics
# Check correlations of the items
apccs.corr.test <-
  corr.test(apccs.efa.data, use = "pairwise",
            method = "pearson", adjust = "holm",
            alpha = 0.05)

apccs.corr.r <- as.data.table(apccs.corr.test$r,
                              keep.rownames = TRUE)
apccs.corr.p <- as.data.table(apccs.corr.test$p,
                              keep.rownames = TRUE)

apccs.corr <- merge(apccs.corr.r,
                    apccs.corr.p,
                    by = "rn",
                    suffixes = c(".r", ".p"),
                    sort = FALSE)

# Export correlations table
write_csv(apccs.corr,
          "APCCS Correlations.csv")

# Run Kaiser-Meyer-Olkin Test for Sampling Adequacy
# >= 0.8 to pass
KMO(apccs.efa.data) # 0.83, good enough

# Run Bartlett's test of sphericity
# Needs to be significant
# i.e., correlation matrix is different from identity matrix
cortest.bartlett(apccs.efa.data) # p < .05

# Determining number of factors
apccs.ev <- eigen(cor(apccs.efa.data))
apccs.ev$values # 3 eigenvalues > 1, suggesting 3 factor structure

apccs.ap <-
  parallel(subject = nrow(apccs.efa.data),
           var = ncol(na.omit(apccs.efa.data)),
           rep = 100,
           cent = .05)

apccs.nscree <-
  nScree(x = apccs.ev$values,
         aparallel = apccs.ap$eigen$qevpea)

plotnScree(apccs.nscree) # Suggests 3 factor structure

# EFA with 3 factors
apccs.efa.3.factor <-
  factanal(apccs.efa.data, factors = 3, rotation = "oblimin")

apccs.efa.3.factor.loadings <-
  tidy(apccs.efa.3.factor, simplify = TRUE)

apccs.efa.3.factor.stats <-
  glance(apccs.efa.3.factor, simplify = TRUE)

semPaths(apccs.efa.3.factor, what = "est",
         residuals = T, cut = 0.3,
         posCol = c("white", "darkgreen"),
         negCol = c("white", "red"),
         edge.label.cex = 0.75, nCharNodes = 7)

# Export factor loadings
write.csv(apccs.efa.3.factor.loadings,
          "APCCS 3 Factor EFA.csv",
          row.names = FALSE)

# EFA with 2 factors
apccs.efa.2.factor <-
  factanal(apccs.efa.data, factors = 2, rotation = "oblimin")

apccs.efa.2.factor.loadings <-
  tidy(apccs.efa.2.factor, simplify = TRUE)

# ESEM
x <- esem(r = apccs.efa.data,
          varsX = c("APCCS_1", "APCCS_2", "APCCS_3", 
                    "APCCS_4", "APCCS_5", "APCCS_6",
                    "APCCS_7"),
          varsY = c("APCCS_8", "APCCS_9", "APCCS_10",
                    "APCCS_11", "APCCS_12"),
          nfX = 1,
          nfY = 1)

x.diagram <- esem.diagram(x)