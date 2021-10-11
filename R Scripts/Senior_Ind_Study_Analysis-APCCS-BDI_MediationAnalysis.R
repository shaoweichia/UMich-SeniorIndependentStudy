##### SENIOR INDEPENDENT STUDY ANALYSIS - ANALYSIS #####
# Career Congruence and Depression in College Students:
# Does Interpersonal-Psychological Theory Matter?

#### PREP ####
options(scipen = 999)

### Load libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(dplyr)
  library(readr)
  library(data.table)
  library(broom)
  library(foreign)
  library(psych)
  library(semTools)
  library(lavaan)
  library(semPlot)
})

### Load cleaned data
## Set directory to import data
setwd("C:/Users/Shao Wei/Documents/Chia Shao Wei/U of M/2014-2015/Fall 2014/PSYCH 422/Data")

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
### Set directory for results output
setwd("C:/Users/Shao Wei/Documents/Chia Shao Wei/U of M/2014-2015/Fall 2014/PSYCH 422/Results/Raw R Output")

### User-defined functions
GetRespondentStats <- function(data, var.list) {
  #' Return n-counts and proportions by criteria given
  #' @param data: Data table
  #' @param var.list: A vector of strings with respondent groups of interest
  
  # Get frequency
  frequency.list <- apply(data.table(var.list), 1, function(x) {
    data[, table(get(x))] %>%
      addmargins(1) %>%
      as.data.table()
  })
  names(frequency.list) <- var.list
  frequency <- rbindlist(frequency.list, idcol = "var")
  
  # Get proportions
  proportion.list <- apply(data.table(var.list), 1, function(x) {
    data[, table(get(x))] %>%
      prop.table() %>%
      as.data.table()
  })
  names(proportion.list) <- var.list
  proportion <- rbindlist(proportion.list, idcol = "var")
  
  # Merge frequency and proportions table
  freq.prop <- merge(frequency, proportion,
                     by = c("var", "V1"),
                     all = TRUE, sort = FALSE,
                     suffixes = c(".count", ".pct"))
  
  return(freq.prop)
}

## Respondents' characteristics
respondent.stats <-
  GetRespondentStats(final.data,
                     demog.var[-length(demog.var)]) # drop the last item

# Export output to CSV
write_csv(respondent.stats,
          "Respondents Stats.csv")

### Descriptives
decriptive.stats <- describe(final.data[, ..composite.var]) %>%
  as.data.table(keep.rownames = TRUE)

### Correlations
corr.table <- corr.test(final.data[, ..composite.var])

# Extract coefficients, n-counts, and p-values
corr.r <- corr.table$r %>% data.table(keep.rownames = TRUE)
corr.n <- corr.table$n %>% data.table(keep.rownames = TRUE)
corr.p <- corr.table$p %>% data.table(keep.rownames = TRUE)

# Extract significant coefficients
corr.r.sig <- mapply(function (x, y) {
  ifelse(test = y %in% composite.var,
         yes = y,
         no = ifelse(test = x == 1,
                     yes = "-",
                     no = ifelse(test = y <= 0.001,
                                 yes = paste0(round(x, 2),
                                              "***"),
                                 no = ifelse(test = y <= 0.01,
                                             yes = paste0(round(x, 2),
                                                          "**"),
                                             no = ifelse(test = y < 0.05,
                                                         yes = paste0(round(x,2),
                                                                      "*"),
                                                         no = round(x, 2))))))
}, corr.r, corr.p) %>%
  data.table()

# Export correlation table
write_csv(corr.r.sig,
          "Correlations.csv")

### Mediation analysis
## Hypothesis 1: perceived burdensomeness (inq_burden) and thwarted
## belongingness (inq_belong) fully mediate the relationship between
## adolescent-parent career congruence (apccs_complement, apccs_supplement) and
## student's depressive symptoms (bdi_composite)
var.h1 <- c("bdi_composite", "apccs_complement", "apccs_supplement",
            "inq_burden", "inq_belong")

corr.h1 <- corr.r.sig[rn %in% var.h1, .SD, .SDcols = c("rn", var.h1)]

# Adolescent-parent career complementary congruence (apccs_complement) and 
# depressive symptoms (bdi_composite)
model.h1.complement <- '
  # full regression model
  bdi_composite ~ b1 * inq_burden + b2 * inq_belong + c * apccs_complement
  
  # mediators
  inq_burden ~ a1 * apccs_complement
  inq_belong ~ a2 * apccs_complement
  
  # indirect effects
  indirect_burden := a1 * b1
  indirect_belong := a2 * b2
  
  # direct effect
  direct := c
  
  # total effect
  total := c + (a1 * b1) + (a2 * b2)
  
  # covariance
  inq_burden ~~ inq_belong
'

fit.h1.complement <- sem(model = model.h1.complement,
                         data = final.data,
                         se = "bootstrap",
                         bootstrap = 5000)

fit.h1.complement.summary <- tidy(fit.h1.complement,
                                  conf.int = TRUE,
                                  conf.level = 0.95,
                                  boot.ci.type = "bca.simple")

# Test if the indirect effects are equal to each other
# Initialize contrast model
constrain.model.h1.complement <- '
  # full regression model
  bdi_composite ~ b1 * inq_burden + b2 * inq_belong + c * apccs_complement
  
  # mediators
  inq_burden ~ a1 * apccs_complement
  inq_belong ~ a2 * apccs_complement
  
  # indirect effects
  indirect_burden := a1 * b1
  indirect_belong := a2 * b2
  
  # direct effect
  direct := c
  
  # contrast
  contrast := indirect_burden - (indirect_belong)
  
  # total effect
  total := c + (a1 * b1) + (a2 * b2)
  
  # covariance
  inq_burden ~~ inq_belong
'

constrain.fit.h1.complement <- sem(model = constrain.model.h1.complement,
                                   data = final.data,
                                   se = "bootstrap",
                                   bootstrap = 5000)

constrain.fit.h1.complement.summary <- tidy(fit.h1.complement,
                                            conf.int = TRUE,
                                            conf.level = 0.95,
                                            boot.ci.type = "bca.simple")

# Export model summary to CSV
write_csv(constrain.fit.h1.complement.summary,
          "APCCS-Complement Mediation.csv")

# Adolescent-parent career supplementary congruence (apccs_supplement) and 
# depressive symptoms (bdi_composite)
model.h1.supplement <- '
  # full regression model
  bdi_composite ~ b1 * inq_burden + b2 * inq_belong + c * apccs_supplement
  
  # mediators
  inq_burden ~ a1 * apccs_supplement
  inq_belong ~ a2 * apccs_supplement
  
  # indirect effects
  indirect_burden := a1 * b1
  indirect_belong := a2 * b2
  
  # direct effect
  direct := c
  
  # total effect
  total := c + (a1 * b1) + (a2 * b2)
  
  # covariance
  inq_burden ~~ inq_belong
'

fit.h1.supplement <- sem(model = model.h1.supplement,
                         data = final.data,
                         se = "bootstrap",
                         bootstrap = 5000)

fit.h1.supplement.summary <- tidy(fit.h1.supplement,
                                  conf.int = TRUE,
                                  conf.level = 0.95,
                                  boot.ci.type = "bca.simple")

# Test if the indirect effects are equal to each other
# Initialize contrast model
contrast.model.h1.supplement <- '
  # full regression model
  bdi_composite ~ b1 * inq_burden + b2 * inq_belong + c * apccs_supplement
  
  # mediators
  inq_burden ~ a1 * apccs_supplement
  inq_belong ~ a2 * apccs_supplement
  
  # indirect effects
  indirect_burden := a1 * b1
  indirect_belong := a2 * b2
  
  # direct effect
  direct := c
  
  # contrast
  contrast := indirect_burden - (indirect_belong)
  
  # total effect
  total := c + (a1 * b1) + (a2 * b2)
  
  # covariance
  inq_burden ~~ inq_belong
'
contrast.fit.h1.supplement <- sem(model = contrast.model.h1.supplement,
                                  data = final.data,
                                  se = "bootstrap",
                                  bootstrap = 5000)

contrast.fit.h1.supplement.summary <- tidy(contrast.fit.h1.supplement,
                                           conf.int = TRUE,
                                           conf.level = 0.95,
                                           boot.ci.type = "bca.simple")

# Export model summary to CSV
write_csv(contrast.fit.h1.supplement.summary,
          "APCCS-Supplement Mediation.csv")
