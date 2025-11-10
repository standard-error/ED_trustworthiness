###################################################################
#####    Estimating trait negative emotion differentiation:   #####
#####        How many measurement occasions and               #####
#####              emotion items are needed?                  #####
###################################################################

###################################################################
#####                  Sample Description                     #####
###################################################################


# for description of data cleaning, see data preparation scripts

# note: due to data privacy, no demographic variables are uploaded
# to OSF -> sample description cannot be run



# Packages ----------------------------------------------------------------
library(tidyverse)



# Source Function to Calculate ICCs ---------------------------------------
source("functions/function_calculate_iccs.R")


# Load Data ---------------------------------------------------------------
load("internal use/prepared data/emolive_clean_all_participants.rda")


names(AA.c)
table(dplyr::distinct(AA.c, SERIAL, n_occ)$n_occ >= 70)


# Select Benchmark Sample -------------------------------------------------
samp <- AA.c[which(AA.c$n_occ >= 70), ]
# length(unique(samp$SERIAL))

samp <- as.data.frame(samp) # data frame needed for ICC calculation


# Calculate ICCs for Benchmark Sample -------------------------------------
# with all valid occasions (ICC_all, ICC.z_all)
ICC_all <- calculate_icc(data = samp, id.var = "SERIAL",
                         items = c('aerger1', 'aerger2', 'aerger3',
                                   'traurigkeit1', 'traurigkeit2', 'traurigkeit3',
                                   'angst1', 'angst2', 'angst3',
                                   'scham1', 'scham2', 'scham3',
                                   'schuld1', 'schuld2', 'schuld3'),
                         type = "consistency",
                         unit = "single")
colnames(ICC_all) <- c("SERIAL", "ICC_all", "ICC.z_all")


# with 70 occasions used for the analyses (ICC_70, ICC.z_70)
## select first 70 occasions per participant
## order occasions
samp <- samp[order(samp$SERIAL, samp$occasion_total), ]
## re-number the measurement occasions (only valid occasions)
samp <- samp %>% 
  group_by(SERIAL) %>% 
  mutate(occ_running = 1:n()) %>%  # new running number
  as.data.frame()


## select only 70 occasions for each participant
# by order!
samp_70 <- samp[which(samp$occ_running <= 70), ]

## calculate ICCs
ICC_70 <- calculate_icc(data = samp_70, id.var = "SERIAL",
                         items = c('aerger1', 'aerger2', 'aerger3',
                                   'traurigkeit1', 'traurigkeit2', 'traurigkeit3',
                                   'angst1', 'angst2', 'angst3',
                                   'scham1', 'scham2', 'scham3',
                                   'schuld1', 'schuld2', 'schuld3'),
                         type = "consistency",
                         unit = "single")
colnames(ICC_70) <- c("SERIAL", "ICC_70", "ICC.z_70")

## merge with sample data
samp <- merge(samp, ICC_all, by="SERIAL")
samp <- merge(samp, ICC_70, by="SERIAL")


samp_70 <- merge(samp_70, ICC_all, by="SERIAL")
samp_70 <- merge(samp_70, ICC_70, by="SERIAL")

# Describe Sample ---------------------------------------------------------

# Level 2 -----------------------------------------------------------------
# L2 variables: gender, age, education, education_other, occupation,
# occupation_other, language_skills, schedule, n_occ_completed,
# n_occ_completed_perc, n_occ_valid, n_occ_valid_perc, ICC_all,
# ICC.z_all, ICC_70, ICC.z_70



# select distinct rows with L2 variables
L2 <- dplyr::distinct(samp, SERIAL, gender, age, education, education_other,
                      occupation, occupation_other, language_skills,
                      schedule, n_occ_completed, n_occ_completed_perc,
                      n_occ_valid, n_occ_valid_perc, ICC_all, ICC.z_all,
                      ICC_70, ICC.z_70)

table(L2$gender, useNA="always")
prop.table(table(L2$gender, useNA="always"))

table(L2$education)
prop.table(table(L2$education))

table(L2$education_other) # Bachelor = university degree
# Fachhochschul-/Hochschulabschluss = 53
# 53/109 = 0.486 = 49%

table(L2$occupation, useNA="always")
prop.table(table(L2$occupation, useNA="always"))

table(L2$occupation_other)


table(L2$language_skills, useNA="always")
prop.table(table(L2$language_skills, useNA="always"))

table(L2$schedule, useNA="always")
prop.table(table(L2$schedule, useNA="always"))


psych::describe(L2$age)
psych::describe(L2$n_occ_completed)
psych::describe(L2$n_occ_completed_perc)
psych::describe(L2$n_occ_valid)
psych::describe(L2$n_occ_valid_perc)
psych::describe(L2$ICC_all)
psych::describe(L2$ICC.z_all)
psych::describe(L2$ICC_70)
psych::describe(L2$ICC.z_70)

# Level 1 -----------------------------------------------------------------
# L1 variables: emotions
# -> M and SD (within/between) for each emotion term, inter-correlations between emotion terms

# based on 70 occasions per participant (samp_70)


# empty storage for descriptive statistics
# 15 rows for 15 items
desc_stats <- data.frame(item = c('aerger1', 'aerger2', 'aerger3',
                                  'traurigkeit1', 'traurigkeit2', 'traurigkeit3',
                                  'angst1', 'angst2', 'angst3',
                                  'scham1', 'scham2', 'scham3',
                                  'schuld1', 'schuld2', 'schuld3'),
                         M = NA,
                         SD_within = NA,
                         SD_between = NA,
                         ICC = NA,
                         range = NA)


for (item in desc_stats$item) {
  
  formula <- as.formula(paste0(item, "~ 1 + (1 | SERIAL)"))
  
  null.mod <- lme4::lmer(formula, data=samp_70)
  
  # extract information from model
  mean <- summary(null.mod)[["coefficients"]][1,1] # extract intercept from null model
  sd_between <- as.data.frame(lme4::VarCorr(null.mod))[1, "sdcor"] # L2 ("SERIAL" intercept)
  sd_within <- as.data.frame(lme4::VarCorr(null.mod))[2, "sdcor"] # L1 ("residual")
  
  icc <- performance::icc(null.mod)$ICC_unadjusted
  
  range <- paste0(range(samp_70[ , item])[1], " - ", range(samp_70[ , item])[2])
  
  # round and save in storage
  desc_stats[which(desc_stats$item == item), "M"] <- round(mean, 2)
  desc_stats[which(desc_stats$item == item), "SD_within"] <- round(sd_within, 2)
  desc_stats[which(desc_stats$item == item), "SD_between"] <- round(sd_between, 2)
  desc_stats[which(desc_stats$item == item), "ICC"] <- round(icc, 2)
  desc_stats[which(desc_stats$item == item), "range"] <- range
}

# save table as .csv
write.csv(desc_stats, "results/descriptive_statistics_emotions.csv", row.names = F)




# Session Info ------------------------------------------------------------
sessionInfo()

# R version 4.5.1 (2025-06-13 ucrt)
# Platform: x86_64-w64-mingw32/x64
# Running under: Windows 11 x64 (build 26200)
# 
# Matrix products: default
#   LAPACK version 3.12.1
# 
# locale:
# [1] LC_COLLATE=German_Germany.utf8  LC_CTYPE=German_Germany.utf8    LC_MONETARY=German_Germany.utf8
# [4] LC_NUMERIC=C                    LC_TIME=German_Germany.utf8    
# 
# time zone: Europe/Berlin
# tzcode source: internal
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#  [1] lubridate_1.9.4 forcats_1.0.0   stringr_1.5.1   dplyr_1.1.4     purrr_1.1.0     readr_2.1.5     tidyr_1.3.1    
#  [8] tibble_3.3.0    ggplot2_3.5.2   tidyverse_2.0.0
# 
# loaded via a namespace (and not attached):
#  [1] generics_0.1.4     lpSolve_5.6.23     stringi_1.8.7      lattice_0.22-7     lme4_1.1-37        hms_1.1.3         
#  [7] magrittr_2.0.3     grid_4.5.1         timechange_0.3.0   RColorBrewer_1.1-3 Matrix_1.7-3       scales_1.4.0      
# [13] mnormt_2.1.1       reformulas_0.4.1   Rdpack_2.6.4       cli_3.6.5          rlang_1.1.6        rbibutils_2.3     
# [19] performance_0.15.0 irr_0.84.1         splines_4.5.1      withr_3.0.2        tools_4.5.1        parallel_4.5.1    
# [25] tzdb_0.5.0         nloptr_2.2.1       minqa_1.2.8        boot_1.3-31        vctrs_0.6.5        R6_2.6.1          
# [31] lifecycle_1.0.4    MASS_7.3-65        psych_2.5.6        insight_1.3.1      pkgconfig_2.0.3    pillar_1.11.0     
# [37] gtable_0.3.6       glue_1.8.0         Rcpp_1.1.0         tidyselect_1.2.1   rstudioapi_0.17.1  farver_2.1.2      
# [43] nlme_3.1-168       compiler_4.5.1