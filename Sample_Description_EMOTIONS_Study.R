###################################################################
#####      Estimating trait emotion differentiation:          #####
#####         How many measurement occasions and              #####
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
load("internal use/prepared data/EMOTIONS_benchmark_with_sociodemographic_var.rda")

# # sanity check: check whether benchmark data is identical to that used in the simulation
# load("prepared data/EMOTIONS_benchmark_data.rda")
# 
# names(bench_with_demo)
# sub_demo <- bench_with_demo[ , names(bench)] # select only variables that are in benchmark used for simulation
# # i.e., no sociodemographic variables
# 
# all.equal(sub_demo, bench)
# # row names differ
# 
# all.equal(sub_demo, bench, check.attributes=FALSE)
# 
# rownames(sub_demo) <- NULL
# rownames(bench) <- NULL
# 
# identical(sub_demo, bench)
# # identical (besides row names, but these do not matter and aren't used in simulation)
# 
# rm(bench, sub_demo)





# Compliance in Overall Sample --------------------------------------------
load("internal use/prepared data/EMOTIONS_clean_all_participants.rda")
"n_occ" %in% names(dat)

tmp <- dplyr::distinct(dat, id, n_occ)
psych::describe(tmp$n_occ)


table(tmp$n_occ >= 70) # 251 participants had >= 70 occasions
# BUT there were variance problems for 75 of them
# -> these participants were excluded (SEE DATA PREPARATION SCRIPT)
# 176 remained
length(unique(bench_with_demo$id))
251 - 176 # 75, check

rm(tmp, dat)



# Describe Benchmark Sample -----------------------------------------------
bench_with_demo
length(unique(bench_with_demo$id)) # 176 participants

bench_with_demo <- as.data.frame(bench_with_demo) # data frame needed for ICC calculation


# Calculate ICCs for Benchmark Sample -------------------------------------
# with all valid occasions (ICC_all, ICC.z_all)

# negative emotions
neg_ICC <- calculate_icc(data = bench_with_demo, id.var = "id",
                         items = c("angry", "excluded", "envious",
                                   "resentful", "ashamed", "insecure",
                                   "anxious", "sad", "lonely"),
                         type = "consistency",
                         unit = "single")
colnames(neg_ICC) <- c("id", "neg_ICC", "neg_ICC.z")

# positive emotions
pos_ICC <- calculate_icc(data = bench_with_demo, id.var = "id",
                         items = c("proud", "success", "superior",
                                   "enthusiastic", "relaxed"),
                         type = "consistency",
                         unit = "single")
colnames(pos_ICC) <- c("id", "pos_ICC", "pos_ICC.z")


## merge with sample data
bench_with_demo <- merge(bench_with_demo, neg_ICC, by = "id")
bench_with_demo <- merge(bench_with_demo, pos_ICC, by = "id")




# Describe Sample ---------------------------------------------------------

# Level 2 -----------------------------------------------------------------
# use those variables that we also report for emolive study
# L2 variables: gender, gender_specification, age, education, education_other, occupational_status,
 #neg_ICC, neg_ICC.z, pos_ICC, pos_ICC.z

# create factor variables according to codebook

# gender: 1 = female, 2 = male, 3 = other
bench_with_demo$gender.f <- factor(bench_with_demo$gender_fin,
                                   levels=c(1,2,3),
                                   labels=c("female", "male", "other"))

# educational status
bench_with_demo$educational_status.f <- factor(bench_with_demo$educational_status_fin,
                                               levels=c(1,2,3,4,5,6,7,8,9,10),
                                               labels=c("no certificate",
                                                        "9th grade school-leaving certificate with no additional vocational training",
                                                        "9th grade school-leaving certificate plus vocational training",
                                                        "10th grade school-leaving certificate with no additional vocational training",
                                                        "10th grade school-leaving certificate plus vocational training",
                                                        "general qualification for university entrance with no additional vocational training",
                                                        "general qualification for universtiy entrance plus vocational training",
                                                        "university of applied sciences degree",
                                                        "university degree",
                                                        "university degree and PhD"))

# occupational status
bench_with_demo$occupational_status.f <- factor(bench_with_demo$occupational_status_fin,
                                                levels = c(1,2,3,4,5,6,7,8,9,10),
                                                labels = c("in school",
                                                           "at university",
                                                           "in vocational training",
                                                           "completing voluntary national service/voluntary military service",
                                                           "full-time employment",
                                                           "part-time employment",
                                                           "self-employed",
                                                           "full-time parental leave/care leave/full-time homemaker",
                                                           "unemployed/looking for work",
                                                           "retired"))
# higher education
bench_with_demo$higher_ed.f <- factor(bench_with_demo$higher_ed_fin,
                                      levels = c(1,2,3),
                                      labels = c("yes, at a university",
                                                 "yes, at a university of applied sciences",
                                                 "no"))
# higher education type
bench_with_demo$higher_ed_type.f <- factor(bench_with_demo$higher_ed_type_fin,
                                      levels = c(1,2,3,4,5,6),
                                      labels = c("bachelor",
                                                 "master",
                                                 "diploma",
                                                 "master in the old German system (before Bologna reform)",
                                                 "state examination",
                                                 "teacher education"))

# select distinct rows with L2 variables
L2 <- dplyr::distinct(bench_with_demo, id, wave, gender.f, gender_specification_fin,
                      age_fin, educational_status.f, occupational_status.f,
                      higher_ed.f, higher_ed_type.f,
                      neg_ICC, neg_ICC.z, pos_ICC, pos_ICC.z)

# describe sample
table(L2$wave, useNA="always")
prop.table(table(L2$wave, useNA="always"))

table(L2$gender.f, useNA="always")
prop.table(table(L2$gender.f, useNA="always"))

table(L2$gender_specification_fin, useNA="always")

table(L2$educational_status.f,useNA="always")
prop.table(table(L2$educational_status.f, useNA="always"))

table(L2$higher_ed.f, useNA="always") 
prop.table(table(L2$higher_ed.f, useNA="always"))

table(L2$higher_ed_type.f, useNA="always") 
prop.table(table(L2$higher_ed_type.f, useNA="always") )

table(L2$occupational_status.f, useNA="always")
prop.table(table(L2$occupational_status.f, useNA="always"))


psych::describe(L2$age_fin)
psych::describe(L2$neg_ICC)
psych::describe(L2$neg_ICC.z)
psych::describe(L2$pos_ICC)
psych::describe(L2$pos_ICC.z)



# Level 1 -----------------------------------------------------------------
# L1 variables: emotions
# -> M and SD (within/between) for each emotion term

# based on 70 occasions per participant (benchmark data)


# empty storage for descriptive statistics
# 9 + 5 rows for 9 + 5 items
desc_stats <- data.frame(item = c("angry", "excluded", "envious", "resentful", "ashamed", "insecure", # negative emotions
                                  "anxious", "sad", "lonely", 
                                  "proud", "success", "superior", "enthusiastic", "relaxed"), # positive emotions
                         M = NA,
                         SD_within = NA,
                         SD_between = NA,
                         ICC = NA,
                         range = NA)


for (item in desc_stats$item) {
  
  formula <- as.formula(paste0(item, "~ 1 + (1 | id)"))
  
  null.mod <- lme4::lmer(formula, data=bench_with_demo)
  
  # extract information from model
  mean <- summary(null.mod)[["coefficients"]][1,1] # extract intercept from null model
  sd_between <- as.data.frame(lme4::VarCorr(null.mod))[1, "sdcor"] # L2 ("id" intercept)
  sd_within <- as.data.frame(lme4::VarCorr(null.mod))[2, "sdcor"] # L1 ("residual")
  
  icc <- performance::icc(null.mod)$ICC_adjusted
  
  range <- paste0(range(bench_with_demo[ , item])[1], " - ", range(bench_with_demo[ , item])[2])
  
  # round and save in storage
  desc_stats[which(desc_stats$item == item), "M"] <- round(mean, 2)
  desc_stats[which(desc_stats$item == item), "SD_within"] <- round(sd_within, 2)
  desc_stats[which(desc_stats$item == item), "SD_between"] <- round(sd_between, 2)
  desc_stats[which(desc_stats$item == item), "ICC"] <- round(icc, 2)
  desc_stats[which(desc_stats$item == item), "range"] <- range
}


# save table as .csv
write.csv(desc_stats, "results/02_revision_1/EMOTIONS study/EMOTIONS_descriptive_statistics_emotions.csv", row.names = F)




# Session Info ------------------------------------------------------------
sessionInfo()

# R version 4.5.3 (2026-03-11 ucrt)
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
#  [1] lubridate_1.9.4 forcats_1.0.1   stringr_1.6.0   dplyr_1.1.4     purrr_1.1.0     readr_2.1.5    
#  [7] tidyr_1.3.1     tibble_3.3.0    ggplot2_4.0.2   tidyverse_2.0.0
# 
# loaded via a namespace (and not attached):
#  [1] generics_0.1.4     lpSolve_5.6.23     gtools_3.9.5       stringi_1.8.7      lattice_0.22-9    
#  [6] lme4_2.0-1         hms_1.1.4          magrittr_2.0.3     timechange_0.3.0   grid_4.5.3        
# [11] RColorBrewer_1.1-3 Matrix_1.7-4       writexl_1.5.4      scales_1.4.0       mnormt_2.1.2      
# [16] reformulas_0.4.4   Rdpack_2.6.6       cli_3.6.5          rlang_1.2.0        rbibutils_2.4.1   
# [21] irr_0.84.1         performance_0.16.0 splines_4.5.3      withr_3.0.2        tools_4.5.3       
# [26] parallel_4.5.3     tzdb_0.5.0         nloptr_2.2.1       minqa_1.2.8        boot_1.3-32       
# [31] vctrs_0.6.5        R6_2.6.1           lifecycle_1.0.5    MASS_7.3-65        psych_2.6.5       
# [36] insight_1.4.6      pkgconfig_2.0.3    pillar_1.11.1      gtable_0.3.6       glue_1.8.0        
# [41] Rcpp_1.1.1-1       tidyselect_1.2.1   rstudioapi_0.18.0  farver_2.1.2       nlme_3.1-168      
# [46] gdata_3.0.1        compiler_4.5.3     S7_0.2.0   
