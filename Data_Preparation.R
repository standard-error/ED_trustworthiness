###################################################################
#####    Estimating trait negative emotion differentiation:   #####
#####        How many measurement occasions and               #####
#####              emotion items are needed?                  #####
###################################################################

###################################################################
#####                 Data Preparation Script                 #####
###################################################################




# Packages ----------------------------------------------------------------
library(tidyverse)


# Load Data ---------------------------------------------------------------
load("../Rohdaten emolive/newly managed/running number ID/managed-and-cleaned_AA-started-only_pretest_and_AA.rda")
# data newly managed for this project


# Sanity Check

# calculate number of occasions in this data set as sanity check
# should be equal to n_occ_valid
AA.c %>% 
  group_by(SERIAL) %>% 
  mutate(n_occ = n()) -> AA.c

L2 <- dplyr::distinct(AA.c, SERIAL, n_occ, n_occ_valid)
identical(L2$n_occ, L2$n_occ_valid) # TRUE

table(L2$n_occ)
table(L2$n_occ >= 34) # 141
# -> not as reported in paper
# reason for this is unclear
# however, data were newly managed and cleaned and should be correct

rm(L2)



# sort
AA.c <- AA.c[order(AA.c$SERIAL, AA.c$day_since_planned_start, AA.c$occasion_for_day, AA.c$occasion_total), ]



# '' Save Cleaned Data Set ------------------------------------------------
save(AA.c, file="internal use/prepared data/emolive_clean_all_participants.rda")
rm(list=ls())





# PREPARE BENCHMARK DATA SET ----------------------------------------------

load("internal use/prepared data/emolive_clean_all_participants.rda")

L2 <- dplyr::distinct(AA.c, SERIAL, n_occ) 
table(L2$n_occ >= 100)
# 36 participants with at least 100 occasions
# use these as benchmark


# select participants with at least 100 occasions
bench <- AA.c[which(AA.c$n_occ >= 100), ]

# order occasions
bench <- bench[order(bench$SERIAL, bench$occasion_total), ]
# re-number the measurement occasions (only valid occasions)
bench <- bench %>% 
  group_by(SERIAL) %>% 
  mutate(occ_running = 1:n()) %>%  # new running number
  as.data.frame()


# select only 100 occasions for each participant
# by order!
bench <- bench[which(bench$occ_running <= 100), ]


# select variables relevant for analyses (i.e. ID [SERIAL],
# occasion running, occasion total, emotion terms)
bench <- bench[ , c("SERIAL", "occ_running", "occasion_total", "aerger1",
                    "aerger2", "aerger3", "traurigkeit1",
                    "traurigkeit2", "traurigkeit3", "angst1",
                    "angst2", "angst3", "scham1", "scham2", "scham3",
                    "schuld1", "schuld2", "schuld3")]



# save benchmark data set
save(bench, file = "internal use/prepared data/benchmark_data_Study1.rda") # for internal use (just for consistency)
save(bench, file = "prepared data/benchmark_data_Study1.rda") # for sharing




# PREPARE BENCHMARK DATA FOR GROUPS ---------------------------------------
# subset the data frame based on quantiles -> high, medium, low NED
source("functions/function_calculate_iccs.R")

ICCdata <- calculate_icc(data=bench,id.var="SERIAL", items = c('aerger1', 'aerger2', 'aerger3',
                                                               'traurigkeit1', 'traurigkeit2', 'traurigkeit3',
                                                               'angst1', 'angst2', 'angst3',
                                                               'scham1', 'scham2', 'scham3',
                                                               'schuld1', 'schuld2', 'schuld3'),
                         type="consistency", unit="single")
quantile(ICCdata[ ,"ICC"], probs=c(0.33, 0.66))[1]
# raw ICCs: 33% percentile  = 0.1921, 66% percentile = 0.2882
quantile(ICCdata[ ,"ICC.z"], probs=c(0.33, 0.66))
# ICC.z: 33% percentile = 0.7595, 66% percentile = 0.9781

# -> same participants in sub groups when using ICC vs. ICC.z?
# CAVE: ICCs not reversed -> low ICC = high NED
highNED.ID <- ICCdata[which(ICCdata[ , "ICC"] < quantile(ICCdata[ ,"ICC"], probs=c(0.33, 0.66))[1]), ]
mediumNED.ID <- ICCdata[which(quantile(ICCdata[ ,"ICC"], probs=c(0.33, 0.66))[1] < ICCdata[ , "ICC"] &
                                ICCdata[ , "ICC"] < quantile(ICCdata[ ,"ICC"], probs=c(0.33, 0.66))[2]), ]
lowNED.ID <- ICCdata[which(ICCdata[ , "ICC"] > quantile(ICCdata[ , "ICC"], probs = c(0.33, 0.66))[2]), ]

# highNED.ID.z <- ICCdata[which(ICCdata[ , "ICC.z"] < quantile(ICCdata[ ,"ICC.z"], probs=c(0.33, 0.66))[1]), ]
# mediumNED.ID.z <- ICCdata[which(quantile(ICCdata[ ,"ICC.z"], probs=c(0.33, 0.66))[1] < ICCdata[ , "ICC.z"] &
#                                 ICCdata[ , "ICC.z"] < quantile(ICCdata[ ,"ICC.z"], probs=c(0.33, 0.66))[2]), ]
# lowNED.ID.z <- ICCdata[which(ICCdata[ , "ICC.z"] > quantile(ICCdata[ , "ICC.z"], probs = c(0.33, 0.66))[2]), ]
# 
# 
# all(highNED.ID == highNED.ID.z)
# all(mediumNED.ID == mediumNED.ID.z)
# all(lowNED.ID == lowNED.ID.z)
# # all participants are identically assigned for ICC and ICC.z
# rm(highNED.ID.z, mediumNED.ID.z, lowNED.ID.z)

# subset data
bench_highNED <- bench[which(bench$SERIAL %in% highNED.ID), ]
bench_mediumNED <- bench[which(bench$SERIAL %in% mediumNED.ID), ]
bench_lowNED <- bench[which(bench$SERIAL %in% lowNED.ID), ]


# save data
save(bench_highNED, file="internal use/prepared data/benchmark_data_highNED_Study1.rda") # for internal use (just for consistency)
save(bench_mediumNED, file="internal use/prepared data/benchmark_data_mediumNED_Study1.rda") # for internal use (just for consistency)
save(bench_lowNED, file="internal use/prepared data/benchmark_data_lowNED_Study1.rda") # for internal use (just for consistency)


save(bench_highNED, file="prepared data/benchmark_data_highNED_Study1.rda") # for sharing
save(bench_mediumNED, file="prepared data/benchmark_data_mediumNED_Study1.rda") # for sharing
save(bench_lowNED, file="prepared data/benchmark_data_lowNED_Study1.rda") # for sharing



# Session Info ------------------------------------------------------------

rm(list=ls())
sessionInfo()

# R version 4.5.1 (2025-06-13 ucrt)
# Platform: x86_64-w64-mingw32/x64
# Running under: Windows 11 x64 (build 26100)
# 
# Matrix products: default
# LAPACK version 3.12.1
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
# [1] lubridate_1.9.4 forcats_1.0.0   stringr_1.5.1   dplyr_1.1.4     purrr_1.1.0     readr_2.1.5     tidyr_1.3.1    
# [8] tibble_3.3.0    ggplot2_3.5.2   tidyverse_2.0.0
# 
# loaded via a namespace (and not attached):
# [1] vctrs_0.6.5        cli_3.6.5          rlang_1.1.6        stringi_1.8.7      generics_0.1.4     irr_0.84.1        
# [7] glue_1.8.0         hms_1.1.3          lpSolve_5.6.23     scales_1.4.0       grid_4.5.1         tzdb_0.5.0        
# [13] lifecycle_1.0.4    compiler_4.5.1     RColorBrewer_1.1-3 timechange_0.3.0   pkgconfig_2.0.3    rstudioapi_0.17.1 
# [19] farver_2.1.2       R6_2.6.1           tidyselect_1.2.1   pillar_1.11.0      magrittr_2.0.3     tools_4.5.1       
# [25] withr_3.0.2        gtable_0.3.6      