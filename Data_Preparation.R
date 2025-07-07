###################################################################
#####    Estimating trait negative emotion differentiation:   #####
#####        How many measurement occasions and               #####
#####              emotion items are needed?                  #####
###################################################################

###################################################################
#####                 Data Preparation Script                 #####
###################################################################



# Working Directory -------------------------------------------------------
setwd("C:/Users/ecker/Seafile/Meine Bibliothek/Studien/2) ED Reliability/Data Analysis/data_analysis_git") 


# Packages ----------------------------------------------------------------
library(tidyverse)


# STEP 1: FAMILIARIZE WITH DATA -------------------------------------------
# there are two data sets with different numbers of occasions and different
# variables:
# AA_final: 16115 occasion, no variable for carelessness (or duration)
# AA_with_careless: 15967, includes carelessness (and duration)


# '' Load Data ------------------------------------------------------------


# Compare the two data sets that we have
load("internal use/raw data/data provided to us/AA_emolive_including_careless.rda")
AA_careless <- AA
load("internal use/raw data/data provided to us/AA_final.rda")
AA_final <- AA
rm(AA)
# somehow, the final data set includes more occasions than the data set
# with careless responding?


which(! names(AA_final) %in% names(AA_careless))
names(AA_final)[7] # day_problematic not in AA_careless
which(! names(AA_careless) %in% names(AA_final))
names(AA_careless)[54:57] # duration, careless.occasion, incomplete, invalid

AA_careless_sub <- AA_careless[ , c("SERIAL", "occasion_total",
                                    "duration", "careless.occasion",
                                    "incomplete", "invalid")]

# match the two data sets
AA_ges <- merge(AA_final, AA_careless_sub, by=c("SERIAL", "occasion_total"),
                all.x=TRUE)
table(AA_ges$duration, useNA="always") # 148 missing
# day problematic probably already excluded

sub <- AA_ges[which(is.na(AA_ges$duration)), ]
table(sub$day_problematic, useNA="always") # correct
# AA_careless does not include problematic days#
# therefore, AA_ges includes all occasions
rm(sub)

AA <- AA_ges
rm(AA_careless, AA_careless_sub, AA_final, AA_ges)
# now we have all the information we need




# '' Save Data ------------------------------------------------------------
# save whole data frame
save(AA, file="internal use/raw data/AA_with_careless_and_day_problematic.rda")

rm(list=ls())



# STEP 2: PREPARE THE FULL DATA SET ---------------------------------------
load("internal use/raw data/AA_with_careless_and_day_problematic.rda")

# '' Prepare Data ---------------------------------------------------------
length(unique(AA$SERIAL)) # 163 participants with 16115 occasions

names(AA)

# OVERVIEW BEFORE EXCLUSION
table(AA$day_problematic, useNA="always")
# 148 occasions at which day was problematic
# -> exclude

table(AA$Missing, useNA="always")
# # 12499 occasions were completed, rest is missing (dismissed,
# # ignored, incomplete)
# 12499 + 370 + 3106 + 140 # add up to total number
# 16115 - 370 - 3106 - 140 # 12499 should remain when exluding missings
# 370 + 3106 + 140 # 3616 should be excluded
# # -> exclude

table(AA$duration < 50) # 3663 occasions with fewer than 50 seconds
# -> careless
table(AA$careless.occasion, useNA="always")
# but only 263 flagged as careless -> probably done after
# already excluding some occasions
# table(AA$duration <= 50)


# NOTE: Some of these occasions may overlap (e.g., they may be missing
# but also belong to a day that was problematic)


# EXCLUDE INVALID OCCASIONS
# exclude missing occasions
AA.c <- AA[which(AA$Missing != "Dismissed" & AA$Missing != "Ignored" &
                   AA$Missing != "Incomplete"), ]
nrow(AA) - nrow(AA.c) # 3616, correct
nrow(AA.c) # correct, 12499 occasions completed

# exclude occasions from problematic days
table(AA.c$day_problematic, useNA="always")
# 125 of the completed occasions belonged to a problematic day
# (a total of 148 occasions were on problematic days, but
# apparently, 23 of them were not completed)
nrow(AA.c) # before exclusion: 12499
12499 - 125 # = 12374
AA.c <- AA.c[which(AA.c$day_problematic == 0), ]
nrow(AA.c) # after exclusion: 12347 -> correct



# careless responding
table(AA.c$careless.occasion, useNA="always") # 263 that were flagged
table(AA.c$duration < 50) # 206 that were faster than 50 seconds
# why is there a difference?
table(AA.c$duration <= 50) # 263
# View(AA.c[ , c("duration", "careless.occasion")])
# -> occasions with exactly 50 seconds were also flagged as careless
# -> in paper reported that the fastest response on ALL items in the
# pilot study was 50 seconds and that responses faster than 50 seconds
# were seen as careless
# -> apparently, also exactly 50 seconds were flagged as careless

# exclude occasions with <= 50 seconds to be consistent with paper
# 263 should be excluded
# before: 12374
12374 - 263 # 12111 should remain
AA.c <- AA.c[which(AA.c$duration > 50), ]
nrow(AA.c) # correct



# calculate compliance of valid occasions as sanity check
AA.c %>% 
  group_by(SERIAL) %>% 
  mutate(n_occ = n()) -> AA.c

L2 <- dplyr::distinct(AA.c, SERIAL, n_occ)
table(L2$n_occ)
table(L2$n_occ >= 34) # 141
# -> not as reported in paper
# reason for this is unclear
# however, all invalid occasions were removed
# use this data

rm(L2)

# '' Save Cleaned Data Set ------------------------------------------------
save(AA.c, file="internal use/prepared data/emolive_clean_all_participants.rda")
rm(list=ls())


# STEP 3: PREPARE BENCHMARK DATA SET --------------------------------------
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


# for anonymization: replace SERIAL by a running number
matched_serial <- data.frame(SERIAL_original = unique(bench$SERIAL),
                             SERIAL_new = NA)
matched_serial$SERIAL_new <- 1:nrow(matched_serial)
# save matching
save(matched_serial, file="internal use/prepared data/matching_original_serials_and_new_serials.rda")
# 

# now replace original SERIAL with new SERIAL
names(matched_serial) <- c("SERIAL", "SERIAL_new")
bench <- merge(bench, matched_serial, by = "SERIAL")

# remove original serial and order variables
bench <- bench[ , c(19, 2:18)]
names(bench)[1] <- "SERIAL" # replace name again
names(bench)


# save benchmark data set
save(bench, file = "internal use/prepared data/benchmark_data_Study1.rda") # for internal use (just for consistency)
save(bench, file = "prepared data/benchmark_data_Study1.rda") # for sharing








# Session Info ------------------------------------------------------------

rm(list=ls())
sessionInfo()
# R version 4.5.0 (2025-04-11 ucrt)
# Platform: x86_64-w64-mingw32/x64
# Running under: Windows 11 x64 (build 26100)
# 
# Matrix products: default
# LAPACK version 3.12.1
# 
# locale:
#   [1] LC_COLLATE=German_Germany.utf8  LC_CTYPE=German_Germany.utf8   
# [3] LC_MONETARY=German_Germany.utf8 LC_NUMERIC=C                   
# [5] LC_TIME=German_Germany.utf8    
# 
# time zone: Europe/Berlin
# tzcode source: internal
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] lubridate_1.9.4 forcats_1.0.0   stringr_1.5.1   dplyr_1.1.4    
# [5] purrr_1.0.4     readr_2.1.5     tidyr_1.3.1     tibble_3.2.1   
# [9] ggplot2_3.5.2   tidyverse_2.0.0
# 
# loaded via a namespace (and not attached):
#   [1] vctrs_0.6.5        cli_3.6.5          rlang_1.1.6        stringi_1.8.7     
# [5] generics_0.1.4     glue_1.8.0         hms_1.1.3          rsconnect_1.4.1   
# [9] scales_1.4.0       grid_4.5.0         tzdb_0.5.0         lifecycle_1.0.4   
# [13] compiler_4.5.0     RColorBrewer_1.1-3 timechange_0.3.0   pkgconfig_2.0.3   
# [17] rstudioapi_0.17.1  farver_2.1.2       R6_2.6.1           tidyselect_1.2.1  
# [21] pillar_1.10.2      magrittr_2.0.3     tools_4.5.0        withr_3.0.2       
# [25] gtable_0.3.6   