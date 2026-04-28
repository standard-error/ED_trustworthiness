###################################################################
#####       Estimating trait emotion differentiation:         #####
#####          How many measurement occasions and             #####
#####              emotion items are needed?                  #####
###################################################################

###################################################################
#####           Data Preparation Script - Study 2             #####
###################################################################



# Packages ----------------------------------------------------------------
library(tidyverse)



# Compare OpenESM and OSF Data --------------------------------------------
# https://openesmdata.org/datasets/0057_ryvkina/
# DOI: 10.5281/zenodo.17361657 
# install.packages("openesm")
# library(tidyverse)
# 
# d1 <- openesm::get_dataset("0057_ryvkina")[["data"]]
# 
# 
# head(d1)
# names(d1)
# length(unique(d1$id)) # 2272
# # nrow(dplyr::distinct(d1, id, wave, day, beep))
# str(d1)
# table(d1$wave)
# sub <- dplyr::distinct(d1, id, dataset)
# table(sub$dataset, useNA="always")
# 
# 
# d2 <- readr::read_csv("../EMOTIONS Project/Study2_BothWaves_ESM_and_Traitdata_inclWave1[2]-only.csv")
# 
# all(unique(d1$id) == unique(d2$id_for_merging))
# # same IDs, same number of rows
# # but in the OSF data set, we have the data quality indicators
# names(d2)
# sub2 <- dplyr::distinct(d2, id_for_merging, dataset)
# table(sub2$dataset, useNA="always")
# # same number of participants per wave
# 
# # -> use OSF data set due to data quality indicators
# 
# rm(d1, d2, sub, sub2)

d <- readr::read_csv("../EMOTIONS Project/Study2_BothWaves_ESM_and_Traitdata_inclWave1[2]-only.csv")
d <- as.data.frame(d)

# Check Data Quality ------------------------------------------------------
# to be consistent with emolive data set -> use completion times of occasions
# as quality indicator

table(d$outlier_completion_time_esm_perReport_s2w1, useNA="always") # 33 occasions in wave 1
table(d$outlier_completion_time_esm_perReport_s2w2, useNA="always") # 23 occasions in wave 2
table(d$outlier_completion_time_esm_perReport_s2w1,
      d$outlier_completion_time_esm_perReport_s2w2,
      useNA="always")

# exclude outliers due to completion time

# check data structure
# View(d[48:53, c("wave", "dataset", "outlier_completion_time_esm_perReport_s2w1", "outlier_completion_time_esm_perReport_s2w2")])
# if both waves were completed, flags from w1 are also in w2 and vice versa
# -> but exclusion must be wave-wise
# -> e.g., if wave 1, then the flag variable for wave 1 is the relevant one
# 
# View(d[d$wave == "S2W1-only", c("id_not_for_merging", "outlier_completion_time_esm_perReport_s2w1", "outlier_completion_time_esm_perReport_s2w2")])
# # if only one wave -> missings for other wave

# create own flag variable
d$flagW1 <- NA
d$flagW1[d$dataset == "S2W1" & d$outlier_completion_time_esm_perReport_s2w1 == 1] <- 1
d$flagW1[d$dataset == "S2W1" & d$outlier_completion_time_esm_perReport_s2w1 == 0] <- 0
d$flagW1[d$dataset == "S2W2"] <- NA
table(d$flagW1, useNA="always")


d$flagW2 <- NA
d$flagW2[d$dataset == "S2W2" & d$outlier_completion_time_esm_perReport_s2w2 == 1] <- 1
d$flagW2[d$dataset == "S2W2" & d$outlier_completion_time_esm_perReport_s2w2 == 0] <- 0
d$flagW2[d$dataset == "S2W1"] <- NA
table(d$flagW2, useNA="always")

# the same as original variables, correct
# now combine into one variable
d$flag <- 0
d$flag[d$flagW1 == 1 | d$flagW2 == 1 ] <-  1

table(d$flag, useNA="always")
# 56
# 23 + 33 = 56
# correct


# remove outliers due to completion time:
d2 <- d[d$flag == 0, ]


# Check Demographic Variables ---------------------------------------------
# multiple variables for demographic variables:
# e.g., household, household_t3
names(d2)[startsWith(names(d2), "household")]

# some participants completed both waves
# do they have demographic variables twice?
# are there mismatches?
demo_vars <- c("household", "educational_status", "occupational_status", "higher_ed", "higher_ed_type", "sidejob")
demo_vars_t3 <- paste0(demo_vars, "_t3")

has_w1 <- rowSums(!is.na(d2[demo_vars])) > 0     # values on demo_vars TRUE/FALSE
has_w2 <- rowSums(!is.na(d2[demo_vars_t3])) > 0  # values on demo_vars_t3 TRUE/FALSE

w1_only <- has_w1 & !has_w2 # only values on wave 1
w2_only <- !has_w1 & has_w2 # only values on wave 2
both <- has_w1 & has_w2 # values on both waves


# Check W1-only
all(
  rowSums(!is.na(d2[w1_only, demo_vars_t3])) == 0
)
# TRUE -> all who only have values on wave 1, do not have values on demo_vars_t3

# Check W2-only
all(
  rowSums(!is.na(d2[w2_only, demo_vars])) == 0
)
# TRUE -> all who only have values on wave 2, do not have values on demo_vars


# # sanity check with internal variable:
# all(
#   rowSums(!is.na(d2[d2$wave == "S2W1-only", demo_vars_t3])) == 0
# )
# # TRUE, all who have W1 only, do not have values on demo_vars_t3 (wave 2)
# all(
#   rowSums(!is.na(d2[d2$wave == "S2W2-only", demo_vars])) == 0
# )
# # TRUE, all who have W3 only, do not have values on demo_vars (wave 1)


# now check those who have both waves
all(
  rowSums(!is.na(d2[d2$wave == "both", demo_vars])) > 0 & rowSums(!is.na(d2[both, demo_vars_t3])) > 0
)
# all who have completed both waves, have values on both demographic variable sets


# check match between those variables
matches <- sapply(seq_along(demo_vars), function(i) {
  v1 <- demo_vars[i]
  v2 <- demo_vars_t3[i]
  
  d2[both, v1] == d2[both, v2]
})

# demographic variables do not match in all cases
all(matches, na.rm = TRUE) 

# inspect cases
mismatch_cases <- sapply(seq_along(demo_vars), function(i) {
  v1 <- demo_vars[i]
  v2 <- demo_vars_t3[i]
  
  sum(d2[both, v1] != d2[both, v2], na.rm = TRUE)
})

names(mismatch_cases) <- demo_vars
mismatch_cases


mismatch_rows <- rep(FALSE, nrow(d2))

for (i in seq_along(demo_vars)) {
  v1 <- demo_vars[i]
  v2 <- demo_vars_t3[i]
  
  mismatch <- d2[ ,v1] != d2[, v2] &
    !is.na(d2[ ,v1]) &
    !is.na(d2[ ,v2])
  
  mismatch_rows <- mismatch_rows | mismatch
}

d2_mismatch <- d2[mismatch_rows, ]
length(unique(d2_mismatch$id_for_merging)) # 89 participants

# subset distinct rows for each participant
d2_mismatch_L2 <- dplyr::distinct(d2_mismatch, id_for_merging, household, educational_status,
                                  occupational_status, higher_ed, higher_ed_type, sidejob, household_t3,
                                  educational_status_t3, occupational_status_t3, higher_ed_t3,
                                  higher_ed_type_t3, sidejob_t3)
# inspect
View(d2_mismatch_L2)


for (var in demo_vars) {
  d2_mismatch_L2[ , paste0(var, "_flag")] <- ifelse(
    !is.na(d2_mismatch_L2[ , var]) & !is.na(d2_mismatch_L2[ , paste0(var, "_t3")]) &
    d2_mismatch_L2[ , var] != d2_mismatch_L2[ , paste0(var, "_t3")],
    # if both variables are NOT missing, and they're not identical ...
    1, # flag with 1
    0 # else (identical or both missing), do not flag (= 0)
  )
}

table(d2_mismatch_L2$household_flag, useNA="always")
# 30 differ in household size -> plausible, can change over time

table(d2_mismatch_L2$educational_status_flag, useNA="always")
# 27 differ in educational status
# look at this data
View(d2_mismatch_L2[d2_mismatch_L2$educational_status_flag == 1,
                    c("id_for_merging", "educational_status", "educational_status_t3")])
# for some, it seems plausible that educational status is now higher
# (e.g., Abitur but no vocational training to Abitur plus vocational training
# or Abitur to university degree)
# for some, it does not seem plausible 
# (e.g., mittlere Reife to Hauptschulabschluss)

table(d2_mismatch_L2$occupational_status_flag, useNA="always")
# 36 mismatches in occupational status
View(d2_mismatch_L2[d2_mismatch_L2$occupational_status_flag == 1,
                    c("id_for_merging", "occupational_status", "occupational_status_t3")])
# changes in occupational status can be plausible (e.g., at university to full-time employment)

table(d2_mismatch_L2$higher_ed_flag, useNA="always") # 6 mismatches
View(d2_mismatch_L2[d2_mismatch_L2$higher_ed_flag == 1,
                    c("id_for_merging", "higher_ed", "higher_ed_t3")])
# may be plausible (not studying to studying or vice versa)


table(d2_mismatch_L2$higher_ed_type_flag, useNA="always") # 4 mismatches
View(d2_mismatch_L2[d2_mismatch_L2$higher_ed_type_flag == 1,
                    c("id_for_merging", "higher_ed_type", "higher_ed_type_t3")])
# apparently confusion with teacher education and bachelor/master --> teacher education is
# also in bachelor/master system

table(d2_mismatch_L2$sidejob_flag, useNA="always") # 5 mismatches
View(d2_mismatch_L2[d2_mismatch_L2$sidejob_flag == 1,
                    c("id_for_merging", "sidejob", "sidejob_t3")])
# seems plausible


# -> some changes appear plausible (or are, probably, due to the response categories [teacher education])
# -> educational status does not always seem plausible

# -> to be consistent, use the first data that everyone provided
# i.e., for wave1-only: use wave1 variables, for wave2-only: use wave2 variables,
# for both: use wave1 variables

# create variables:
d2$household_fin <- ifelse(
  d2$wave == "S2W1-only" | d2$wave == "both", # if wave is either W1-only or both
  d2$household, # use wave 1 variables
  d2$household_t3) # else, use wave 2 variables

d2$educational_status_fin <- ifelse(
  d2$wave == "S2W1-only" | d2$wave == "both", # if wave is either W1-only or both
  d2$educational_status, # use wave 1 variables
  d2$educational_status_t3) # else, use wave 2 variables

d2$occupational_status_fin <- ifelse(
  d2$wave == "S2W1-only" | d2$wave == "both", # if wave is either W1-only or both
  d2$occupational_status, # use wave 1 variables
  d2$occupational_status_t3)

d2$higher_ed_fin <- ifelse(
  d2$wave == "S2W1-only" | d2$wave == "both", # if wave is either W1-only or both
  d2$higher_ed, # use wave 1 variables
  d2$higher_ed_t3)

d2$higher_ed_type_fin <- ifelse(
  d2$wave == "S2W1-only" | d2$wave == "both", # if wave is either W1-only or both
  d2$higher_ed_type, # use wave 1 variables
  d2$higher_ed_type_t3)

d2$sidejob_fin <- ifelse(
  d2$wave == "S2W1-only" | d2$wave == "both", # if wave is either W1-only or both
  d2$sidejob, # use wave 1 variables
  d2$sidejob_t3)

rm(d2_mismatch, d2_mismatch_L2, matches, demo_vars, demo_vars_t3, both, has_w1, has_w2, mismatch,
   v1, v2, var, w1_only, w2_only, i, mismatch_cases, mismatch_rows)



# Check Filtering ---------------------------------------------------------

# create emotion variable name vector
int_vars <- grep("^int_", names(d2), value = TRUE) # get all int_vars
int_vars
int_vars <- int_vars[1:14] # remove pleasure and activity (no emotion variables)

occup_vars <- grep("^occup_", names(d2), value = TRUE) # get all occup_vars
occup_vars
occup_vars <- occup_vars[1:14] # remove pleasure and activity (no emotion variables)


# check:
# if interaction = yes, all int_ variables !is.na()
# if interaction = no, all occup_ variables !is.na() (and vice versa)
# codebook: interaction = 1 (yes), interaction = 2 (no)
table(d2$interaction)

# Hilfsvariablen: hat Werte (mind. ein nicht-NA)
d2$int_filled   <- rowSums(!is.na(d2[, int_vars])) > 0
d2$occup_filled <- rowSums(!is.na(d2[, occup_vars])) > 0

# Check cases
# case 1: interaction = 1, but occup not NA
error_int <- d2$interaction == 1 & d2$occup_filled

# case 2: interaction = 2, but int not NA
error_occup <- d2$interaction == 2 & d2$int_filled

# all errors
errors <- d2[error_int | error_occup, ]

# number of errors
nrow(errors)
# no errors

rm(errors)


all(is.na(d2[d2$interaction == 1, 
                  occup_vars]))
# all activity emotions mising if interaction = yes
all(!is.na(d2[d2$interaction == 2, 
                   occup_vars]))
# all activity emotions NOT missing if interaction = no

all(!is.na(d2[d2$interaction == 1, 
                   int_vars]))
# all interaction emotions NOT missing if interaction = yes
all(is.na(d2[d2$interaction == 2, 
                  int_vars]))
# all interaction emotions  missing if interaction = no



# Create Final Emotion Variables ------------------------------------------

# merge emotions from interaction and activity to single emotion items

# prepare emotion data
emotion_vars <- sub("^int_", "", int_vars)
# check <- sub("^occup_", "", occup_vars)
# emotion_vars == check # TRUE
# rm(check)

for (item in emotion_vars) {
  d2[ , item] <- rowMeans(d2[ , c(paste0("int_", item), paste0("occup_", item))], na.rm=T)
}

# View(d2[ , emotion_vars])

# # sanity check:
# for (item in emotion_vars) {
#   d2[ , paste0(item, "2")] <- ifelse(d2$interaction == 1, d2[ , paste0("int_", item)], d2[ , paste0("occup_", item)])
# }
# 
# 
# d2[ , "proud"] == d2[ , "proud2"]
# all(d2[ , "proud"] == d2[ , "proud2"])
# 
# for (item in emotion_vars) {
#   print(paste0(item, ": ", all(d2[ , item] == d2[ , paste0(item, "2")])))
# }



# Order Data Set by Participant and Beep Number ---------------------------
# check whether time variable is recognized as such
str(d2$created_esm)
# correct

d3 <- d2[order(d2$id_for_merging, d2$created_esm), ]

# create running number for beep
d3 %>% 
  group_by(id_for_merging) %>% 
  mutate(occ_total = row_number()) %>% 
  ungroup() -> d3

d3 <- d3[order(d3$id_for_merging, d3$created_esm), ] # order


# Create Subset with Relevant Variables -----------------------------------

d4 <- d3[ , c("id_for_merging", "wave", "dataset", "consent",
              "household_fin", "educational_status_fin", "occupational_status_fin",
              "higher_ed_fin", "higher_ed_type_fin", "sidejob_fin",
              "created_esm", "ended_esm", "occ_total", "interaction",
              emotion_vars)]


# Calculate Occasions per Participant -------------------------------------
# combine both waves and calculate how many occasions each participant has
d4 %>% 
  group_by(id_for_merging) %>% 
  mutate(n_occ = n()) -> d4

L2_both <- dplyr::distinct(d4, id_for_merging, n_occ)
table(L2_both$n_occ)

table(L2_both$n_occ >= 70)
# 251 participants




# Rename Variables and Order Final Data Set -------------------------------

# rename demographic variables
# rename demographic variables (remove _clean)
vars_demo_fin <- c("household_fin", "educational_status_fin",
                   "occupational_status_fin", "higher_ed_fin",
                   "higher_ed_type_fin", "sidejob_fin")

vars_demo_final <- sub("_fin$", "", vars_demo_fin)

d4 <- gdata::rename.vars(
  d4,
  from = vars_demo_fin,
  to   = vars_demo_final
)


d4 <- gdata::rename.vars(
  d4,
  from = "id_for_merging",
  to = "id"
)



# now order data set -> negative emotions en bloc
d5 <- d4[ , c("id", "wave", "dataset", "consent",
              "household", "educational_status", "occupational_status", "higher_ed", "higher_ed_type", "sidejob",
              "n_occ",
              "created_esm", "ended_esm", "occ_total",
              "interaction",
              "proud", "success", "superior", "enthusiastic", "relaxed",
              "angry", "excluded", "envious", "resentful", "ashamed", "insecure",
              "anxious", "sad", "lonely")]


# Save Data Set -----------------------------------------------------------
dat <- d5
save(dat, file="internal use/prepared data/EMOTIONS_clean_all_participants.rda")



# Create Benchmark Data Set -----------------------------------------------
# select all participants with >= 70 occasions

L2 <- dplyr::distinct(dat, id, n_occ) 

table(L2$n_occ >= 70) 
# 251 participants with at least 70 occasions
# use these as benchmark


# select participants with at least 70 occasions
bench <- dat[which(dat$n_occ >= 70), ]

# order occasions
bench <- bench[order(bench$id, bench$occ_total), ]
# re-number the measurement occasions (only valid occasions)
bench <- bench %>% 
  group_by(id) %>% 
  mutate(occ_running = 1:n()) %>%  # new running number
  as.data.frame()


# select only 70 occasions for each participant
# by order!
bench <- bench[which(bench$occ_running <= 70), ]


# check whether all participants have variance in their emotion ratings across the 70 occasions
# check overall (total item set) and for all item sets


neg_emo <- c("angry", "excluded", "envious", "resentful", "ashamed", "insecure", # negative emotions
             "anxious", "sad", "lonely")
pos_emo <- c("proud", "success", "superior", "enthusiastic", "relaxed")


matrix_variance_check_neg <- bench %>% 
  group_by(id) %>% 
  summarise(
    var_zero = all(var(as.matrix(across(all_of(neg_emo))),
                       na.rm=TRUE) == 0),
    .groups = "drop"
  )

matrix_variance_check_neg

no_var_neg <- matrix_variance_check_neg %>% filter(var_zero == TRUE)
# person id = 225

sub <- bench[bench$id == 225, neg_emo]
all(var(sub[ ,neg_emo]) == 0) # actually no variance

# remove this participant --> no ICCs can be calculated (for negative emotions)


matrix_variance_check_pos <- bench %>% 
  group_by(id) %>% 
  summarise(
    var_zero = all(var(as.matrix(across(all_of(pos_emo))),
                       na.rm=TRUE) == 0),
    .groups = "drop"
  )

matrix_variance_check_pos

no_var_pos <- matrix_variance_check_pos %>% filter(var_zero == TRUE)
no_var_pos # no participants


# Now check for all possible item sets across the 70 occasions -> if subsets of occasions are drawn,
# the simulation will continue forever (while var == 0, repeat drawing of occasions...), as there will
# never be variance in subsets of occasions if there is no variance in all occasions

# negative emotions:
itemsets_neg <- unlist(
  lapply(3:9, function(k) combn(neg_emo, k, simplify = FALSE)),
  recursive = FALSE
) # 466 item sets in total -> correct


itemset_info_neg <- tibble(
  itemset_id = seq_along(itemsets_neg),
  itemset_size = lengths(itemsets_neg),
  itemset = map_chr(itemsets_neg, paste, collapse = ", ")
)

itemset_info_neg

dat_split <- split(bench, bench$id)

check_person_item_sets <- function(df_person, person_id) {
  itemset_info_neg %>% 
    mutate(id = person_id,
           has_variance = map_lgl(itemset_id, function(i) {
             
             set <- itemsets_neg[[i]]
             
             v <- var(as.matrix(df_person[ , set]), na.rm=TRUE)
             
             !all(v == 0, na.rm=TRUE)
             
           }))
}

itemset_info_by_person_neg <- map2_dfr(
  dat_split,
  names(dat_split),
  check_person_item_sets
)

itemset_info_by_person_neg
all(itemset_info_by_person_neg$has_variance == TRUE)


problems <- itemset_info_by_person_neg %>%
  filter(has_variance == FALSE)

unique(problems$id)
length(unique(problems$id))
# 75 participants are affected
unique(problems$itemset_size)
table(problems$itemset_size)

# -> implement check in simulation study and mark these as no variance across ALL occasions and remove from analysis


# positive emotions
itemsets_pos <- unlist(
  lapply(3:5, function(k) combn(pos_emo, k, simplify = FALSE)),
  recursive = FALSE
) # 16 item sets in total -> correct


itemset_info_pos <- tibble(
  itemset_id = seq_along(itemsets_pos),
  itemset_size = lengths(itemsets_pos),
  itemset = map_chr(itemsets_pos, paste, collapse = ", ")
)

itemset_info_pos

dat_split <- split(bench, bench$id)

check_person_item_sets <- function(df_person, person_id) {
  itemset_info_pos %>% 
    mutate(id = person_id,
           has_variance = map_lgl(itemset_id, function(i) {
             
             set <- itemsets_pos[[i]]
             
             v <- var(as.matrix(df_person[ , set]), na.rm=TRUE)
             
             !all(v == 0, na.rm=TRUE)
             
           }))
}

itemset_info_by_person_pos <- map2_dfr(
  dat_split,
  names(dat_split),
  check_person_item_sets
)

itemset_info_by_person_pos
all(itemset_info_by_person_pos$has_variance == TRUE)


problems <- itemset_info_by_person_pos %>%
  filter(has_variance == FALSE)
# zero problems with missing variance in positive emotion terms




# Remove participant 225, who has no variance across all negative emotion items across all occasions
bench <- bench[bench$id != 225, ]



# select variables relevant for analyses (i.e. ID,
# occasion running, emotion terms)
bench <- bench[ , c("id", "occ_running",
                    "proud", "success", "superior", "enthusiastic", "relaxed", # positive emotions
                    "angry", "excluded", "envious", "resentful", "ashamed", "insecure", # negative emotions
                    "anxious", "sad", "lonely")]

# save benchmark data set
save(bench, file = "internal use/prepared data/EMOTIONS_benchmark_data.rda") # for internal use (just for consistency)
save(bench, file = "prepared data/EMOTIONS_benchmark_data.rda") # for sharing


# Session Info ------------------------------------------------------------

rm(list=ls())
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
#  [1] rappdirs_0.3.4     generics_0.1.4     gtools_3.9.5       stringi_1.8.7      lattice_0.22-9    
#  [6] lme4_2.0-1         hms_1.1.4          magrittr_2.0.3     openesm_0.1.2      timechange_0.3.0  
# [11] RColorBrewer_1.1-3 grid_4.5.3         Matrix_1.7-4       scales_1.4.0       httr2_1.2.2       
# [16] mnormt_2.1.2       reformulas_0.4.4   Rdpack_2.6.6       cli_3.6.5          rlang_1.2.0       
# [21] crayon_1.5.3       rbibutils_2.4.1    performance_0.16.0 bit64_4.6.0-1      splines_4.5.3     
# [26] withr_3.0.2        otel_0.2.0         tools_4.5.3        parallel_4.5.3     tzdb_0.5.0        
# [31] nloptr_2.2.1       minqa_1.2.8        boot_1.3-32        vctrs_0.6.5        R6_2.6.1          
# [36] lifecycle_1.0.5    fs_2.0.1           bit_4.6.0          vroom_1.6.5        MASS_7.3-65       
# [41] psych_2.6.3        insight_1.4.6      pkgconfig_2.0.3    archive_1.1.12.1   gtable_0.3.6      
# [46] pillar_1.11.1      glue_1.8.0         Rcpp_1.1.1-1       tidyselect_1.2.1   rstudioapi_0.18.0 
# [51] farver_2.1.2       nlme_3.1-168       gdata_3.0.1        compiler_4.5.3     S7_0.2.0  
