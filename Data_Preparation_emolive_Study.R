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

# determine completion time of the surveys:
nullmod <- lme4::lmer(duration_occ_sec ~ 1 + (1 | SERIAL), data=AA.c)
summary(nullmod)
# 118.52 sec
# -> approximately 2 mins

L2 <- dplyr::distinct(AA.c, SERIAL, n_occ) 

table(L2$n_occ >= 70) 
# 109 participants with at least 70 occasions
# use these as benchmark


# select participants with at least 70 occasions
bench <- AA.c[which(AA.c$n_occ >= 70), ]

# order occasions
bench <- bench[order(bench$SERIAL, bench$occasion_total), ]
# re-number the measurement occasions (only valid occasions)
bench <- bench %>% 
  group_by(SERIAL) %>% 
  mutate(occ_running = 1:n()) %>%  # new running number
  as.data.frame()


# select only 70 occasions for each participant
# by order!
bench <- bench[which(bench$occ_running <= 70), ]


# select variables relevant for analyses (i.e. ID [SERIAL],
# occasion running, occasion total, emotion terms)
bench <- bench[ , c("SERIAL", "occ_running", "occasion_total",
                    "aerger1", "aerger2", "aerger3", "traurigkeit1",   # negative emotion items
                    "traurigkeit2", "traurigkeit3", "angst1",
                    "angst2", "angst3", "scham1", "scham2", "scham3",
                    "schuld1", "schuld2", "schuld3",
                    "freude1", "freude2", "freude3",                   # positive emotion items
                    "interesse1", "interesse2", "interesse3",
                    "liebe1", "liebe2", "liebe3",
                    "stolz1", "stolz2", "stolz3")]



# save benchmark data set
save(bench, file = "internal use/prepared data/emolive_benchmark_data.rda") # for internal use (just for consistency)
save(bench, file = "prepared data/emolive_benchmark_data.rda") # for sharing


# check whether all participants have variance in their emotion ratings across the 70 occasions
# check overall (total item set) and for all item sets
source("functions/function_determine_all_possible_item_sets.R")



neg_emo <- c('aerger1', 'aerger2', 'aerger3',
             'traurigkeit1', 'traurigkeit2', 'traurigkeit3',
             'angst1', 'angst2', 'angst3',
             'scham1', 'scham2', 'scham3',
             'schuld1', 'schuld2', 'schuld3')
pos_emo <- c('freude1', 'freude2', 'freude3',
             'interesse1', 'interesse2', 'interesse3',
             'liebe1', 'liebe2', 'liebe3',
             'stolz1', 'stolz2', 'stolz3')


matrix_variance_check_neg <- bench %>% 
  group_by(SERIAL) %>% 
  summarise(
    var_zero = all(var(as.matrix(across(all_of(neg_emo))),
                       na.rm=TRUE) == 0),
    .groups = "drop"
  )

matrix_variance_check_neg

no_var_neg <- matrix_variance_check_neg %>% filter(var_zero == TRUE)
# no problems


matrix_variance_check_pos <- bench %>% 
  group_by(SERIAL) %>% 
  summarise(
    var_zero = all(var(as.matrix(across(all_of(pos_emo))),
                       na.rm=TRUE) == 0),
    .groups = "drop"
  )

matrix_variance_check_pos

no_var_pos <- matrix_variance_check_pos %>% filter(var_zero == TRUE)
no_var_pos # no participants



# create all item sets (negative)
itemsets_neg <- unlist(
  lapply( # apply to each unique condition 
  c(5,10,15),
  function(condition) { # for each unique condition, do the following:
    
   all_item_sets <- generate_all_item_sets( # generate all possible item sets for this condition
      all_items = c('aerger1', 'aerger2', 'aerger3',
                                'traurigkeit1', 'traurigkeit2', 'traurigkeit3',
                                'angst1', 'angst2', 'angst3',
                                'scham1', 'scham2', 'scham3',
                                'schuld1', 'schuld2', 'schuld3'),
      n_items = condition, # pass item number for this condition
      categories = c("aerger", "aerger", "aerger",
                     "traurigkeit", "traurigkeit", "traurigkeit",
                     "angst", "angst", "angst",
                     "scham", "scham", "scham",
                     "schuld", "schuld", "schuld") # pass categories for items from simulation study
    )
   
  }
), recursive = FALSE
)
# 487 correct


itemset_info_neg <- tibble(
  itemset_id = seq_along(itemsets_neg),
  itemset_size = lengths(itemsets_neg),
  itemset = map_chr(itemsets_neg, paste, collapse = ", ")
)

itemset_info_neg

dat_split <- split(bench, bench$SERIAL)

check_person_item_sets <- function(df_person, person_id) {
  itemset_info_neg %>% 
    mutate(SERIAL = person_id,
           has_variance = map_lgl(itemset_id, function(i) {
             
             set <- unlist(strsplit(itemsets_neg[[i]], split = ", "))
             
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
# no problems


# create all item sets (positive)
itemsets_pos <- unlist(
  lapply( # apply to each unique condition 
    c(4,8,12),
    function(condition) { # for each unique condition, do the following:
      
      all_item_sets <- generate_all_item_sets( # generate all possible item sets for this condition
        all_items = c('freude1', 'freude2', 'freude3',
                      'interesse1', 'interesse2', 'interesse3',
                      'liebe1', 'liebe2', 'liebe3',
                      'stolz1', 'stolz2', 'stolz3'),
        n_items = condition, # pass item number for this condition
        categories = c('freude', 'freude', 'freude',
                       'interesse', 'interesse', 'interesse',
                       'liebe', 'liebe', 'liebe',
                       'stolz', 'stolz', 'stolz') # pass categories for items from simulation study
      )
      
    }
  ), recursive = FALSE
)
# 163 correct


itemset_info_pos <- tibble(
  itemset_id = seq_along(itemsets_pos),
  itemset_size = lengths(itemsets_pos),
  itemset = map_chr(itemsets_pos, paste, collapse = ", ")
)

itemset_info_pos

dat_split <- split(bench, bench$SERIAL)

check_person_item_sets <- function(df_person, person_id) {
  itemset_info_pos %>% 
    mutate(SERIAL = person_id,
           has_variance = map_lgl(itemset_id, function(i) {
             
             set <- unlist(strsplit(itemsets_pos[[i]], split = ", "))
             
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
# no problems



# Session Info ------------------------------------------------------------

rm(list=ls())
sessionInfo()

# Time Stamp: 10.04.2026, 14:53
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
#  [1] lubridate_1.9.4 forcats_1.0.1   stringr_1.5.1   dplyr_1.1.4     purrr_1.1.0     readr_2.1.5    
#  [7] tidyr_1.3.1     tibble_3.3.0    ggplot2_4.0.2   tidyverse_2.0.0
# 
# loaded via a namespace (and not attached):
#  [1] vctrs_0.6.5        cli_3.6.5          rlang_1.1.6        stringi_1.8.7      generics_0.1.4    
#  [6] S7_0.2.0           glue_1.8.0         hms_1.1.4          scales_1.4.0       grid_4.5.3        
# [11] tzdb_0.5.0         lifecycle_1.0.5    compiler_4.5.3     RColorBrewer_1.1-3 timechange_0.3.0  
# [16] pkgconfig_2.0.3    rstudioapi_0.17.1  farver_2.1.2       R6_2.6.1           tidyselect_1.2.1  
# [21] pillar_1.11.0      magrittr_2.0.3     tools_4.5.3        withr_3.0.2        gtable_0.3.6      