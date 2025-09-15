###################################################################
#####    Estimating trait negative emotion differentiation:   #####
#####        How many measurement occasions and               #####
#####              emotion items are needed?                  #####
###################################################################

###################################################################
#####                  Sample Description                     #####
###################################################################


# for description of data cleaning, see data preparation scripts



# Packages ----------------------------------------------------------------
library(tidyverse)



# Source Function to Calculate ICCs ---------------------------------------
source("functions/function_calculate_iccs.R")


# Load Data ---------------------------------------------------------------
load("internal use/prepared data/emolive_clean_all_participants.rda")


names(AA.c)
table(dplyr::distinct(AA.c, SERIAL, n_occ)$n_occ >= 100)


# Select Benchmark Sample -------------------------------------------------
samp <- AA.c[which(AA.c$n_occ >= 100), ]
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


# with 100 occasions used for the analyses (ICC_100, ICC.z_100)
## select first 100 occasions per participant
## order occasions
samp <- samp[order(samp$SERIAL, samp$occasion_total), ]
## re-number the measurement occasions (only valid occasions)
samp <- samp %>% 
  group_by(SERIAL) %>% 
  mutate(occ_running = 1:n()) %>%  # new running number
  as.data.frame()


## select only 100 occasions for each participant
# by order!
samp_100 <- samp[which(samp$occ_running <= 100), ]

## calculate ICCs
ICC_100 <- calculate_icc(data = samp_100, id.var = "SERIAL",
                         items = c('aerger1', 'aerger2', 'aerger3',
                                   'traurigkeit1', 'traurigkeit2', 'traurigkeit3',
                                   'angst1', 'angst2', 'angst3',
                                   'scham1', 'scham2', 'scham3',
                                   'schuld1', 'schuld2', 'schuld3'),
                         type = "consistency",
                         unit = "single")
colnames(ICC_100) <- c("SERIAL", "ICC_100", "ICC.z_100")

## merge with sample data
samp <- merge(samp, ICC_all, by="SERIAL")
samp <- merge(samp, ICC_100, by="SERIAL")


samp_100 <- merge(samp_100, ICC_all, by="SERIAL")
samp_100 <- merge(samp_100, ICC_100, by="SERIAL")

# Describe Sample ---------------------------------------------------------

# Level 2 -----------------------------------------------------------------
# L2 variables: gender, age, education, education_other, occupation,
# occupation_other, language_skills, schedule, n_occ_completed,
# n_occ_completed_perc, n_occ_valid, n_occ_valid_perc, ICC_all,
# ICC.z_all, ICC_100, ICC.z_100



# select distinct rows with L2 variables
L2 <- dplyr::distinct(samp, SERIAL, gender, age, education, education_other,
                      occupation, occupation_other, language_skills,
                      schedule, n_occ_completed, n_occ_completed_perc,
                      n_occ_valid, n_occ_valid_perc, ICC_all, ICC.z_all,
                      ICC_100, ICC.z_100)

table(L2$gender, useNA="always")
prop.table(table(L2$gender, useNA="always"))

table(L2$education)
prop.table(table(L2$education))

table(L2$education_other)

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
psych::describe(L2$ICC_100)
psych::describe(L2$ICC.z_100)

# Level 1 -----------------------------------------------------------------
# L1 variables: emotions
# -> M and SD (within/between) for each emotion term, inter-correlations between emotion terms

# based on 100 occasions per participant (samp_100)


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
  
  null.mod <- lme4::lmer(formula, data=samp_100)
  
  # extract information from model
  mean <- summary(null.mod)[["coefficients"]][1,1] # extract intercept from null model
  sd_between <- as.data.frame(lme4::VarCorr(null.mod))[1, "sdcor"] # L2 ("SERIAL" intercept)
  sd_within <- as.data.frame(lme4::VarCorr(null.mod))[2, "sdcor"] # L1 ("residual")
  
  icc <- performance::icc(null.mod)$ICC_unadjusted
  
  range <- paste0(range(samp_100[ , item])[1], " - ", range(samp_100[ , item])[2])
  
  # round and save in storage
  desc_stats[which(desc_stats$item == item), "M"] <- round(mean, 2)
  desc_stats[which(desc_stats$item == item), "SD_within"] <- round(sd_within, 2)
  desc_stats[which(desc_stats$item == item), "SD_between"] <- round(sd_between, 2)
  desc_stats[which(desc_stats$item == item), "ICC"] <- round(icc, 2)
  desc_stats[which(desc_stats$item == item), "range"] <- range
}

# save table as .csv
write.csv(desc_stats, "results/descriptive_statistics_emotions_Study1.csv", row.names = F)



# intercorrelations (within- and between person)
# cors <- misty::multilevel.cor(samp_100[ , c('SERIAL',
#                                             'aerger1', 'aerger2', 'aerger3',
#                                             'traurigkeit1', 'traurigkeit2', 'traurigkeit3',
#                                             'angst1', 'angst2', 'angst3',
#                                             'scham1', 'scham2', 'scham3',
#                                             'schuld1', 'schuld2', 'schuld3')],
#                               cluster = "SERIAL",
#                               sig = TRUE,
#                               alpha = 0.05,
#                               print = c("cor", "p"))

# model is too complex / between-person correlations of the same category emotion terms are too high


# 
# # possible solution: calculate all pairwise within- and between-person correlations
# 
# pairs <- as.data.frame(t(combn(c('aerger1', 'aerger2', 'aerger3',
#                  'traurigkeit1', 'traurigkeit2', 'traurigkeit3',
#                  'angst1', 'angst2', 'angst3',
#                  'scham1', 'scham2', 'scham3',
#                  'schuld1', 'schuld2', 'schuld3'), 2)))
# colnames(pairs) <- c("item1", "item2")
# 
# 
# pairs$cor_within <- NA
# pairs$p_within <- NA
# pairs$cor_between <- NA
# pairs$p_between <- NA
# 
# 
# for (row in 1:nrow(pairs)) {
#   
#   cor <- misty::multilevel.cor(samp_100[ , c('SERIAL',
#                                              pairs[row, "item1"], # select item 1
#                                              pairs[row, "item2"])], # select item 2
#                                cluster = "SERIAL",
#                                sig = TRUE,
#                                alpha = 0.05,
#                                print = c("cor", "p"))
#  
#   pairs[row, "cor_within"] <- round(cor$result$with.cor[1,2], 2)
#   pairs[row, "p_within"] <- round(cor$result$with.p[1,2], 3)
#   pairs[row, "cor_between"] <- round(cor$result$betw.cor[1,2], 2)
#   pairs[row, "p_between"] <- round(cor$result$betw.p[1,2], 3)
# }
# 
# # cor
# # cor$result$betw.cor[1,2]
# # cor$result$betw.p[1,2]
# # 
# # cor$result$with.cor[1,2]
# # cor$result$with.p[1,2]
# 
