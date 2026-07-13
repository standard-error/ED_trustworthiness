###################################################################
#####      Estimating trait emotion differentiation:          #####
#####          How many measurement occasions and             #####
#####              emotion items are needed?                  #####
###################################################################

###################################################################
#####       Plots for Supplement (Item Set Analysis)          #####
###################################################################

# Determine item sets (for 5 and 10 items for NED, or 4 and 8 items
# for PED, respectively) with highest and lowest mean value
# -> plot outcomes for the item sets with the highest and lowest
# mean value to inspect how the results differ in dependence 
# of endorsement of the items.


# Load Packages -----------------------------------------------------------
library(tidyverse)


# Source Helper Functions -------------------------------------------------
source("functions/function_determine_all_possible_item_sets.R")
source("functions/function_plot_outcomes.R")


# Load Data ---------------------------------------------------------------
load("prepared data/emolive_benchmark_data.rda")


# for reporting: mean of full item sets
# NED:
# calculate mean for each emotion across time (column means) and then calculate overall mean
round(mean(colMeans(bench[ , c('aerger1', 'aerger2', 'aerger3',
                               'traurigkeit1', 'traurigkeit2', 'traurigkeit3',
                               'angst1', 'angst2', 'angst3',
                               'scham1', 'scham2', 'scham3',
                               'schuld1', 'schuld2', 'schuld3')])),
      2)

# PED:
# calculate mean for each emotion across time (column means) and then calculate overall mean
round(mean(colMeans(bench[ , c('freude1', 'freude2', 'freude3',
                               'interesse1', 'interesse2', 'interesse3',
                               'liebe1', 'liebe2', 'liebe3',
                               'stolz1', 'stolz2', 'stolz3')])),
      2)



### now subsets:


itemsets5_neg <- generate_all_item_sets(all_items = c('aerger1', 'aerger2', 'aerger3',
                                                  'traurigkeit1', 'traurigkeit2', 'traurigkeit3',
                                                  'angst1', 'angst2', 'angst3',
                                                  'scham1', 'scham2', 'scham3',
                                                  'schuld1', 'schuld2', 'schuld3'),
                                    categories = c('aerger', 'aerger', 'aerger',
                                                   'traurigkeit', 'traurigkeit', 'traurigkeit',
                                                   'angst', 'angst', 'angst',
                                                   'scham', 'scham', 'scham',
                                                   'schuld', 'schuld', 'schuld'),
                                    n_items = 5)


itemsets5_neg_means <- sapply(itemsets5_neg, function(itemset) {
  
  # extract item names
  items <- strsplit(itemset, ", ")[[1]]
  
  # calculate mean value for each item
  item_means <- colMeans(bench[, items], na.rm=TRUE)
  
  # calculate mean value across items
  mean(item_means, na.rm=T)
})


highest_itemset5_neg <- names(which.max(itemsets5_neg_means))
highest_itemset5_neg
max(itemsets5_neg_means)
itemsets5_neg_means[which.max(itemsets5_neg_means)]
lowest_itemset5_neg <- names(which.min(itemsets5_neg_means))
lowest_itemset5_neg
min(itemsets5_neg_means)
itemsets5_neg_means[which.min(itemsets5_neg_means)]

itemsets10_neg <- generate_all_item_sets(all_items = c('aerger1', 'aerger2', 'aerger3',
                                                  'traurigkeit1', 'traurigkeit2', 'traurigkeit3',
                                                  'angst1', 'angst2', 'angst3',
                                                  'scham1', 'scham2', 'scham3',
                                                  'schuld1', 'schuld2', 'schuld3'),
                                    categories = c('aerger', 'aerger', 'aerger',
                                                   'traurigkeit', 'traurigkeit', 'traurigkeit',
                                                   'angst', 'angst', 'angst',
                                                   'scham', 'scham', 'scham',
                                                   'schuld', 'schuld', 'schuld'),
                                    n_items = 10)


itemsets10_neg_means <- sapply(itemsets10_neg, function(itemset) {
  
  # extract item names
  items <- strsplit(itemset, ", ")[[1]]
  
  # calculate mean value for each item
  item_means <- colMeans(bench[, items], na.rm=TRUE)
  
  # calculate mean value across items
  mean(item_means, na.rm=T)
})


highest_itemset10_neg <- names(which.max(itemsets10_neg_means))
highest_itemset10_neg
max(itemsets10_neg_means)
itemsets10_neg_means[which.max(itemsets10_neg_means)]
lowest_itemset10_neg <- names(which.min(itemsets10_neg_means))
lowest_itemset10_neg
min(itemsets10_neg_means)
itemsets10_neg_means[which.min(itemsets10_neg_means)]


itemsets4_pos <- generate_all_item_sets(all_items = c('freude1', 'freude2', 'freude3',
                                                      'interesse1', 'interesse2', 'interesse3',
                                                      'liebe1', 'liebe2', 'liebe3',
                                                      'stolz1', 'stolz2', 'stolz3'),
                                        categories = c('freude', 'freude', 'freude',
                                                       'interesse', 'interesse', 'interesse',
                                                       'liebe', 'liebe', 'liebe',
                                                       'stolz', 'stolz', 'stolz'),
                                        n_items = 4)


itemsets4_pos_means <- sapply(itemsets4_pos, function(itemset) {
  
  # extract item names
  items <- strsplit(itemset, ", ")[[1]]
  
  # calculate mean value for each item
  item_means <- colMeans(bench[, items], na.rm=TRUE)
  
  # calculate mean value across items
  mean(item_means, na.rm=T)
})


highest_itemset4_pos <- names(which.max(itemsets4_pos_means))
highest_itemset4_pos
max(itemsets4_pos_means)
itemsets4_pos_means[which.max(itemsets4_pos_means)]
lowest_itemset4_pos <- names(which.min(itemsets4_pos_means))
lowest_itemset4_pos
min(itemsets4_pos_means)
itemsets4_pos_means[which.min(itemsets4_pos_means)]

itemsets8_pos <- generate_all_item_sets(all_items = c('freude1', 'freude2', 'freude3',
                                                      'interesse1', 'interesse2', 'interesse3',
                                                      'liebe1', 'liebe2', 'liebe3',
                                                      'stolz1', 'stolz2', 'stolz3'),
                                        categories = c('freude', 'freude', 'freude',
                                                       'interesse', 'interesse', 'interesse',
                                                       'liebe', 'liebe', 'liebe',
                                                       'stolz', 'stolz', 'stolz'),
                                         n_items = 8)


itemsets8_pos_means <- sapply(itemsets8_pos, function(itemset) {
  
  # extract item names
  items <- strsplit(itemset, ", ")[[1]]
  
  # calculate mean value for each item
  item_means <- colMeans(bench[, items], na.rm=TRUE)
  
  # calculate mean value across items
  mean(item_means, na.rm=T)
})


highest_itemset8_pos <- names(which.max(itemsets8_pos_means))
highest_itemset8_pos
max(itemsets8_pos_means)
itemsets8_pos_means[which.max(itemsets8_pos_means)]
lowest_itemset8_pos <- names(which.min(itemsets8_pos_means))
lowest_itemset8_pos
min(itemsets8_pos_means)
itemsets8_pos_means[which.min(itemsets8_pos_means)]


rm(itemsets10_neg, itemsets10_neg_means, itemsets5_neg, itemsets5_neg_means, bench,
   generate_all_item_sets, order_item_sets, itemsets8_pos, itemsets8_pos_means, itemsets4_pos, itemsets4_pos_means)



# Load Simulation Results and Aggregate -----------------------------------



# '' NED ------------------------------------------------------------------


# NED
load("results/02_revision_1/emolive study/NED/main/raw/sim_results_NED_emolive_Study.rda")
NED <- res
rm(res)


names(NED)

# subset
# use only highest and lowest intensity item set for 5 and 10 items
# only use random draws (main analysis; ordered draws are sensitivity analysis)
NED_sub <- NED[NED$items %in% c(lowest_itemset5_neg,
                                highest_itemset5_neg,
                                lowest_itemset10_neg,
                                highest_itemset10_neg) &
                 NED$occasions_drawn == "random", ]



# add label
NED_sub$itemset_type[NED_sub$items == lowest_itemset5_neg]  <- "5 items: low endorsement"
NED_sub$itemset_type[NED_sub$items == highest_itemset5_neg] <- "5 items: high endorsement"

NED_sub$itemset_type[NED_sub$items == lowest_itemset10_neg]  <- "10 items: low endorsement"
NED_sub$itemset_type[NED_sub$items == highest_itemset10_neg] <- "10 items: high endorsement"


# Determine the number of replications for each item set in each condition
table(NED_sub$itemset_type, NED_sub$n_occasions)


# aggregate results
n_persons <- max(NED_sub$n_total_persons)

# calculate % negative ICCs
NED_sub$percnegICC_raw <- ifelse(
  NED_sub$N_merged_ICC_raw == 0, # if there were no ICCs across all participants
  NA_real_, # return NA
  NED_sub$negICC_raw / NED_sub$N_merged_ICC_raw # divide by number of participants for whom a raw ICC was calculated (before handling of negative ICCs)
)


# extract person-level ICC estimates
person_level_ICC_estimates <- data.frame(matrix(nrow=nrow(NED_sub), ncol=n_persons+4)) # + 4 for simulation conditions
person_level_ICC_estimates [ , 1:4] <- NED_sub[ , c(1,3,4,34)] # extract simulation conditions (incl. design_row_id)
names(person_level_ICC_estimates ) <- c("design_row_id", "n_occasions", "occasions_drawn", "itemset_type", paste0("person_ICC_", 1:n_persons))

ICC_matrix <- do.call(rbind, NED_sub$person_estimates_ICC) # extract the N person_estimates_ICC values per row (replication) and bind them
# -> matrix of N participants (columns) and their values in each replication (rows)

# bind with part_dat
person_level_ICC_estimates [ , 5:(n_persons+4)] <- ICC_matrix


# extract person-level differences for each replication and each condition per participant
person_level_ICC.z_estimates  <- data.frame(matrix(nrow=nrow(NED_sub), ncol=n_persons+4))
person_level_ICC.z_estimates [ , 1:4] <- NED_sub[ , c(1,3,4,34)]
names(person_level_ICC.z_estimates ) <- c("design_row_id", "n_occasions", "occasions_drawn", "itemset_type", paste0("person_ICC.z_", 1:n_persons))

ICC.z_matrix <- do.call(rbind, NED_sub$person_estimates_ICC.z) # extract the N person_estimates_ICC.z values per row (replication) and bind them
# -> matrix of N participants (columns) and their values in each replication (rows)

# bind with part_dat
person_level_ICC.z_estimates [ , 5:(n_persons+4)] <- ICC.z_matrix

# Calculate Person-Level Deviation ("Bias") Across Replications 
# For ICCs 
# extract person-level differences for each replication and each condition per participant
person_level_diff <- data.frame(matrix(nrow=nrow(NED_sub), ncol=n_persons+4))
person_level_diff[ , 1:4] <- NED_sub[ , c(1,3,4,34)]
names(person_level_diff) <- c("design_row_id", "n_occasions", "occasions_drawn", "itemset_type", paste0("person_diff_ICC_", 1:n_persons))

diff_matrix <- do.call(rbind, NED_sub$person_diff_ICC) # extract the N person_diff_ICC values per row (replication) and bind them
# -> matrix of N participants (columns) and their values in each replication (rows)

# bind with part_dat
person_level_diff[ , 5:(n_persons+4)] <- diff_matrix



# For ICC.z 
# extract person-level differences for each replication and each condition per participant
person_level_diff.z <- data.frame(matrix(nrow=nrow(NED_sub), ncol=n_persons+4))
person_level_diff.z[ , 1:4] <- NED_sub[ , c(1,3,4,34)]
names(person_level_diff.z) <- c("design_row_id", "n_occasions", "occasions_drawn", "itemset_type", paste0("person_diff_ICC.z_", 1:n_persons))

diff.z_matrix <- do.call(rbind, NED_sub$person_diff_ICC.z) # extract the N person_diff_ICC.z values per row (replication) and bind them
# -> matrix of N participants (columns) and their values in each replication (rows)

# bind with part_dat
person_level_diff.z[ , 5:(n_persons+4)] <- diff.z_matrix

# Aggregate Across Replications 

## for ICC
vars <- paste0("person_diff_ICC_", 1:n_persons)
person_diff_agg <- aggregate(person_level_diff[ , vars],
                             by = person_level_diff[ , c("occasions_drawn", "n_occasions", "itemset_type")],
                             FUN = function(x) {
                               if (all(is.na(x))) {
                                 NA_real_ # if person does not have ANY valid value -> return NA
                               } else {
                                 mean(x, na.rm=TRUE) # calculate mean, but remove NAs
                               }
                             }) 

names(person_diff_agg) <- c("occasions_drawn", "n_occasions", "itemset_type", paste0("person_difference_", 1:n_persons))



### for ICC.z
vars <- paste0("person_diff_ICC.z_", 1:n_persons)
person_diff_agg.z <- aggregate(person_level_diff.z[ , vars],
                               by = person_level_diff.z[ , c("occasions_drawn", "n_occasions", "itemset_type")],
                               FUN = function(x) {
                                 if (all(is.na(x))) {
                                   NA_real_ # if person does not have ANY valid value -> return NA
                                 } else {
                                   mean(x, na.rm=TRUE) # calculate mean, but remove NAs
                                 }
                               }) 

names(person_diff_agg.z) <- c("occasions_drawn", "n_occasions", "itemset_type", paste0("person_difference.z_", 1:n_persons))



# Aggregate Across Participants (for Plotting) 
## for ICC
# calculate overall difference (mean difference across participants)
person_diff_agg$difference_mean <- rowMeans(person_diff_agg[ , 4:(n_persons+3)], na.rm=T)
person_diff_agg$difference_median <- apply(person_diff_agg[ ,4:(n_persons+3)], 1, median, na.rm=T) 

# determine which participant deviates most
person_diff_agg$difference_min_id <- apply(person_diff_agg[ ,4:(n_persons+3)], 1, which.min)
person_diff_agg$difference_max_id <- apply(person_diff_agg[ ,4:(n_persons+3)], 1, which.max)

# calculate min and max
person_diff_agg$difference_min <- apply(person_diff_agg[ ,4:(n_persons+3)], 1, min, na.rm=T)
person_diff_agg$difference_max <- apply(person_diff_agg[ ,4:(n_persons+3)], 1, max, na.rm=T)


person_diff_agg <- person_diff_agg[ , c("occasions_drawn", "n_occasions", "itemset_type",
                                        "difference_mean", "difference_median", "difference_min_id",
                                        "difference_max_id", "difference_min", "difference_max")]


## for ICC.z
# calculate overall difference (mean difference across participants)
person_diff_agg.z$difference.z_mean <- rowMeans(person_diff_agg.z[ , 4:(n_persons+3)], na.rm=T)
person_diff_agg.z$difference.z_median <- apply(person_diff_agg.z[ ,4:(n_persons+3)], 1, median, na.rm=T) 

# determine which participant deviates most
person_diff_agg.z$difference.z_min_id <- apply(person_diff_agg.z[ ,4:(n_persons+3)], 1, which.min)
person_diff_agg.z$difference.z_max_id <- apply(person_diff_agg.z[ ,4:(n_persons+3)], 1, which.max)

# calculate min and max
person_diff_agg.z$difference.z_min <- apply(person_diff_agg.z[ ,4:(n_persons+3)], 1, min, na.rm=T)
person_diff_agg.z$difference.z_max <- apply(person_diff_agg.z[ ,4:(n_persons+3)], 1, max, na.rm=T)



person_diff_agg.z <- person_diff_agg.z[ , c("occasions_drawn", "n_occasions", "itemset_type",
                                            "difference.z_mean", "difference.z_median", "difference.z_min_id",
                                            "difference.z_max_id", "difference.z_min", "difference.z_max")]


# Calculate RMSE for Each Participant Across Replications 
# '' For ICCs 
# use person-level differences -> square
person_level_diff_sq <- person_level_diff[ ,1:4]
person_level_diff_sq[ , 5:(n_persons+4)] <- (person_level_diff[ ,5:(n_persons+4)])^2
names(person_level_diff_sq)[5:(n_persons+4)] <- paste0("sq_diff_ICC_", 1:n_persons)

### aggregate

person_level_diff_sq.rd <- person_level_diff_sq[person_level_diff_sq$occasions_drawn == "random", ]
# only select conditions with random draws of occasions

# RMSE = sqrt(sum(sq_diff_ICC)/n_replication)
sq_diff_cols <- paste0("sq_diff_ICC_", 1:n_persons)

RMSE <- aggregate(person_level_diff_sq.rd[ , sq_diff_cols], # for each participant-specific sq_diff column
                  by = person_level_diff_sq.rd[ , c("occasions_drawn", "n_occasions", "itemset_type")], # aggregate across conditions
                  FUN = function(x) {
                    
                    n_valid <- sum(!is.na(x)) # determine the number of valid replications in given condition for participant
                    
                    if (n_valid == 0) { # if there are no valid replications, return NA
                      NA_real_
                    } else { # else, take the square root of the the summed sq_diff divided by number of (valid) replications
                      sqrt(sum(x, na.rm=TRUE) / n_valid)
                    }
                    
                  })
names(RMSE)[4:(n_persons+3)] <- c(paste0("RMSE_", 1:n_persons))
# order
RMSE <- RMSE[order(RMSE$occasions_drawn, RMSE$itemset_type, RMSE$n_occasions), ]

# Calculate min, mean, and max across participants
RMSE$RMSE_min <- apply(RMSE[ , 4:(n_persons+3)], 1, FUN = min, na.rm = TRUE)
RMSE$RMSE_mean <- rowMeans(RMSE[ ,4:(n_persons+3)], na.rm=TRUE)
RMSE$RMSE_max <- apply(RMSE[ , 4:(n_persons+3)], 1, FUN = max, na.rm = TRUE)
# subset 
RMSE <- RMSE[ , c("occasions_drawn", "n_occasions", "itemset_type",
                  "RMSE_min", "RMSE_mean", "RMSE_max")]


# '' For ICC.z 
# use person-level differences -> square
person_level_diff.z_sq <- person_level_diff.z[ ,1:4]
person_level_diff.z_sq[ , 5:(n_persons+4)] <- (person_level_diff.z[ ,5:(n_persons+4)])^2
names(person_level_diff.z_sq)[5:(n_persons+4)] <- paste0("sq_diff_ICC.z_", 1:n_persons)


### aggregate
person_level_diff.z_sq.rd <- person_level_diff.z_sq[person_level_diff.z_sq$occasions_drawn == "random", ]
# only select conditions with random draws of occasions

# RMSE.z = sqrt(sum(sq_diff_ICC)/n_replication)
sq_diff_cols <- paste0("sq_diff_ICC.z_", 1:n_persons)


RMSE.z <- aggregate(person_level_diff.z_sq.rd[ , sq_diff_cols], # for each participant-specific sq_diff column
                    by = person_level_diff.z_sq.rd[ , c("occasions_drawn", "n_occasions", "itemset_type")], # aggregate across conditions
                    FUN = function(x) {
                      
                      n_valid <- sum(!is.na(x)) # determine the number of valid replications in given condition for participant
                      
                      if (n_valid == 0) { # if there are no valid replications, return NA
                        NA_real_
                      } else { # else, take the square root of the the summed sq_diff divided by number of (valid) replications
                        sqrt(sum(x, na.rm=TRUE) / n_valid)
                      }
                      
                    })
names(RMSE.z)[4:(n_persons+3)] <- c(paste0("RMSE.z_", 1:n_persons))
# order
RMSE.z <- RMSE.z[order(RMSE.z$occasions_drawn, RMSE.z$itemset_type, RMSE.z$n_occasions), ]


# Calculate min, mean, and max across participants
RMSE.z$RMSE.z_min <- apply(RMSE.z[ , 4:(n_persons+3)], 1, FUN = min, na.rm = TRUE)
RMSE.z$RMSE.z_mean <- rowMeans(RMSE.z[ ,4:(n_persons+3)], na.rm=TRUE)
RMSE.z$RMSE.z_max <- apply(RMSE.z[ , 4:(n_persons+3)], 1, FUN = max, na.rm = TRUE)
# subset 
RMSE.z <- RMSE.z[ , c("occasions_drawn", "n_occasions", "itemset_type",
                      "RMSE.z_min", "RMSE.z_mean", "RMSE.z_max")]


rm(diff_matrix, diff.z_matrix, ICC_matrix, ICC.z_matrix, person_level_diff,
   person_level_diff.z, person_level_diff_sq, person_level_diff.z_sq, person_level_ICC_estimates,
   person_level_diff.z_sq.rd, person_level_ICC.z_estimates, person_level_diff_sq.rd,
   sq_diff_cols, vars, n_persons)



# aggregate remaining outcomes

# helper function
# Fisher's Z-transformation
fisher_z <- function(r) {
  z <- 0.5 * log( (1 + r) / (1 - r) )
  return(z)
}

# back-transformation
inverse_fisher_z <- function(z) {
  r <- ( exp(2*z) - 1 ) / ( exp(2*z) + 1 )
  return(r)
}


cor <- do.call(
  data.frame,
  aggregate(cor_ICC ~ occasions_drawn + n_occasions + itemset_type, data = NED_sub, FUN = function(x) {
    # include check -> results may be NA if there was no valid ICC data at all
    if (all(is.na(x))) {
      return(c(NA_real_, NA_real_, NA_real_))
    }
    
    c(min(x, na.rm=TRUE),
      inverse_fisher_z(mean(fisher_z(x), na.rm=TRUE)), # apply Fisher's Z-transformation, average, backtransform
      max(x, na.rm=TRUE)) 

  },
  na.action = na.pass),
)
cor
names(cor) <- c("occasions_drawn", "n_occasions", "itemset_type", "cor_ICC_min", "cor_ICC_mean", "cor_ICC_max")

cor.z <- do.call(
  data.frame,
  aggregate(cor_ICC.z ~ occasions_drawn + n_occasions + itemset_type, data = NED_sub, FUN = function(x) {
    # include check -> results may be NA if there was no valid ICC data at all
    if (all(is.na(x))) {
      return(c(NA_real_, NA_real_, NA_real_))
    }
    
    c(min(x, na.rm=TRUE),
      inverse_fisher_z(mean(fisher_z(x), na.rm=TRUE)), # apply Fisher's Z-transformation, average, backtransform
      max(x, na.rm=TRUE)) 
    
  },
  na.action = na.pass),
)
cor.z
names(cor.z) <- c("occasions_drawn", "n_occasions", "itemset_type", "cor_ICC.z_min", "cor_ICC.z_mean", "cor_ICC.z_max")



sd <- do.call(
  data.frame,
  aggregate(sd_ICC ~ occasions_drawn + n_occasions + itemset_type, data = NED_sub, FUN = function(x) {
    # include check -> results may be NA if there was no valid ICC data at all
    if (all(is.na(x))) {
      return(c(NA_real_, NA_real_, NA_real_))
    }
    
    c(min(x, na.rm=TRUE),
      mean(x, na.rm=TRUE), 
      max(x, na.rm=TRUE)) 
    
  },
  na.action = na.pass),
)
sd
names(sd) <- c("occasions_drawn", "n_occasions", "itemset_type", "sd_ICC_min", "sd_ICC_mean", "sd_ICC_max")




sd.z <- do.call(
  data.frame,
  aggregate(sd_ICC.z~ occasions_drawn + n_occasions + itemset_type, data = NED_sub, FUN = function(x) {
    # include check -> results may be NA if there was no valid ICC data at all
    if (all(is.na(x))) {
      return(c(NA_real_, NA_real_, NA_real_))
    }
    
    c(min(x, na.rm=TRUE),
      mean(x, na.rm=TRUE), # apply Fisher's Z-transformation, average, backtransform
      max(x, na.rm=TRUE)) 
    
  },
  na.action = na.pass),
)
sd.z
names(sd.z) <- c("occasions_drawn", "n_occasions", "itemset_type", "sd_ICC.z_min", "sd_ICC.z_mean", "sd_ICC.z_max")

rel <- do.call(
  data.frame,
  aggregate(rel ~ occasions_drawn + n_occasions + itemset_type, data = NED_sub, FUN = function(x) {
    # include check -> results may be NA if there was no valid ICC data at all
    if (all(is.na(x))) {
      return(c(NA_real_, NA_real_, NA_real_))
    }
    
    c(min(x, na.rm=TRUE),
      mean(x, na.rm=TRUE), # apply Fisher's Z-transformation, average, backtransform
      max(x, na.rm=TRUE)) 
    
  },
  na.action = na.pass),
)
rel
names(rel) <- c("occasions_drawn", "n_occasions", "itemset_type", "rel_min", "rel_mean", "rel_max")


percnegICC <- do.call(
  data.frame,
  aggregate(percnegICC_raw ~ occasions_drawn + n_occasions + itemset_type, data = NED_sub, FUN = function(x) {
    # include check -> results may be NA if there was no valid ICC data at all
    if (all(is.na(x))) {
      return(c(NA_real_, NA_real_, NA_real_))
    }
    
    c(min(x, na.rm=TRUE),
      mean(x, na.rm=TRUE), # apply Fisher's Z-transformation, average, backtransform
      max(x, na.rm=TRUE)) 
    
  },
  na.action = na.pass),
)
percnegICC
names(percnegICC) <- c("occasions_drawn", "n_occasions", "itemset_type", "percnegICC_raw_min", "percnegICC_raw_mean", "percnegICC_raw_max")



agg_res_ned <- list(cor = cor,
     cor.z = cor.z,
     sd = sd,
     sd.z = sd.z,
     rel = rel,
     percnegICC = percnegICC,
     person_diff_agg = person_diff_agg,
     person_diff_agg.z = person_diff_agg.z,
     RMSE = RMSE,
     RMSE.z = RMSE.z)

save(agg_res_ned, file = "results/02_revision_1/emolive study/NED/main/processed/extreme_item_sets_agg_res_NED_emolive_Study.rda")

rm(cor, cor.z, sd, sd.z, rel, percnegICC, person_diff_agg, person_diff_agg.z, RMSE, RMSE.z,
   NED, NED_sub)


# now do the same for PED


# '' PED ------------------------------------------------------------------
# PED
load("results/02_revision_1/emolive study/PED/main/raw/sim_results_PED_emolive_Study.rda")
PED <- res
rm(res)


names(PED)

# subset
# use only highest and lowest intensity item set for 4 and 8 items
# only use random draws (main analysis; ordered draws are sensitivity analysis)
PED_sub <- PED[PED$items %in% c(lowest_itemset4_pos,
                                highest_itemset4_pos,
                                lowest_itemset8_pos,
                                highest_itemset8_pos) &
                 PED$occasions_drawn == "random", ]



# add label
PED_sub$itemset_type[PED_sub$items == lowest_itemset4_pos]  <- "4 items: low endorsement"
PED_sub$itemset_type[PED_sub$items == highest_itemset4_pos] <- "4 items: high endorsement"

PED_sub$itemset_type[PED_sub$items == lowest_itemset8_pos]  <- "8 items: low endorsement"
PED_sub$itemset_type[PED_sub$items == highest_itemset8_pos] <- "8 items: high endorsement"



# Determine the number of replications for each item set in each condition
table(PED_sub$itemset_type, PED_sub$n_occasions)



# aggregate results
n_persons <- max(PED_sub$n_total_persons)

# calculate % negative ICCs
PED_sub$percnegICC_raw <- ifelse(
  PED_sub$N_merged_ICC_raw == 0, # if there were no ICCs across all participants
  NA_real_, # return NA
  PED_sub$negICC_raw / PED_sub$N_merged_ICC_raw # divide by number of participants for whom a raw ICC was calculated (before handling of negative ICCs)
)


# extract person-level ICC estimates
person_level_ICC_estimates <- data.frame(matrix(nrow=nrow(PED_sub), ncol=n_persons+4)) # + 4 for simulation conditions
person_level_ICC_estimates [ , 1:4] <- PED_sub[ , c(1,3,4,34)] # extract simulation conditions (incl. design_row_id)
names(person_level_ICC_estimates ) <- c("design_row_id", "n_occasions", "occasions_drawn", "itemset_type", paste0("person_ICC_", 1:n_persons))

ICC_matrix <- do.call(rbind, PED_sub$person_estimates_ICC) # extract the N person_estimates_ICC values per row (replication) and bind them
# -> matrix of N participants (columns) and their values in each replication (rows)

# bind with part_dat
person_level_ICC_estimates [ , 5:(n_persons+4)] <- ICC_matrix


# extract person-level differences for each replication and each condition per participant
person_level_ICC.z_estimates  <- data.frame(matrix(nrow=nrow(PED_sub), ncol=n_persons+4))
person_level_ICC.z_estimates [ , 1:4] <- PED_sub[ , c(1,3,4,34)]
names(person_level_ICC.z_estimates ) <- c("design_row_id", "n_occasions", "occasions_drawn", "itemset_type", paste0("person_ICC.z_", 1:n_persons))

ICC.z_matrix <- do.call(rbind, PED_sub$person_estimates_ICC.z) # extract the N person_estimates_ICC.z values per row (replication) and bind them
# -> matrix of N participants (columns) and their values in each replication (rows)

# bind with part_dat
person_level_ICC.z_estimates [ , 5:(n_persons+4)] <- ICC.z_matrix

# Calculate Person-Level Deviation ("Bias") Across Replications 
# For ICCs 
# extract person-level differences for each replication and each condition per participant
person_level_diff <- data.frame(matrix(nrow=nrow(PED_sub), ncol=n_persons+4))
person_level_diff[ , 1:4] <- PED_sub[ , c(1,3,4,34)]
names(person_level_diff) <- c("design_row_id", "n_occasions", "occasions_drawn", "itemset_type", paste0("person_diff_ICC_", 1:n_persons))

diff_matrix <- do.call(rbind, PED_sub$person_diff_ICC) # extract the N person_diff_ICC values per row (replication) and bind them
# -> matrix of N participants (columns) and their values in each replication (rows)

# bind with part_dat
person_level_diff[ , 5:(n_persons+4)] <- diff_matrix



# For ICC.z 
# extract person-level differences for each replication and each condition per participant
person_level_diff.z <- data.frame(matrix(nrow=nrow(PED_sub), ncol=n_persons+4))
person_level_diff.z[ , 1:4] <- PED_sub[ , c(1,3,4,34)]
names(person_level_diff.z) <- c("design_row_id", "n_occasions", "occasions_drawn", "itemset_type", paste0("person_diff_ICC.z_", 1:n_persons))

diff.z_matrix <- do.call(rbind, PED_sub$person_diff_ICC.z) # extract the N person_diff_ICC.z values per row (replication) and bind them
# -> matrix of N participants (columns) and their values in each replication (rows)

# bind with part_dat
person_level_diff.z[ , 5:(n_persons+4)] <- diff.z_matrix

# Aggregate Across Replications 

## for ICC
vars <- paste0("person_diff_ICC_", 1:n_persons)
person_diff_agg <- aggregate(person_level_diff[ , vars],
                             by = person_level_diff[ , c("occasions_drawn", "n_occasions", "itemset_type")],
                             FUN = function(x) {
                               if (all(is.na(x))) {
                                 NA_real_ # if person does not have ANY valid value -> return NA
                               } else {
                                 mean(x, na.rm=TRUE) # calculate mean, but remove NAs
                               }
                             }) 

names(person_diff_agg) <- c("occasions_drawn", "n_occasions", "itemset_type", paste0("person_difference_", 1:n_persons))



### for ICC.z
vars <- paste0("person_diff_ICC.z_", 1:n_persons)
person_diff_agg.z <- aggregate(person_level_diff.z[ , vars],
                               by = person_level_diff.z[ , c("occasions_drawn", "n_occasions", "itemset_type")],
                               FUN = function(x) {
                                 if (all(is.na(x))) {
                                   NA_real_ # if person does not have ANY valid value -> return NA
                                 } else {
                                   mean(x, na.rm=TRUE) # calculate mean, but remove NAs
                                 }
                               }) 

names(person_diff_agg.z) <- c("occasions_drawn", "n_occasions", "itemset_type", paste0("person_difference.z_", 1:n_persons))



# Aggregate Across Participants (for Plotting) -
## for ICC
# calculate overall difference (mean difference across participants)
person_diff_agg$difference_mean <- rowMeans(person_diff_agg[ , 4:(n_persons+3)], na.rm=T)
person_diff_agg$difference_median <- apply(person_diff_agg[ ,4:(n_persons+3)], 1, median, na.rm=T) 

# determine which participant deviates most
person_diff_agg$difference_min_id <- apply(person_diff_agg[ ,4:(n_persons+3)], 1, which.min)
person_diff_agg$difference_max_id <- apply(person_diff_agg[ ,4:(n_persons+3)], 1, which.max)

# calculate min and max
person_diff_agg$difference_min <- apply(person_diff_agg[ ,4:(n_persons+3)], 1, min, na.rm=T)
person_diff_agg$difference_max <- apply(person_diff_agg[ ,4:(n_persons+3)], 1, max, na.rm=T)


person_diff_agg <- person_diff_agg[ , c("occasions_drawn", "n_occasions", "itemset_type",
                                        "difference_mean", "difference_median", "difference_min_id",
                                        "difference_max_id", "difference_min", "difference_max")]


## for ICC.z
# calculate overall difference (mean difference across participants)
person_diff_agg.z$difference.z_mean <- rowMeans(person_diff_agg.z[ , 4:(n_persons+3)], na.rm=T)
person_diff_agg.z$difference.z_median <- apply(person_diff_agg.z[ ,4:(n_persons+3)], 1, median, na.rm=T) 

# determine which participant deviates most
person_diff_agg.z$difference.z_min_id <- apply(person_diff_agg.z[ ,4:(n_persons+3)], 1, which.min)
person_diff_agg.z$difference.z_max_id <- apply(person_diff_agg.z[ ,4:(n_persons+3)], 1, which.max)

# calculate min and max
person_diff_agg.z$difference.z_min <- apply(person_diff_agg.z[ ,4:(n_persons+3)], 1, min, na.rm=T)
person_diff_agg.z$difference.z_max <- apply(person_diff_agg.z[ ,4:(n_persons+3)], 1, max, na.rm=T)



person_diff_agg.z <- person_diff_agg.z[ , c("occasions_drawn", "n_occasions", "itemset_type",
                                            "difference.z_mean", "difference.z_median", "difference.z_min_id",
                                            "difference.z_max_id", "difference.z_min", "difference.z_max")]


# Calculate RMSE for Each Participant Across Replications 
# '' For ICCs 
# use person-level differences -> square
person_level_diff_sq <- person_level_diff[ ,1:4]
person_level_diff_sq[ , 5:(n_persons+4)] <- (person_level_diff[ ,5:(n_persons+4)])^2
names(person_level_diff_sq)[5:(n_persons+4)] <- paste0("sq_diff_ICC_", 1:n_persons)

### aggregate

person_level_diff_sq.rd <- person_level_diff_sq[person_level_diff_sq$occasions_drawn == "random", ]
# only select conditions with random draws of occasions

# RMSE = sqrt(sum(sq_diff_ICC)/n_replication)
sq_diff_cols <- paste0("sq_diff_ICC_", 1:n_persons)

RMSE <- aggregate(person_level_diff_sq.rd[ , sq_diff_cols], # for each participant-specific sq_diff column
                  by = person_level_diff_sq.rd[ , c("occasions_drawn", "n_occasions", "itemset_type")], # aggregate across conditions
                  FUN = function(x) {
                    
                    n_valid <- sum(!is.na(x)) # determine the number of valid replications in given condition for participant
                    
                    if (n_valid == 0) { # if there are no valid replications, return NA
                      NA_real_
                    } else { # else, take the square root of the the summed sq_diff divided by number of (valid) replications
                      sqrt(sum(x, na.rm=TRUE) / n_valid)
                    }
                    
                  })
names(RMSE)[4:(n_persons+3)] <- c(paste0("RMSE_", 1:n_persons))
# order
RMSE <- RMSE[order(RMSE$occasions_drawn, RMSE$itemset_type, RMSE$n_occasions), ]

# Calculate min, mean, and max across participants
RMSE$RMSE_min <- apply(RMSE[ , 4:(n_persons+3)], 1, FUN = min, na.rm = TRUE)
RMSE$RMSE_mean <- rowMeans(RMSE[ ,4:(n_persons+3)], na.rm=TRUE)
RMSE$RMSE_max <- apply(RMSE[ , 4:(n_persons+3)], 1, FUN = max, na.rm = TRUE)
# subset 
RMSE <- RMSE[ , c("occasions_drawn", "n_occasions", "itemset_type",
                  "RMSE_min", "RMSE_mean", "RMSE_max")]


# '' For ICC.z 
# use person-level differences -> square
person_level_diff.z_sq <- person_level_diff.z[ ,1:4]
person_level_diff.z_sq[ , 5:(n_persons+4)] <- (person_level_diff.z[ ,5:(n_persons+4)])^2
names(person_level_diff.z_sq)[5:(n_persons+4)] <- paste0("sq_diff_ICC.z_", 1:n_persons)


### aggregate
person_level_diff.z_sq.rd <- person_level_diff.z_sq[person_level_diff.z_sq$occasions_drawn == "random", ]
# only select conditions with random draws of occasions

# RMSE.z = sqrt(sum(sq_diff_ICC)/n_replication)
sq_diff_cols <- paste0("sq_diff_ICC.z_", 1:n_persons)


RMSE.z <- aggregate(person_level_diff.z_sq.rd[ , sq_diff_cols], # for each participant-specific sq_diff column
                    by = person_level_diff.z_sq.rd[ , c("occasions_drawn", "n_occasions", "itemset_type")], # aggregate across conditions
                    FUN = function(x) {
                      
                      n_valid <- sum(!is.na(x)) # determine the number of valid replications in given condition for participant
                      
                      if (n_valid == 0) { # if there are no valid replications, return NA
                        NA_real_
                      } else { # else, take the square root of the the summed sq_diff divided by number of (valid) replications
                        sqrt(sum(x, na.rm=TRUE) / n_valid)
                      }
                      
                    })
names(RMSE.z)[4:(n_persons+3)] <- c(paste0("RMSE.z_", 1:n_persons))
# order
RMSE.z <- RMSE.z[order(RMSE.z$occasions_drawn, RMSE.z$itemset_type, RMSE.z$n_occasions), ]


# Calculate min, mean, and max across participants
RMSE.z$RMSE.z_min <- apply(RMSE.z[ , 4:(n_persons+3)], 1, FUN = min, na.rm = TRUE)
RMSE.z$RMSE.z_mean <- rowMeans(RMSE.z[ ,4:(n_persons+3)], na.rm=TRUE)
RMSE.z$RMSE.z_max <- apply(RMSE.z[ , 4:(n_persons+3)], 1, FUN = max, na.rm = TRUE)
# subset 
RMSE.z <- RMSE.z[ , c("occasions_drawn", "n_occasions", "itemset_type",
                      "RMSE.z_min", "RMSE.z_mean", "RMSE.z_max")]


rm(diff_matrix, diff.z_matrix, ICC_matrix, ICC.z_matrix, person_level_diff,
   person_level_diff.z, person_level_diff_sq, person_level_diff.z_sq, person_level_ICC_estimates,
   person_level_diff.z_sq.rd, person_level_ICC.z_estimates, person_level_diff_sq.rd,
   sq_diff_cols, vars, n_persons)



# aggregate remaining outcomes

# helper function
# Fisher's Z-transformation
fisher_z <- function(r) {
  z <- 0.5 * log( (1 + r) / (1 - r) )
  return(z)
}

# back-transformation
inverse_fisher_z <- function(z) {
  r <- ( exp(2*z) - 1 ) / ( exp(2*z) + 1 )
  return(r)
}


cor <- do.call(
  data.frame,
  aggregate(cor_ICC ~ occasions_drawn + n_occasions + itemset_type, data = PED_sub, FUN = function(x) {
    # include check -> results may be NA if there was no valid ICC data at all
    if (all(is.na(x))) {
      return(c(NA_real_, NA_real_, NA_real_))
    }
    
    c(min(x, na.rm=TRUE),
      inverse_fisher_z(mean(fisher_z(x), na.rm=TRUE)), # apply Fisher's Z-transformation, average, backtransform
      max(x, na.rm=TRUE)) 
    
  },
  na.action = na.pass),
)
cor
names(cor) <- c("occasions_drawn", "n_occasions", "itemset_type", "cor_ICC_min", "cor_ICC_mean", "cor_ICC_max")

cor.z <- do.call(
  data.frame,
  aggregate(cor_ICC.z ~ occasions_drawn + n_occasions + itemset_type, data = PED_sub, FUN = function(x) {
    # include check -> results may be NA if there was no valid ICC data at all
    if (all(is.na(x))) {
      return(c(NA_real_, NA_real_, NA_real_))
    }
    
    c(min(x, na.rm=TRUE),
      inverse_fisher_z(mean(fisher_z(x), na.rm=TRUE)), # apply Fisher's Z-transformation, average, backtransform
      max(x, na.rm=TRUE)) 
    
  },
  na.action = na.pass),
)
cor.z
names(cor.z) <- c("occasions_drawn", "n_occasions", "itemset_type", "cor_ICC.z_min", "cor_ICC.z_mean", "cor_ICC.z_max")



sd <- do.call(
  data.frame,
  aggregate(sd_ICC ~ occasions_drawn + n_occasions + itemset_type, data = PED_sub, FUN = function(x) {
    # include check -> results may be NA if there was no valid ICC data at all
    if (all(is.na(x))) {
      return(c(NA_real_, NA_real_, NA_real_))
    }
    
    c(min(x, na.rm=TRUE),
      mean(x, na.rm=TRUE), 
      max(x, na.rm=TRUE)) 
    
  },
  na.action = na.pass),
)
sd
names(sd) <- c("occasions_drawn", "n_occasions", "itemset_type", "sd_ICC_min", "sd_ICC_mean", "sd_ICC_max")




sd.z <- do.call(
  data.frame,
  aggregate(sd_ICC.z~ occasions_drawn + n_occasions + itemset_type, data = PED_sub, FUN = function(x) {
    # include check -> results may be NA if there was no valid ICC data at all
    if (all(is.na(x))) {
      return(c(NA_real_, NA_real_, NA_real_))
    }
    
    c(min(x, na.rm=TRUE),
      mean(x, na.rm=TRUE), # apply Fisher's Z-transformation, average, backtransform
      max(x, na.rm=TRUE)) 
    
  },
  na.action = na.pass),
)
sd.z
names(sd.z) <- c("occasions_drawn", "n_occasions", "itemset_type", "sd_ICC.z_min", "sd_ICC.z_mean", "sd_ICC.z_max")

rel <- do.call(
  data.frame,
  aggregate(rel ~ occasions_drawn + n_occasions + itemset_type, data = PED_sub, FUN = function(x) {
    # include check -> results may be NA if there was no valid ICC data at all
    if (all(is.na(x))) {
      return(c(NA_real_, NA_real_, NA_real_))
    }
    
    c(min(x, na.rm=TRUE),
      mean(x, na.rm=TRUE), # apply Fisher's Z-transformation, average, backtransform
      max(x, na.rm=TRUE)) 
    
  },
  na.action = na.pass),
)
rel
names(rel) <- c("occasions_drawn", "n_occasions", "itemset_type", "rel_min", "rel_mean", "rel_max")


percnegICC <- do.call(
  data.frame,
  aggregate(percnegICC_raw ~ occasions_drawn + n_occasions + itemset_type, data = PED_sub, FUN = function(x) {
    # include check -> results may be NA if there was no valid ICC data at all
    if (all(is.na(x))) {
      return(c(NA_real_, NA_real_, NA_real_))
    }
    
    c(min(x, na.rm=TRUE),
      mean(x, na.rm=TRUE), # apply Fisher's Z-transformation, average, backtransform
      max(x, na.rm=TRUE)) 
    
  },
  na.action = na.pass),
)
percnegICC
names(percnegICC) <- c("occasions_drawn", "n_occasions", "itemset_type", "percnegICC_raw_min", "percnegICC_raw_mean", "percnegICC_raw_max")



agg_res_PED <- list(cor = cor,
                    cor.z = cor.z,
                    sd = sd,
                    sd.z = sd.z,
                    rel = rel,
                    percnegICC = percnegICC,
                    person_diff_agg = person_diff_agg,
                    person_diff_agg.z = person_diff_agg.z,
                    RMSE = RMSE,
                    RMSE.z = RMSE.z)

save(agg_res_PED, file = "results/02_revision_1/emolive study/PED/main/processed/extreme_item_sets_agg_res_PED_emolive_Study.rda")

rm(list=ls())




# Plot Results for Item Sets ----------------------------------------------
load("results/02_revision_1/emolive study/PED/main/processed/extreme_item_sets_agg_res_PED_emolive_Study.rda")
load("results/02_revision_1/emolive study/NED/main/processed/extreme_item_sets_agg_res_NED_emolive_Study.rda")



# '' Create Function ------------------------------------------------------

my_theme <- theme_bw() +
  theme(panel.grid.minor = element_blank(),
        text = element_text(size=12),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 0)) 



plot_outcome <- function(data, ylims=NULL, ylabel=NULL, x_breaks = seq(0, 70, 10), theme_custom = my_theme,
                         dodge_width = 2,
                         scale_color = scale_color_grey(start = 0.45, end = 0.00),
                         split_facets = FALSE,
                         facet_var = "occasions_drawn",
                         facet_order = c("random", "by order")) {
  # data : data frame with the results
  # ylims: optional y-limit per outcome 
  # ylabel: optional y-axis label
  # x_breaks: breaks to use on x-axis, e.g., seq(0, 100, 10)
  # theme_custom : ggplot theme
  # scale_color: define color theme
  # dodge_width: argument indicating how much to jitter points from different grouping variables
  # split_facets: logical indicating whether the two facets (e.g., occasions_drawn) should be
  # plotted in one plot or in separate plots
  # facet_var: chr indicating name of the facet variable to split by
  # facet_order: chr defining order of the facet levels so that order is the same across plots
  
  
  # Identify mean, min, max columns automatically from data frame
  col_mean <- grep("_mean$", names(data), value = TRUE)
  col_min  <- grep("_min$", names(data), value = TRUE)
  col_max  <- grep("_max$", names(data), value = TRUE)
  
  if (length(col_mean) != 1 || length(col_min) != 1 || length(col_max) != 1) {
    stop("Expected exactly one *_mean, *_min, and *_max column.")
  }
  
  
  # read the outcome name from the last column in data (should be outcome_max)
  # last column = length(data)
  outcome_name <- sub("_max$", "", names(data)[length(data)])
  
  
  # Define facet variable (e.g., occasions_drawn or NED/PED...)
  if (!(facet_var %in% names(data))) {
    stop(sprintf("Facet variable '%s' not found in data.", facet_var))
  }
  
  data[ , facet_var] <- factor(data[, facet_var], levels = facet_order) # order factor variable in order as specified
  
  facet_formula <- ggh4x::facet_manual(as.formula(paste0("~", facet_var)), design=matrix(seq_along(facet_order), nrow=1, byrow=TRUE), drop=FALSE)
  
  
  # Build function for base plot
  base_plot <- function(data) {
    p <- ggplot(data, aes(
      x = n_occasions, # x axis: n_occasions
      y = .data[[col_mean]], # y axis: mean outcome
      color = factor(endorsement), # different lines for endorsement
      shape = factor(endorsement),
      linetype = factor(endorsement),
      group = factor(endorsement)
    )) +
      geom_point(position = position_dodge(width = dodge_width)) +
      geom_line(linewidth = 0.3, position = position_dodge(width = dodge_width)) +
      geom_errorbar(aes(ymin = .data[[col_min]], ymax = .data[[col_max]]),
                    position = position_dodge(width = dodge_width),
                    linewidth = 0.3) + # error bar: min and max outcome
      scale_x_continuous(breaks = x_breaks) +
      expand_limits(x = 70) + # make sure that 70 is always covered
      xlab("Number of Measurement Occasions") +
      #  if y label is provided, use it; else, use the outcome name extracted from column names of data 
      ylab(ifelse(!is.null(ylabel), ylabel, outcome_name)) +
      scale_color +
      scale_shape_manual(values = c(16, 17, 15, 18, 1, 2, 0)) +
      labs(color = "Item Set Type", shape = "Item Set Type", linetype = "Item Set Type") +
      guides(color = guide_legend(title = "Item Set Type"),
             shape = guide_legend(title = "Item Set Type"),
             linetype = guide_legend(title = "Item Set Type")) +
      theme_custom
    
    if (!is.null(ylims)) {
      
      ymin <- ylims[1]
      ymax <- ylims[2]
      
      p <- p +
        coord_cartesian(ylim = c(ymin, ymax)) +
        scale_y_continuous(breaks = scales::breaks_pretty(n = 5), labels = function(x) {
          ifelse(x < 0, sprintf("%6.2f", x), sprintf(" %6.2f", x))
        }
        )
    }
    
    return(p)
  }
  
  
  # Plot according to split_facet == TRUE or FALSE
  if (split_facets == FALSE) {
    return(base_plot(data) + facet_formula +
             force_panelsizes(rows=1, cols=rep(1,length(facet_order))))
  } else if (split_facets == TRUE) {
    
    split_plots <- lapply(facet_order, # apply to each unique facet of the facet_var to split by
                          function(facet) { # function of facet
                            data_sub <- data[which(data[ , facet_var] == facet), ] # subset data according to facet
                            p <- base_plot(data_sub)
                            p <- p + ggtitle(paste0(facet_var, ": ", facet))
                            return(p)
                          })
    return(split_plots)
    
  }
  
}





# add variable to all outcomes in agg_ned and agg_ped specifying NED or PED (will be used as facet in plots)
# str(agg_ned)
# names(agg_ned)

for (out in names(agg_res_ned)) {
  agg_res_ned[[out]]$diff_type <- "NED"
}

for (out in names(agg_res_PED)) {
  agg_res_PED[[out]]$diff_type <- "PED"
}


# now merge agg_ned and agg_ped into one data frame for each outcome
both <- list()
for (out in names(agg_res_ned)) {
  both[[out]] <- rbind(agg_res_ned[[out]], agg_res_PED[[out]])
}



# now add a new item sets grouping variables

for (out in names(both)) {
 
  both[[out]]$endorsement <- NA
  
  both[[out]]$endorsement[
    grepl("low endorsement", both[[out]]$itemset_type)
  ] <- "low endorsement"
  
  both[[out]]$endorsement[
    grepl("high endorsement", both[[out]]$itemset_type)
  ] <- "high endorsement"
  
  both[[out]]$endorsement <- factor(
    both[[out]]$endorsement,
    levels = c("low endorsement", "high endorsement")
  )
  
  both[[out]]$item_count <- NA
  
  both[[out]]$item_count[
    both[[out]]$itemset_type %in% c("4 items: low endorsement",
                                      "4 items: high endorsement",
                                      "5 items: low endorsement",
                                      "5 items: high endorsement")
  ] <- "4 or 5 items"
  
  both[[out]]$item_count[
    both[[out]]$itemset_type %in% c("8 items: low endorsement",
                                      "8 items: high endorsement",
                                      "10 items: low endorsement",
                                      "10 items: high endorsement")
  ] <- "8 or 10 items"
  
}


# Create subset with smallest item set each (4 or 5 items)
items45 <- lapply(both, function(x) {
  x[x$item_count == "4 or 5 items", ]
})

# Create subset with second item set each (8 or 10 items)
items810 <- lapply(both, function(x) {
  x[x$item_count == "8 or 10 items", ]
})



# plot outcomes
# data frames already stored in a list
# but use only those that we want to plot
# separately for different item counts
data_list45 <- list(cor = items45[["cor"]],
                  cor.z = items45[["cor.z"]],
                  diff = items45[["person_diff_agg"]],
                  diff.z = items45[["person_diff_agg.z"]],
                  rmse = items45[["RMSE"]],
                  rmse.z = items45[["RMSE.z"]],
                  sd = items45[["sd"]],
                  sd.z = items45[["sd.z"]],
                  rel = items45[["rel"]],
                  percnegICC = items45[["percnegICC"]])


data_list810 <- list(cor = items810[["cor"]],
                    cor.z = items810[["cor.z"]],
                    diff = items810[["person_diff_agg"]],
                    diff.z = items810[["person_diff_agg.z"]],
                    rmse = items810[["RMSE"]],
                    rmse.z = items810[["RMSE.z"]],
                    sd = items810[["sd"]],
                    sd.z = items810[["sd.z"]],
                    rel = items810[["rel"]],
                    percnegICC = items810[["percnegICC"]])





# define the y label for each outcome plot
ylabels <- list("Correlation with Benchmark",
                "Correlation with Benchmark (ICC.z)",
                "Difference in ICCs to Benchmark",
                "Difference in ICCs to Benchmark (ICC.z)",
                "RMSE",
                "RMSE (ICC.z)",
                "SD of ICCs",
                "SD of ICC.z",
                "Reliability of ICCs",
                "Proportion of Negative ICCs") 
names(ylabels) <- names(data_list45)


# for correlation, reliability, proportions, ... -> use theoretical range (e.g., difference max and min: 1 and -1)
ylim_list <- list(
  c(0, 1), # correlation with benchmark, theoretical range
  c(0, 1), # correlation with benchmark (ICC.z), theoretical range
  c(-1, 1), # difference in ICCs (compared to benchmark), theoretical range
  c(-1.4, 0.2), # difference in ICCs (compared to benchmark) for ICC.z
  c(0, 0.6), # RMSE, theoretical lower bound
  c(0, 1.5), # RMSE (ICC.z)
  c(0, 0.3), # SD of ICCs, theoretical lower bound
  c(0, 0.5), # SD of ICC.z, theoretical lower bound
  c(0, 1), # Reliability
  c(0, 1) # proportion of negative ICCs
)
names(ylim_list) <- names(data_list45)


# plot outcomes
plot_list45 <- lapply(names(data_list45), function(outcome) {
  df <- data_list45[[outcome]]
  plot_outcome(df,
               ylabel = ylabels[[outcome]],
               ylims=ylim_list[[outcome]],
               x_breaks = seq(0, 70, 10),
               theme_custom = my_theme,
               dodge_width = 3,
               split_facets = FALSE,
               facet_var = "diff_type",
               facet_order = c("NED", "PED"))
})

names(plot_list45) <- names(data_list45)

plot_list45[["cor"]]
plot_list45[["cor.z"]]
plot_list45[["diff"]]
plot_list45[["diff.z"]]
plot_list45[["rmse"]]
plot_list45[["rmse.z"]]
plot_list45[["sd"]]
plot_list45[["sd.z"]]
plot_list45[["rel"]]
plot_list45[["percnegICC"]]


# plot outcomes
plot_list810 <- lapply(names(data_list810), function(outcome) {
  df <- data_list810[[outcome]]
  plot_outcome(df,
               ylabel = ylabels[[outcome]],
               ylims=ylim_list[[outcome]],
               x_breaks = seq(0, 70, 10),
               theme_custom = my_theme,
               dodge_width = 3,
               split_facets = FALSE,
               facet_var = "diff_type",
               facet_order = c("NED", "PED"))
})

names(plot_list810) <- names(data_list810)

plot_list810[["cor"]]
plot_list810[["cor.z"]]
plot_list810[["diff"]]
plot_list810[["diff.z"]]
plot_list810[["rmse"]]
plot_list810[["rmse.z"]]
plot_list810[["sd"]]
plot_list810[["sd.z"]]
plot_list810[["rel"]]
plot_list810[["percnegICC"]]


# Combine plots

## for raw ICCS
# adjust the plots a little (e.g., no y-axis lable but title, no x-axis label)
a45 <- plot_list45[["cor"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                axis.text.y = element_text(hjust=1),
                                axis.text = element_text(size=10)) + ggtitle("(A) Correlation with Benchmark") +
  geom_hline(yintercept=0.80, linetype="twodash", color = "black") + force_panelsizes(rows=1, cols = c(1,1))
# a
b45 <- plot_list45[["diff"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                 plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                 axis.text.y = element_text(hjust=1),
                                 axis.text = element_text(size=10)) + ggtitle("(B) Difference from Benchmark") + force_panelsizes(rows=1, cols = c(1,1))
# b
c45 <- plot_list45[["rmse"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                 plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                 axis.text.y = element_text(hjust=1),
                                 axis.text = element_text(size=10)) + ggtitle("(C) RMSE")+ force_panelsizes(rows=1, cols = c(1,1))


# c
d45 <- plot_list45[["sd"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                               plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                               axis.text.y = element_text(hjust=1),
                               axis.text = element_text(size=10)) + ggtitle("(D) SD of ICCs") + force_panelsizes(rows=1, cols = c(1,1))

# d
e45 <- plot_list45[["rel"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                axis.text.y = element_text(hjust=1),
                                axis.text = element_text(size=10)) + ggtitle("(E) Reliability of ICCs") +
  geom_hline(yintercept=0.80, linetype="twodash", color = "black")+ force_panelsizes(rows=1, cols = c(1,1))
# e
f45 <- plot_list45[["percnegICC"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                       plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                       axis.text.y = element_text(hjust=1),
                                       axis.text = element_text(size=10)) + ggtitle("(F) Proportion of Negative ICCs")+ force_panelsizes(rows=1, cols = c(1,1))


combined45 <- ggpubr::ggarrange(a45,b45,c45,d45,e45,f45 , ncol=3, nrow=2, common.legend = TRUE, legend="top",
                              align = "hv", widths = c(1,1,1), heights = c(1, 1)) # equal panel sizes

combined45 <- annotate_figure(combined45,
                               bottom = text_grob("Number of Measurement Occasions", size = 12))


combined45


## for raw ICCS
# adjust the plots a little (e.g., no y-axis lable but title, no x-axis label)
a810 <- plot_list810[["cor"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                  plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                  axis.text.y = element_text(hjust=1),
                                  axis.text = element_text(size=10)) + ggtitle("(A) Correlation with Benchmark") +
  geom_hline(yintercept=0.80, linetype="twodash", color = "black") + force_panelsizes(rows=1, cols = c(1,1))
# a
b810 <- plot_list810[["diff"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                   plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                   axis.text.y = element_text(hjust=1),
                                   axis.text = element_text(size=10)) + ggtitle("(B) Difference from Benchmark") + force_panelsizes(rows=1, cols = c(1,1))
# b
c810 <- plot_list810[["rmse"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                   plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                   axis.text.y = element_text(hjust=1),
                                   axis.text = element_text(size=10)) + ggtitle("(C) RMSE")+ force_panelsizes(rows=1, cols = c(1,1))


# c
d810 <- plot_list810[["sd"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                 plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                 axis.text.y = element_text(hjust=1),
                                 axis.text = element_text(size=10)) + ggtitle("(D) SD of ICCs") + force_panelsizes(rows=1, cols = c(1,1))

# d
e810 <- plot_list810[["rel"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                  plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                  axis.text.y = element_text(hjust=1),
                                  axis.text = element_text(size=10)) + ggtitle("(E) Reliability of ICCs") +
  geom_hline(yintercept=0.80, linetype="twodash", color = "black")+ force_panelsizes(rows=1, cols = c(1,1))
# e
f810 <- plot_list810[["percnegICC"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                         plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                         axis.text.y = element_text(hjust=1),
                                         axis.text = element_text(size=10)) + ggtitle("(F) Proportion of Negative ICCs")+ force_panelsizes(rows=1, cols = c(1,1))


combined810 <- ggpubr::ggarrange(a810,b810,c810,d810,e810,f810 , ncol=3, nrow=2, common.legend = TRUE, legend="top",
                                align = "hv", widths = c(1,1,1), heights = c(1, 1)) # equal panel sizes


combined810 <- annotate_figure(combined810,
                              bottom = text_grob("Number of Measurement Occasions", size = 12))

combined810




ggsave("plots/02_revision_1/for publication/emolive_item_set_specific_plots_4_5_items_random_draws_NED_PED.pdf",plot = combined45, device="pdf", height = 148, width = 220, unit="mm")

ggsave("plots/02_revision_1/for publication/emolive_item_set_specific_plots_4_5_items_random_draws_NED_PED.svg",plot = combined45, device="svg", height = 148, width = 220, unit="mm")

# as tiff
tiff("plots/02_revision_1/for publication/emolive_item_set_specific_plots_4_5_items_random_draws_NED_PED.tiff", units="mm", width=220, height=148, res=1200)
combined45
dev.off()


ggsave("plots/02_revision_1/for publication/emolive_item_set_specific_plots_8_10_items_random_draws_NED_PED.pdf",plot = combined810, device="pdf", height = 148, width = 220, unit="mm")

ggsave("plots/02_revision_1/for publication/emolive_item_set_specific_plots_8_10_items_random_draws_NED_PED.svg",plot = combined810, device="svg", height = 148, width = 220, unit="mm")

# as tiff
tiff("plots/02_revision_1/for publication/emolive_item_set_specific_plots_8_10_items_random_draws_NED_PED.tiff", units="mm", width=220, height=148, res=1200)
combined810
dev.off()





# '' Make Results Table ---------------------------------------------------

# NED
# remove diff_type variable before joining results into a table
agg_res_ned_join <- lapply(
  agg_res_ned,
  function(x) {
    x[ , !names(x) %in% "diff_type"]
  }
)

results_ned <- merge(agg_res_ned_join[["cor"]], agg_res_ned_join[["person_diff_agg"]],
                     by = c("occasions_drawn", "n_occasions", "itemset_type"))

results_ned <- merge(results_ned, agg_res_ned_join[["RMSE"]],
                     by = c("occasions_drawn", "n_occasions", "itemset_type"))

results_ned <- merge(results_ned, agg_res_ned_join[["sd"]],
                     by = c("occasions_drawn", "n_occasions", "itemset_type"))

results_ned <- merge(results_ned, agg_res_ned_join[["rel"]],
                     by = c("occasions_drawn", "n_occasions", "itemset_type"))

results_ned <- merge(results_ned, agg_res_ned_join[["percnegICC"]],
                     by = c("occasions_drawn", "n_occasions", "itemset_type"))

results_ned

# order by factor:
# order factor
results_ned$itemset_type <- factor(
  results_ned$itemset_type,
  levels = c(
    "5 items: low endorsement",
    "5 items: high endorsement",
    "10 items: low endorsement",
    "10 items: high endorsement"
  )
)

results_ned <- results_ned[order(results_ned$occasions_drawn, results_ned$itemset_type, results_ned$n_occasions), ]
# round
results_ned[ , 4:24] <- round(results_ned[ , 4:24], 3)


write.csv(results_ned,  
          "results/02_revision_1/emolive study/NED/main/processed/extreme_item_sets_results_table_NED_emolive.csv",
          row.names = FALSE)


# PED
# remove diff_type variable before joining results into a table
agg_res_PED_join <- lapply(
  agg_res_PED,
  function(x) {
    x[ , !names(x) %in% "diff_type"]
  }
)

results_PED <- merge(agg_res_PED_join[["cor"]], agg_res_PED_join[["person_diff_agg"]],
                     by = c("occasions_drawn", "n_occasions", "itemset_type"))

results_PED <- merge(results_PED, agg_res_PED_join[["RMSE"]],
                     by = c("occasions_drawn", "n_occasions", "itemset_type"))

results_PED <- merge(results_PED, agg_res_PED_join[["sd"]],
                     by = c("occasions_drawn", "n_occasions", "itemset_type"))

results_PED <- merge(results_PED, agg_res_PED_join[["rel"]],
                     by = c("occasions_drawn", "n_occasions", "itemset_type"))

results_PED <- merge(results_PED, agg_res_PED_join[["percnegICC"]],
                     by = c("occasions_drawn", "n_occasions", "itemset_type"))

results_PED

# order by factor:
# order factor
results_PED$itemset_type <- factor(
  results_PED$itemset_type,
  levels = c(
    "4 items: low endorsement",
    "4 items: high endorsement",
    "8 items: low endorsement",
    "8 items: high endorsement"
  )
)

results_PED <- results_PED[order(results_PED$occasions_drawn, results_PED$itemset_type, results_PED$n_occasions), ]
# round
results_PED[ , 4:24] <- round(results_PED[ , 4:24], 3)


write.csv(results_PED,  
          "results/02_revision_1/emolive study/PED/main/processed/extreme_item_sets_results_table_PED_emolive.csv",
          row.names = FALSE)
