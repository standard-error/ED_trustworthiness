###################################################################
#####      Estimating trait emotion differentiation:          #####
#####          How many measurement occasions and             #####
#####              emotion items are needed?                  #####
###################################################################

###################################################################
#####       Plots for Supplement (Item Set Analysis)          #####
###################################################################

# Determine item sets with highest and lowest mean value
# -> plot outcomes for the item sets with the highest and lowest
# mean value to inspect how the results differ in dependence 
# of endorsement of the items.

# To keep it feasible:
# Use 3, 6 and 8 items for NED (for 9 items, there is only one item set)
# Use 3 and 4 for PED (for 5 items, there is only one item set)


# Load Packages -----------------------------------------------------------
library(tidyverse)
library(ggpubr)


# Source Helper Functions -------------------------------------------------
source("functions/function_determine_all_possible_item_sets.R")
source("functions/function_plot_outcomes.R")


# Load Data ---------------------------------------------------------------
load("prepared data/EMOTIONS_benchmark_data.rda")



itemsets3_neg <- generate_all_item_sets(all_items = c("angry", "excluded", "envious",
                                                      "resentful", "ashamed", "insecure",
                                                      "anxious", "sad", "lonely"),
                                        categories = NULL,
                                        n_items = 3)


itemsets3_neg_means <- sapply(itemsets3_neg, function(itemset) {
  
  # extract item names
  items <- strsplit(itemset, ", ")[[1]]
  
  # calculate mean value for each item
  item_means <- colMeans(bench[, items], na.rm=TRUE)
  
  # calculate mean value across items
  mean(item_means, na.rm=T)
})


highest_itemset3_neg <- names(which.max(itemsets3_neg_means))
highest_itemset3_neg
max(itemsets3_neg_means)
itemsets3_neg_means[which.max(itemsets3_neg_means)]
lowest_itemset3_neg <- names(which.min(itemsets3_neg_means))
lowest_itemset3_neg
min(itemsets3_neg_means)
itemsets3_neg_means[which.min(itemsets3_neg_means)]
# means are quite close to each other

itemsets6_neg <- generate_all_item_sets(all_items = c("angry", "excluded", "envious",
                                                      "resentful", "ashamed", "insecure",
                                                      "anxious", "sad", "lonely"),
                                         categories = NULL,
                                         n_items = 6)


itemsets6_neg_means <- sapply(itemsets6_neg, function(itemset) {
  
  # extract item names
  items <- strsplit(itemset, ", ")[[1]]
  
  # calculate mean value for each item
  item_means <- colMeans(bench[, items], na.rm=TRUE)
  
  # calculate mean value across items
  mean(item_means, na.rm=T)
})


highest_itemset6_neg <- names(which.max(itemsets6_neg_means))
highest_itemset6_neg
max(itemsets6_neg_means)
itemsets6_neg_means[which.max(itemsets6_neg_means)]
lowest_itemset6_neg <- names(which.min(itemsets6_neg_means))
lowest_itemset6_neg
min(itemsets6_neg_means)
itemsets6_neg_means[which.min(itemsets6_neg_means)]
# again, means are close to each other



itemsets8_neg <- generate_all_item_sets(all_items = c("angry", "excluded", "envious",
                                                      "resentful", "ashamed", "insecure",
                                                      "anxious", "sad", "lonely"),
                                        categories = NULL,
                                        n_items = 8)


itemsets8_neg_means <- sapply(itemsets8_neg, function(itemset) {
  
  # extract item names
  items <- strsplit(itemset, ", ")[[1]]
  
  # calculate mean value for each item
  item_means <- colMeans(bench[, items], na.rm=TRUE)
  
  # calculate mean value across items
  mean(item_means, na.rm=T)
})


highest_itemset8_neg <- names(which.max(itemsets8_neg_means))
highest_itemset8_neg
max(itemsets8_neg_means)
itemsets8_neg_means[which.max(itemsets8_neg_means)]
lowest_itemset8_neg <- names(which.min(itemsets8_neg_means))
lowest_itemset8_neg
min(itemsets8_neg_means)
itemsets8_neg_means[which.min(itemsets8_neg_means)]
# again, means are close to each other


itemsets3_pos <- generate_all_item_sets(all_items = c("proud", "success", "superior",
                                                      "enthusiastic", "relaxed"),
                                        categories = NULL,
                                        n_items = 3)


itemsets3_pos_means <- sapply(itemsets3_pos, function(itemset) {
  
  # extract item names
  items <- strsplit(itemset, ", ")[[1]]
  
  # calculate mean value for each item
  item_means <- colMeans(bench[, items], na.rm=TRUE)
  
  # calculate mean value across items
  mean(item_means, na.rm=T)
})


highest_itemset3_pos <- names(which.max(itemsets3_pos_means))
highest_itemset3_pos
max(itemsets3_pos_means)
itemsets3_pos_means[which.max(itemsets3_pos_means)]
lowest_itemset3_pos <- names(which.min(itemsets3_pos_means))
lowest_itemset3_pos
min(itemsets3_pos_means)
itemsets3_pos_means[which.min(itemsets3_pos_means)]






itemsets4_pos <- generate_all_item_sets(all_items = c("proud", "success", "superior",
                                                      "enthusiastic", "relaxed"),
                                        categories = NULL,
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





rm(itemsets3_neg, itemsets3_neg_means, itemsets6_neg, itemsets6_neg_means, itemsets8_neg, itemsets8_neg_means,
   itemsets3_pos, itemsets3_pos_means, itemsets4_pos, itemsets4_pos_means,
   generate_all_item_sets, order_item_sets)



# Load Simulation Results and Aggregate -----------------------------------



# '' NED ------------------------------------------------------------------


# NED
load("results/02_revision_1/EMOTIONS study/NED/main/raw/sim_results_NED_EMOTIONS_Study.rda")
NED <- res
rm(res)


names(NED)

# subset
# use only highest and lowest intensity item set for 3, 6 and 8 items
# only use random draws (main analysis; ordered draws are sensitivity analysis)
NED_sub <- NED[NED$items %in% c(lowest_itemset3_neg,
                                highest_itemset3_neg,
                                lowest_itemset6_neg,
                                highest_itemset6_neg,
                                lowest_itemset8_neg,
                                highest_itemset8_neg) &
                 NED$occasions_drawn == "random", ]



# add label
NED_sub$itemset_type[NED_sub$items == lowest_itemset3_neg]  <- "3 items: low endorsement"
NED_sub$itemset_type[NED_sub$items == highest_itemset3_neg] <- "3 items: high endorsement"

NED_sub$itemset_type[NED_sub$items == lowest_itemset6_neg]  <- "6 items: low endorsement"
NED_sub$itemset_type[NED_sub$items == highest_itemset6_neg] <- "6 items: high endorsement"


NED_sub$itemset_type[NED_sub$items == lowest_itemset8_neg]  <- "8 items: low endorsement"
NED_sub$itemset_type[NED_sub$items == highest_itemset8_neg] <- "8 items: high endorsement"


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


estimProbPos <- do.call(
  data.frame,
  aggregate(estimationProbPos_raw ~ occasions_drawn + n_occasions + itemset_type, data = NED_sub, FUN = function(x) {
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
estimProbPos
names(estimProbPos) <- c("occasions_drawn", "n_occasions", "itemset_type", "estimProbPos_raw_min", "estimProbPos_raw_mean", "estimProbPos_raw_max")



estimProbNeg <- do.call(
  data.frame,
  aggregate(estimationProbNeg_raw ~ occasions_drawn + n_occasions + itemset_type, data = NED_sub, FUN = function(x) {
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
estimProbNeg
names(estimProbNeg) <- c("occasions_drawn", "n_occasions", "itemset_type", "estimProbNeg_raw_min", "estimProbNeg_raw_mean", "estimProbNeg_raw_max")






agg_res_ned <- list(cor = cor,
                    cor.z = cor.z,
                    sd = sd,
                    sd.z = sd.z,
                    rel = rel,
                    percnegICC = percnegICC,
                    person_diff_agg = person_diff_agg,
                    person_diff_agg.z = person_diff_agg.z,
                    RMSE = RMSE,
                    RMSE.z = RMSE.z,
                    estimProbPos = estimProbPos,
                    estimProbNeg = estimProbNeg)

save(agg_res_ned, file = "results/02_revision_1/EMOTIONS study/NED/main/processed/extreme_item_sets_agg_res_NED_EMOTIONS_Study.rda")

rm(cor, cor.z, sd, sd.z, rel, percnegICC, person_diff_agg, person_diff_agg.z, RMSE, RMSE.z, estimProbPos, estimProbNeg,
   NED, NED_sub)


# now do the same for PED


# '' PED ------------------------------------------------------------------
# PED
load("results/02_revision_1/EMOTIONS study/PED/main/raw/sim_results_PED_EMOTIONS_Study.rda")
PED <- res
rm(res)


names(PED)

# subset
# use only highest and lowest intensity item set for 3 and 4 items
# only use random draws (main analysis; ordered draws are sensitivity analysis)
PED_sub <- PED[PED$items %in% c(lowest_itemset3_pos,
                                highest_itemset3_pos,
                                lowest_itemset4_pos,
                                highest_itemset4_pos) &
                 PED$occasions_drawn == "random", ]



# add label
PED_sub$itemset_type[PED_sub$items == lowest_itemset3_pos]  <- "3 items: low endorsement"
PED_sub$itemset_type[PED_sub$items == highest_itemset3_pos] <- "3 items: high endorsement"

PED_sub$itemset_type[PED_sub$items == lowest_itemset4_pos]  <- "4 items: low endorsement"
PED_sub$itemset_type[PED_sub$items == highest_itemset4_pos] <- "4 items: high endorsement"


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


estimProbPos <- do.call(
  data.frame,
  aggregate(estimationProbPos_raw ~ occasions_drawn + n_occasions + itemset_type, data = PED_sub, FUN = function(x) {
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
estimProbPos
names(estimProbPos) <- c("occasions_drawn", "n_occasions", "itemset_type", "estimProbPos_raw_min", "estimProbPos_raw_mean", "estimProbPos_raw_max")



estimProbNeg <- do.call(
  data.frame,
  aggregate(estimationProbNeg_raw ~ occasions_drawn + n_occasions + itemset_type, data = PED_sub, FUN = function(x) {
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
estimProbNeg
names(estimProbNeg) <- c("occasions_drawn", "n_occasions", "itemset_type", "estimProbNeg_raw_min", "estimProbNeg_raw_mean", "estimProbNeg_raw_max")



agg_res_PED <- list(cor = cor,
                    cor.z = cor.z,
                    sd = sd,
                    sd.z = sd.z,
                    rel = rel,
                    percnegICC = percnegICC,
                    person_diff_agg = person_diff_agg,
                    person_diff_agg.z = person_diff_agg.z,
                    RMSE = RMSE,
                    RMSE.z = RMSE.z,
                    estimProbPos = estimProbPos,
                    estimProbNeg = estimProbNeg)

save(agg_res_PED, file = "results/02_revision_1/EMOTIONS study/PED/main/processed/extreme_item_sets_agg_res_PED_EMOTIONS_Study.rda")

rm(list=ls())




# Plot Results for Item Sets ----------------------------------------------
load("results/02_revision_1/EMOTIONS study/PED/main/processed/extreme_item_sets_agg_res_PED_EMOTIONS_Study.rda")
load("results/02_revision_1/EMOTIONS study/NED/main/processed/extreme_item_sets_agg_res_NED_EMOTIONS_Study.rda")



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
  
  
  # Define facet variable (e.g., occasions_drawn or NED/PED...)
  if (!is.null(facet_var)) {
    if (!(facet_var %in% names(data))) {
      stop(sprintf("Facet variable '%s' not found in data.", facet_var))
    }
    
    if (!is.null(facet_order)) {
      data[ , facet_var] <- factor(data[, facet_var], levels = facet_order) # order factor variable in order as specified
      facet_formula <- ggh4x::facet_manual(as.formula(paste0("~", facet_var)), design=matrix(seq_along(facet_order), nrow=1, byrow=TRUE), drop=FALSE)
      }
    
   
    
    
    
  }
  
  
  
  # Plot according to split_facet == TRUE or FALSE
  if (split_facets == FALSE) {
    
    p <- base_plot(data)
    
    if (!is.null(facet_var)) {
      p <- p + facet_formula +  force_panelsizes(rows=1, cols=rep(1,length(facet_order)))
    }
    
    return(p)
    
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






# add endorsement variable to agg_res_ned and agg_res_PED for plotting

add_endorsement <- function(x) {
  
  x$endorsement <- NA
  
  x$endorsement[grepl("low endorsement", x$itemset_type)] <- "low endorsement"
  x$endorsement[grepl("high endorsement", x$itemset_type)] <- "high endorsement"
  
  x$endorsement <- factor(
    x$endorsement,
    levels = c("low endorsement", "high endorsement")
  )
  
  x
}

agg_res_ned <- lapply(agg_res_ned, add_endorsement)
agg_res_PED <- lapply(agg_res_PED, add_endorsement)



# now split by item number so we can create separate plots
agg_res_ned_3 <- lapply(agg_res_ned, function(x) {
  x[x$itemset_type %in% c(
    "3 items: low endorsement",
    "3 items: high endorsement"
  ), ]
})

agg_res_ned_6 <- lapply(agg_res_ned, function(x) {
  x[x$itemset_type %in% c(
    "6 items: low endorsement",
    "6 items: high endorsement"
  ), ]
})


agg_res_ned_8 <- lapply(agg_res_ned, function(x) {
  x[x$itemset_type %in% c(
    "8 items: low endorsement",
    "8 items: high endorsement"
  ), ]
})



agg_res_PED_3 <- lapply(agg_res_PED, function(x) {
  x[x$itemset_type %in% c(
    "3 items: low endorsement",
    "3 items: high endorsement"
  ), ]
})


agg_res_PED_4 <- lapply(agg_res_PED, function(x) {
  x[x$itemset_type %in% c(
    "4 items: low endorsement",
    "4 items: high endorsement"
  ), ]
})


# plot outcomes
# data frames already stored in a list
# but use only those that we want to plot
# separately for NED and PED due to different item numbers
# separately for the item numbers
data_list_neg_3 <- list(cor = agg_res_ned_3[["cor"]],
                       cor.z = agg_res_ned_3[["cor.z"]],
                       diff = agg_res_ned_3[["person_diff_agg"]],
                       diff.z = agg_res_ned_3[["person_diff_agg.z"]],
                       rmse = agg_res_ned_3[["RMSE"]],
                       rmse.z = agg_res_ned_3[["RMSE.z"]],
                       sd = agg_res_ned_3[["sd"]],
                       sd.z = agg_res_ned_3[["sd.z"]],
                       rel = agg_res_ned_3[["rel"]],
                       percnegICC = agg_res_ned_3[["percnegICC"]],
                       estimProbPos = agg_res_ned_3[["estimProbPos"]],
                       estimProbNeg = agg_res_ned_3[["estimProbNeg"]])

data_list_neg_6 <- list(cor = agg_res_ned_6[["cor"]],
                       cor.z = agg_res_ned_6[["cor.z"]],
                       diff = agg_res_ned_6[["person_diff_agg"]],
                       diff.z = agg_res_ned_6[["person_diff_agg.z"]],
                       rmse = agg_res_ned_6[["RMSE"]],
                       rmse.z = agg_res_ned_6[["RMSE.z"]],
                       sd = agg_res_ned_6[["sd"]],
                       sd.z = agg_res_ned_6[["sd.z"]],
                       rel = agg_res_ned_6[["rel"]],
                       percnegICC = agg_res_ned_6[["percnegICC"]],
                       estimProbPos = agg_res_ned_6[["estimProbPos"]],
                       estimProbNeg = agg_res_ned_6[["estimProbNeg"]])



data_list_neg_8 <- list(cor = agg_res_ned_8[["cor"]],
                       cor.z = agg_res_ned_8[["cor.z"]],
                       diff = agg_res_ned_8[["person_diff_agg"]],
                       diff.z = agg_res_ned_8[["person_diff_agg.z"]],
                       rmse = agg_res_ned_8[["RMSE"]],
                       rmse.z = agg_res_ned_8[["RMSE.z"]],
                       sd = agg_res_ned_8[["sd"]],
                       sd.z = agg_res_ned_8[["sd.z"]],
                       rel = agg_res_ned_8[["rel"]],
                       percnegICC = agg_res_ned_8[["percnegICC"]],
                       estimProbPos = agg_res_ned_8[["estimProbPos"]],
                       estimProbNeg = agg_res_ned_8[["estimProbNeg"]])



data_list_pos_3 <- list(cor = agg_res_PED_3[["cor"]],
                       cor.z = agg_res_PED_3[["cor.z"]],
                       diff = agg_res_PED_3[["person_diff_agg"]],
                       diff.z = agg_res_PED_3[["person_diff_agg.z"]],
                       rmse = agg_res_PED_3[["RMSE"]],
                       rmse.z = agg_res_PED_3[["RMSE.z"]],
                       sd = agg_res_PED_3[["sd"]],
                       sd.z = agg_res_PED_3[["sd.z"]],
                       rel = agg_res_PED_3[["rel"]],
                       percnegICC = agg_res_PED_3[["percnegICC"]],
                       estimProbPos = agg_res_PED_3[["estimProbPos"]],
                       estimProbNeg = agg_res_PED_3[["estimProbNeg"]])



data_list_pos_4 <- list(cor = agg_res_PED_4[["cor"]],
                       cor.z = agg_res_PED_4[["cor.z"]],
                       diff = agg_res_PED_4[["person_diff_agg"]],
                       diff.z = agg_res_PED_4[["person_diff_agg.z"]],
                       rmse = agg_res_PED_4[["RMSE"]],
                       rmse.z = agg_res_PED_4[["RMSE.z"]],
                       sd = agg_res_PED_4[["sd"]],
                       sd.z = agg_res_PED_4[["sd.z"]],
                       rel = agg_res_PED_4[["rel"]],
                       percnegICC = agg_res_PED_4[["percnegICC"]],
                       estimProbPos = agg_res_PED_4[["estimProbPos"]],
                       estimProbNeg = agg_res_PED_4[["estimProbNeg"]])






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
                "Proportion of Negative ICCs",
                "Estimation Problems (Upper Bound)",
                "Estimation Problems (Lower Bound)") 
names(ylabels) <- names(data_list_neg_3)


# for correlation, reliability, proportions, ... -> use theoretical range (e.g., difference max and min: 1 and -1)
ylim_list <- list(
  c(0, 1), # correlation with benchmark, theoretical range
  c(0, 1), # correlation with benchmark (ICC.z), theoretical range
  c(-1, 1), # difference in ICCs (compared to benchmark), theoretical range
  c(-1.4, 18), # difference in ICCs (compared to benchmark) for ICC.z
  c(0, 1), # RMSE, theoretical lower bound
  c(0, 18), # RMSE (ICC.z)
  c(0, 0.4), # SD of ICCs, theoretical lower bound
  c(0, 3), # SD of ICC.z, theoretical lower bound
  c(0, 1), # Reliability
  c(0, 1), # proportion of negative ICCs
  c(0, 11), # estimation problems upper bound
  c(0, 1) # estimation problems lower bound  
)
names(ylim_list) <- names(data_list_neg_3)




# '''' NED, 3 Items ----

# plot outcomes
plot_list_neg_3 <- lapply(names(data_list_neg_3), function(outcome) {
  df <- data_list_neg_3[[outcome]]
  plot_outcome(df,
               ylabel = ylabels[[outcome]],
               ylims=ylim_list[[outcome]],
               x_breaks = seq(0, 70, 10),
               theme_custom = my_theme,
               dodge_width = 3,
               split_facets = FALSE,
               facet_var = NULL,
               facet_order = NULL)
})

names(plot_list_neg_3) <- names(data_list_neg_3)

plot_list_neg_3[["cor"]]
plot_list_neg_3[["cor.z"]]
plot_list_neg_3[["diff"]]
plot_list_neg_3[["diff.z"]]
plot_list_neg_3[["rmse"]]
plot_list_neg_3[["rmse.z"]]
plot_list_neg_3[["sd"]]
plot_list_neg_3[["sd.z"]]
plot_list_neg_3[["rel"]]
plot_list_neg_3[["percnegICC"]]
plot_list_neg_3[["estimProbPos"]]
plot_list_neg_3[["estimProbNeg"]]



# Combine plots

## for raw ICCS
# adjust the plots a little (e.g., no y-axis lable but title, no x-axis label)
a_neg_3 <- plot_list_neg_3[["cor"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                     plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                     axis.text.y = element_text(hjust=1),
                                     axis.text = element_text(size=10)) + ggtitle("(A) Correlation with Benchmark") +
  geom_hline(yintercept=0.80, linetype="twodash", color = "black") + force_panelsizes(rows=1, cols = c(1,1))
# a
b_neg_3 <- plot_list_neg_3[["diff"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                      plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                      axis.text.y = element_text(hjust=1),
                                      axis.text = element_text(size=10)) + ggtitle("(B) Difference from Benchmark") + force_panelsizes(rows=1, cols = c(1,1))
# b
c_neg_3 <- plot_list_neg_3[["rmse"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                      plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                      axis.text.y = element_text(hjust=1),
                                      axis.text = element_text(size=10)) + ggtitle("(C) RMSE")+ force_panelsizes(rows=1, cols = c(1,1))


# c
d_neg_3 <- plot_list_neg_3[["sd"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                    plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                    axis.text.y = element_text(hjust=1),
                                    axis.text = element_text(size=10)) + ggtitle("(D) SD of ICCs") + force_panelsizes(rows=1, cols = c(1,1))

# d
e_neg_3 <- plot_list_neg_3[["rel"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                     plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                     axis.text.y = element_text(hjust=1),
                                     axis.text = element_text(size=10)) + ggtitle("(E) Reliability of ICCs") +
  geom_hline(yintercept=0.80, linetype="twodash", color = "black")+ force_panelsizes(rows=1, cols = c(1,1))
# e
f_neg_3 <- plot_list_neg_3[["percnegICC"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                            plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                            axis.text.y = element_text(hjust=1),
                                            axis.text = element_text(size=10)) + ggtitle("(F) Proportion of Negative ICCs")+ force_panelsizes(rows=1, cols = c(1,1))


g_neg_3 <- plot_list_neg_3[["estimProbNeg"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                         plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                         axis.text.y = element_text(hjust=1),
                                         axis.text = element_text(size=10)) + ggtitle("(G) Estimation Problems (LB)") +
  force_panelsizes(rows=1, cols = c(1,1))



h_neg_3 <- plot_list_neg_3[["estimProbPos"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                         plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                         axis.text.y = element_text(hjust=1),
                                         axis.text = element_text(size=10)) + ggtitle("(H) Estimation Problems (UB)") +
  force_panelsizes(rows=1, cols = c(1,1))






combined_neg_3 <- ggpubr::ggarrange(a_neg_3,b_neg_3,c_neg_3,d_neg_3,e_neg_3,f_neg_3,g_neg_3,h_neg_3 , ncol=3, nrow=3, common.legend = TRUE, legend="top",
                                align = "hv", widths = c(1,1,1), heights = c(1,1,1)) # equal panel sizes

combined_neg_3 <- annotate_figure(combined_neg_3,
                              bottom = text_grob("Number of Measurement Occasions", size = 12))


combined_neg_3


ggsave("plots/02_revision_1/for publication/EMOTIONS_item_set_specific_plots_3_items_random_draws_NED.pdf",plot = combined_neg_3, device="pdf", height = 222, width = 222, unit="mm")

ggsave("plots/02_revision_1/for publication/EMOTIONS_item_set_specific_plots_3_items_random_draws_NED.svg",plot = combined_neg_3, device="svg", height = 222, width = 222, unit="mm")

# as tiff
tiff("plots/02_revision_1/for publication/EMOTIONS_item_set_specific_plots_3_items_random_draws_NED.tiff", units="mm", width=222, height=222, res=1200)
combined_neg_3
dev.off()



# '''' NED, 6 Items ----

# plot outcomes
plot_list_neg_6 <- lapply(names(data_list_neg_6), function(outcome) {
  df <- data_list_neg_6[[outcome]]
  plot_outcome(df,
               ylabel = ylabels[[outcome]],
               ylims=ylim_list[[outcome]],
               x_breaks = seq(0, 70, 10),
               theme_custom = my_theme,
               dodge_width = 3,
               split_facets = FALSE,
               facet_var = NULL,
               facet_order = NULL)
})

names(plot_list_neg_6) <- names(data_list_neg_6)

plot_list_neg_6[["cor"]]
plot_list_neg_6[["cor.z"]]
plot_list_neg_6[["diff"]]
plot_list_neg_6[["diff.z"]]
plot_list_neg_6[["rmse"]]
plot_list_neg_6[["rmse.z"]]
plot_list_neg_6[["sd"]]
plot_list_neg_6[["sd.z"]]
plot_list_neg_6[["rel"]]
plot_list_neg_6[["percnegICC"]]
plot_list_neg_6[["estimProbPos"]]
plot_list_neg_6[["estimProbNeg"]]



# Combine plots

## for raw ICCS
# adjust the plots a little (e.g., no y-axis lable but title, no x-axis label)
a_neg_6 <- plot_list_neg_6[["cor"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                            plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                            axis.text.y = element_text(hjust=1),
                                            axis.text = element_text(size=10)) + ggtitle("(A) Correlation with Benchmark") +
  geom_hline(yintercept=0.80, linetype="twodash", color = "black") + force_panelsizes(rows=1, cols = c(1,1))
# a
b_neg_6 <- plot_list_neg_6[["diff"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                             plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                             axis.text.y = element_text(hjust=1),
                                             axis.text = element_text(size=10)) + ggtitle("(B) Difference from Benchmark") + force_panelsizes(rows=1, cols = c(1,1))
# b
c_neg_6 <- plot_list_neg_6[["rmse"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                             plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                             axis.text.y = element_text(hjust=1),
                                             axis.text = element_text(size=10)) + ggtitle("(C) RMSE")+ force_panelsizes(rows=1, cols = c(1,1))


# c
d_neg_6 <- plot_list_neg_6[["sd"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                           plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                           axis.text.y = element_text(hjust=1),
                                           axis.text = element_text(size=10)) + ggtitle("(D) SD of ICCs") + force_panelsizes(rows=1, cols = c(1,1))

# d
e_neg_6 <- plot_list_neg_6[["rel"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                            plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                            axis.text.y = element_text(hjust=1),
                                            axis.text = element_text(size=10)) + ggtitle("(E) Reliability of ICCs") +
  geom_hline(yintercept=0.80, linetype="twodash", color = "black")+ force_panelsizes(rows=1, cols = c(1,1))
# e
f_neg_6 <- plot_list_neg_6[["percnegICC"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                                   plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                                   axis.text.y = element_text(hjust=1),
                                                   axis.text = element_text(size=10)) + ggtitle("(F) Proportion of Negative ICCs")+ force_panelsizes(rows=1, cols = c(1,1))


g_neg_6 <- plot_list_neg_6[["estimProbNeg"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                                     plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                                     axis.text.y = element_text(hjust=1),
                                                     axis.text = element_text(size=10)) + ggtitle("(G) Estimation Problems (LB)") +
  force_panelsizes(rows=1, cols = c(1,1))



h_neg_6 <- plot_list_neg_6[["estimProbPos"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                                     plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                                     axis.text.y = element_text(hjust=1),
                                                     axis.text = element_text(size=10)) + ggtitle("(H) Estimation Problems (UB)") +
  force_panelsizes(rows=1, cols = c(1,1))






combined_neg_6 <- ggpubr::ggarrange(a_neg_6,b_neg_6,c_neg_6,d_neg_6,e_neg_6,f_neg_6,g_neg_6,h_neg_6 , ncol=3, nrow=3, common.legend = TRUE, legend="top",
                                    align = "hv", widths = c(1,1,1), heights = c(1,1,1)) # equal panel sizes

combined_neg_6 <- annotate_figure(combined_neg_6,
                                  bottom = text_grob("Number of Measurement Occasions", size = 12))


combined_neg_6


ggsave("plots/02_revision_1/for publication/EMOTIONS_item_set_specific_plots_6_items_random_draws_NED.pdf",plot = combined_neg_6, device="pdf", height = 222, width = 222, unit="mm")

ggsave("plots/02_revision_1/for publication/EMOTIONS_item_set_specific_plots_6_items_random_draws_NED.svg",plot = combined_neg_6, device="svg", height = 222, width = 222, unit="mm")

# as tiff
tiff("plots/02_revision_1/for publication/EMOTIONS_item_set_specific_plots_6_items_random_draws_NED.tiff", units="mm", width=220, height=222, res=1200)
combined_neg_6
dev.off()




# '''' NED, 8 Items ----

# plot outcomes
plot_list_neg_8 <- lapply(names(data_list_neg_8), function(outcome) {
  df <- data_list_neg_8[[outcome]]
  plot_outcome(df,
               ylabel = ylabels[[outcome]],
               ylims=ylim_list[[outcome]],
               x_breaks = seq(0, 70, 10),
               theme_custom = my_theme,
               dodge_width = 3,
               split_facets = FALSE,
               facet_var = NULL,
               facet_order = NULL)
})

names(plot_list_neg_8) <- names(data_list_neg_8)

plot_list_neg_8[["cor"]]
plot_list_neg_8[["cor.z"]]
plot_list_neg_8[["diff"]]
plot_list_neg_8[["diff.z"]]
plot_list_neg_8[["rmse"]]
plot_list_neg_8[["rmse.z"]]
plot_list_neg_8[["sd"]]
plot_list_neg_8[["sd.z"]]
plot_list_neg_8[["rel"]]
plot_list_neg_8[["percnegICC"]]
plot_list_neg_8[["estimProbPos"]]
plot_list_neg_8[["estimProbNeg"]]



# Combine plots

## for raw ICCS
# adjust the plots a little (e.g., no y-axis lable but title, no x-axis label)
a_neg_8 <- plot_list_neg_8[["cor"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                            plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                            axis.text.y = element_text(hjust=1),
                                            axis.text = element_text(size=10)) + ggtitle("(A) Correlation with Benchmark") +
  geom_hline(yintercept=0.80, linetype="twodash", color = "black") + force_panelsizes(rows=1, cols = c(1,1))
# a
b_neg_8 <- plot_list_neg_8[["diff"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                             plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                             axis.text.y = element_text(hjust=1),
                                             axis.text = element_text(size=10)) + ggtitle("(B) Difference from Benchmark") + force_panelsizes(rows=1, cols = c(1,1))
# b
c_neg_8 <- plot_list_neg_8[["rmse"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                             plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                             axis.text.y = element_text(hjust=1),
                                             axis.text = element_text(size=10)) + ggtitle("(C) RMSE")+ force_panelsizes(rows=1, cols = c(1,1))


# c
d_neg_8 <- plot_list_neg_8[["sd"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                           plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                           axis.text.y = element_text(hjust=1),
                                           axis.text = element_text(size=10)) + ggtitle("(D) SD of ICCs") + force_panelsizes(rows=1, cols = c(1,1))

# d
e_neg_8 <- plot_list_neg_8[["rel"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                            plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                            axis.text.y = element_text(hjust=1),
                                            axis.text = element_text(size=10)) + ggtitle("(E) Reliability of ICCs") +
  geom_hline(yintercept=0.80, linetype="twodash", color = "black")+ force_panelsizes(rows=1, cols = c(1,1))
# e
f_neg_8 <- plot_list_neg_8[["percnegICC"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                                   plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                                   axis.text.y = element_text(hjust=1),
                                                   axis.text = element_text(size=10)) + ggtitle("(F) Proportion of Negative ICCs")+ force_panelsizes(rows=1, cols = c(1,1))


g_neg_8 <- plot_list_neg_8[["estimProbNeg"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                                     plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                                     axis.text.y = element_text(hjust=1),
                                                     axis.text = element_text(size=10)) + ggtitle("(G) Estimation Problems (LB)") +
  force_panelsizes(rows=1, cols = c(1,1))



h_neg_8 <- plot_list_neg_8[["estimProbPos"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                                     plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                                     axis.text.y = element_text(hjust=1),
                                                     axis.text = element_text(size=10)) + ggtitle("(H) Estimation Problems (UB)") +
  force_panelsizes(rows=1, cols = c(1,1))






combined_neg_8 <- ggpubr::ggarrange(a_neg_8,b_neg_8,c_neg_8,d_neg_8,e_neg_8,f_neg_8,g_neg_8,h_neg_8 , ncol=3, nrow=3, common.legend = TRUE, legend="top",
                                    align = "hv", widths = c(1,1,1), heights = c(1,1,1)) # equal panel sizes

combined_neg_8 <- annotate_figure(combined_neg_8,
                                  bottom = text_grob("Number of Measurement Occasions", size = 12))


combined_neg_8


ggsave("plots/02_revision_1/for publication/EMOTIONS_item_set_specific_plots_8_items_random_draws_NED.pdf",plot = combined_neg_8, device="pdf", height = 222, width = 222, unit="mm")

ggsave("plots/02_revision_1/for publication/EMOTIONS_item_set_specific_plots_8_items_random_draws_NED.svg",plot = combined_neg_8, device="svg", height = 222, width = 222, unit="mm")

# as tiff
tiff("plots/02_revision_1/for publication/EMOTIONS_item_set_specific_plots_8_items_random_draws_NED.tiff", units="mm", width=220, height=222, res=1200)
combined_neg_8
dev.off()



# '''' PED, 3 Items ----

# plot outcomes
plot_list_pos_3 <- lapply(names(data_list_pos_3), function(outcome) {
  df <- data_list_pos_3[[outcome]]
  plot_outcome(df,
               ylabel = ylabels[[outcome]],
               ylims=ylim_list[[outcome]],
               x_breaks = seq(0, 70, 10),
               theme_custom = my_theme,
               dodge_width = 3,
               split_facets = FALSE,
               facet_var = NULL,
               facet_order = NULL)
})

names(plot_list_pos_3) <- names(data_list_pos_3)

plot_list_pos_3[["cor"]]
plot_list_pos_3[["cor.z"]]
plot_list_pos_3[["diff"]]
plot_list_pos_3[["diff.z"]]
plot_list_pos_3[["rmse"]]
plot_list_pos_3[["rmse.z"]]
plot_list_pos_3[["sd"]]
plot_list_pos_3[["sd.z"]]
plot_list_pos_3[["rel"]]
plot_list_pos_3[["percnegICC"]]
plot_list_pos_3[["estimProbPos"]]
plot_list_pos_3[["estimProbNeg"]]



# Combine plots

## for raw ICCS
# adjust the plots a little (e.g., no y-axis lable but title, no x-axis label)
a_pos_3 <- plot_list_pos_3[["cor"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                            plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                            axis.text.y = element_text(hjust=1),
                                            axis.text = element_text(size=10)) + ggtitle("(A) Correlation with Benchmark") +
  geom_hline(yintercept=0.80, linetype="twodash", color = "black") + force_panelsizes(rows=1, cols = c(1,1))
# a
b_pos_3 <- plot_list_pos_3[["diff"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                             plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                             axis.text.y = element_text(hjust=1),
                                             axis.text = element_text(size=10)) + ggtitle("(B) Difference from Benchmark") + force_panelsizes(rows=1, cols = c(1,1))
# b
c_pos_3 <- plot_list_pos_3[["rmse"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                             plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                             axis.text.y = element_text(hjust=1),
                                             axis.text = element_text(size=10)) + ggtitle("(C) RMSE")+ force_panelsizes(rows=1, cols = c(1,1))


# c
d_pos_3 <- plot_list_pos_3[["sd"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                           plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                           axis.text.y = element_text(hjust=1),
                                           axis.text = element_text(size=10)) + ggtitle("(D) SD of ICCs") + force_panelsizes(rows=1, cols = c(1,1))

# d
e_pos_3 <- plot_list_pos_3[["rel"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                            plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                            axis.text.y = element_text(hjust=1),
                                            axis.text = element_text(size=10)) + ggtitle("(E) Reliability of ICCs") +
  geom_hline(yintercept=0.80, linetype="twodash", color = "black")+ force_panelsizes(rows=1, cols = c(1,1))
# e
f_pos_3 <- plot_list_pos_3[["percnegICC"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                                   plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                                   axis.text.y = element_text(hjust=1),
                                                   axis.text = element_text(size=10)) + ggtitle("(F) Proportion of Negative ICCs")+ force_panelsizes(rows=1, cols = c(1,1))


g_pos_3 <- plot_list_pos_3[["estimProbNeg"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                                     plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                                     axis.text.y = element_text(hjust=1),
                                                     axis.text = element_text(size=10)) + ggtitle("(G) Estimation Problems (LB)") +
  force_panelsizes(rows=1, cols = c(1,1))



h_pos_3 <- plot_list_pos_3[["estimProbPos"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                                     plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                                     axis.text.y = element_text(hjust=1),
                                                     axis.text = element_text(size=10)) + ggtitle("(H) Estimation Problems (UB)") +
  force_panelsizes(rows=1, cols = c(1,1))






combined_pos_3 <- ggpubr::ggarrange(a_pos_3,b_pos_3,c_pos_3,d_pos_3,e_pos_3,f_pos_3,g_pos_3,h_pos_3 , ncol=3, nrow=3, common.legend = TRUE, legend="top",
                                    align = "hv", widths = c(1,1,1), heights = c(1,1,1)) # equal panel sizes

combined_pos_3 <- annotate_figure(combined_pos_3,
                                  bottom = text_grob("Number of Measurement Occasions", size = 12))


combined_pos_3


ggsave("plots/02_revision_1/for publication/EMOTIONS_item_set_specific_plots_3_items_random_draws_PED.pdf",plot = combined_pos_3, device="pdf", height = 222, width = 222, unit="mm")

ggsave("plots/02_revision_1/for publication/EMOTIONS_item_set_specific_plots_3_items_random_draws_PED.svg",plot = combined_pos_3, device="svg", height = 222, width = 222, unit="mm")

# as tiff
tiff("plots/02_revision_1/for publication/EMOTIONS_item_set_specific_plots_3_items_random_draws_PED.tiff", units="mm", width=220, height=222, res=1200)
combined_pos_3
dev.off()



# '''' PED, 4 Items ----

# plot outcomes
plot_list_pos_4 <- lapply(names(data_list_pos_4), function(outcome) {
  df <- data_list_pos_4[[outcome]]
  plot_outcome(df,
               ylabel = ylabels[[outcome]],
               ylims=ylim_list[[outcome]],
               x_breaks = seq(0, 70, 10),
               theme_custom = my_theme,
               dodge_width = 3,
               split_facets = FALSE,
               facet_var = NULL,
               facet_order = NULL)
})

names(plot_list_pos_4) <- names(data_list_pos_4)

plot_list_pos_4[["cor"]]
plot_list_pos_4[["cor.z"]]
plot_list_pos_4[["diff"]]
plot_list_pos_4[["diff.z"]]
plot_list_pos_4[["rmse"]]
plot_list_pos_4[["rmse.z"]]
plot_list_pos_4[["sd"]]
plot_list_pos_4[["sd.z"]]
plot_list_pos_4[["rel"]]
plot_list_pos_4[["percnegICC"]]
plot_list_pos_4[["estimProbPos"]]
plot_list_pos_4[["estimProbNeg"]]



# Combine plots

## for raw ICCS
# adjust the plots a little (e.g., no y-axis lable but title, no x-axis label)
a_pos_4 <- plot_list_pos_4[["cor"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                            plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                            axis.text.y = element_text(hjust=1),
                                            axis.text = element_text(size=10)) + ggtitle("(A) Correlation with Benchmark") +
  geom_hline(yintercept=0.80, linetype="twodash", color = "black") + force_panelsizes(rows=1, cols = c(1,1))
# a
b_pos_4 <- plot_list_pos_4[["diff"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                             plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                             axis.text.y = element_text(hjust=1),
                                             axis.text = element_text(size=10)) + ggtitle("(B) Difference from Benchmark") + force_panelsizes(rows=1, cols = c(1,1))
# b
c_pos_4 <- plot_list_pos_4[["rmse"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                             plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                             axis.text.y = element_text(hjust=1),
                                             axis.text = element_text(size=10)) + ggtitle("(C) RMSE")+ force_panelsizes(rows=1, cols = c(1,1))


# c
d_pos_4 <- plot_list_pos_4[["sd"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                           plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                           axis.text.y = element_text(hjust=1),
                                           axis.text = element_text(size=10)) + ggtitle("(D) SD of ICCs") + force_panelsizes(rows=1, cols = c(1,1))

# d
e_pos_4 <- plot_list_pos_4[["rel"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                            plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                            axis.text.y = element_text(hjust=1),
                                            axis.text = element_text(size=10)) + ggtitle("(E) Reliability of ICCs") +
  geom_hline(yintercept=0.80, linetype="twodash", color = "black")+ force_panelsizes(rows=1, cols = c(1,1))
# e
f_pos_4 <- plot_list_pos_4[["percnegICC"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                                   plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                                   axis.text.y = element_text(hjust=1),
                                                   axis.text = element_text(size=10)) + ggtitle("(F) Proportion of Negative ICCs")+ force_panelsizes(rows=1, cols = c(1,1))


g_pos_4 <- plot_list_pos_4[["estimProbNeg"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                                     plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                                     axis.text.y = element_text(hjust=1),
                                                     axis.text = element_text(size=10)) + ggtitle("(G) Estimation Problems (LB)") +
  force_panelsizes(rows=1, cols = c(1,1))



h_pos_4 <- plot_list_pos_4[["estimProbPos"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                                     plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                                     axis.text.y = element_text(hjust=1),
                                                     axis.text = element_text(size=10)) + ggtitle("(H) Estimation Problems (UB)") +
  force_panelsizes(rows=1, cols = c(1,1))






combined_pos_4 <- ggpubr::ggarrange(a_pos_4,b_pos_4,c_pos_4,d_pos_4,e_pos_4,f_pos_4,g_pos_4,h_pos_4 , ncol=3, nrow=3, common.legend = TRUE, legend="top",
                                    align = "hv", widths = c(1,1,1), heights = c(1,1,1)) # equal panel sizes

combined_pos_4 <- annotate_figure(combined_pos_4,
                                  bottom = text_grob("Number of Measurement Occasions", size = 12))


combined_pos_4


ggsave("plots/02_revision_1/for publication/EMOTIONS_item_set_specific_plots_4_items_random_draws_PED.pdf",plot = combined_pos_4, device="pdf", height = 222, width = 222, unit="mm")

ggsave("plots/02_revision_1/for publication/EMOTIONS_item_set_specific_plots_4_items_random_draws_PED.svg",plot = combined_pos_4, device="svg", height = 222, width = 222, unit="mm")

# as tiff
tiff("plots/02_revision_1/for publication/EMOTIONS_item_set_specific_plots_4_items_random_draws_PED.tiff", units="mm", width=220, height=222, res=1200)
combined_pos_4
dev.off()





# '' Make Results Table ---------------------------------------------------

# NED
# remove endorsement variable before joining results into a table
agg_res_ned_join <- lapply(
  agg_res_ned,
  function(x) {
    x[ , !names(x) %in% "endorsement"]
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

results_ned <- merge(results_ned, agg_res_ned_join[["estimProbNeg"]],
                     by = c("occasions_drawn", "n_occasions", "itemset_type"))

results_ned <- merge(results_ned, agg_res_ned_join[["estimProbPos"]],
                     by = c("occasions_drawn", "n_occasions", "itemset_type"))


results_ned

# order by factor:
# order factor
results_ned$itemset_type <- factor(
  results_ned$itemset_type,
  levels = c(
    "3 items: low endorsement",
    "3 items: high endorsement",
    "6 items: low endorsement",
    "6 items: high endorsement",
    "8 items: low endorsement",
    "8 items: high endorsement"
  )
)

results_ned <- results_ned[order(results_ned$occasions_drawn, results_ned$itemset_type, results_ned$n_occasions), ]
# round
results_ned[ , 4:30] <- round(results_ned[ , 4:30], 3)


write.csv(results_ned,  
          "results/02_revision_1/EMOTIONS study/NED/main/processed/extreme_item_sets_results_table_NED_EMOTIONS.csv",
          row.names = FALSE)


# PED
# remove endorsement variable before joining results into a table
agg_res_PED_join <- lapply(
  agg_res_PED,
  function(x) {
    x[ , !names(x) %in% "endorsement"]
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

results_PED <- merge(results_PED, agg_res_PED_join[["estimProbNeg"]],
                     by = c("occasions_drawn", "n_occasions", "itemset_type"))

results_PED <- merge(results_PED, agg_res_PED_join[["estimProbPos"]],
                     by = c("occasions_drawn", "n_occasions", "itemset_type"))


results_PED

# order by factor:
# order factor
results_PED$itemset_type <- factor(
  results_PED$itemset_type,
  levels = c(
    "3 items: low endorsement",
    "3 items: high endorsement",
    "4 items: low endorsement",
    "4 items: high endorsement"
  )
)

results_PED <- results_PED[order(results_PED$occasions_drawn, results_PED$itemset_type, results_PED$n_occasions), ]
# round
results_PED[ , 4:30] <- round(results_PED[ , 4:30], 3)


write.csv(results_PED,  
          "results/02_revision_1/EMOTIONS study/PED/main/processed/extreme_item_sets_results_table_PED_EMOTIONS.csv",
          row.names = FALSE)
