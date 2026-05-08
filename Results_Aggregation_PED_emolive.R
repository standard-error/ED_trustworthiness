###################################################################
#####      Estimating trait emotion differentiation:          #####
#####         How many measurement occasions and              #####
#####               emotion items are needed?                 #####
###################################################################

###################################################################
#####                Results Aggregation                      #####
###################################################################


###################################################################
#####             Positive Emotion Differentiation            #####
###################################################################


###################################################################
#####                     emolive Data                        #####
###################################################################




# Load Simulation Results -------------------------------------------------
load("results/02_revision_1/sim_results_PED_emolive_Study.rda")


# Check Missings ----------------------------------------------------------
any(is.na(res))



# Source Function ---------------------------------------------------------
source("functions/function_aggregate_results.R")



# Overview ----------------------------------------------------------------
names(res)


# Determine Total Number of Redraws ---------------------------------------
# not saved with other results because it is not treated as evaluation criterion
# -> information for Method section
sum(res$total_redraws)

table(res$total_redraws)

redraw <- aggregate(total_redraws ~ occasions_drawn + n_occasions + n_items,
                    data=res, FUN = sum)
redraw

rm(redraw)




# Calculate %negICC -------------------------------------------------------
# calculate proportion of negative ICCs
# -> relative to number of calculated ICCs (i.e., not ALL participants,
# but participants for whom ICC was calculated at all [no zero variances]
# and before handling of negative ICCs)
res$percnegICC_raw <- res$negICC_raw / res$N_merged_ICC_raw
# divide by number of participants for whom a raw ICC was calculated (before handling of negative ICCs)




# Extract Person-Level ICC Estimates --------------------------------------
# '' For ICCs -------------------------------------------------------------
# extract person-level differences for each replication and each condition per participant
person_level_ICC_estimates <- data.frame(matrix(nrow=nrow(res), ncol=max(res$n_total_persons)+4)) # + 4 for simulation conditions
person_level_ICC_estimates [ , 1:4] <- res[ , c(1,3:5)] # extract simulation conditions (incl. design_row_id)
names(person_level_ICC_estimates ) <- c("design_row_id", "n_occasions", "occasions_drawn", "n_items", paste0("person_ICC_", 1:max(res$n_total_persons)))

ICC_matrix <- do.call(rbind, res$person_estimates_ICC) # extract the N person_estimates_ICC values per row (replication) and bind them
# -> matrix of N participants (columns) and their values in each replication (rows)

# bind with part_dat
person_level_ICC_estimates [ , 5:(max(res$n_total_persons)+4)] <- ICC_matrix


# save
save(person_level_ICC_estimates , file="results/02_revision_1/person_level_ICC_per_replication_PED_emolive_Study.rda")



# '' For ICC.z ------------------------------------------------------------
# extract person-level differences for each replication and each condition per participant
person_level_ICC.z_estimates  <- data.frame(matrix(nrow=nrow(res), ncol=max(res$n_total_persons)+4))
person_level_ICC.z_estimates [ , 1:4] <- res[ , c(1,3:5)]
names(person_level_ICC.z_estimates ) <- c("design_row_id", "n_occasions", "occasions_drawn", "n_items", paste0("person_ICC.z_", 1:max(res$n_total_persons)))

ICC.z_matrix <- do.call(rbind, res$person_estimates_ICC.z) # extract the N person_estimates_ICC.z values per row (replication) and bind them
# -> matrix of N participants (columns) and their values in each replication (rows)

# bind with part_dat
person_level_ICC.z_estimates [ , 5:(max(res$n_total_persons)+4)] <- ICC.z_matrix


# save
save(person_level_ICC.z_estimates , file="results/02_revision_1/person_level_ICC.z_per_replication_PED_emolive_Study.rda")








# Calculate Person-Level Deviation ("Bias") Across Replications -----------


# '' For ICCs -------------------------------------------------------------
# extract person-level differences for each replication and each condition per participant
person_level_diff <- data.frame(matrix(nrow=nrow(res), ncol=max(res$n_total_persons)+4))
person_level_diff[ , 1:4] <- res[ , c(1,3:5)]
names(person_level_diff) <- c("design_row_id", "n_occasions", "occasions_drawn", "n_items", paste0("person_diff_ICC_", 1:max(res$n_total_persons)))

diff_matrix <- do.call(rbind, res$person_diff_ICC) # extract the N person_diff_ICC values per row (replication) and bind them
# -> matrix of N participants (columns) and their values in each replication (rows)

# bind with part_dat
person_level_diff[ , 5:(max(res$n_total_persons)+4)] <- diff_matrix


# save
save(person_level_diff, file="results/02_revision_1/person_level_difference_per_replication_PED_emolive_Study.rda")




# '' For ICC.z ------------------------------------------------------------
# extract person-level differences for each replication and each condition per participant
person_level_diff.z <- data.frame(matrix(nrow=nrow(res), ncol=max(res$n_total_persons)+4))
person_level_diff.z[ , 1:4] <- res[ , c(1,3:5)]
names(person_level_diff.z) <- c("design_row_id", "n_occasions", "occasions_drawn", "n_items", paste0("person_diff_ICC.z_", 1:max(res$n_total_persons)))

diff_matrix.z <- do.call(rbind, res$person_diff_ICC.z) # extract the N person_diff_ICC.z values per row (replication) and bind them
# -> matrix of N participants (columns) and their values in each replication (rows)

# bind with part_dat
person_level_diff.z[ , 5:(max(res$n_total_persons)+4)] <- diff_matrix.z


# save
save(person_level_diff.z, file="results/02_revision_1/person_level_difference.z_per_replication.rda")




# '' Aggegrate Across Replications ----------------------------------------

## for ICC
vars <- paste0("person_diff_ICC_", 1:max(res$n_total_persons))
person_diff_agg <- aggregate(person_level_diff[ , vars],
                             by = person_level_diff[ , c("occasions_drawn", "n_occasions", "n_items")],
                             FUN = function(x) {
                               if (all(is.na(x))) {
                                 NA_real_ # if person does not have ANY valid value -> return NA
                               } else {
                                 mean(x, na.rm=TRUE) # calculate mean, but remove NAs
                               }
                             }) 

names(person_diff_agg) <- c("occasions_drawn", "n_occasions", "n_items", paste0("person_difference_", 1:max(res$n_total_persons)))

# save
save(person_diff_agg, file="results/02_revision_1/person_level_difference_aggregated_all_participants_PED_emolive_Study.rda")


# plot
# reshape data
library(tidyverse)
long <- person_diff_agg %>%
  pivot_longer(
    cols = starts_with("person_difference_"),   
    names_to = "participant",
    values_to = "person_difference"
  )



ggplot(long, aes(x = n_occasions, y = person_difference, group = participant)) +
  geom_line(alpha = 0.3, aes(col=participant)) +
  theme_minimal() + 
  facet_grid(rows=vars(n_items), cols=vars(occasions_drawn)) +
  theme(legend.position = "none")


# calculate overall difference (mean difference across participants)
person_diff_agg$difference_mean <- rowMeans(person_diff_agg[ , 4:(max(res$n_total_persons)+3)])
person_diff_agg$difference_median <- apply(person_diff_agg[ ,4:(max(res$n_total_persons)+3)], 1, median, na.rm=T) 

# View(person_diff_agg[ , c("occasions_drawn", "n_occasions", "n_items", "difference_mean")])

ggplot(person_diff_agg, aes(x = n_occasions, y = difference_mean, group = n_items)) +
  geom_line(aes(col=n_items)) +
  theme_minimal() + 
  facet_wrap(vars(occasions_drawn)) +
  theme(legend.position = "none")

ggplot(person_diff_agg, aes(x = n_occasions, y = difference_median, group = n_items)) +
  geom_line(aes(col=n_items)) +
  theme_minimal() + 
  facet_wrap(vars(occasions_drawn)) +
  theme(legend.position = "none")


### for ICC.z
vars <- paste0("person_diff_ICC.z_", 1:max(res$n_total_persons))
person_diff_agg.z <- aggregate(person_level_diff.z[ , vars],
                             by = person_level_diff.z[ , c("occasions_drawn", "n_occasions", "n_items")],
                             FUN = function(x) {
                               if (all(is.na(x))) {
                                 NA_real_ # if person does not have ANY valid value -> return NA
                               } else {
                                 mean(x, na.rm=TRUE) # calculate mean, but remove NAs
                               }
                             }) 

names(person_diff_agg.z) <- c("occasions_drawn", "n_occasions", "n_items", paste0("person_difference.z_", 1:max(res$n_total_persons)))

# save
save(person_diff_agg.z, file="results/02_revision_1/person_level_difference.z_aggregated_all_participants_PED_emolive_Study.rda")


# plot
# reshape data
library(tidyverse)
long <- person_diff_agg.z %>%
  pivot_longer(
    cols = starts_with("person_difference.z_"),   
    names_to = "participant",
    values_to = "person_difference.z"
  )



ggplot(long, aes(x = n_occasions, y = person_difference.z, group = participant)) +
  geom_line(alpha = 0.3, aes(col=participant)) +
  theme_minimal() + 
  facet_grid(rows=vars(n_items), cols=vars(occasions_drawn)) +
  theme(legend.position = "none")


# calculate overall difference (mean difference across participants)
person_diff_agg.z$difference.z_mean <- rowMeans(person_diff_agg.z[ , 4:(max(res$n_total_persons)+3)])
person_diff_agg.z$difference.z_median <- apply(person_diff_agg.z[ ,4:(max(res$n_total_persons)+3)], 1, median, na.rm=T) 


ggplot(person_diff_agg.z, aes(x = n_occasions, y = difference.z_mean, group = n_items)) +
  geom_line(aes(col=n_items)) +
  theme_minimal() + 
  facet_wrap(vars(occasions_drawn)) +
  theme(legend.position = "none")

ggplot(person_diff_agg.z, aes(x = n_occasions, y = difference.z_median, group = n_items)) +
  geom_line(aes(col=n_items)) +
  theme_minimal() + 
  facet_wrap(vars(occasions_drawn)) +
  theme(legend.position = "none")






# '' Aggregate Across Participants (for Plotting) -------------------------
## for ICC
# calculate overall difference (mean difference across participants)
person_diff_agg$difference_mean <- rowMeans(person_diff_agg[ , 4:(max(res$n_total_persons)+3)])
person_diff_agg$difference_median <- apply(person_diff_agg[ ,4:(max(res$n_total_persons)+3)], 1, median, na.rm=T) 

# determine which participant deviates most
person_diff_agg$difference_min_id <- apply(person_diff_agg[ ,4:(max(res$n_total_persons)+3)], 1, which.min)
person_diff_agg$difference_max_id <- apply(person_diff_agg[ ,4:(max(res$n_total_persons)+3)], 1, which.max)

# calculate min and max
person_diff_agg$difference_min <- apply(person_diff_agg[ ,4:(max(res$n_total_persons)+3)], 1, min, na.rm=T)
person_diff_agg$difference_max <- apply(person_diff_agg[ ,4:(max(res$n_total_persons)+3)], 1, max, na.rm=T)


person_diff_agg <- person_diff_agg[ , c("occasions_drawn", "n_occasions", "n_items",
                                        "difference_mean", "difference_median", "difference_min_id",
                                        "difference_max_id", "difference_min", "difference_max")]


## for ICC.z
# calculate overall difference (mean difference across participants)
person_diff_agg.z$difference.z_mean <- rowMeans(person_diff_agg.z[ , 4:(max(res$n_total_persons)+3)])
person_diff_agg.z$difference.z_median <- apply(person_diff_agg.z[ ,4:(max(res$n_total_persons)+3)], 1, median, na.rm=T) 

# determine which participant deviates most
person_diff_agg.z$difference.z_min_id <- apply(person_diff_agg.z[ ,4:(max(res$n_total_persons)+3)], 1, which.min)
person_diff_agg.z$difference.z_max_id <- apply(person_diff_agg.z[ ,4:(max(res$n_total_persons)+3)], 1, which.max)

# calculate min and max
person_diff_agg.z$difference.z_min <- apply(person_diff_agg.z[ ,4:(max(res$n_total_persons)+3)], 1, min, na.rm=T)
person_diff_agg.z$difference.z_max <- apply(person_diff_agg.z[ ,4:(max(res$n_total_persons)+3)], 1, max, na.rm=T)



person_diff_agg.z <- person_diff_agg.z[ , c("occasions_drawn", "n_occasions", "n_items",
                                            "difference.z_mean", "difference.z_median", "difference.z_min_id",
                                            "difference.z_max_id", "difference.z_min", "difference.z_max")]



# Calcute RMSE for Each Participant Across Replications -------------------


# '' For ICCs -------------------------------------------------------------
# use person-level differences -> square
person_level_diff_sq <- person_level_diff[ ,1:4]
person_level_diff_sq[ , 5:(max(res$n_total_persons)+4)] <- (person_level_diff[ ,5:(max(res$n_total_persons)+4)])^2
names(person_level_diff_sq)[5:(max(res$n_total_persons)+4)] <- paste0("sq_diff_ICC_", 1:max(res$n_total_persons))

### aggregate
# for random-draw conditions, we have replications across different drawn occasions
# for ordered-draw conditions, we have replications across different item sets
# -> automatically determine the number of replications
# CAVE: Participants may have missing in given replications
# -> use the number of valid occasions
# -> automatically determine this
# storage
RMSE <- as.data.frame(matrix(nrow=nrow(unique(res[ , c("occasions_drawn", "n_occasions", "n_items")])),
                             ncol=max(res$n_total_persons)+3))
names(RMSE)[1:3] <- c("occasions_drawn", "n_occasions", "n_items")
names(RMSE)[4:(max(res$n_total_persons)+3)] <- c(paste0("RMSE_", 1:max(res$n_total_persons)))


# RMSE = sqrt(sum(sq_diff_ICC)/n_replication)
sq_diff_cols <- paste0("sq_diff_ICC_", 1:max(res$n_total_persons))

RMSE <- aggregate(person_level_diff_sq[ , sq_diff_cols], # for each participant-specific sq_diff column
                  by = person_level_diff_sq[ , c("occasions_drawn", "n_occasions", "n_items")], # aggregate across conditions
                        FUN = function(x) {
                           
                           n_valid <- sum(!is.na(x)) # determine the number of valid replications in given condition for participant
                           
                           if (n_valid == 0) { # if there are no valid replications, return NA
                             NA_real_
                           } else { # else, take the square root of the the summed sq_diff divided by number of (valid) replications
                             sqrt(sum(x, na.rm=TRUE) / n_valid)
                           }
                           
                         })
names(RMSE)[4:(max(res$n_total_persons)+3)] <- c(paste0("RMSE_", 1:max(res$n_total_persons)))
# order
RMSE <- RMSE[order(RMSE$occasions_drawn, RMSE$n_items, RMSE$n_occasions), ]


# also create data frame with information on how many replications per participant were used for RMSE calculation
# storage
RMSE_N <- as.data.frame(matrix(nrow=nrow(unique(res[, c("occasions_drawn", "n_occasions", "n_items")])),
                               ncol=max(res$n_total_persons)+3))
names(RMSE_N)[1:3] <- c("occasions_drawn", "n_occasions", "n_items")
names(RMSE_N)[4:(max(res$n_total_persons)+3)] <- c(paste0("RMSE_N_", 1:max(res$n_total_persons)))

RMSE_N <- aggregate(person_level_diff_sq[ , sq_diff_cols], # for each participant-specific sq_diff column
                    by = person_level_diff_sq[ , c("occasions_drawn", "n_occasions", "n_items")], # aggregate across conditions
                    FUN = function(x) {
                             
                         sum(!is.na(x)) # determine the number of valid replications in given condition for participant
                        # same code as in RMSE calculation, but now stored individually
                    })
names(RMSE_N)[4:(max(res$n_total_persons)+3)] <- c(paste0("RMSE_N_", 1:max(res$n_total_persons)))

RMSE_N <- RMSE_N[order(RMSE_N$occasions_drawn, RMSE_N$n_items, RMSE_N$n_occasions), ]


# remove benchmark row
# (values are correctly 0)
# benchmark is "drawn by order" and maximum number of occasions and items
RMSE <- RMSE[-(which(RMSE$occasions_drawn == "by order" & RMSE$n_occasions == max(RMSE$n_occasions) & RMSE$n_items == max(RMSE$n_items))), ]
RMSE_N <- RMSE_N[-(which(RMSE_N$occasions_drawn == "by order" & RMSE_N$n_occasions == max(RMSE_N$n_occasions) & RMSE_N$n_items == max(RMSE_N$n_items))), ]



# save 
save(RMSE, file="results/02_revision_1/RMSE_values_per_participant_PED_emolive_Study.rda")
save(RMSE_N, file="results/02_revision_1/RMSE_replication_number_per_participant_PED_emolive_Study.rda")



# Calculate min, mean, and max across participants
RMSE$RMSE_min <- apply(RMSE[ , 4:(max(res$n_total_persons)+3)], 1, FUN = min, na.rm = TRUE)
RMSE$RMSE_mean <- rowMeans(RMSE[ ,4:(max(res$n_total_persons)+3)], na.rm=TRUE)
RMSE$RMSE_max <- apply(RMSE[ , 4:(max(res$n_total_persons)+3)], 1, FUN = max, na.rm = TRUE)
# subset 
RMSE <- RMSE[ , c("occasions_drawn", "n_occasions", "n_items",
                  "RMSE_min", "RMSE_mean", "RMSE_max")]


RMSE_N$RMSE_N_min <- apply(RMSE_N[ , 4:(max(res$n_total_persons)+3)], 1, FUN = min, na.rm = TRUE)
RMSE_N$RMSE_N_mean <- rowMeans(RMSE_N[ ,4:(max(res$n_total_persons)+3)], na.rm=TRUE)
RMSE_N$RMSE_N_max <- apply(RMSE_N[ , 4:(max(res$n_total_persons)+3)], 1, FUN = max, na.rm = TRUE)
# subset 
RMSE_N <- RMSE_N[ , c("occasions_drawn", "n_occasions", "n_items",
                  "RMSE_N_min", "RMSE_N_mean", "RMSE_N_max")]




# '' For ICC.z ------------------------------------------------------------
# use person-level differences -> square
person_level_diff.z_sq <- person_level_diff.z[ ,1:4]
person_level_diff.z_sq[ , 5:(max(res$n_total_persons)+4)] <- (person_level_diff.z[ ,5:(max(res$n_total_persons)+4)])^2
names(person_level_diff.z_sq)[5:(max(res$n_total_persons)+4)] <- paste0("sq_diff_ICC.z_", 1:max(res$n_total_persons))


### aggregate
# for random-draw conditions, we have replications across different drawn occasions
# for ordered-draw conditions, we have replications across different item sets
# -> automatically determine the number of replications
# CAVE: Participants may have missing in given replications
# -> use the number of valid occasions
# -> automatically determine this
# storage
RMSE.z <- as.data.frame(matrix(nrow=nrow(unique(res[ , c("occasions_drawn", "n_occasions", "n_items")])),
                             ncol=max(res$n_total_persons)+3))
names(RMSE.z)[1:3] <- c("occasions_drawn", "n_occasions", "n_items")
names(RMSE.z)[4:(max(res$n_total_persons)+3)] <- c(paste0("RMSE.z_", 1:max(res$n_total_persons)))


# RMSE.z = sqrt(sum(sq_diff_ICC)/n_replication)
sq_diff_cols <- paste0("sq_diff_ICC.z_", 1:max(res$n_total_persons))


RMSE.z <- aggregate(person_level_diff.z_sq[ , sq_diff_cols], # for each participant-specific sq_diff column
                  by = person_level_diff.z_sq[ , c("occasions_drawn", "n_occasions", "n_items")], # aggregate across conditions
                  FUN = function(x) {
                    
                    n_valid <- sum(!is.na(x)) # determine the number of valid replications in given condition for participant
                    
                    if (n_valid == 0) { # if there are no valid replications, return NA
                      NA_real_
                    } else { # else, take the square root of the the summed sq_diff divided by number of (valid) replications
                      sqrt(sum(x, na.rm=TRUE) / n_valid)
                    }
                    
                  })
names(RMSE.z)[4:(max(res$n_total_persons)+3)] <- c(paste0("RMSE.z_", 1:max(res$n_total_persons)))
# order
RMSE.z <- RMSE.z[order(RMSE.z$occasions_drawn, RMSE.z$n_items, RMSE.z$n_occasions), ]


# also create data frame with information on how many replications per participant were used for RMSE calculation
# storage
RMSE.z_N <- as.data.frame(matrix(nrow=nrow(unique(res[, c("occasions_drawn", "n_occasions", "n_items")])),
                               ncol=max(res$n_total_persons)+3))
names(RMSE.z_N)[1:3] <- c("occasions_drawn", "n_occasions", "n_items")
names(RMSE.z_N)[4:(max(res$n_total_persons)+3)] <- c(paste0("RMSE.z_N_", 1:max(res$n_total_persons)))

RMSE.z_N <- aggregate(person_level_diff.z_sq[ , sq_diff_cols], # for each participant-specific sq_diff column
                    by = person_level_diff.z_sq[ , c("occasions_drawn", "n_occasions", "n_items")], # aggregate across conditions
                    FUN = function(x) {
                      
                      sum(!is.na(x)) # determine the number of valid replications in given condition for participant
                      # same code as in RMSE.z calculation, but now stored individually
                    })
names(RMSE.z_N)[4:(max(res$n_total_persons)+3)] <- c(paste0("RMSE.z_N_", 1:max(res$n_total_persons)))

RMSE.z_N <- RMSE.z_N[order(RMSE.z_N$occasions_drawn, RMSE.z_N$n_items, RMSE.z_N$n_occasions), ]


# remove benchmark row
# (values are correctly 0)
# benchmark is "drawn by order" and maximum number of occasions and items
RMSE.z <- RMSE.z[-(which(RMSE.z$occasions_drawn == "by order" & RMSE.z$n_occasions == max(RMSE.z$n_occasions) & RMSE.z$n_items == max(RMSE.z$n_items))), ]
RMSE.z_N <- RMSE.z_N[-(which(RMSE.z_N$occasions_drawn == "by order" & RMSE.z_N$n_occasions == max(RMSE.z_N$n_occasions) & RMSE.z_N$n_items == max(RMSE.z_N$n_items))), ]



# save 
save(RMSE.z, file="results/02_revision_1/RMSE.z_values_per_participant_PED_emolive_Study.rda")
save(RMSE.z_N, file="results/02_revision_1/RMSE.z_replication_number_per_participant_PED_emolive_Study.rda")



# Calculate min, mean, and max across participants
RMSE.z$RMSE.z_min <- apply(RMSE.z[ , 4:(max(res$n_total_persons)+3)], 1, FUN = min, na.rm = TRUE)
RMSE.z$RMSE.z_mean <- rowMeans(RMSE.z[ ,4:(max(res$n_total_persons)+3)], na.rm=TRUE)
RMSE.z$RMSE.z_max <- apply(RMSE.z[ , 4:(max(res$n_total_persons)+3)], 1, FUN = max, na.rm = TRUE)
# subset 
RMSE.z <- RMSE.z[ , c("occasions_drawn", "n_occasions", "n_items",
                  "RMSE.z_min", "RMSE.z_mean", "RMSE.z_max")]


RMSE.z_N$RMSE.z_N_min <- apply(RMSE.z_N[ , 4:(max(res$n_total_persons)+3)], 1, FUN = min, na.rm = TRUE)
RMSE.z_N$RMSE.z_N_mean <- rowMeans(RMSE.z_N[ ,4:(max(res$n_total_persons)+3)], na.rm=TRUE)
RMSE.z_N$RMSE.z_N_max <- apply(RMSE.z_N[ , 4:(max(res$n_total_persons)+3)], 1, FUN = max, na.rm = TRUE)
# subset 
RMSE.z_N <- RMSE.z_N[ , c("occasions_drawn", "n_occasions", "n_items",
                      "RMSE.z_N_min", "RMSE.z_N_mean", "RMSE.z_N_max")]






# Aggregate Results -------------------------------------------------------
agg <- aggregate_results(res,
                         outcomes = c('N_merged_ICC_raw', 'N_merged_ICC_handled',
                                      'N_valid_ICC.z_handled',
                                      'N_cor_ICC', 'N_cor_ICC.z',
                                      'N_rel',
                                      'cor_ICC', 'cor_ICC.z',
                                      'rel',
                                      'sd_ICC', 'sd_ICC.z',
                                      'negICC_raw', 'negICC_handled', 'percnegICC_raw',
                                      'estimationProbNeg_raw', 'estimationProbPos_raw',
                                      'total_redraws', 'n_valid_persons_var', 'n_skipped_persons_var'),
                         rel_outcomes = c('cor_ICC', 'cor_ICC.z'),
                         abs_outcomes = c('N_merged_ICC_raw', 'N_merged_ICC_handled',
                                          'N_valid_ICC.z_handled',
                                          'N_cor_ICC', 'N_cor_ICC.z',
                                          'N_rel',
                                          'rel',
                                          'sd_ICC', 'sd_ICC.z',
                                          'negICC_raw', 'negICC_handled', 'percnegICC_raw',
                                          'estimationProbNeg_raw', 'estimationProbPos_raw',
                                          'total_redraws', 'n_valid_persons_var', 'n_skipped_persons_var'),
                         groupwise = FALSE,
                         group_var = NULL)


# merge difference to agg
agg_res <- list(person_diff_agg)
names(agg_res) <- "agg_res"
agg$person_diff <- agg_res

agg_res <- list(person_diff_agg.z)
names(agg_res) <- "agg_res"
agg$person_diff.z <- agg_res



# merge RMSE to agg
agg_res <- list(RMSE) # should be nested as other outcomes so that function works
names(agg_res) <- "agg_res"
agg$RMSE_ICC <- agg_res

agg_res <- list(RMSE_N) # should be nested as other outcomes so that function works
names(agg_res) <- "agg_res"
agg$RMSE_ICC_N <- agg_res


agg_res <- list(RMSE.z) # should be nested as other outcomes so that function works
names(agg_res) <- "agg_res"
agg$RMSE_ICC.z <- agg_res


agg_res <- list(RMSE.z_N) # should be nested as other outcomes so that function works
names(agg_res) <- "agg_res"
agg$RMSE_ICC.z_N <- agg_res


# Save Aggregated Results -------------------------------------------------
save(agg, file = "results/02_revision_1/aggregated_results_PED_emolive_Study.rda")




#----------------------------------------------------------------------------------------





# Calculate Monte Carlo Standard Error ------------------------------------
# for formulas, see Siepe et al. (2024), doi: 10.1037/met0000695


# use subset with random draws (ordered draws are not independent
# and the only "replications" there are are the different item sets)
rd <- res[which(res$occasions_drawn == "random"),]



max(rd$n_iteration) # maximum number of iterations



# write function to calculate MCSE for mean of generic statistic G
# (used for all outcomes except person-level difference ("bias") and person-level RMSE)

mcse_generic <- function(x) {
  
  r_valid <- sum(!is.na(x)) # determine number of valid replications
  
  if (r_valid < 2) { # if there is only one replication -> no variability -> no MCSE
    return(NA_real_) # return empty value (NA)
  } else {
    
    x_valid <- x[!is.na(x)] # use only those values of the outcome that are not NA
    
    MCSE <- sqrt( ( (sum( ( x_valid - (sum(x_valid)/r_valid ) )^2 )) / (r_valid - 1) ) / r_valid )
    # use r_valid (number of valid replications [i.e., replication with value instead of NA]) to calculate MCSE
    return(MCSE)
    
  }
  
}


# Calculate MCSE:

## for correlations (ICC)
# use generic formula
MCSE1 <- do.call(data.frame,
                 aggregate(cor_ICC ~ n_occasions + n_items, data = rd,
                           FUN = mcse_generic))

names(MCSE1) <- c("n_occasions", "n_items", "cor_ICC_MCSE")



## for correlations (ICC.z)
# use generic formula

MCSE2 <- do.call(data.frame,
                 aggregate(cor_ICC.z ~ n_occasions + n_items, data = rd,
                           FUN = mcse_generic))

names(MCSE2) <- c("n_occasions", "n_items", "cor_ICC.z_MCSE")


## for reliability
# -> mean of generic statistic G

MCSE3 <- do.call(data.frame,
                 aggregate(rel ~ n_occasions + n_items, data = rd,
                           FUN = mcse_generic))

names(MCSE3) <- c("n_occasions", "n_items", "rel_MCSE")


## for SD (ICC)
# use formula for mean of generic statistic G

MCSE4 <- do.call(data.frame,
                 aggregate(sd_ICC ~ n_occasions + n_items, data = rd,
                           FUN = mcse_generic))

names(MCSE4) <- c("n_occasions", "n_items", "sd_ICC_MCSE")


## for SD (ICC.z)
# use formula for mean of generic statistic G

MCSE5 <- do.call(data.frame,
                 aggregate(sd_ICC.z ~ n_occasions + n_items, data = rd,
                           FUN = mcse_generic))

names(MCSE5) <- c("n_occasions", "n_items", "sd_ICC.z_MCSE")


## for % negICC
# performance measure: mean of generic statistic G

MCSE6 <- do.call(data.frame,
                 aggregate(percnegICC_raw ~ n_occasions + n_items, data = rd,
                           FUN = mcse_generic))

names(MCSE6) <- c("n_occasions", "n_items", "percnegICC_raw_MCSE")





# combine
MCSE <- merge(MCSE1, MCSE2, by = c("n_occasions", "n_items"))
MCSE <- merge(MCSE, MCSE3, by = c("n_occasions", "n_items"))
MCSE <- merge(MCSE, MCSE4, by = c("n_occasions", "n_items"))
MCSE <- merge(MCSE, MCSE5, by = c("n_occasions", "n_items"))
MCSE <- merge(MCSE, MCSE6, by = c("n_occasions", "n_items"))

MCSE
names(MCSE)



# for person-level differences (person-level "bias")
# for each participant, calculate sampling variance and mean of differences ("bias") (across replications per condition)
# -> calculate MCSE per participant and condition
# for formula, see Siepe et al. (2024), Table 3, formula for bias

# MCSE = sqrt( s^2(estimates) / nsim )
# sampling variance -> sampling variance of estimates (not difference!)
# nsim -> here: valid number of replications per participant and condition

## for ICCs
# use data from random draws only
person_level_ICC_estimates.rd <- person_level_ICC_estimates[
  which(person_level_ICC_estimates$occasions_drawn == "random"), 
  ]


# automize over participants
# get ICC columns (one for each participant)
ICC_cols <- grep("^person_ICC_", names(person_level_ICC_estimates.rd), value=TRUE)

# calculate MCSE
MCSE_difference <- aggregate(person_level_ICC_estimates.rd[ , ICC_cols],
                             by = person_level_ICC_estimates.rd[ , c("n_occasions", "n_items")],
                             FUN = function(x) {
                               
                               r_valid <- sum(!is.na(x)) # determine number of valid replications (without NA)
                               
                               if (r_valid < 2) { # if there is only one replication -> no variability -> no MCSE
                                 return(NA_real_)
                               } else {
                                 
                                 x_valid <- x[!is.na(x)] # use only those values of the outcome that are not NA
                                 
                                 # calculate MCSE across valid values and use number of valid replications (r_valid)
                                 MCSE <- sqrt( ( (sum( ( x_valid - (sum(x_valid)/r_valid) )^2 )) / (r_valid - 1) ) / r_valid )
                                 
                               }

                               })

# adjust col names
names(MCSE_difference) <- c("n_occasions", "n_items", paste0("MCSE_difference_", sub("^person_ICC_", "", ICC_cols)))

# save
save(MCSE_difference, file="results/02_revision_1/MCSE_difference_per_participant_PED_emolive_Study.rda")


# calculate mean, min, max per condition
MCSE_difference$MCSE_difference_mean <- rowMeans(MCSE_difference[ , c(3:(max(res$n_total_persons)+2))], na.rm=T)
MCSE_difference$MCSE_difference_min <- apply(MCSE_difference[ , 3:(max(res$n_total_persons)+2)], 1, FUN = min, na.rm = TRUE)
MCSE_difference$MCSE_difference_max <- apply(MCSE_difference[ , 3:(max(res$n_total_persons)+2)], 1, FUN = max, na.rm = TRUE)



# add to MCSE object
MCSE <- merge(MCSE, MCSE_difference[ , c("n_occasions", "n_items", "MCSE_difference_min", "MCSE_difference_mean", "MCSE_difference_max")],
              by = c("n_occasions", "n_items"))




## for ICC.z
# use data from random draws only
person_level_ICC.z_estimates.rd <- person_level_ICC.z_estimates[
  which(person_level_ICC.z_estimates$occasions_drawn == "random"),
  ]


# automize over participants
# get ICC columns (one for each participant)
ICC_cols <- grep("^person_ICC.z_", names(person_level_ICC.z_estimates.rd), value=TRUE)


MCSE_difference.z <- aggregate(person_level_ICC.z_estimates.rd[ , ICC_cols],
                               by = person_level_ICC.z_estimates.rd[ , c("n_occasions", "n_items")],
                               FUN = function(x) {
                                 
                                 r_valid <- sum(!is.na(x)) # determine number of valid replications (without NA)
                                 
                                 if (r_valid < 2) { # if there is only one replication -> no variability -> no MCSE
                                   return(NA_real_)
                                 } else {
                                   
                                   x_valid <- x[!is.na(x)] # use only those values of the outcome that are not NA
                                   
                                   # calculate MCSE across valid values and use number of valid replications (r_valid)
                                   MCSE <- sqrt( ( (sum( ( x_valid - (sum(x_valid)/r_valid) )^2 )) / (r_valid - 1) ) / r_valid )
                                   
                                 }
                               })
# adjust col names
names(MCSE_difference.z) <- c("n_occasions", "n_items", paste0("MCSE_difference.z_", sub("^person_ICC.z_", "", ICC_cols)))


# save
save(MCSE_difference.z, file="results/02_revision_1/MCSE_difference.z_per_participant_PED_emolive_Study.rda")


# calculate mean, min, max per condition
MCSE_difference.z$MCSE_difference.z_mean <- rowMeans(MCSE_difference.z[ , c(3:(max(res$n_total_persons)+2))], na.rm=T)
MCSE_difference.z$MCSE_difference.z_min <- apply(MCSE_difference.z[ , 3:(max(res$n_total_persons)+2)], 1, FUN = min, na.rm = TRUE)
MCSE_difference.z$MCSE_difference.z_max <- apply(MCSE_difference.z[ , 3:(max(res$n_total_persons)+2)], 1, FUN = max, na.rm = TRUE)

# add to MCSE object
MCSE <- merge(MCSE, MCSE_difference.z[ , c("n_occasions", "n_items", "MCSE_difference.z_min", "MCSE_difference.z_mean", "MCSE_difference.z_max")],
              by = c("n_occasions", "n_items"))





# for RMSE per participant
# for each participant, calculate sampling variance of squared errors
# and mean of squared errors (across replications per condition)
# -> calculate MCSE per participant and condition

# for formula, see Siepe et al. (2024), Table 3, formula for MCSE of RMSE
# MSE hat = expected value for squared errors = mean of squared errors across replications

# use data from random draws only
person_level_diff_sq.rd <- person_level_diff_sq[which(person_level_diff_sq$occasions_drawn == "random"), ]



## for ICCs
# automize over participants
MCSE_RMSE <- data.frame(matrix(ncol=111, nrow=18))
names(MCSE_RMSE) <- c("n_occasions", "n_items", paste0("MCSE_RMSE_", 1:109))

sq_diff_cols <- grep("^sq_diff_ICC_", names(person_level_diff_sq.rd), value=TRUE)


MCSE_RMSE <- aggregate(person_level_diff_sq.rd[ , sq_diff_cols],
                       by = person_level_diff_sq.rd[ , c("n_occasions", "n_items")],
                       FUN = function(x) {
                         
                         r_valid <- sum(!is.na(x)) # determine number of valid replications (without NA)
                         
                         if (r_valid < 2) { # if there is only one replication -> no variability -> no MCSE
                           return(NA_real_)
                         } else {
                           
                           x_valid <- x[!is.na(x)] # use only those values of the outcome that are not NA
                           
                           # calculate MCSE across valid values and use number of valid replications (r_valid)
                           MCSE <- sqrt( ( (sum( ( x_valid - (sum(x_valid)/r_valid) )^2 )) / (r_valid - 1) ) / (4*r_valid*mean(x_valid)))
                         }
                      })

names(MCSE_RMSE) <- c("n_occasions", "n_items", paste0("MCSE_RMSE_", sub("^sq_diff_ICC_", "", sq_diff_cols )))



# save
save(MCSE_RMSE, file="results/02_revision_1/MCSE_RMSE_per_participant_PED_emolive_Study.rda")

# calculate mean, min, max per condition
MCSE_RMSE$MCSE_RMSE_mean <- rowMeans(MCSE_RMSE[ , c(3:(max(res$n_total_persons)+2))], na.rm=T)
MCSE_RMSE$MCSE_RMSE_min <- apply(MCSE_RMSE[ , 3:(max(res$n_total_persons)+2)], 1, FUN = min, na.rm = TRUE)
MCSE_RMSE$MCSE_RMSE_max <- apply(MCSE_RMSE[ , 3:(max(res$n_total_persons)+2)], 1, FUN = max, na.rm = TRUE)


# add to MCSE object
MCSE <- merge(MCSE, MCSE_RMSE[ , c("n_occasions", "n_items", "MCSE_RMSE_min", "MCSE_RMSE_mean", "MCSE_RMSE_max")],
              by = c("n_occasions", "n_items"))




## for ICC.z
person_level_diff.z_sq.rd <- person_level_diff.z_sq[which(person_level_diff.z_sq$occasions_drawn == "random"), ]


sq_diff_cols <- grep("^sq_diff_ICC.z_", names(person_level_diff.z_sq.rd), value=TRUE)

MCSE_RMSE.z <- aggregate(person_level_diff.z_sq.rd[ , sq_diff_cols],
                         by = person_level_diff.z_sq.rd[ , c("n_occasions", "n_items")],
                         FUN = function(x) {
                           
                           r_valid <- sum(!is.na(x)) # determine number of valid replications (without NA)
                           
                           if (r_valid < 2) { # if there is only one replication -> no variability -> no MCSE
                             return(NA_real_)
                           } else {
                             
                             x_valid <- x[!is.na(x)] # use only those values of the outcome that are not NA
                             
                             # calculate MCSE across valid values and use number of valid replications (r_valid)
                             MCSE <- sqrt( ( (sum( ( x_valid - (sum(x_valid)/r_valid) )^2 )) / (r_valid - 1) ) / (4*r_valid*mean(x_valid)))
                           }
                          
                     })

names(MCSE_RMSE.z) <- c("n_occasions", "n_items", paste0("MCSE_RMSE.z_", sub("^sq_diff_ICC.z_", "", sq_diff_cols )))


# save
save(MCSE_RMSE.z, file="results/02_revision_1/MCSE_RMSE.z_per_participant_PED_emolive_Study.rda")

# calculate mean, min, max per condition
MCSE_RMSE.z$MCSE_RMSE.z_mean <- rowMeans(MCSE_RMSE.z[ , c(3:(max(res$n_total_persons)+2))], na.rm=T)
MCSE_RMSE.z$MCSE_RMSE.z_min <- apply(MCSE_RMSE.z[ , 3:(max(res$n_total_persons)+2)], 1, FUN = min, na.rm = TRUE)
MCSE_RMSE.z$MCSE_RMSE.z_max <- apply(MCSE_RMSE.z[ , 3:(max(res$n_total_persons)+2)], 1, FUN = max, na.rm = TRUE)



# merge to MCSE
MCSE <- merge(MCSE, MCSE_RMSE.z[ , c("n_occasions", "n_items", "MCSE_RMSE.z_min", "MCSE_RMSE.z_mean", "MCSE_RMSE.z_max")],
              by = c("n_occasions", "n_items"))



# round and save MCSE as csv
# round to 3 decimals in this case
MCSE[3:20] <- round(MCSE[3:20], 3)
MCSE <- MCSE[order(MCSE$n_occasions, MCSE$n_items), ] # order
write.csv(MCSE, "results/02_revision_1/MCSE_table_PED_emolive_Study.csv", row.names = F)



rm(list=ls())

#====================================================================




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
#  [1] lubridate_1.9.4     forcats_1.0.1       stringr_1.6.0       dplyr_1.1.4         purrr_1.1.0        
#  [6] readr_2.1.5         tidyr_1.3.1         tibble_3.3.0        ggplot2_4.0.2       tidyverse_2.0.0    
# [11] future.apply_1.20.2 future_1.70.0      
# 
# loaded via a namespace (and not attached):
#  [1] gtable_0.3.6       compiler_4.5.3     tidyselect_1.2.1   parallel_4.5.3     irr_0.84.1        
#  [6] globals_0.19.1     scales_1.4.0       R6_2.6.1           labeling_0.4.3     generics_0.1.4    
# [11] pillar_1.11.1      RColorBrewer_1.1-3 tzdb_0.5.0         rlang_1.2.0        stringi_1.8.7     
# [16] S7_0.2.0           timechange_0.3.0   cli_3.6.5          withr_3.0.2        magrittr_2.0.3    
# [21] tictoc_1.2.1       digest_0.6.39      grid_4.5.3         rstudioapi_0.18.0  hms_1.1.4         
# [26] lifecycle_1.0.5    vctrs_0.6.5        lpSolve_5.6.23     glue_1.8.0         farver_2.1.2      
# [31] listenv_0.10.1     codetools_0.2-20   parallelly_1.46.1  tools_4.5.3        pkgconfig_2.0.3   