###################################################################
#####    Estimating trait negative emotion differentiation:   #####
#####        How many measurement occasions and               #####
#####              emotion items are needed?                  #####
###################################################################

###################################################################
#####                Results Aggregation                      #####
###################################################################



# Load Simulation Results -------------------------------------------------
load("results/sim_results.rda")


# Check Missings ----------------------------------------------------------
any(is.na(res))



# Source Function ---------------------------------------------------------
source("functions/function_aggregate_results.R")



# Determine Total Number of Redraws ---------------------------------------
# not saved with other results because it is not treated as evaluation criterion
# -> information for Method section
sum(res$total_redraws)
# zero
table(res$total_redraws)
# all are zero
redraw <- aggregate(total_redraws ~ occasions_drawn + n_occasions + n_items,
                    data=res, FUN = sum)
# zero in all conditions

rm(redraw)




# Calculate %negICC -------------------------------------------------------
# calculate proportion of negative ICCs
res$percnegICC <- res$negICC / 109 # divide by total number of participants




# Extract Person-Level ICC Estimates --------------------------------------
# '' For ICCs -------------------------------------------------------------
# extract person-level differences for each replication and each condition per participant
person_level_ICC_estimates <- data.frame(matrix(nrow=90021, ncol=112))
person_level_ICC_estimates [ , 1:3] <- res[ , 2:4]
names(person_level_ICC_estimates ) <- c("n_occasions", "occasions_drawn", "n_items", paste0("person_ICC_", 1:109))

ICC_matrix <- do.call(rbind, res$person_estimates_ICC) # extract the 109 person_estimates_ICC values per row (replication) and bind them
# -> matrix of 109 participants (columns) and their values in each replication (rows)

# bind with part_dat
person_level_ICC_estimates [ , 4:112] <- ICC_matrix


# save
save(person_level_ICC_estimates , file="results/person_level_ICC_per_replication.rda")



# '' For ICC.z ------------------------------------------------------------
# extract person-level differences for each replication and each condition per participant
person_level_ICC.z_estimates  <- data.frame(matrix(nrow=90021, ncol=112))
person_level_ICC.z_estimates [ , 1:3] <- res[ , 2:4]
names(person_level_ICC.z_estimates ) <- c("n_occasions", "occasions_drawn", "n_items", paste0("person_ICC.z_", 1:109))

ICC.z_matrix <- do.call(rbind, res$person_estimates_ICC.z) # extract the 109 person_estimates_ICC.z values per row (replication) and bind them
# -> matrix of 109 participants (columns) and their values in each replication (rows)

# bind with part_dat
person_level_ICC.z_estimates [ , 4:112] <- ICC.z_matrix


# save
save(person_level_ICC.z_estimates , file="results/person_level_ICC.z_per_replication.rda")








# Calculate Person-Level Deviation ("Bias") Across Replications -----------


# '' For ICCs -------------------------------------------------------------
# extract person-level differences for each replication and each condition per participant
person_level_diff <- data.frame(matrix(nrow=90021, ncol=112))
person_level_diff[ , 1:3] <- res[ , 2:4]
names(person_level_diff) <- c("n_occasions", "occasions_drawn", "n_items", paste0("person_diff_ICC_", 1:109))

diff_matrix <- do.call(rbind, res$person_diff_ICC) # extract the 109 person_diff_ICC values per row (replication) and bind them
# -> matrix of 109 participants (columns) and their values in each replication (rows)

# bind with part_dat
person_level_diff[ , 4:112] <- diff_matrix


# save
save(person_level_diff, file="results/person_level_difference_per_replication.rda")




# '' For ICC.z ------------------------------------------------------------
# extract person-level differences for each replication and each condition per participant
person_level_diff.z <- data.frame(matrix(nrow=90021, ncol=112))
person_level_diff.z[ , 1:3] <- res[ , 2:4]
names(person_level_diff.z) <- c("n_occasions", "occasions_drawn", "n_items", paste0("person_diff_ICC.z_", 1:109))

diff_matrix.z <- do.call(rbind, res$person_diff_ICC.z) # extract the 109 person_diff_ICC.z values per row (replication) and bind them
# -> matrix of 109 participants (columns) and their values in each replication (rows)

# bind with part_dat
person_level_diff.z[ , 4:112] <- diff_matrix.z


# save
save(person_level_diff.z, file="results/person_level_difference.z_per_replication.rda")




# '' Aggegrate Across Replications ----------------------------------------

## for ICC
vars <- paste0("person_diff_ICC_", 1:109)
person_diff_agg <- aggregate(person_level_diff[ , vars],
                             by = person_level_diff[ , c("occasions_drawn", "n_occasions", "n_items")],
                             FUN = function(x) {
                               mean(x)
                             }) 

names(person_diff_agg) <- c("occasions_drawn", "n_occasions", "n_items", paste0("person_difference_", 1:109))

# save
save(person_diff_agg, file="results/person_level_difference_aggregated_all_participants.rda")


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
person_diff_agg$difference_mean <- rowMeans(person_diff_agg[ , 4:112])
person_diff_agg$difference_median <- apply(person_diff_agg[ ,4:112], 1, median, na.rm=T) 

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
vars <- paste0("person_diff_ICC.z_", 1:109)
person_diff_agg.z <- aggregate(person_level_diff.z[ , vars],
                             by = person_level_diff.z[ , c("occasions_drawn", "n_occasions", "n_items")],
                             FUN = function(x) {
                               mean(x)
                             }) 

names(person_diff_agg.z) <- c("occasions_drawn", "n_occasions", "n_items", paste0("person_difference.z_", 1:109))

# save
save(person_diff_agg.z, file="results/person_level_difference.z_aggregated_all_participants.rda")


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
person_diff_agg.z$difference.z_mean <- rowMeans(person_diff_agg.z[ , 4:112])
person_diff_agg.z$difference.z_median <- apply(person_diff_agg.z[ ,4:112], 1, median, na.rm=T) 


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
person_diff_agg$difference_mean <- rowMeans(person_diff_agg[ , 4:112])
person_diff_agg$difference_median <- apply(person_diff_agg[ ,4:112], 1, median, na.rm=T) 

# determine which participant deviates most
person_diff_agg$difference_min_id <- apply(person_diff_agg[ ,4:112], 1, which.min)
person_diff_agg$difference_max_id <- apply(person_diff_agg[ ,4:112], 1, which.max)

# calculate min and max
person_diff_agg$difference_min <- apply(person_diff_agg[ ,4:112], 1, min, na.rm=T)
person_diff_agg$difference_max <- apply(person_diff_agg[ ,4:112], 1, max, na.rm=T)


person_diff_agg <- person_diff_agg[ , c(1:3, 113:118)]


## for ICC.z
# calculate overall difference (mean difference across participants)
person_diff_agg.z$difference.z_mean <- rowMeans(person_diff_agg.z[ , 4:112])
person_diff_agg.z$difference.z_median <- apply(person_diff_agg.z[ ,4:112], 1, median, na.rm=T) 

# determine which participant deviates most
person_diff_agg.z$difference.z_min_id <- apply(person_diff_agg.z[ ,4:112], 1, which.min)
person_diff_agg.z$difference.z_max_id <- apply(person_diff_agg.z[ ,4:112], 1, which.max)

# calculate min and max
person_diff_agg.z$difference.z_min <- apply(person_diff_agg.z[ ,4:112], 1, min, na.rm=T)
person_diff_agg.z$difference.z_max <- apply(person_diff_agg.z[ ,4:112], 1, max, na.rm=T)



person_diff_agg.z <- person_diff_agg.z[ , c(1:3, 113:118)]



# Calcute RMSE for Each Participant Across Replications -------------------


# '' For ICCs -------------------------------------------------------------
# use person-level differences -> square
person_level_diff_sq <- person_level_diff[ ,1:3]
person_level_diff_sq[ , 4:112] <- (person_level_diff[ ,4:112])^2
names(person_level_diff_sq)[4:112] <- paste0("sq_diff_ICC_", 1:109)

### aggregate

# subset for random draws
rd <- person_level_diff_sq[person_level_diff_sq$occasions_drawn == "random", ]

# storage
RMSE_random <- as.data.frame(matrix(nrow=18, ncol=112))
names(RMSE_random)[1:3] <- c("occasions_drawn", "n_occasions", "n_items")
names(RMSE_random)[4:112] <- c(paste0("RMSE_", 1:109))


# RMSE = sqrt(sum(sq_diff_ICC)/n_replication)
sq_diff_cols <- paste0("sq_diff_ICC_", 1:109)

RMSE_random <- aggregate(rd[ , sq_diff_cols], # for each participant-specific sq_diff column
                      by = rd[ , c("occasions_drawn", "n_occasions", "n_items")], # aggregate across conditions
                      FUN = function(x) {
                        sqrt(sum(x)/5000) # by taking square root of the the summed sq_diff divided by number of replications
                      })
names(RMSE_random)[4:112] <- c(paste0("RMSE_", 1:109))



# repeat for ordered draws -> different number of replications (i.e., 1)
# subset for ordered draws
od <- person_level_diff_sq[person_level_diff_sq$occasions_drawn == "by order", ]

# storage
RMSE_order <- as.data.frame(matrix(nrow=21, ncol=112))
names(RMSE_order)[1:3] <- c("occasions_drawn", "n_occasions", "n_items")
names(RMSE_order)[4:112] <- c(paste0("RMSE_", 1:109))



# RMSE = sqrt(sum(sq_diff_ICC)/n_replication)
sq_diff_cols <- paste0("sq_diff_ICC_", 1:109)

RMSE_order <- aggregate(od[ , sq_diff_cols], # for each participant-specific sq_diff column
                        by = od[ , c("occasions_drawn", "n_occasions", "n_items")], # aggregate across conditions
                         FUN = function(x) {
                           sqrt(sum(x)/1) # by taking square root of the the summed sq_diff divided by number of replications
                         })
names(RMSE_order)[4:112] <- c(paste0("RMSE_", 1:109))



# combine RMSE data frames
RMSE <- rbind(RMSE_random, RMSE_order)
rm(RMSE_random, RMSE_order)



# remove benchmark row
# (values are correctly 0)
RMSE <- RMSE[-(which(RMSE$occasions_drawn == "by order" & RMSE$n_occasions == 70 & RMSE$n_items == 15)), ]



# save 
save(RMSE, file="results/RMSE_values_per_participant.rda")


# Calculate min, mean, and max across participants
RMSE$RMSE_min <- apply(RMSE[ , 4:112], 1, FUN = min, na.rm = TRUE)
RMSE$RMSE_mean <- rowMeans(RMSE[ ,4:112], na.rm=TRUE)
RMSE$RMSE_max <- apply(RMSE[ , 4:112], 1, FUN = max, na.rm = TRUE)
# subset 
RMSE <- RMSE[ , c(1:3, 113:115)]





# '' For ICC.z ------------------------------------------------------------
# use person-level differences -> square
person_level_diff.z_sq <- person_level_diff.z[ ,1:3]
person_level_diff.z_sq[ , 4:112] <- (person_level_diff.z[ ,4:112])^2
names(person_level_diff.z_sq)[4:112] <- paste0("sq_diff_ICC.z_", 1:109)



### aggregate

# subset for random draws
rd.z <- person_level_diff.z_sq[person_level_diff.z_sq$occasions_drawn == "random", ]

# storage
RMSE.z_random <- as.data.frame(matrix(nrow=18, ncol=112))
names(RMSE.z_random)[1:3] <- c("occasions_drawn", "n_occasions", "n_items")
names(RMSE.z_random)[4:112] <- c(paste0("RMSE.z_", 1:109))


# RMSE = sqrt(sum(sq_diff_ICC.z)/n_replication)
sq_diff_cols <- paste0("sq_diff_ICC.z_", 1:109)

RMSE.z_random <- aggregate(rd.z[ , sq_diff_cols], # for each participant-specific sq_diff column
                         by = rd.z[ , c("occasions_drawn", "n_occasions", "n_items")], # aggregate across conditions
                         FUN = function(x) {
                           sqrt(sum(x)/5000) # by taking square root of the the summed sq_diff divided by number of replications
                         })
names(RMSE.z_random)[4:112] <- c(paste0("RMSE.z_", 1:109))




# repeat for ordered draws -> different number of replications (i.e., 1)
# subset for ordered draws
od.z <- person_level_diff.z_sq[person_level_diff.z_sq$occasions_drawn == "by order", ]

# storage
RMSE.z_order <- as.data.frame(matrix(nrow=21, ncol=112))
names(RMSE.z_order)[1:3] <- c("occasions_drawn", "n_occasions", "n_items")
names(RMSE.z_order)[4:112] <- c(paste0("RMSE.z_", 1:109))


# RMSE = sqrt(sum(sq_diff_ICC.z)/n_replication)
sq_diff_cols <- paste0("sq_diff_ICC.z_", 1:109)

RMSE.z_order <- aggregate(od.z[ , sq_diff_cols], # for each participant-specific sq_diff column
                           by = od.z[ , c("occasions_drawn", "n_occasions", "n_items")], # aggregate across conditions
                           FUN = function(x) {
                             sqrt(sum(x)/1) # by taking square root of the the summed sq_diff divided by number of replications
                           })
names(RMSE.z_order)[4:112] <- c(paste0("RMSE.z_", 1:109))



# combine RMSE data frames
RMSE.z <- rbind(RMSE.z_random, RMSE.z_order)
rm(RMSE.z_random, RMSE.z_order)

# remove benchmark row
# (values are correctly 0)
RMSE.z <- RMSE.z[-(which(RMSE.z$occasions_drawn == "by order" & RMSE.z$n_occasions == 70 & RMSE.z$n_items == 15)), ]


# save 
save(RMSE.z, file="results/RMSE.z_values_per_participant.rda")


# Calculate min, mean, and max across participants
RMSE.z$RMSE.z_min <- apply(RMSE.z[ , 4:112], 1, FUN = min, na.rm = TRUE)
RMSE.z$RMSE.z_mean <- rowMeans(RMSE.z[ ,4:112], na.rm=TRUE)
RMSE.z$RMSE.z_max <- apply(RMSE.z[ , 4:112], 1, FUN = max, na.rm = TRUE)
# subset 
RMSE.z <- RMSE.z[ , c(1:3, 113:115)]






# Aggregate Results -------------------------------------------------------
agg <- aggregate_results(res,
                         outcomes = c('N_valid_ICC.z',
                                      'cor_ICC', 'cor_ICC.z',
                                      'rel', 'N_rel',
                                      'sd_ICC', 'sd_ICC.z',
                                      'negICC', 'percnegICC',
                                      'estimationProbNeg', 'estimationProbPos'),
                         rel_outcomes = c('cor_ICC', 'cor_ICC.z'),
                         abs_outcomes = c('N_valid_ICC.z',
                                          'rel', 'N_rel',
                                          'sd_ICC', 'sd_ICC.z',
                                          'negICC', 'percnegICC',
                                          'estimationProbNeg', 'estimationProbPos'),
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


agg_res <- list(RMSE.z) # should be nested as other outcomes so that function works
names(agg_res) <- "agg_res"
agg$RMSE_ICC.z <- agg_res





# Save Aggregated Results -------------------------------------------------
save(agg, file = "results/aggregated_results.rda")



# Calculate Monte Carlo Standard Error ------------------------------------
# for formulas, see Siepe et al. (2024), doi: 10.1037/met0000695


# use subset with random draws (ordered draws are not independent and there is only one simulation
# run for each condition -> no variance = no MCSE)
rd <- res[which(res$occasions_drawn == "random"),]



max(rd$n_iteration) # 5000 iterations



## for correlations (ICC)
# mean of correlations as performance measure

MCSE1 <- do.call(data.frame, aggregate(cor_ICC ~ n_occasions + n_items, data = rd, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/5000 ) )^2 )) / (5000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/5000 ) )^2 )) / (5000 - 1) ) / 5000 )) 
})
)

names(MCSE1) <- c("n_occasions", "n_items", "cor_ICC_sim_mean", "cor_ICC_sim_var", "cor_ICC_MCSE")


## for correlations (ICC.z)
# mean of correlations as performance measure

MCSE2 <- do.call(data.frame, aggregate(cor_ICC.z ~ n_occasions + n_items, data = rd, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/5000 ) )^2 )) / (5000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/5000 ) )^2 )) / (5000 - 1) ) / 5000 )) 
})
)

names(MCSE2) <- c("n_occasions", "n_items", "cor_ICC.z_sim_mean", "cor_ICC.z_sim_var", "cor_ICC.z_MCSE")



## for reliability
# -> mean of generic statistic G

MCSE3 <- do.call(data.frame, aggregate(rel ~ n_occasions + n_items, data = rd, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/5000 ) )^2 )) / (5000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/5000 ) )^2 )) / (5000 - 1) ) / 5000 )) 
})
)

names(MCSE3) <- c("n_occasions", "n_items", "rel_sim_mean", "rel_sim_var", "rel_MCSE")


## for SD (ICC)
# we have one SD for each replicate -> performance measure = mean of SD across replicates
# use formula for mean of generic statistic G
MCSE4 <- do.call(data.frame, aggregate(sd_ICC ~ n_occasions + n_items, data = rd, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/5000 ) )^2 )) / (5000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/5000 ) )^2 )) / (5000 - 1) ) / 5000 )) 
})
)

names(MCSE4) <- c("n_occasions", "n_items", "sd_ICC_sim_mean", "sd_ICC_sim_var", "sd_ICC_MCSE")


## for SD (ICC.z)
# we have one SD for each replicate -> performance measure = mean of SD across replicates
# use formula for mean of generic statistic G
MCSE5 <- do.call(data.frame, aggregate(sd_ICC.z ~ n_occasions + n_items, data = rd, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/5000 ) )^2 )) / (5000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/5000 ) )^2 )) / (5000 - 1) ) / 5000 )) 
})
)

names(MCSE5) <- c("n_occasions", "n_items", "sd_ICC.z_sim_mean", "sd_ICC.z_sim_var", "sd_ICC.z_MCSE")


## for % negICC
# performance measure: mean of generic statistic G
MCSE6 <- do.call(data.frame, aggregate(percnegICC ~ n_occasions + n_items, data = rd, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/5000 ) )^2 )) / (5000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/5000 ) )^2 )) / (5000 - 1) ) / 5000 )) 
})
)

names(MCSE6) <- c("n_occasions", "n_items", "percnegICC_sim_mean", "percnegICC_sim_var", "percnegICC_MCSE")


## for N_rel
# performance measure: mean of generic statistic G

MCSE7 <- do.call(data.frame, aggregate(N_rel ~ n_occasions + n_items, data = rd, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/5000 ) )^2 )) / (5000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/5000 ) )^2 )) / (5000 - 1) ) / 5000 )) 
})
)

names(MCSE7) <- c("n_occasions", "n_items", "N_rel_sim_mean", "N_rel_sim_var", "N_rel_MCSE")

## for estimation problems, no MCSE can be calculated because there is zero variance
range(rd$estimationProbNeg)
range(rd$estimationProbPos)


# combine
MCSE <- merge(MCSE1, MCSE2, by = c("n_occasions", "n_items"))
MCSE <- merge(MCSE, MCSE3, by = c("n_occasions", "n_items"))
MCSE <- merge(MCSE, MCSE4, by = c("n_occasions", "n_items"))
MCSE <- merge(MCSE, MCSE5, by = c("n_occasions", "n_items"))
MCSE <- merge(MCSE, MCSE6, by = c("n_occasions", "n_items"))
MCSE <- merge(MCSE, MCSE7, by = c("n_occasions", "n_items"))

MCSE
names(MCSE)



# for person-level differences (person-level "bias")
# for each participant, calculate sampling variance of differences ("bias") and mean of differences ("bias") (across replications per condition)
# -> calculate MCSE per participant and condition
# for formula, see Siepe et al. (2024), Table 3, formula for bias

# MCSE = sqrt( s^2(estimates) / nsim )
# sampling variance -> sampling variance of estimates (not difference!)

## for ICCs
# use data from random draws only
person_level_ICC_estimates.rd <- person_level_ICC_estimates[which(person_level_ICC_estimates$occasions_drawn == "random"), ]


# automize over participants
MCSE_difference <- data.frame(matrix(ncol=111, nrow=18))
names(MCSE_difference) <- c("n_occasions", "n_items", paste0("MCSE_difference_", 1:109))

ICC_cols <- paste0("person_ICC_", 1:109)

MCSE_difference[] <- aggregate(person_level_ICC_estimates.rd[ , ICC_cols],
                         by = person_level_ICC_estimates.rd[ , c("n_occasions", "n_items")],
                         FUN = function(x) {
                           MCSE = sqrt( ( (sum( ( x - (sum(x)/5000 ) )^2 )) / (5000 - 1) ) / 5000 )
                         })



# save
save(MCSE_difference, file="results/MCSE_difference_per_participant.rda")


# calculate mean, min, max per condition
MCSE_difference$MCSE_difference_mean <- rowMeans(MCSE_difference[ , c(3:111)], na.rm=T)
MCSE_difference$MCSE_difference_min <- apply(MCSE_difference[ , 3:111], 1, FUN = min, na.rm = TRUE)
MCSE_difference$MCSE_difference_max <- apply(MCSE_difference[ , 3:111], 1, FUN = max, na.rm = TRUE)

# add to MCSE object
MCSE <- merge(MCSE, MCSE_difference[ , c("n_occasions", "n_items", "MCSE_difference_min", "MCSE_difference_mean", "MCSE_difference_max")],
              by = c("n_occasions", "n_items"))




## for ICC.z
# use data from random draws only
person_level_ICC.z_estimates.rd <- person_level_ICC.z_estimates[which(person_level_ICC.z_estimates$occasions_drawn == "random"), ]


# automize over participants
MCSE_difference.z <- data.frame(matrix(ncol=111, nrow=18))
names(MCSE_difference.z) <- c("n_occasions", "n_items", paste0("MCSE_difference.z_", 1:109))

ICC_cols <- paste0("person_ICC.z_", 1:109)

MCSE_difference.z[] <- aggregate(person_level_ICC.z_estimates.rd[ , ICC_cols],
                               by = person_level_ICC.z_estimates.rd[ , c("n_occasions", "n_items")],
                               FUN = function(x) {
                                 MCSE = sqrt( ( (sum( ( x - (sum(x)/5000 ) )^2 )) / (5000 - 1) ) / 5000 )
                               })


# save
save(MCSE_difference.z, file="results/MCSE_difference.z_per_participant.rda")


# calculate mean, min, max per condition
MCSE_difference.z$MCSE_difference.z_mean <- rowMeans(MCSE_difference.z[ , c(3:111)], na.rm=T)
MCSE_difference.z$MCSE_difference.z_min <- apply(MCSE_difference.z[ , 3:111], 1, FUN = min, na.rm = TRUE)
MCSE_difference.z$MCSE_difference.z_max <- apply(MCSE_difference.z[ , 3:111], 1, FUN = max, na.rm = TRUE)

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


# # for one participant
# MCSE_RMSE_part1 <- data.frame(matrix(ncol=3, nrow=18))
# names(MCSE_RMSE_part1) <- c("n_occasions", "n_items", "MCSE_RMSE_1")
# 
# MCSE_RMSE_part1 <- do.call(data.frame,
#                            aggregate(sq_diff_ICC_1 ~ n_occasions + n_items,
#                                      data = part_dat.rd,
#                                      FUN = function(x) {
#                                        MCSE = sqrt( ( (sum( ( x - (sum(x)/5000) )^2 )) / (5000 - 1) ) / (4*5000*mean(x)))
#                                      }))

## for ICCs
# automize over participants
MCSE_RMSE <- data.frame(matrix(ncol=111, nrow=18))
names(MCSE_RMSE) <- c("n_occasions", "n_items", paste0("MCSE_RMSE_", 1:109))

sq_diff_cols <- paste0("sq_diff_ICC_", 1:109)

MCSE_RMSE[] <- aggregate(person_level_diff_sq.rd[ , sq_diff_cols],
                  by = person_level_diff_sq.rd[ , c("n_occasions", "n_items")],
                  FUN = function(x) {
                    MCSE = sqrt( ( (sum( ( x - (sum(x)/5000) )^2 )) / (5000 - 1) ) / (4*5000*mean(x)))
                  })


# save
save(MCSE_RMSE, file="results/MCSE_RMSE_per_participant.rda")

# calculate mean, min, max per condition
MCSE_RMSE$MCSE_RMSE_mean <- rowMeans(MCSE_RMSE[ , c(3:111)], na.rm=T)
MCSE_RMSE$MCSE_RMSE_min <- apply(MCSE_RMSE[ , 3:111], 1, FUN = min, na.rm = TRUE)
MCSE_RMSE$MCSE_RMSE_max <- apply(MCSE_RMSE[ , 3:111], 1, FUN = max, na.rm = TRUE)


# add to MCSE object
MCSE <- merge(MCSE, MCSE_RMSE[ , c("n_occasions", "n_items", "MCSE_RMSE_min", "MCSE_RMSE_mean", "MCSE_RMSE_max")],
              by = c("n_occasions", "n_items"))




## for ICC.z
person_level_diff.z_sq.rd <- person_level_diff.z_sq[which(person_level_diff.z_sq$occasions_drawn == "random"), ]

MCSE_RMSE.z <- data.frame(matrix(ncol=111, nrow=18))
names(MCSE_RMSE.z) <- c("n_occasions", "n_items", paste0("MCSE_RMSE.z_", 1:109))

sq_diff_cols <- paste0("sq_diff_ICC.z_", 1:109)

MCSE_RMSE.z[] <- aggregate(person_level_diff.z_sq.rd[ , sq_diff_cols],
                           by = person_level_diff.z_sq.rd[ , c("n_occasions", "n_items")],
                           FUN = function(x) {
                             MCSE = sqrt( ( (sum( ( x - (sum(x)/5000) )^2 )) / (5000 - 1) ) / (4*5000*mean(x)))
                         })

# save
save(MCSE_RMSE.z, file="results/MCSE_RMSE.z_per_participant.rda")

# calculate mean, min, max per condition
MCSE_RMSE.z$MCSE_RMSE.z_mean <- rowMeans(MCSE_RMSE.z[ , c(3:111)], na.rm=T)
MCSE_RMSE.z$MCSE_RMSE.z_min <- apply(MCSE_RMSE.z[ , 3:111], 1, FUN = min, na.rm = TRUE)
MCSE_RMSE.z$MCSE_RMSE.z_max <- apply(MCSE_RMSE.z[ , 3:111], 1, FUN = max, na.rm = TRUE)



# merge to MCSE
MCSE <- merge(MCSE, MCSE_RMSE.z[ , c("n_occasions", "n_items", "MCSE_RMSE.z_min", "MCSE_RMSE.z_mean", "MCSE_RMSE.z_max")],
              by = c("n_occasions", "n_items"))



# round and save MCSE as csv
# round to 3 decimals in this case
MCSE[3:35] <- round(MCSE[3:35], 3)
MCSE <- MCSE[order(MCSE$n_occasions, MCSE$n_items), ]
# use only MCSE (not mean or var)
MCSE <- MCSE[ , c('n_occasions', 'n_items', 'cor_ICC_MCSE', 'cor_ICC.z_MCSE',
                  'rel_MCSE', 'sd_ICC_MCSE','sd_ICC.z_MCSE', 'percnegICC_MCSE', 'N_rel_MCSE', 
                  'MCSE_difference_min', 'MCSE_difference_mean', 'MCSE_difference_max',
                  'MCSE_difference.z_min', 'MCSE_difference.z_mean', 'MCSE_difference.z_max',
                  'MCSE_RMSE_min',
                  'MCSE_RMSE_mean', 'MCSE_RMSE_max', 'MCSE_RMSE.z_min', 'MCSE_RMSE.z_mean', 'MCSE_RMSE.z_max')]
write.csv(MCSE, "results/MCSE_table.csv", row.names = F)



rm(list=ls())




# Check Sufficient Number of Iterations  ----------------------------------
source("functions/function_aggregate_results.R")

load("results/check nr of iterations/sim_results.rda")



# Calculate %negICC
# calculate proportion of negative ICCs
res2$percnegICC <- res2$negICC / 109 # divide by total number of participants


# '' Determine Total Number of Redraws ------------------------------------
# not saved with other results because it is not treated as evaluation criterion
# -> information for Method section
sum(res2$total_redraws)
# zero
table(res2$total_redraws)
# all are zero
redraw2 <- aggregate(total_redraws ~ occasions_drawn + n_occasions + n_items,
                    data=res2, FUN = sum)
# 1 in random draws, 14 occasions, 5 items

rm(redraw2)

# '' Extract Person-Level ICC Estimates -----------------------------------
# '''' For ICCs -----------------------------------------------------------
# extract person-level differences for each replication and each condition per participant
person_level_ICC_estimates2 <- data.frame(matrix(nrow=90021, ncol=112))
person_level_ICC_estimates2[ , 1:3] <- res2[ , 2:4]
names(person_level_ICC_estimates2) <- c("n_occasions", "occasions_drawn", "n_items", paste0("person_ICC_", 1:109))

ICC_matrix <- do.call(rbind, res2$person_estimates_ICC) # extract the 109 person_estimates_ICC values per row (replication) and bind them
# -> matrix of 109 participants (columns) and their values in each replication (rows)

# bind with part_dat
person_level_ICC_estimates2[ , 4:112] <- ICC_matrix


# save
save(person_level_ICC_estimates2, file="results/check nr of iterations/person_level_ICC_per_replication.rda")


# '''' For ICC.z ----------------------------------------------------------
# extract person-level differences for each replication and each condition per participant
person_level_ICC.z_estimates2  <- data.frame(matrix(nrow=90021, ncol=112))
person_level_ICC.z_estimates2[ , 1:3] <- res2[ , 2:4]
names(person_level_ICC.z_estimates2) <- c("n_occasions", "occasions_drawn", "n_items", paste0("person_ICC.z_", 1:109))

ICC.z_matrix <- do.call(rbind, res2$person_estimates_ICC.z) # extract the 109 person_estimates_ICC.z values per row (replication) and bind them
# -> matrix of 109 participants (columns) and their values in each replication (rows)

# bind with part_dat
person_level_ICC.z_estimates2[ , 4:112] <- ICC.z_matrix


# save
save(person_level_ICC.z_estimates2, file="results/check nr of iterations/person_level_ICC.z_per_replication.rda")



# '' Calculate Person-Level Deviation ("Bias") Across Replications --------


# '''' For ICCs -----------------------------------------------------------
# extract person-level differences for each replication and each condition per participant
person_level_diff2 <- data.frame(matrix(nrow=90021, ncol=112))
person_level_diff2[ , 1:3] <- res2[ , 2:4]
names(person_level_diff2) <- c("n_occasions", "occasions_drawn", "n_items", paste0("person_diff_ICC_", 1:109))

diff_matrix <- do.call(rbind, res2$person_diff_ICC) # extract the 109 person_diff_ICC values per row (replication) and bind them
# -> matrix of 109 participants (columns) and their values in each replication (rows)

# bind with part_dat
person_level_diff2[ , 4:112] <- diff_matrix


# save
save(person_level_diff2, file="results/check nr of iterations/person_level_difference_per_replication.rda")



# '''' For ICC.z ----------------------------------------------------------
# extract person-level differences for each replication and each condition per participant
person_level_diff.z2 <- data.frame(matrix(nrow=90021, ncol=112))
person_level_diff.z2[ , 1:3] <- res2[ , 2:4]
names(person_level_diff.z2) <- c("n_occasions", "occasions_drawn", "n_items", paste0("person_diff_ICC.z_", 1:109))

diff_matrix.z <- do.call(rbind, res2$person_diff_ICC.z) # extract the 109 person_diff_ICC.z values per row (replication) and bind them
# -> matrix of 109 participants (columns) and their values in each replication (rows)

# bind with part_dat
person_level_diff.z2[ , 4:112] <- diff_matrix.z


# save
save(person_level_diff.z2, file="results/check nr of iterations/person_level_difference.z_per_replication.rda")



# '''' Aggegrate Across Replications --------------------------------------

## for ICC
vars <- paste0("person_diff_ICC_", 1:109)
person_diff_agg2 <- aggregate(person_level_diff2[ , vars],
                             by = person_level_diff2[ , c("occasions_drawn", "n_occasions", "n_items")],
                             FUN = function(x) {
                               mean(x)
                             }) 

names(person_diff_agg2) <- c("occasions_drawn", "n_occasions", "n_items", paste0("person_difference_", 1:109))

# save
save(person_diff_agg2, file="results/check nr of iterations/person_level_difference_aggregated_all_participants.rda")



# plot
# reshape data
library(tidyverse)
long2 <- person_diff_agg2 %>%
  pivot_longer(
    cols = starts_with("person_difference_"),   
    names_to = "participant",
    values_to = "person_difference"
  )



ggplot(long2, aes(x = n_occasions, y = person_difference, group = participant)) +
  geom_line(alpha = 0.3, aes(col=participant)) +
  theme_minimal() + 
  facet_grid(rows=vars(n_items), cols=vars(occasions_drawn)) +
  theme(legend.position = "none")


# calculate overall difference (mean difference across participants)
person_diff_agg2$difference_mean <- rowMeans(person_diff_agg2[ , 4:112])
person_diff_agg2$difference_median <- apply(person_diff_agg2[ ,4:112], 1, median, na.rm=T) 

# View(person_diff_agg2[ , c("occasions_drawn", "n_occasions", "n_items", "difference_mean")])

ggplot(person_diff_agg2, aes(x = n_occasions, y = difference_mean, group = n_items)) +
  geom_line(aes(col=n_items)) +
  theme_minimal() + 
  facet_wrap(vars(occasions_drawn)) +
  theme(legend.position = "none")

ggplot(person_diff_agg2, aes(x = n_occasions, y = difference_median, group = n_items)) +
  geom_line(aes(col=n_items)) +
  theme_minimal() + 
  facet_wrap(vars(occasions_drawn)) +
  theme(legend.position = "none")


### for ICC.z
vars <- paste0("person_diff_ICC.z_", 1:109)
person_diff_agg.z2 <- aggregate(person_level_diff.z2[ , vars],
                               by = person_level_diff.z2[ , c("occasions_drawn", "n_occasions", "n_items")],
                               FUN = function(x) {
                                 mean(x)
                               }) 

names(person_diff_agg.z2) <- c("occasions_drawn", "n_occasions", "n_items", paste0("person_difference.z_", 1:109))

# save
save(person_diff_agg.z2, file="results/check nr of iterations/person_level_difference.z_aggregated_all_participants.rda")


# plot
# reshape data
library(tidyverse)
long2 <- person_diff_agg.z2 %>%
  pivot_longer(
    cols = starts_with("person_difference.z_"),   
    names_to = "participant",
    values_to = "person_difference.z"
  )



ggplot(long2, aes(x = n_occasions, y = person_difference.z, group = participant)) +
  geom_line(alpha = 0.3, aes(col=participant)) +
  theme_minimal() + 
  facet_grid(rows=vars(n_items), cols=vars(occasions_drawn)) +
  theme(legend.position = "none")


# calculate overall difference (mean difference across participants)
person_diff_agg.z2$difference.z_mean <- rowMeans(person_diff_agg.z2[ , 4:112])
person_diff_agg.z2$difference.z_median <- apply(person_diff_agg.z2[ ,4:112], 1, median, na.rm=T) 


ggplot(person_diff_agg.z2, aes(x = n_occasions, y = difference.z_mean, group = n_items)) +
  geom_line(aes(col=n_items)) +
  theme_minimal() + 
  facet_wrap(vars(occasions_drawn)) +
  theme(legend.position = "none")

ggplot(person_diff_agg.z2, aes(x = n_occasions, y = difference.z_median, group = n_items)) +
  geom_line(aes(col=n_items)) +
  theme_minimal() + 
  facet_wrap(vars(occasions_drawn)) +
  theme(legend.position = "none")


# '' Aggregate Across Participants (for Plotting) -------------------------
## for ICC
# calculate overall difference (mean difference across participants)
person_diff_agg2$difference_mean <- rowMeans(person_diff_agg2[ , 4:112])
person_diff_agg2$difference_median <- apply(person_diff_agg2[ ,4:112], 1, median, na.rm=T) 

# determine which participant deviates most
person_diff_agg2$difference_min_id <- apply(person_diff_agg2[ ,4:112], 1, which.min)
person_diff_agg2$difference_max_id <- apply(person_diff_agg2[ ,4:112], 1, which.max)

# calculate min and max
person_diff_agg2$difference_min <- apply(person_diff_agg2[ ,4:112], 1, min, na.rm=T)
person_diff_agg2$difference_max <- apply(person_diff_agg2[ ,4:112], 1, max, na.rm=T)


person_diff_agg2 <- person_diff_agg2[ , c(1:3, 113:118)]


## for ICC.z
# calculate overall difference (mean difference across participants)
person_diff_agg.z2$difference.z_mean <- rowMeans(person_diff_agg.z2[ , 4:112])
person_diff_agg.z2$difference.z_median <- apply(person_diff_agg.z2[ ,4:112], 1, median, na.rm=T) 

# determine which participant deviates most
person_diff_agg.z2$difference.z_min_id <- apply(person_diff_agg.z2[ ,4:112], 1, which.min)
person_diff_agg.z2$difference.z_max_id <- apply(person_diff_agg.z2[ ,4:112], 1, which.max)


# calculate min and max
person_diff_agg.z2$difference.z_min <- apply(person_diff_agg.z2[ ,4:112], 1, min, na.rm=T)
person_diff_agg.z2$difference.z_max <- apply(person_diff_agg.z2[ ,4:112], 1, max, na.rm=T)

person_diff_agg.z2 <- person_diff_agg.z2[ , c(1:3, 113:118)]





# '' Calcute RMSE for Each Participant Across Replications ----------------


# '''' For ICCs -----------------------------------------------------------
# use person-level differences -> square
person_level_diff_sq2 <- person_level_diff2[ ,1:3]
person_level_diff_sq2[ , 4:112] <- (person_level_diff2[ ,4:112])^2
names(person_level_diff_sq2)[4:112] <- paste0("sq_diff_ICC_", 1:109)

### aggregate

# subset for random draws
rd2 <- person_level_diff_sq2[person_level_diff_sq2$occasions_drawn == "random", ]

# storage
RMSE_random2 <- as.data.frame(matrix(nrow=18, ncol=112))
names(RMSE_random2)[1:3] <- c("occasions_drawn", "n_occasions", "n_items")
names(RMSE_random2)[4:112] <- c(paste0("RMSE_", 1:109))


# RMSE = sqrt(sum(sq_diff_ICC)/n_replication)
sq_diff_cols <- paste0("sq_diff_ICC_", 1:109)

RMSE_random2 <- aggregate(rd2[ , sq_diff_cols], # for each participant-specific sq_diff column
                      by = rd2[ , c("occasions_drawn", "n_occasions", "n_items")], # aggregate across conditions
                      FUN = function(x) {
                        sqrt(sum(x)/5000) # by taking square root of the the summed sq_diff divided by number of replications
                      })
names(RMSE_random2)[4:112] <- c(paste0("RMSE_", 1:109))



# repeat for ordered draws -> different number of replications (i.e., 1)
# subset for ordered draws
od2 <- person_level_diff_sq2[person_level_diff_sq2$occasions_drawn == "by order", ]

# storage
RMSE_order2 <- as.data.frame(matrix(nrow=21, ncol=112))
names(RMSE_order2)[1:3] <- c("occasions_drawn", "n_occasions", "n_items")
names(RMSE_order2)[4:112] <- c(paste0("RMSE_", 1:109))



# RMSE = sqrt(sum(sq_diff_ICC)/n_replication)
sq_diff_cols <- paste0("sq_diff_ICC_", 1:109)

RMSE_order2 <- aggregate(od2[ , sq_diff_cols], # for each participant-specific sq_diff column
                        by = od2[ , c("occasions_drawn", "n_occasions", "n_items")], # aggregate across conditions
                         FUN = function(x) {
                           sqrt(sum(x)/1) # by taking square root of the the summed sq_diff divided by number of replications
                         })
names(RMSE_order2)[4:112] <- c(paste0("RMSE_", 1:109))



# combine RMSE data frames
RMSE2 <- rbind(RMSE_random2, RMSE_order2)
rm(RMSE_random2, RMSE_order2)



# remove benchmark row
# (values are correctly 0)
RMSE2 <- RMSE2[-(which(RMSE2$occasions_drawn == "by order" & RMSE2$n_occasions == 70 & RMSE2$n_items == 15)), ]



# save 
save(RMSE2, file="results/check nr of iterations/RMSE_values_per_participant.rda")


# Calculate min, mean, and max across participants
RMSE2$RMSE_min <- apply(RMSE2[ , 4:112], 1, FUN = min, na.rm = TRUE)
RMSE2$RMSE_mean <- rowMeans(RMSE2[ ,4:112], na.rm=TRUE)
RMSE2$RMSE_max <- apply(RMSE2[ , 4:112], 1, FUN = max, na.rm = TRUE)
# subset 
RMSE2 <- RMSE2[ , c(1:3, 113:115)]





# '''' For ICC.z ----------------------------------------------------------
# use person-level differences -> square
person_level_diff.z_sq2 <- person_level_diff.z2[ ,1:3]
person_level_diff.z_sq2[ , 4:112] <- (person_level_diff.z2[ ,4:112])^2
names(person_level_diff.z_sq2)[4:112] <- paste0("sq_diff_ICC.z_", 1:109)



### aggregate

# subset for random draws
rd.z2 <- person_level_diff.z_sq2[person_level_diff.z_sq2$occasions_drawn == "random", ]

# storage
RMSE.z_random2 <- as.data.frame(matrix(nrow=18, ncol=112))
names(RMSE.z_random2)[1:3] <- c("occasions_drawn", "n_occasions", "n_items")
names(RMSE.z_random2)[4:112] <- c(paste0("RMSE.z_", 1:109))


# RMSE = sqrt(sum(sq_diff_ICC.z)/n_replication)
sq_diff_cols <- paste0("sq_diff_ICC.z_", 1:109)

RMSE.z_random2 <- aggregate(rd.z2[ , sq_diff_cols], # for each participant-specific sq_diff column
                         by = rd.z2[ , c("occasions_drawn", "n_occasions", "n_items")], # aggregate across conditions
                         FUN = function(x) {
                           sqrt(sum(x)/5000) # by taking square root of the the summed sq_diff divided by number of replications
                         })
names(RMSE.z_random2)[4:112] <- c(paste0("RMSE.z_", 1:109))




# repeat for ordered draws -> different number of replications (i.e., 1)
# subset for ordered draws
od.z2 <- person_level_diff.z_sq2[person_level_diff.z_sq2$occasions_drawn == "by order", ]

# storage
RMSE.z_order2 <- as.data.frame(matrix(nrow=21, ncol=112))
names(RMSE.z_order2)[1:3] <- c("occasions_drawn", "n_occasions", "n_items")
names(RMSE.z_order2)[4:112] <- c(paste0("RMSE.z_", 1:109))


# RMSE = sqrt(sum(sq_diff_ICC.z)/n_replication)
sq_diff_cols <- paste0("sq_diff_ICC.z_", 1:109)

RMSE.z_order2 <- aggregate(od.z2[ , sq_diff_cols], # for each participant-specific sq_diff column
                           by = od.z2[ , c("occasions_drawn", "n_occasions", "n_items")], # aggregate across conditions
                           FUN = function(x) {
                             sqrt(sum(x)/1) # by taking square root of the the summed sq_diff divided by number of replications
                           })
names(RMSE.z_order2)[4:112] <- c(paste0("RMSE.z_", 1:109))



# combine RMSE data frames
RMSE.z2 <- rbind(RMSE.z_random2, RMSE.z_order2)
rm(RMSE.z_random2, RMSE.z_order2)

# remove benchmark row
# (values are correctly 0)
RMSE.z2 <- RMSE.z2[-(which(RMSE.z2$occasions_drawn == "by order" & RMSE.z2$n_occasions == 70 & RMSE.z2$n_items == 15)), ]


# save 
save(RMSE.z2, file="results/check nr of iterations/RMSE.z_values_per_participant.rda")


# Calculate min, mean, and max across participants
RMSE.z2$RMSE.z_min <- apply(RMSE.z2[ , 4:112], 1, FUN = min, na.rm = TRUE)
RMSE.z2$RMSE.z_mean <- rowMeans(RMSE.z2[ ,4:112], na.rm=TRUE)
RMSE.z2$RMSE.z_max <- apply(RMSE.z2[ , 4:112], 1, FUN = max, na.rm = TRUE)
# subset 
RMSE.z2 <- RMSE.z2[ , c(1:3, 113:115)]



agg2 <- aggregate_results(res2,
                         outcomes = c('N_valid_ICC.z',
                                      'cor_ICC', 'cor_ICC.z',
                                      'rel', 'N_rel',
                                      'sd_ICC', 'sd_ICC.z',
                                      'negICC', 'percnegICC',
                                      'estimationProbNeg', 'estimationProbPos'),
                         rel_outcomes = c('cor_ICC', 'cor_ICC.z'),
                         abs_outcomes = c('N_valid_ICC.z',
                                          'rel', 'N_rel',
                                          'sd_ICC', 'sd_ICC.z',
                                          'negICC', 'percnegICC',
                                          'estimationProbNeg', 'estimationProbPos'),
                         groupwise = FALSE,
                         group_var = NULL)


# merge difference to agg
agg_res <- list(person_diff_agg2)
names(agg_res) <- "agg_res"
agg2$person_diff <- agg_res

agg_res <- list(person_diff_agg.z2)
names(agg_res) <- "agg_res"
agg2$person_diff.z <- agg_res



# merge RMSE to agg
agg_res <- list(RMSE2) # should be nested as other outcomes so that function works
names(agg_res) <- "agg_res"
agg2$RMSE_ICC <- agg_res


agg_res <- list(RMSE.z2) # should be nested as other outcomes so that function works
names(agg_res) <- "agg_res"
agg2$RMSE_ICC.z <- agg_res


save(agg2, file = "results/check nr of iterations/aggregated_results.rda")





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
# [1] LC_COLLATE=German_Germany.utf8  LC_CTYPE=German_Germany.utf8   
# [3] LC_MONETARY=German_Germany.utf8 LC_NUMERIC=C                   
# [5] LC_TIME=German_Germany.utf8    
# 
# time zone: Europe/Berlin
# tzcode source: internal
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#  [1] lubridate_1.9.4 forcats_1.0.0   stringr_1.5.1   dplyr_1.1.4     purrr_1.1.0    
#  [6] readr_2.1.5     tidyr_1.3.1     tibble_3.3.0    ggplot2_3.5.2   tidyverse_2.0.0
# 
# loaded via a namespace (and not attached):
#  [1] vctrs_0.6.5        cli_3.6.5          rlang_1.1.6        stringi_1.8.7     
#  [5] generics_0.1.4     labeling_0.4.3     glue_1.8.0         hms_1.1.3         
#  [9] scales_1.4.0       grid_4.5.1         tzdb_0.5.0         lifecycle_1.0.4   
# [13] compiler_4.5.1     RColorBrewer_1.1-3 timechange_0.3.0   pkgconfig_2.0.3   
# [17] rstudioapi_0.17.1  farver_2.1.2       R6_2.6.1           tidyselect_1.2.1  
# [21] pillar_1.11.0      magrittr_2.0.3     tools_4.5.1        withr_3.0.2       
# [25] gtable_0.3.6   