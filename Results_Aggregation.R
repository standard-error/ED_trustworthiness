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



# Determine Which Participants Deviate Most from Benchmark ----------------

## for ICC
length(unique(res$id_min_diff_ICC)) # 98 different participants differed most (in negative direction)
# in at least one replication

length(unique(res$id_max_diff_ICC)) # 105 different participants differed most (in negative direction)
# in at least one replication

table(res$id_min_diff_ICC) # some differed most in only few replications, some differed most in >40,000 replications

table(res$id_max_diff_ICC) # some differed most in only few replications, some differed most in >20,000 replications


# find replication with maximum deviation for each condition -> determine corresponding person
## i.e., find the participants who correspond to most extreme deviation across all replications per condition
# negative deviations
diff_min <- do.call(data.frame,
                aggregate(min_diff_ICC ~ occasions_drawn + n_occasions + n_items,
                  data=res,
                  FUN = function(x) {
                    min(x)
                  })
)

# now merge minimum with corresponding replication and ID variable
diff_min <- merge(diff_min, res[ , c("occasions_drawn", "n_occasions", "n_items", "min_diff_ICC", "n_iteration", "id_min_diff_ICC")], by=c("occasions_drawn", "n_occasions", "n_items", "min_diff_ICC"), all.y=FALSE)


colnames(diff_min) <- c("occasions_drawn", "n_occasions", "n_items", "min_diff_ICC_across_repl", "n_iteration", "id_min_diff_ICC_across_repl")

table(diff_min$id_min_diff_ICC_across_repl) # in most conditions, participant 210 has the most extreme negative deviation across replications 


## positive deviations
diff_max <- do.call(data.frame,
                    aggregate(max_diff_ICC ~ occasions_drawn + n_occasions + n_items,
                              data=res,
                              FUN = function(x) {
                                max(x)
                              })
)

# now merge maximum with corresponding replication and ID variable
diff_max <- merge(diff_max, res[ , c("occasions_drawn", "n_occasions", "n_items", "max_diff_ICC", "n_iteration", "id_max_diff_ICC")], by=c("occasions_drawn", "n_occasions", "n_items", "max_diff_ICC"), all.y=FALSE)


colnames(diff_max) <- c("occasions_drawn", "n_occasions", "n_items", "max_diff_ICC_across_repl", "n_iteration", "id_max_diff_ICC_across_repl")

table(diff_max$id_max_diff_ICC_across_repl) # in most conditions, participant 198 (random) and 114 (ordered) have the most extreme positive deviation across replications





## for ICC.z
length(unique(res$id_min_diff_ICC.z)) # 106 different participants differed most (in negative direction)
# in at least one replication

length(unique(res$id_max_diff_ICC.z)) # 109 different participants differed most (in negative direction)
# in at least one replication

# find replication with maximum deviation for each condition -> determine corresponding person
## i.e., find the participants who correspond to most extreme deviation across all replications per condition
# negative deviations
diff_min.z <- do.call(data.frame,
                    aggregate(min_diff_ICC.z ~ occasions_drawn + n_occasions + n_items,
                              data=res,
                              FUN = function(x) {
                                min(x)
                              })
)

# now merge minimum with corresponding replication and ID variable
diff_min.z <- merge(diff_min.z, res[ , c("occasions_drawn", "n_occasions", "n_items", "min_diff_ICC.z", "n_iteration", "id_min_diff_ICC.z")], by=c("occasions_drawn", "n_occasions", "n_items", "min_diff_ICC.z"), all.y=FALSE)


colnames(diff_min.z) <- c("occasions_drawn", "n_occasions", "n_items", "min_diff_ICC.z_across_repl", "n_iteration", "id_min_diff_ICC.z_across_repl")

table(diff_min.z$id_min_diff_ICC.z_across_repl) # in most conditions, participant 210 has the most extreme negative deviation across replications 


## positive deviations
diff_max.z <- do.call(data.frame,
                    aggregate(max_diff_ICC.z ~ occasions_drawn + n_occasions + n_items,
                              data=res,
                              FUN = function(x) {
                                max(x)
                              })
)

# now merge maximum with corresponding replication and ID variable
diff_max.z <- merge(diff_max.z, res[ , c("occasions_drawn", "n_occasions", "n_items", "max_diff_ICC.z", "n_iteration", "id_max_diff_ICC.z")], by=c("occasions_drawn", "n_occasions", "n_items", "max_diff_ICC.z"), all.y=FALSE)


colnames(diff_max.z) <- c("occasions_drawn", "n_occasions", "n_items", "max_diff_ICC.z_across_repl", "n_iteration", "id_max_diff_ICC.z_across_repl")

table(diff_max.z$id_max_diff_ICC.z_across_repl) # in most conditions, participant 198 (random) and 114 (ordered) have the most extreme positive deviation across replications


# drop n_iteration from each result data frame
diff_min$n_iteration <- NULL
diff_max$n_iteration <- NULL
diff_min.z$n_iteration <- NULL
diff_max.z$n_iteration <- NULL

## merge all results
diff_min_max_ids <- merge(diff_min, diff_max, by=c("occasions_drawn", "n_occasions", "n_items"))
diff_min_max_ids <- merge(diff_min_max_ids, diff_min.z, by=c("occasions_drawn", "n_occasions", "n_items"))
diff_min_max_ids <- merge(diff_min_max_ids, diff_max.z, by=c("occasions_drawn", "n_occasions", "n_items"))

# save results
save(diff_min_max_ids, file="results/min_max_person_level_difference_and_id_per_condition_across_replications.rda")


table(diff_min_max_ids$id_min_diff_ICC_across_repl)
table(diff_min_max_ids$id_max_diff_ICC_across_repl)

table(diff_min_max_ids$id_min_diff_ICC.z_across_repl)
table(diff_min_max_ids$id_max_diff_ICC.z_across_repl)

# for random draws only
sub <- diff_min_max_ids[diff_min_max_ids$occasions_drawn == "random", ]

table(sub$id_min_diff_ICC_across_repl)
table(sub$id_max_diff_ICC_across_repl)

table(sub$id_min_diff_ICC.z_across_repl)
table(sub$id_max_diff_ICC.z_across_repl)


# Calculate %negICC -------------------------------------------------------
# calculate proportion of negative ICCs
res$percnegICC <- res$negICC / 109 # divide by total number of participants



# Calcute RMSE for Each Participant Across Replications -------------------


# '' For ICCs -------------------------------------------------------------
# extract squared differences for each replication and each condition per participant
part_dat <- data.frame(matrix(nrow=90021, ncol=112))
part_dat[ , 1:3] <- res[ , 2:4]
names(part_dat) <- c("n_occasions", "occasions_drawn", "n_items", paste0("sq_diff_ICC_", 1:109))

sq_matrix <- do.call(rbind, res$sq_diff_ICC) # extract the 109 sq_diff_ICC values per row (replication) and bind them
# -> matrix of 109 participants (columns) and their values in each replication (rows)

# bind with part_dat
part_dat[ , 4:112] <- sq_matrix


# save
save(part_dat, file="results/squared_errors_per_replication_and_participant.rda")


### aggregate

# subset for random draws
rd <- part_dat[part_dat$occasions_drawn == "random", ]

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
od <- part_dat[part_dat$occasions_drawn == "by order", ]

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
# extract squared differences for each replication and each condition per participant
part_dat.z <- data.frame(matrix(nrow=90021, ncol=112))
part_dat.z[ , 1:3] <- res[ , 2:4]
names(part_dat.z) <- c("n_occasions", "occasions_drawn", "n_items", paste0("sq_diff_ICC.z_", 1:109))


sq_matrix.z <- do.call(rbind, res$sq_diff_ICC.z) # extract the 109 sq_diff_ICC.z values per row (replication) and bind them
# -> matrix of 109 participants (columns) and their values in each replication (rows)

# bind with part_dat
part_dat.z[ , 4:112] <- sq_matrix.z


# save
save(part_dat.z, file="results/squared_errors.z_per_replication_and_participant.rda")



### aggregate

# subset for random draws
rd.z <- part_dat.z[part_dat.z$occasions_drawn == "random", ]

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
od.z <- part_dat.z[part_dat.z$occasions_drawn == "by order", ]

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
                         outcomes = c('min_diff_ICC', 'mean_diff_ICC', 'max_diff_ICC',
                                      'N_valid_ICC.z',
                                      'min_diff_ICC.z', 'mean_diff_ICC.z', 'max_diff_ICC.z',
                                      'cor_ICC', 'cor_ICC.z',
                                      'rel', 'N_rel',
                                      'sd_ICC', 'sd_ICC.z',
                                      'negICC', 'percnegICC',
                                      'estimationProbNeg', 'estimationProbPos'),
                         rel_outcomes = c('min_diff_ICC', 'mean_diff_ICC', 'max_diff_ICC',
                                          'min_diff_ICC.z', 'mean_diff_ICC.z', 'max_diff_ICC.z',
                                          'cor_ICC', 'cor_ICC.z'),
                         abs_outcomes = c('N_valid_ICC.z',
                                          'rel', 'N_rel',
                                          'sd_ICC', 'sd_ICC.z',
                                          'negICC', 'percnegICC',
                                          'estimationProbNeg', 'estimationProbPos'),
                         groupwise = FALSE,
                         group_var = NULL)

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


## for "bias" (i.e., difference in ICCs)
# -> "mean of generic statistic G"

# calculate the overall across-replicate mean of the single-replicate mean difference
# i.e., each iteration reports a mean difference (across) participants, and these
# mean differences are averaged across iterations
# also calculate variance of the mean difference across iterations

max(rd$n_iteration) # 5000 iterations

MCSE <- do.call(data.frame, aggregate(mean_diff_ICC ~ n_occasions + n_items, data = rd, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/5000 ) )^2 )) / (5000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/5000 ) )^2 )) / (5000 - 1) ) / 5000 )) 
})
)

names(MCSE) <- c("n_occasions", "n_items", "diff_ICC_sim_mean", "diff_ICC_sim_var", "diff_ICC_MCSE")



## for bias.z (i.e., difference in ICC.z)

MCSE2 <- do.call(data.frame, aggregate(mean_diff_ICC.z ~ n_occasions + n_items, data = rd, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/5000 ) )^2 )) / (5000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/5000 ) )^2 )) / (5000 - 1) ) / 5000 )) 
})
)

names(MCSE2) <- c("n_occasions", "n_items", "diff_ICC.z_sim_mean", "diff_ICC.z_sim_var", "diff_ICC.z_MCSE")


## for correlations (ICC)
# mean of correlations as performance measure

MCSE3 <- do.call(data.frame, aggregate(cor_ICC ~ n_occasions + n_items, data = rd, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/5000 ) )^2 )) / (5000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/5000 ) )^2 )) / (5000 - 1) ) / 5000 )) 
})
)

names(MCSE3) <- c("n_occasions", "n_items", "cor_ICC_sim_mean", "cor_ICC_sim_var", "cor_ICC_MCSE")


## for correlations (ICC.z)
# mean of correlations as performance measure

MCSE4 <- do.call(data.frame, aggregate(cor_ICC.z ~ n_occasions + n_items, data = rd, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/5000 ) )^2 )) / (5000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/5000 ) )^2 )) / (5000 - 1) ) / 5000 )) 
})
)

names(MCSE4) <- c("n_occasions", "n_items", "cor_ICC.z_sim_mean", "cor_ICC.z_sim_var", "cor_ICC.z_MCSE")



## for reliability
# -> mean of generic statistic G

MCSE5 <- do.call(data.frame, aggregate(rel ~ n_occasions + n_items, data = rd, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/5000 ) )^2 )) / (5000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/5000 ) )^2 )) / (5000 - 1) ) / 5000 )) 
})
)

names(MCSE5) <- c("n_occasions", "n_items", "rel_sim_mean", "rel_sim_var", "rel_MCSE")


## for SD (ICC)
# we have one SD for each replicate -> performance measure = mean of SD across replicates
# use formula for mean of generic statistic G
MCSE6 <- do.call(data.frame, aggregate(sd_ICC ~ n_occasions + n_items, data = rd, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/5000 ) )^2 )) / (5000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/5000 ) )^2 )) / (5000 - 1) ) / 5000 )) 
})
)

names(MCSE6) <- c("n_occasions", "n_items", "sd_ICC_sim_mean", "sd_ICC_sim_var", "sd_ICC_MCSE")


## for SD (ICC.z)
# we have one SD for each replicate -> performance measure = mean of SD across replicates
# use formula for mean of generic statistic G
MCSE7 <- do.call(data.frame, aggregate(sd_ICC.z ~ n_occasions + n_items, data = rd, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/5000 ) )^2 )) / (5000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/5000 ) )^2 )) / (5000 - 1) ) / 5000 )) 
})
)

names(MCSE7) <- c("n_occasions", "n_items", "sd_ICC.z_sim_mean", "sd_ICC.z_sim_var", "sd_ICC.z_MCSE")


## for % negICC
# performance measure: mean of generic statistic G
MCSE8 <- do.call(data.frame, aggregate(percnegICC ~ n_occasions + n_items, data = rd, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/5000 ) )^2 )) / (5000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/5000 ) )^2 )) / (5000 - 1) ) / 5000 )) 
})
)

names(MCSE8) <- c("n_occasions", "n_items", "percnegICC_sim_mean", "percnegICC_sim_var", "percnegICC_MCSE")


## for N_rel
# performance measure: mean of generic statistic G

MCSE9 <- do.call(data.frame, aggregate(N_rel ~ n_occasions + n_items, data = rd, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/5000 ) )^2 )) / (5000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/5000 ) )^2 )) / (5000 - 1) ) / 5000 )) 
})
)

names(MCSE9) <- c("n_occasions", "n_items", "N_rel_sim_mean", "N_rel_sim_var", "N_rel_MCSE")

## for estimation problems, no MCSE can be calculated because there is zero variance
range(rd$estimationProbNeg)
range(rd$estimationProbPos)


# combine
MCSE <- merge(MCSE, MCSE2, by = c("n_occasions", "n_items"))
MCSE <- merge(MCSE, MCSE3, by = c("n_occasions", "n_items"))
MCSE <- merge(MCSE, MCSE4, by = c("n_occasions", "n_items"))
MCSE <- merge(MCSE, MCSE5, by = c("n_occasions", "n_items"))
MCSE <- merge(MCSE, MCSE6, by = c("n_occasions", "n_items"))
MCSE <- merge(MCSE, MCSE7, by = c("n_occasions", "n_items"))
MCSE <- merge(MCSE, MCSE8, by = c("n_occasions", "n_items"))
MCSE <- merge(MCSE, MCSE9, by = c("n_occasions", "n_items"))

MCSE
names(MCSE)



# for RMSE per participant
# for each participant, calculate sampling variance of squared errors
# and mean of squared errors (across replications per condition)
# -> calculate MCSE per participant and condition

# for formula, see Siepe et al. (2024), Table 3, formula for MCSE of RMSE
# MSE hat = expected value for squared errors = mean of squared errors across replications

# use data from random draws only
part_dat.rd <- part_dat[which(part_dat$occasions_drawn == "random"), ]

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

MCSE_RMSE[] <- aggregate(part_dat.rd[ , sq_diff_cols],
                  by = part_dat.rd[ , c("n_occasions", "n_items")],
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
part_dat.z.rd <- part_dat.z[which(part_dat.z$occasions_drawn == "random"), ]

MCSE_RMSE.z <- data.frame(matrix(ncol=111, nrow=18))
names(MCSE_RMSE.z) <- c("n_occasions", "n_items", paste0("MCSE_RMSE.z_", 1:109))

sq_diff_cols <- paste0("sq_diff_ICC.z_", 1:109)

MCSE_RMSE.z[] <- aggregate(part_dat.z.rd[ , sq_diff_cols],
                           by = part_dat.z.rd[ , c("n_occasions", "n_items")],
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
MCSE <- MCSE[ , c('n_occasions', 'n_items', 'diff_ICC_MCSE','diff_ICC.z_MCSE','cor_ICC_MCSE', 'cor_ICC.z_MCSE',
                  'rel_MCSE', 'sd_ICC_MCSE','sd_ICC.z_MCSE', 'percnegICC_MCSE', 'N_rel_MCSE', 'MCSE_RMSE_min',
                  'MCSE_RMSE_mean', 'MCSE_RMSE_max', 'MCSE_RMSE.z_min', 'MCSE_RMSE.z_mean', 'MCSE_RMSE.z_max')]
write.csv(MCSE, "results/MCSE_table.csv", row.names = F)



rm(list=ls())




# Check Sufficient Number of Iterations  ----------------------------------
source("functions/function_aggregate_results.R")

load("results/check nr of iterations/sim_results.rda")



# Calculate %negICC
# calculate proportion of negative ICCs
res2$percnegICC <- res2$negICC / 109 # divide by total number of participants


# Calcute RMSE for Each Participant Across Replications 

# '' For ICCs 
# extract squared differences for each replication and each condition per participant
part_dat2 <- data.frame(matrix(nrow=90021, ncol=112))
part_dat2[ , 1:3] <- res2[ , 2:4]
names(part_dat2) <- c("n_occasions", "occasions_drawn", "n_items", paste0("sq_diff_ICC_", 1:109))

sq_matrix2 <- do.call(rbind, res2$sq_diff_ICC) # extract the 109 sq_diff_ICC values per row (replication) and bind them
# -> matrix of 109 participants (columns) and their values in each replication (rows)

# bind with part_dat
part_dat2[ , 4:112] <- sq_matrix2


# save
save(part_dat2, file="results/check nr of iterations/squared_errors_per_replication_and_participant.rda")


### aggregate

# subset for random draws
rd2 <- part_dat2[part_dat2$occasions_drawn == "random", ]

# storage
RMSE2_random <- as.data.frame(matrix(nrow=18, ncol=112))
names(RMSE2_random)[1:3] <- c("occasions_drawn", "n_occasions", "n_items")
names(RMSE2_random)[4:112] <- c(paste0("RMSE_", 1:109))


# RMSE = sqrt(sum(sq_diff_ICC)/n_replication)
sq_diff_cols <- paste0("sq_diff_ICC_", 1:109)

RMSE2_random <- aggregate(rd2[ , sq_diff_cols], # for each participant-specific sq_diff column
                          by = rd2[ , c("occasions_drawn", "n_occasions", "n_items")], # aggregate across conditions
                          FUN = function(x) {
                            sqrt(sum(x)/5000) # by taking square root of the the summed sq_diff divided by number of replications
                         })
names(RMSE2_random)[4:112] <- c(paste0("RMSE_", 1:109))




# repeat for ordered draws -> different number of replications (i.e., 1)
# subset for ordered draws
od2 <- part_dat2[part_dat2$occasions_drawn == "by order", ]

# storage
RMSE2_order <- as.data.frame(matrix(nrow=21, ncol=112))
names(RMSE2_order)[1:3] <- c("occasions_drawn", "n_occasions", "n_items")
names(RMSE2_order)[4:112] <- c(paste0("RMSE_", 1:109))

# RMSE = sqrt(sum(sq_diff_ICC)/n_replication)
sq_diff_cols <- paste0("sq_diff_ICC_", 1:109)

RMSE2_order <- aggregate(od2[ , sq_diff_cols], # for each participant-specific sq_diff column
                         by = od2[ , c("occasions_drawn", "n_occasions", "n_items")], # aggregate across conditions
                         FUN = function(x) {
                          sqrt(sum(x)/1) # by taking square root of the the summed sq_diff divided by number of replications
                        })
names(RMSE2_order)[4:112] <- c(paste0("RMSE_", 1:109))



# combine RMSE data frames
RMSE2 <- rbind(RMSE2_random, RMSE2_order)
rm(RMSE2_random, RMSE2_order)

# remove benchmark row
# (values are correctly 0)
RMSE2 <- RMSE2[-(which(RMSE2$occasions_drawn == "by order" & RMSE2$n_occasions == 70 & RMSE2$n_items == 15)), ]


# save 
save(RMSE2, file="results/check nr of iterations/RMSE_values_per_participant.rda")


# Calculate min, mean, and max across participants
RMSE2$RMSE_mean <- rowMeans(RMSE2[ ,4:112], na.rm=TRUE)
RMSE2$RMSE_min <- apply(RMSE2[ , 4:112], 1, FUN = min, na.rm = TRUE)
RMSE2$RMSE_max <- apply(RMSE2[ , 4:112], 1, FUN = max, na.rm = TRUE)
# subset 
RMSE2 <- RMSE2[ , c(1:3, 113:115)]





# '' For ICC.z 
# extract squared differences for each replication and each condition per participant
part_dat.z2 <- data.frame(matrix(nrow=90021, ncol=112))
part_dat.z2[ , 1:3] <- res2[ , 2:4]
names(part_dat.z2) <- c("n_occasions", "occasions_drawn", "n_items", paste0("sq_diff_ICC.z_", 1:109))


sq_matrix.z2 <- do.call(rbind, res2$sq_diff_ICC.z) # extract the 109 sq_diff_ICC.z values per row (replication) and bind them
# -> matrix of 109 participants (columns) and their values in each replication (rows)

# bind with part_dat
part_dat.z2[ , 4:112] <- sq_matrix.z2


# save
save(part_dat.z2, file="results/check nr of iterations/squared_errors.z_per_replication_and_participant.rda")


### aggregate

# subset for random draws
rd.z2 <- part_dat.z2[part_dat.z2$occasions_drawn == "random", ]

# storage
RMSE.z_random2 <- as.data.frame(matrix(nrow=18, ncol=112))
names(RMSE.z_random2)[1:3] <- c("occasions_drawn", "n_occasions", "n_items")
names(RMSE.z_random2)[4:112] <- c(paste0("RMSE.z_", 1:109))


# RMSE = sqrt(sum(sq_diff_ICC)/n_replication)
sq_diff_cols <- paste0("sq_diff_ICC.z_", 1:109)

RMSE.z_random2 <- aggregate(rd.z2[ , sq_diff_cols], # for each participant-specific sq_diff column
                            by = rd.z2[ , c("occasions_drawn", "n_occasions", "n_items")], # aggregate across conditions
                            FUN = function(x) {
                              sqrt(sum(x)/5000) # by taking square root of the the summed sq_diff divided by number of replications
                          })
names(RMSE.z_random2)[4:112] <- c(paste0("RMSE.z_", 1:109))



# repeat for ordered draws -> different number of replications (i.e., 1)
# subset for ordered draws
od.z2 <- part_dat.z2[part_dat.z2$occasions_drawn == "by order", ]

# storage
RMSE.z_order2 <- as.data.frame(matrix(nrow=21, ncol=112))
names(RMSE.z_order2)[1:3] <- c("occasions_drawn", "n_occasions", "n_items")
names(RMSE.z_order2)[4:112] <- c(paste0("RMSE.z_", 1:109))



# RMSE = sqrt(sum(sq_diff_ICC)/n_replication)
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
RMSE.z2$RMSE.z_mean <- rowMeans(RMSE.z2[ ,4:112], na.rm=TRUE)
RMSE.z2$RMSE.z_min <- apply(RMSE.z2[ , 4:112], 1, FUN = min, na.rm = TRUE)
RMSE.z2$RMSE.z_max <- apply(RMSE.z2[ , 4:112], 1, FUN = max, na.rm = TRUE)
# subset 
RMSE.z2 <- RMSE.z2[ , c(1:3, 113:115)]



agg2 <- aggregate_results(res2,
                         outcomes = c('min_diff_ICC', 'mean_diff_ICC', 'max_diff_ICC',
                                      'N_valid_ICC.z',
                                      'min_diff_ICC.z', 'mean_diff_ICC.z', 'max_diff_ICC.z',
                                      'cor_ICC', 'cor_ICC.z',
                                      'rel', 'N_rel',
                                      'sd_ICC', 'sd_ICC.z',
                                      'negICC', 'percnegICC',
                                      'estimationProbNeg', 'estimationProbPos'),
                         rel_outcomes = c('min_diff_ICC', 'mean_diff_ICC', 'max_diff_ICC',
                                          'min_diff_ICC.z', 'mean_diff_ICC.z', 'max_diff_ICC.z',
                                          'cor_ICC', 'cor_ICC.z'),
                         abs_outcomes = c('N_valid_ICC.z',
                                          'rel', 'N_rel',
                                          'sd_ICC', 'sd_ICC.z',
                                          'negICC', 'percnegICC',
                                          'estimationProbNeg', 'estimationProbPos'),
                         groupwise = FALSE,
                         group_var = NULL)

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
# [1] LC_COLLATE=German_Germany.utf8  LC_CTYPE=German_Germany.utf8    LC_MONETARY=German_Germany.utf8
# [4] LC_NUMERIC=C                    LC_TIME=German_Germany.utf8    
# 
# time zone: Europe/Berlin
# tzcode source: internal
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# loaded via a namespace (and not attached):
# [1] compiler_4.5.1    tools_4.5.1       rstudioapi_0.17.1