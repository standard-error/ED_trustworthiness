###################################################################
#####    Estimating trait negative emotion differentiation:   #####
#####        How many measurement occasions and               #####
#####              emotion items are needed?                  #####
###################################################################

###################################################################
#####                Results Aggregation                      #####
###################################################################



# Load Simulation Results -------------------------------------------------
load("results/sim_results_whole_data_set_Study1.rda")
load("results/sim_results_subgroups_Study1.rda")



# Check Missings ----------------------------------------------------------
any(is.na(res))
any(is.na(res_group))



# Source Function ---------------------------------------------------------
source("functions/function_aggregate_results.R")




# Aggregate Results -------------------------------------------------------
agg <- aggregate_results(res,
                         outcomes = c('min_diff_ICC', 'mean_diff_ICC', 'max_diff_ICC',
                                      'N_valid_ICC.z',
                                      'min_diff_ICC.z', 'mean_diff_ICC.z', 'max_diff_ICC.z',
                                      'cor_ICC', 'cor_ICC.z',
                                      'RMSE_ICC', 'RMSE_ICC.z',
                                      'rel', 'N_rel',
                                      'sd_ICC', 'sd_ICC.z',
                                      'negICC', 'estimationProbNeg', 'estimationProbPos'),
                         rel_outcomes = c('min_diff_ICC', 'mean_diff_ICC', 'max_diff_ICC',
                                          'min_diff_ICC.z', 'mean_diff_ICC.z', 'max_diff_ICC.z',
                                          'cor_ICC', 'cor_ICC.z',
                                          'RMSE_ICC', 'RMSE_ICC.z'),
                         abs_outcomes = c('N_valid_ICC.z',
                                          'rel', 'N_rel',
                                          'sd_ICC', 'sd_ICC.z',
                                          'negICC', 'estimationProbNeg', 'estimationProbPos'),
                         groupwise = FALSE,
                         group_var = NULL)



agg_grp <- aggregate_results(res_group,
                             outcomes = c('min_diff_ICC', 'mean_diff_ICC', 'max_diff_ICC',
                                         'N_valid_ICC.z',
                                         'min_diff_ICC.z', 'mean_diff_ICC.z', 'max_diff_ICC.z',
                                         'cor_ICC', 'cor_ICC.z',
                                         'RMSE_ICC', 'RMSE_ICC.z',
                                         'rel', 'N_rel',
                                         'sd_ICC', 'sd_ICC.z',
                                         'negICC', 'estimationProbNeg', 'estimationProbPos'),
                            rel_outcomes = c('min_diff_ICC', 'mean_diff_ICC', 'max_diff_ICC',
                                             'min_diff_ICC.z', 'mean_diff_ICC.z', 'max_diff_ICC.z',
                                             'cor_ICC', 'cor_ICC.z',
                                             'RMSE_ICC', 'RMSE_ICC.z'),
                            abs_outcomes = c('N_valid_ICC.z',
                                             'rel', 'N_rel',
                                             'sd_ICC', 'sd_ICC.z',
                                             'negICC', 'estimationProbNeg', 'estimationProbPos'),
                            groupwise = TRUE,
                            group_var = "group")




# Save Aggregated Results -------------------------------------------------
save(agg, file = "results/aggregated_whole_data_set_Study1.rda")
save(agg_grp, file = "results/aggregated_subgroups_Study1.rda")




# Calculate Monte Carlo Standard Error ------------------------------------
# for formulas, see Siepe et al. (2024), doi: 10.1037/met0000695



# '' For Whole Data Set ---------------------------------------------------
# use subset with random draws (ordered draws are not independent and there is only one simulation
# run for each condition -> no variance = no MCSE)
rd <- res[which(res$occasions_drawn == "random"),]


## for "bias" (i.e., difference in ICCs)
# -> "mean of generic statistic G"

# calculate the overall across-replicate mean of the single-replicate mean difference
# i.e., each iteration reports a mean difference (across) participants, and these
# mean differences are averaged across iterations
# also calculate variance of the mean difference across iterations

max(rd$n_iteration) # 1000 iterations

MCSE <- do.call(data.frame, aggregate(mean_diff_ICC ~ n_occasions + n_items, data = rd, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1) ) / 1000 )) 
})
)

names(MCSE) <- c("n_occasions", "n_items", "diff_ICC_sim_mean", "diff_ICC_sim_var", "diff_ICC_MCSE")



## for bias.z (i.e., difference in ICC.z)

MCSE2 <- do.call(data.frame, aggregate(mean_diff_ICC.z ~ n_occasions + n_items, data = rd, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1) ) / 1000 )) 
})
)

names(MCSE2) <- c("n_occasions", "n_items", "diff_ICC.z_sim_mean", "diff_ICC.z_sim_var", "diff_ICC.z_MCSE")


## for correlations (ICC)
# mean of correlations as performance measure

MCSE3 <- do.call(data.frame, aggregate(cor_ICC ~ n_occasions + n_items, data = rd, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1) ) / 1000 )) 
})
)

names(MCSE3) <- c("n_occasions", "n_items", "cor_ICC_sim_mean", "cor_ICC_sim_var", "cor_ICC_MCSE")


## for correlations (ICC.z)
# mean of correlations as performance measure

MCSE4 <- do.call(data.frame, aggregate(cor_ICC.z ~ n_occasions + n_items, data = rd, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1) ) / 1000 )) 
})
)

names(MCSE4) <- c("n_occasions", "n_items", "cor_ICC.z_sim_mean", "cor_ICC.z_sim_var", "cor_ICC.z_MCSE")


## for RMSE (ICC)
# we have RMSE for each simulation run (because we have multiple participants)
# -> therefore, the performance measure across iterations is not RMSE, but mean of RMSE
# -> use formula for mean of generic statistic G (see Siepe et al., 2024)

MCSE5 <- do.call(data.frame, aggregate(RMSE_ICC ~ n_occasions + n_items, data = rd, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1) ) / 1000 )) 
})
)

names(MCSE5) <- c("n_occasions", "n_items", "RMSE_ICC_sim_mean", "RMSE_ICC_sim_var", "RMSE_ICC_MCSE")

## for RMSE (ICC.z)
# we have RMSE for each simulation run (because we have multiple participants)
# -> therefore, the performance measure across iterations is not RMSE, but mean of RMSE
# -> use formula for mean of generic statistic G (see Siepe et al., 2024)

MCSE6 <- do.call(data.frame, aggregate(RMSE_ICC.z ~ n_occasions + n_items, data = rd, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1) ) / 1000 )) 
})
)

names(MCSE6) <- c("n_occasions", "n_items", "RMSE_ICC.z_sim_mean", "RMSE_ICC.z_sim_var", "RMSE_ICC.z_MCSE")


## for reliability
# -> mean of generic statistic G

MCSE7 <- do.call(data.frame, aggregate(rel ~ n_occasions + n_items, data = rd, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1) ) / 1000 )) 
})
)

names(MCSE7) <- c("n_occasions", "n_items", "rel_sim_mean", "rel_sim_var", "rel_MCSE")


## for SD (ICC)
# we have one SD for each replicate -> performance measure = mean of SD across replicates
# use formula for mean of generic statistic G
MCSE8 <- do.call(data.frame, aggregate(sd_ICC ~ n_occasions + n_items, data = rd, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1) ) / 1000 )) 
})
)

names(MCSE8) <- c("n_occasions", "n_items", "sd_ICC_sim_mean", "sd_ICC_sim_var", "sd_ICC_MCSE")


## for SD (ICC.z)
# we have one SD for each replicate -> performance measure = mean of SD across replicates
# use formula for mean of generic statistic G
MCSE9 <- do.call(data.frame, aggregate(sd_ICC.z ~ n_occasions + n_items, data = rd, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1) ) / 1000 )) 
})
)

names(MCSE9) <- c("n_occasions", "n_items", "sd_ICC.z_sim_mean", "sd_ICC.z_sim_var", "sd_ICC.z_MCSE")


## for negICC
# performance measure: mean of generic statistic G
MCSE10 <- do.call(data.frame, aggregate(negICC ~ n_occasions + n_items, data = rd, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1) ) / 1000 )) 
})
)

names(MCSE10) <- c("n_occasions", "n_items", "negICC_sim_mean", "negICC_sim_var", "negICC_MCSE")


## for N_rel
# performance measure: mean of generic statistic G

MCSE11 <- do.call(data.frame, aggregate(N_rel ~ n_occasions + n_items, data = rd, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1) ) / 1000 )) 
})
)

names(MCSE11) <- c("n_occasions", "n_items", "N_rel_sim_mean", "N_rel_sim_var", "N_rel_MCSE")

## for estimation problems, no MCSE can be calculated because there is zero variance


# combine
MCSE <- merge(MCSE, MCSE2, by = c("n_occasions", "n_items"))
MCSE <- merge(MCSE, MCSE3, by = c("n_occasions", "n_items"))
MCSE <- merge(MCSE, MCSE4, by = c("n_occasions", "n_items"))
MCSE <- merge(MCSE, MCSE5, by = c("n_occasions", "n_items"))
MCSE <- merge(MCSE, MCSE6, by = c("n_occasions", "n_items"))
MCSE <- merge(MCSE, MCSE7, by = c("n_occasions", "n_items"))
MCSE <- merge(MCSE, MCSE8, by = c("n_occasions", "n_items"))
MCSE <- merge(MCSE, MCSE9, by = c("n_occasions", "n_items"))
MCSE <- merge(MCSE, MCSE10, by = c("n_occasions", "n_items"))
MCSE <- merge(MCSE, MCSE11, by = c("n_occasions", "n_items"))

MCSE
names(MCSE)


# round and save MCSE as csv
# round to 3 decimals in this case
MCSE[3:35] <- round(MCSE[3:35], 3)
MCSE <- MCSE[order(MCSE$n_occasions, MCSE$n_items), ]
write.csv(MCSE, "results/MCSE_table_whole_data_set_Study1.csv", row.names = F)



# '' For Group-Wise Analysis ----------------------------------------------


# use subset with random draws (ordered draws are not independent and there is only one simulation
# run for each condition -> no variance = no MCSE)
rd_grp <- res_group[which(res_group$occasions_drawn == "random"),]


## for "bias" (i.e., difference in ICCs)
# -> "mean of generic statistic G"

# calculate the overall across-replicate mean of the single-replicate mean difference
# i.e., each iteration reports a mean difference (across) participants, and these
# mean differences are averaged across iterations
# also calculate variance of the mean difference across iterations

max(rd_grp$n_iteration) # 1000 iterations

MCSE <- do.call(data.frame, aggregate(mean_diff_ICC ~ group + n_occasions + n_items, data = rd_grp, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1) ) / 1000 )) 
})
)

names(MCSE) <- c("group", "n_occasions", "n_items", "diff_ICC_sim_mean", "diff_ICC_sim_var", "diff_ICC_MCSE")



## for bias.z (i.e., difference in ICC.z)

MCSE2 <- do.call(data.frame, aggregate(mean_diff_ICC.z ~ group + n_occasions + n_items, data = rd_grp, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1) ) / 1000 )) 
})
)

names(MCSE2) <- c("group", "n_occasions", "n_items", "diff_ICC.z_sim_mean", "diff_ICC.z_sim_var", "diff_ICC.z_MCSE")


## for correlations (ICC)
# mean of correlations as performance measure

MCSE3 <- do.call(data.frame, aggregate(cor_ICC ~ group + n_occasions + n_items, data = rd_grp, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1) ) / 1000 )) 
})
)

names(MCSE3) <- c("group", "n_occasions", "n_items", "cor_ICC_sim_mean", "cor_ICC_sim_var", "cor_ICC_MCSE")


## for correlations (ICC.z)
# mean of correlations as performance measure

MCSE4 <- do.call(data.frame, aggregate(cor_ICC.z ~ group + n_occasions + n_items, data = rd_grp, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1) ) / 1000 )) 
})
)

names(MCSE4) <- c("group", "n_occasions", "n_items", "cor_ICC.z_sim_mean", "cor_ICC.z_sim_var", "cor_ICC.z_MCSE")


## for RMSE (ICC)
# we have RMSE for each simulation run (because we have multiple participants)
# -> therefore, the performance measure across iterations is not RMSE, but mean of RMSE
# -> use formula for mean of generic statistic G (see Siepe et al., 2024)

MCSE5 <- do.call(data.frame, aggregate(RMSE_ICC ~ group + n_occasions + n_items, data = rd_grp, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1) ) / 1000 )) 
})
)

names(MCSE5) <- c("group", "n_occasions", "n_items", "RMSE_ICC_sim_mean", "RMSE_ICC_sim_var", "RMSE_ICC_MCSE")

## for RMSE (ICC.z)
# we have RMSE for each simulation run (because we have multiple participants)
# -> therefore, the performance measure across iterations is not RMSE, but mean of RMSE
# -> use formula for mean of generic statistic G (see Siepe et al., 2024)

MCSE6 <- do.call(data.frame, aggregate(RMSE_ICC.z ~ group + n_occasions + n_items, data = rd_grp, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1) ) / 1000 )) 
})
)

names(MCSE6) <- c("group", "n_occasions", "n_items", "RMSE_ICC.z_sim_mean", "RMSE_ICC.z_sim_var", "RMSE_ICC.z_MCSE")


## for reliability
# -> mean of generic statistic G

MCSE7 <- do.call(data.frame, aggregate(rel ~ group + n_occasions + n_items, data = rd_grp, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1) ) / 1000 )) 
})
)

names(MCSE7) <- c("group", "n_occasions", "n_items", "rel_sim_mean", "rel_sim_var", "rel_MCSE")


## for SD (ICC)
# we have one SD for each replicate -> performance measure = mean of SD across replicates
# use formula for mean of generic statistic G
MCSE8 <- do.call(data.frame, aggregate(sd_ICC ~ group + n_occasions + n_items, data = rd_grp, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1) ) / 1000 )) 
})
)

names(MCSE8) <- c("group", "n_occasions", "n_items", "sd_ICC_sim_mean", "sd_ICC_sim_var", "sd_ICC_MCSE")


## for SD (ICC.z)
# we have one SD for each replicate -> performance measure = mean of SD across replicates
# use formula for mean of generic statistic G
MCSE9 <- do.call(data.frame, aggregate(sd_ICC.z ~ group + n_occasions + n_items, data = rd_grp, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1) ) / 1000 )) 
})
)

names(MCSE9) <- c("group", "n_occasions", "n_items", "sd_ICC.z_sim_mean", "sd_ICC.z_sim_var", "sd_ICC.z_MCSE")


## for negICC
# performance measure: mean of generic statistic G
MCSE10 <- do.call(data.frame, aggregate(negICC ~ group + n_occasions + n_items, data = rd_grp, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1) ) / 1000 )) 
})
)

names(MCSE10) <- c("group", "n_occasions", "n_items", "negICC_sim_mean", "negICC_sim_var", "negICC_MCSE")


## for N_rel
# performance measure: mean of generic statistic G

MCSE11 <- do.call(data.frame, aggregate(N_rel ~ group + n_occasions + n_items, data = rd_grp, FUN = function(x) {
  c(mean = mean(x),
    var = (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1), # equal to using var()
    MCSE = sqrt( ( (sum( ( x - (sum(x)/1000 ) )^2 )) / (1000 - 1) ) / 1000 )) 
})
)

names(MCSE11) <- c("group", "n_occasions", "n_items", "N_rel_sim_mean", "N_rel_sim_var", "N_rel_MCSE")

## for estimation problems, no MCSE can be calculated because there is zero variance


# combine
MCSE <- merge(MCSE, MCSE2, by = c("group", "n_occasions", "n_items"))
MCSE <- merge(MCSE, MCSE3, by = c("group", "n_occasions", "n_items"))
MCSE <- merge(MCSE, MCSE4, by = c("group", "n_occasions", "n_items"))
MCSE <- merge(MCSE, MCSE5, by = c("group", "n_occasions", "n_items"))
MCSE <- merge(MCSE, MCSE6, by = c("group", "n_occasions", "n_items"))
MCSE <- merge(MCSE, MCSE7, by = c("group", "n_occasions", "n_items"))
MCSE <- merge(MCSE, MCSE8, by = c("group", "n_occasions", "n_items"))
MCSE <- merge(MCSE, MCSE9, by = c("group", "n_occasions", "n_items"))
MCSE <- merge(MCSE, MCSE10, by = c("group", "n_occasions", "n_items"))
MCSE <- merge(MCSE, MCSE11, by = c("group", "n_occasions", "n_items"))

MCSE
names(MCSE)


# round and save MCSE as csv
# round to 3 decimals in this case
MCSE[4:36] <- round(MCSE[4:36], 3)
MCSE <- MCSE[order(MCSE$group, MCSE$n_occasions, MCSE$n_items), ]
write.csv(MCSE, "results/MCSE_table_subgroups_Study1.csv", row.names = F)





rm(list=ls())




# Check Sufficient Number of Iterations  ----------------------------------
source("functions/function_aggregate_results.R")

load("results/check nr of iterations/sim_results_whole_data_set_Study1.rda")
load("results/check nr of iterations/sim_results_subgroups_Study1.rda")

agg2 <- aggregate_results(res2,
                         outcomes = c('min_diff_ICC', 'mean_diff_ICC', 'max_diff_ICC',
                                      'N_valid_ICC.z',
                                      'min_diff_ICC.z', 'mean_diff_ICC.z', 'max_diff_ICC.z',
                                      'cor_ICC', 'cor_ICC.z',
                                      'RMSE_ICC', 'RMSE_ICC.z',
                                      'rel', 'N_rel',
                                      'sd_ICC', 'sd_ICC.z',
                                      'negICC', 'estimationProbNeg', 'estimationProbPos'),
                         rel_outcomes = c('min_diff_ICC', 'mean_diff_ICC', 'max_diff_ICC',
                                          'min_diff_ICC.z', 'mean_diff_ICC.z', 'max_diff_ICC.z',
                                          'cor_ICC', 'cor_ICC.z',
                                          'RMSE_ICC', 'RMSE_ICC.z'),
                         abs_outcomes = c('N_valid_ICC.z',
                                          'rel', 'N_rel',
                                          'sd_ICC', 'sd_ICC.z',
                                          'negICC', 'estimationProbNeg', 'estimationProbPos'),
                         groupwise = FALSE,
                         group_var = NULL)

save(agg2, file = "results/check nr of iterations/aggregated_whole_data_set_Study1.rda")




agg_grp2 <- aggregate_results(res2_group,
                              outcomes = c('min_diff_ICC', 'mean_diff_ICC', 'max_diff_ICC',
                                           'N_valid_ICC.z',
                                           'min_diff_ICC.z', 'mean_diff_ICC.z', 'max_diff_ICC.z',
                                           'cor_ICC', 'cor_ICC.z',
                                           'RMSE_ICC', 'RMSE_ICC.z',
                                           'rel', 'N_rel',
                                           'sd_ICC', 'sd_ICC.z',
                                           'negICC', 'estimationProbNeg', 'estimationProbPos'),
                              rel_outcomes = c('min_diff_ICC', 'mean_diff_ICC', 'max_diff_ICC',
                                               'min_diff_ICC.z', 'mean_diff_ICC.z', 'max_diff_ICC.z',
                                               'cor_ICC', 'cor_ICC.z',
                                               'RMSE_ICC', 'RMSE_ICC.z'),
                              abs_outcomes = c('N_valid_ICC.z',
                                               'rel', 'N_rel',
                                               'sd_ICC', 'sd_ICC.z',
                                               'negICC', 'estimationProbNeg', 'estimationProbPos'),
                              groupwise = TRUE,
                              group_var = "group")




# Save Aggregated Results -------------------------------------------------
save(agg_grp2, file = "results/check nr of iterations/aggregated_subgroups_Study1.rda")





# Session Info ------------------------------------------------------------
# 
# R version 4.5.1 (2025-06-13 ucrt)
# Platform: x86_64-w64-mingw32/x64
# Running under: Windows 11 x64 (build 26100)
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




# # Notes
# outcomes <- c('min_diff_ICC', 'mean_diff_ICC', 'max_diff_ICC',
#               'N_valid_ICC.z',
#               'min_diff_ICC.z', 'mean_diff_ICC.z', 'max_diff_ICC.z',
#               'cor_ICC', 'cor_ICC.z',
#               'RMSE_ICC', 'RMSE_ICC.z',
#               'rel', 'N_rel',
#               'sd_ICC', 'sd_ICC.z',
#               'negICC', 'estimationProbNeg', 'estimationProbPos')
#
# 
# relative <- c('min_diff_ICC', 'mean_diff_ICC', 'max_diff_ICC',
#               'min_diff_ICC.z', 'mean_diff_ICC.z', 'max_diff_ICC.z',
#               'cor_ICC', 'cor_ICC.z',
#               'RMSE_ICC', 'RMSE_ICC.z')
#
# absolute <- c('N_valid_ICC.z',
#               'rel', 'N_rel',
#               'sd_ICC', 'sd_ICC.z',
#               'negICC', 'estimationProbNeg', 'estimationProbPos')





