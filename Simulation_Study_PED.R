###################################################################
#####      Estimating trait emotion differentiation:          #####
#####          How many measurement occasions and             #####
#####               emotion items are needed?                 #####
###################################################################

###################################################################
#####                   Simulation Study                      #####
###################################################################


###################################################################
#####             Positive Emotion Differentiation            #####
###################################################################



# Preparation: Load Data --------------------------------------------------
load("prepared data/benchmark_data.rda")

# Make sure that ID variable is numeric
# (relevant for ICC calculation: matrix can only store one data type)
is.numeric(bench$SERIAL)


# Source Functions --------------------------------------------------------
source("functions/function_simulation_study.R")
# simulation study function already sources the other functions needed
# also already loads packages that are needed (future, future.apply)




# Global Seed Item Set ----------------------------------------------------

##### IMPORTANT: IF ITEM SETS SHALL BE FIXED ACROSS REPLICATIONS
##### USE THE SAME SEED SO THAT THE SAME ITEM SETS
##### ARE DRAWN FOR OVERALL SIMULATION IF RUN TWICE 

# source("Global_Seed_Item_Sets.R")

# here: not necessary, because item sets shall be drawn in balanced manner
# across replications



# Run Simulation ----------------------------------------------------------
tictoc::tic()
# n_occasions: 14, 20, ..., 70 in steps of 10
res <- simulation_study(data = bench, n_occasions = c(14, seq(20, 70, 10)),
                        occasions_drawn = c("random", "by order"), n_items = c(4, 8, 12),
                        n_iteration = 5000,
                        id.var = "SERIAL", all_items = c('freude1', 'freude2', 'freude3',
                                                         'interesse1', 'interesse2', 'interesse3',
                                                         'liebe1', 'liebe2', 'liebe3',
                                                         'stolz1', 'stolz2', 'stolz3'),
                        categories = c('freude', 'freude', 'freude',
                                       'interesse', 'interesse', 'interesse',
                                       'liebe', 'liebe', 'liebe',
                                       'stolz', 'stolz', 'stolz'),
                        type = "consistency", unit = "single", occ.running.var = "occ_running",
                        item_sets_across_replications = "balanced",
                        seed_item = 260306, seed_sim = 666, cores = 11)
tictoc::toc()




# Save Results ------------------------------------------------------------
save(res, file="results/02_revision1/sim_results_PED.rda")



# Check Sufficient Number of Iterations -----------------------------------
# i.e., run simulation again with 5000 iterations and
# different seed
# -> check whether results are comparable
tictoc::tic()
res2 <- simulation_study(data = bench, n_occasions = c(14, seq(20, 70, 10)),
                         occasions_drawn = c("random", "by order"), n_items = c(4, 8, 12),
                         n_iteration = 5000,
                         id.var = "SERIAL", all_items = c('freude1', 'freude2', 'freude3',
                                                          'interesse1', 'interesse2', 'interesse3',
                                                          'liebe1', 'liebe2', 'liebe3',
                                                          'stolz1', 'stolz2', 'stolz3'),
                         categories = c('freude', 'freude', 'freude',
                                        'interesse', 'interesse', 'interesse',
                                        'liebe', 'liebe', 'liebe',
                                        'stolz', 'stolz', 'stolz'),
                         type = "consistency", unit = "single", occ.running.var = "occ_running",
                         
                         seed_item = 220224, seed_sim = 23, cores = 11)
tictoc::toc()

# save results
save(res2, file="results/02_revision_1/check nr of iterations/sim_results_PED.rda")






# Session Info ------------------------------------------------------------
sessionInfo()

