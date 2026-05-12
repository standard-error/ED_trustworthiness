###################################################################
#####      Estimating trait emotion differentiation:          #####
#####          How many measurement occasions and             #####
#####               emotion items are needed?                 #####
###################################################################

###################################################################
#####                   Simulation Study                      #####
###################################################################


###################################################################
#####             Negative Emotion Differentiation            #####
###################################################################


###################################################################
#####                    EMOTIONS Data                        #####
###################################################################



# Preparation: Load Data --------------------------------------------------
load("prepared data/EMOTIONS_benchmark_data.rda")

# Make sure that ID variable is numeric
# (relevant for ICC calculation: matrix can only store one data type)
is.numeric(bench$id)

# use only negative emotions:
bench <- bench[ , c("id", "occ_running",
                    "angry", "excluded", "envious",
                    "resentful", "ashamed", "insecure",
                    "anxious", "sad", "lonely")]




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
                        occasions_drawn = c("random", "by order"), n_items = c(3,4,5,6,7,8,9),
                        n_iteration = 5000,
                        id.var = "id", all_items = c("angry", "excluded", "envious",
                                                     "resentful", "ashamed", "insecure",
                                                     "anxious", "sad", "lonely"),
                        categories = NULL,
                        type = "consistency", unit = "single",
                        negative_icc_handling = "keep",
                        occ.running.var = "occ_running",
                        item_sets_across_replications = "balanced",
                        seed_item = 9965, seed_sim = 48, cores = 11)
tictoc::toc()




# Save Results ------------------------------------------------------------
save(res, file="results/02_revision_1/EMOTIONS study/NED/main/raw/sim_results_NED_EMOTIONS_Study.rda")




# Check Number of Iterations ----------------------------------------------
tictoc::tic()
# n_occasions: 14, 20, ..., 70 in steps of 10
res2 <- simulation_study(data = bench, n_occasions = c(14, seq(20, 70, 10)),
                        occasions_drawn = c("random", "by order"), n_items = c(3,4,5,6,7,8,9),
                        n_iteration = 5000,
                        id.var = "id", all_items = c("angry", "excluded", "envious",
                                                     "resentful", "ashamed", "insecure",
                                                     "anxious", "sad", "lonely"),
                        categories = NULL,
                        type = "consistency", unit = "single",
                        negative_icc_handling = "keep",
                        occ.running.var = "occ_running",
                        item_sets_across_replications = "balanced",
                        seed_item = 6809, seed_sim = 97, cores = 11)
tictoc::toc()




# Save Results ------------------------------------------------------------
save(res2, file="results/02_revision_1/EMOTIONS study/NED/check nr of iterations/raw/sim_results_NED_EMOTIONS_Study.rda")

