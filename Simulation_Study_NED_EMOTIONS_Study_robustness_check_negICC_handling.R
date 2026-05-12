###################################################################
#####      Estimating trait emotion differentiation:          #####
#####          How many measurement occasions and             #####
#####               emotion items are needed?                 #####
###################################################################

###################################################################
##### Simulation Study: Robustness Check Negative ICC Handling ####
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
# Robustness checks: different ICC handlings (set to zero, exclude)



# '' Set negative ICCs to zero --------------------------------------------

tictoc::tic()
# n_occasions: 14, 20, ..., 70 in steps of 10
setzero <- simulation_study(data = bench, n_occasions = c(14, seq(20, 70, 10)),
                            occasions_drawn = c("random", "by order"), n_items = c(3,4,5,6,7,8,9),
                            n_iteration = 5000,
                            id.var = "id", all_items = c("angry", "excluded", "envious",
                                                         "resentful", "ashamed", "insecure",
                                                         "anxious", "sad", "lonely"),
                            categories = NULL,
                            type = "consistency", unit = "single",
                            negative_icc_handling = "set to zero",
                            occ.running.var = "occ_running",
                            item_sets_across_replications = "balanced",
                            seed_item = 1757, seed_sim = 2227595, cores = 11)
tictoc::toc()




# Save Results ------------------------------------------------------------
save(setzero, file="results/02_revision_1/EMOTIONS study/NED/robustness check negICC handling/setzero/raw/sim_results_NED_set_neg_ICC_zero_EMOTIONS_Study.rda")




# '' Exclude Negative ICCs ------------------------------------------------


tictoc::tic()
# n_occasions: 14, 20, ..., 70 in steps of 10
excl <- simulation_study(data = bench, n_occasions = c(14, seq(20, 70, 10)),
                         occasions_drawn = c("random", "by order"), n_items = c(3,4,5,6,7,8,9),
                         n_iteration = 5000,
                         id.var = "id", all_items = c("angry", "excluded", "envious",
                                                      "resentful", "ashamed", "insecure",
                                                      "anxious", "sad", "lonely"),
                         categories = NULL,
                         type = "consistency", unit = "single",
                         negative_icc_handling = "exclude",
                         occ.running.var = "occ_running",
                         item_sets_across_replications = "balanced",
                         seed_item = 1804, seed_sim = 456454, cores = 11)
tictoc::toc()




# Save Results ------------------------------------------------------------
save(excl, file="results/02_revision_1/EMOTIONS study/NED/robustness check negICC handling/exclude/raw/sim_results_NED_exclude_neg_ICC_EMOTIONS_Study.rda")

