###################################################################
#####    Estimating trait negative emotion differentiation:   #####
#####        How many measurement occasions and               #####
#####              emotion items are needed?                  #####
###################################################################

###################################################################
#####              Simulation: Whole Data Set                 #####
###################################################################



# Preparation: Load Data --------------------------------------------------
load("prepared data/benchmark_data_Study1.rda")

# Make sure that ID variable is numeric
# (relevant for ICC calculation: matrix can only store one data type)
is.numeric(bench$SERIAL)


# Source Functions --------------------------------------------------------
# source("functions/function_ordered_occasion_draw.R")
# source("functions/function_random_occasion_draw.R")
# source("functions/function_calculate_iccs.R")
# source("functions/function_one_simulation_run.R")

source("functions/function_simulation_study.R")
# simulation study function already sources the other functions needed
# also already loads packages that are needed (future, future.apply)



# Run Simulation ----------------------------------------------------------
tictoc::tic()
res <- simulation_study(data = bench, n_occasions = c(3, 5, seq(10, 100, 10)),
                        occasions_drawn = c("random", "by order"), n_items = c(5, 15),
                        n_iteration = 1000,
                        id.var = "SERIAL", all_items = c('aerger1', 'aerger2', 'aerger3',
                                                         'traurigkeit1', 'traurigkeit2', 'traurigkeit3',
                                                         'angst1', 'angst2', 'angst3',
                                                         'scham1', 'scham2', 'scham3',
                                                         'schuld1', 'schuld2', 'schuld3'),
                        categories = c("aerger", "aerger", "aerger",
                                       "traurigkeit", "traurigkeit", "traurigkeit",
                                       "angst", "angst", "angst",
                                       "scham", "scham", "scham",
                                       "schuld", "schuld", "schuld"),
                        type = "consistency", unit = "single", occ.running.var = "occ_running",
                        seed = 230694, cores = 10)
tictoc::toc()




# Save Results ------------------------------------------------------------
save(res, file="results/sim_results_whole_data_set_Study1.rda")

