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




# Global Seed Item Set ----------------------------------------------------

##### IMPORTANT: USE THE SAME SEED SO THAT THE SAME ITEM SETS
##### ARE DRAWN FOR THE THREE GROUPS AND OVERALL SIMULATION

## -> same seed as for the overall analysis (same item subset)
source("Global_Seed_Item_Sets.R")




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
                        seed_item = global.seed.item.set, seed_sim = 123, cores = 10)
tictoc::toc()




# Save Results ------------------------------------------------------------
save(res, file="results/sim_results_whole_data_set_Study1.rda")



# Session Info ------------------------------------------------------------

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
# other attached packages:
# [1] future.apply_1.20.0 future_1.67.0      
# 
# loaded via a namespace (and not attached):
#  [1] compiler_4.5.1    tictoc_1.2.1      parallelly_1.45.1 parallel_4.5.1    tools_4.5.1       rstudioapi_0.17.1
#  [7] listenv_0.9.1     codetools_0.2-20  irr_0.84.1        digest_0.6.37     globals_0.18.0    lpSolve_5.6.23   
