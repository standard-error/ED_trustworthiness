###################################################################
#####    Estimating trait negative emotion differentiation:   #####
#####        How many measurement occasions and               #####
#####              emotion items are needed?                  #####
###################################################################

###################################################################
#####             Simulation: Subgroup Analyses               #####
###################################################################

# Simulation in Dependence of NED Level (High, Medium, Low)
# -> run simulation for each of the groups and bind data frames


##### IMPORTANT: USE THE SAME SEED SO THAT THE SAME ITEM SETS
##### ARE DRAWN FOR THE THREE GROUPS 


# Preparation: Load Data --------------------------------------------------
load("prepared data/benchmark_data_highNED_Study1.rda")
load("prepared data/benchmark_data_mediumNED_Study1.rda")
load("prepared data/benchmark_data_lowNED_Study1.rda")


# Make sure that ID variable is numeric
# (relevant for ICC calculation: matrix can only store one data type)
is.numeric(bench_highNED$SERIAL)
is.numeric(bench_mediumNED$SERIAL)
is.numeric(bench_lowNED$SERIAL)


# Source Functions --------------------------------------------------------
# source("functions/function_ordered_occasion_draw.R")
# source("functions/function_random_occasion_draw.R")
# source("functions/function_calculate_iccs.R")
# source("functions/function_one_simulation_run.R")

source("functions/function_simulation_study.R")
# simulation study function already sources the other functions needed
# also already loads packages that are needed (future, future.apply)




# Set Seed ----------------------------------------------------------------
##### IMPORTANT: USE THE SAME SEED SO THAT THE SAME ITEM SETS
##### ARE DRAWN FOR THE THREE GROUPS 

## -> same seed as for the overall analysis (same item subset)

source("Global_Seed.R")



# Run Simulation ----------------------------------------------------------


# '' High NED Group -------------------------------------------------------

tictoc::tic()
res_highNED <- simulation_study(data = bench_highNED, n_occasions = c(3, 5, seq(10, 100, 10)),
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
                                seed_item = global.seed.item.set, seed_sim = 456, cores = 10)
tictoc::toc()

res_highNED$group <- "high NED"


# '' Medium NED Group -----------------------------------------------------

tictoc::tic()
res_mediumNED <- simulation_study(data = bench_mediumNED, n_occasions = c(3, 5, seq(10, 100, 10)),
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
                                seed_item = global.seed.item.set, seed_sim = 789, cores = 10)
tictoc::toc()

res_mediumNED$group <- "medium NED"


# '' Low NED Group --------------------------------------------------------

tictoc::tic()
res_lowNED <- simulation_study(data = bench_lowNED, n_occasions = c(3, 5, seq(10, 100, 10)),
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
                                seed_item = global.seed.item.set, seed_sim = 345, cores = 10)
tictoc::toc()

res_lowNED$group <- "low NED"



# Bind Data Frames --------------------------------------------------------
res_group <- rbind(res_highNED, res_mediumNED, res_lowNED)

# adjust variable order
res_group <- res_group[ , c('group', 'condition', 'n_occasions', 'occasions_drawn',
                            'n_items', 'items', 'n_iteration', 'min_diff_ICC',
                            'mean_diff_ICC', 'max_diff_ICC', 'N_valid_ICC.z',
                            'min_diff_ICC.z', 'mean_diff_ICC.z', 'max_diff_ICC.z',
                            'cor_ICC', 'cor_ICC.z', 'RMSE_ICC', 'RMSE_ICC.z',
                            'rel', 'N_rel', 'sd_ICC', 'sd_ICC.z', 'negICC',
                            'estimationProbNeg', 'estimationProbPos')] 






# Save Results ------------------------------------------------------------
save(res_group, file="results/sim_results_subgroups_Study1.rda")





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
# other attached packages:
#  [1] future.apply_1.20.0 future_1.67.0       lubridate_1.9.4     forcats_1.0.0       stringr_1.5.1       dplyr_1.1.4        
#  [7] purrr_1.1.0         readr_2.1.5         tidyr_1.3.1         tibble_3.3.0        ggplot2_3.5.2       tidyverse_2.0.0    
# 
# loaded via a namespace (and not attached):
#  [1] gtable_0.3.6       compiler_4.5.1     tidyselect_1.2.1   parallel_4.5.1     irr_0.84.1         globals_0.18.0    
#  [7] scales_1.4.0       R6_2.6.1           generics_0.1.4     pillar_1.11.0      RColorBrewer_1.1-3 tzdb_0.5.0        
# [13] rlang_1.1.6        stringi_1.8.7      timechange_0.3.0   cli_3.6.5          withr_3.0.2        magrittr_2.0.3    
# [19] tictoc_1.2.1       digest_0.6.37      grid_4.5.1         rstudioapi_0.17.1  hms_1.1.3          lifecycle_1.0.4   
# [25] vctrs_0.6.5        lpSolve_5.6.23     glue_1.8.0         listenv_0.9.1      farver_2.1.2       codetools_0.2-20  
# [31] parallelly_1.45.1  tools_4.5.1        pkgconfig_2.0.3   
