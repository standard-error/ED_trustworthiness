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





