###################################################################
#####      Estimating trait emotion differentiation:          #####
#####          How many measurement occasions and             #####
#####              emotion items are needed?                  #####
###################################################################

###################################################################
#####      Process Visualization of Simulation Results        #####
###################################################################



# Source Processing Function ----------------------------------------------
source("functions/function_process_data_visualization.R")



# Define Simulations and Output Directories -------------------------------
visualization_specifications <- data.frame(
  
  results_dir = c(
    # emolive study
    "results/02_revision_1/emolive study/NED/main/processed",
    "results/02_revision_1/emolive study/PED/main/processed",
    "results/02_revision_1/emolive study/NED/check nr of iterations/processed",
    "results/02_revision_1/emolive study/PED/check nr of iterations/processed",
    "results/02_revision_1/emolive study/NED/neg ICC handling/setzero/processed",
    "results/02_revision_1/emolive study/NED/neg ICC handling/exclude/processed",
    "results/02_revision_1/emolive study/PED/neg ICC handling/setzero/processed",
    "results/02_revision_1/emolive study/PED/neg ICC handling/exclude/processed",
    # EMOTIONS study
    "results/02_revision_1/EMOTIONS study/NED/main/processed",
    "results/02_revision_1/EMOTIONS study/PED/main/processed",
    "results/02_revision_1/EMOTIONS study/NED/check nr of iterations/processed",
    "results/02_revision_1/EMOTIONS study/PED/check nr of iterations/processed",
    "results/02_revision_1/EMOTIONS study/NED/neg ICC handling/setzero/processed",
    "results/02_revision_1/EMOTIONS study/NED/neg ICC handling/exclude/processed",
    "results/02_revision_1/EMOTIONS study/PED/neg ICC handling/setzero/processed",
    "results/02_revision_1/EMOTIONS study/PED/neg ICC handling/exclude/processed"
  ),
  
  
  plot_dir = c(
    # emolive study
    "results/02_revision_1/emolive study/NED/main/plots/",
    "results/02_revision_1/emolive study/PED/main/plots/",
    "results/02_revision_1/emolive study/NED/check nr of iterations/plots/",
    "results/02_revision_1/emolive study/PED/check nr of iterations/plots/",
    "results/02_revision_1/emolive study/NED/neg ICC handling/setzero/plots/",
    "results/02_revision_1/emolive study/NED/neg ICC handling/exclude/plots/",
    "results/02_revision_1/emolive study/PED/neg ICC handling/setzero/plots/",
    "results/02_revision_1/emolive study/PED/neg ICC handling/exclude/plots/",
    # EMOTIONS study
    "results/02_revision_1/EMOTIONS study/NED/main/plots/",
    "results/02_revision_1/EMOTIONS study/PED/main/plots/",
    "results/02_revision_1/EMOTIONS study/NED/check nr of iterations/plots/",
    "results/02_revision_1/EMOTIONS study/PED/check nr of iterations/plots/",
    "results/02_revision_1/EMOTIONS study/NED/neg ICC handling/setzero/plots/",
    "results/02_revision_1/EMOTIONS study/NED/neg ICC handling/exclude/plots/",
    "results/02_revision_1/EMOTIONS study/PED/neg ICC handling/setzero/plots/",
    "results/02_revision_1/EMOTIONS study/PED/neg ICC handling/exclude/plots/"
    
  ),
  
  agg_file = c(
    # emolive
    "aggregated_results_NED_emolive.rda",
    "aggregated_results_PED_emolive.rda",
    "aggregated_results_NED_emolive_repl_check.rda",
    "aggregated_results_PED_emolive_repl_check.rda",
    "aggregated_results_NED_emolive_set_zero.rda",
    "aggregated_results_NED_emolive_exclude.rda",
    "aggregated_results_PED_emolive_set_zero.rda",
    "aggregated_results_PED_emolive_exclude.rda",
    # EMOTIONS study
    "aggregated_results_NED_EMOTIONS.rda",
    "aggregated_results_PED_EMOTIONS.rda",
    "aggregated_results_NED_EMOTIONS_repl_check.rda",
    "aggregated_results_PED_EMOTIONS_repl_check.rda",
    "aggregated_results_NED_EMOTIONS_set_zero.rda",
    "aggregated_results_NED_EMOTIONS_exclude.rda",
    "aggregated_results_PED_EMOTIONS_set_zero.rda",
    "aggregated_results_PED_EMOTIONS_exclude.rda"
  ),

  
  
  
  agg_object = c(
    # emolive study
    "agg",
    "agg",
    "agg",
    "agg",
    "agg",
    "agg",
    "agg",
    "agg",
    # EMOTIONS study
    "agg",
    "agg",
    "agg",
    "agg",
    "agg",
    "agg",
    "agg",
    "agg"
  )
  
)



# Sanity check ------------------------------------------------------------

dir.exists(visualization_specifications$results_dir)
dir.exists(visualization_specifications$plot_dir)

file.exists(file.path(visualization_specifications$results_dir, visualization_specifications$agg_file))



# Run Processing ----------------------------------------------------------

for (i in seq_len(nrow(visualization_specifications))) {
  
  message("=============================================")
  message("Processing: ", visualization_specifications$agg_file[i])
 
  visualize_simulation_results(
    results_dir = visualization_specifications$results_dir[i],
    plot_dir = visualization_specifications$plot_dir[i],
    agg_file = visualization_specifications$agg_file[i],
    agg_object = visualization_specifications$agg_object[i],
    x_breaks = seq(0,70,10),
    formats="pdf"
  ) 
}



sessionInfo()
# 
# R version 4.5.3 (2026-03-11 ucrt)
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
# other attached packages:
#  [1] lubridate_1.9.4   forcats_1.0.1     stringr_1.6.0     dplyr_1.1.4       purrr_1.1.0      
#  [6] readr_2.1.5       tidyr_1.3.1       tibble_3.3.0      tidyverse_2.0.0   ggpubr_0.6.3     
# [11] ggh4x_0.3.1       scales_1.4.0      viridis_0.6.5     viridisLite_0.4.3 ggplot2_4.0.2    
# 
# loaded via a namespace (and not attached):
#  [1] generics_0.1.4     rstatix_0.7.3      stringi_1.8.7      hms_1.1.4          magrittr_2.0.3    
#  [6] grid_4.5.3         timechange_0.3.0   RColorBrewer_1.1-3 backports_1.5.1    Formula_1.2-5     
# [11] gridExtra_2.3      textshaping_1.0.5  abind_1.4-8        cli_3.6.5          crayon_1.5.3      
# [16] rlang_1.2.0        bit64_4.6.0-1      withr_3.0.2        parallel_4.5.3     tools_4.5.3       
# [21] tzdb_0.5.0         ggsignif_0.6.4     broom_1.0.12       vctrs_0.6.5        R6_2.6.1          
# [26] lifecycle_1.0.5    bit_4.6.0          tictoc_1.2.1       car_3.1-5          vroom_1.6.5       
# [31] ragg_1.5.2         archive_1.1.12.1   pkgconfig_2.0.3    pillar_1.11.1      gtable_0.3.6      
# [36] glue_1.8.0         systemfonts_1.3.2  tidyselect_1.2.1   rstudioapi_0.18.0  farver_2.1.2      
# [41] labeling_0.4.3     svglite_2.2.2      carData_3.0-6      compiler_4.5.3     S7_0.2.0          