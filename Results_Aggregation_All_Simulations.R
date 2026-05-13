###################################################################
#####      Estimating trait emotion differentiation:          #####
#####          How many measurement occasions and             #####
#####              emotion items are needed?                  #####
###################################################################

###################################################################
#####        Process & Aggregate Simulation Results           #####
###################################################################



# Source Processing Function ----------------------------------------------
source("functions/function_process_simulation_results.R")



# Define Simulations and Output Directories -------------------------------
simulation_specifications <- data.frame(
  
  input_file = c(
    # emolive study
    "results/02_revision_1/emolive study/NED/main/raw/sim_results_NED_emolive_Study.rda",
    "results/02_revision_1/emolive study/PED/main/raw/sim_results_PED_emolive_Study.rda",
    "results/02_revision_1/emolive study/NED/check nr of iterations/raw/sim_results_NED_emolive_Study.rda",
    "results/02_revision_1/emolive study/PED/check nr of iterations/raw/sim_results_PED_emolive_Study.rda",
    "results/02_revision_1/emolive study/NED/neg ICC handling/setzero/raw/sim_results_NED_set_neg_ICC_zero_emolive_Study.rda",
    "results/02_revision_1/emolive study/NED/neg ICC handling/exclude/raw/sim_results_NED_exclude_neg_ICC_emolive_Study.rda",
    "results/02_revision_1/emolive study/PED/neg ICC handling/setzero/raw/sim_results_PED_set_neg_ICC_zero_emolive_Study.rda",
    "results/02_revision_1/emolive study/PED/neg ICC handling/exclude/raw/sim_results_PED_exclude_neg_ICC_emolive_Study.rda",
    # EMOTIONS study
    "results/02_revision_1/EMOTIONS study/NED/main/raw/sim_results_NED_EMOTIONS_Study.rda",
    "results/02_revision_1/EMOTIONS study/PED/main/raw/sim_results_PED_EMOTIONS_Study.rda",
    "results/02_revision_1/EMOTIONS study/NED/check nr of iterations/raw/sim_results_NED_EMOTIONS_Study.rda",
    "results/02_revision_1/EMOTIONS study/PED/check nr of iterations/raw/sim_results_PED_EMOTIONS_Study.rda",
    "results/02_revision_1/EMOTIONS study/NED/neg ICC handling/setzero/raw/sim_results_NED_set_neg_ICC_zero_EMOTIONS_Study.rda",
    "results/02_revision_1/EMOTIONS study/NED/neg ICC handling/exclude/raw/sim_results_NED_exclude_neg_ICC_EMOTIONS_Study.rda",
    "results/02_revision_1/EMOTIONS study/PED/neg ICC handling/setzero/raw/sim_results_PED_set_neg_ICC_zero_EMOTIONS_Study.rda",
    "results/02_revision_1/EMOTIONS study/PED/neg ICC handling/exclude/raw/sim_results_PED_exclude_neg_ICC_EMOTIONS_Study.rda"
  ),
  
  sim_id = c(
    # emolive study
    "NED_emolive",
    "PED_emolive",
    "NED_emolive_repl_check",
    "PED_emolive_repl_check",
    "NED_emolive_set_zero",
    "NED_emolive_exclude",
    "PED_emolive_set_zero",
    "PED_emolive_exclude",
    # EMOTIONS study
    "NED_EMOTIONS",
    "PED_EMOTIONS",
    "NED_EMOTIONS_repl_check",
    "PED_EMOTIONS_repl_check",
    "NED_EMOTIONS_set_zero",
    "NED_EMOTIONS_exclude",
    "PED_EMOTIONS_set_zero",
    "PED_EMOTIONS_exclude"
    
  ),
  
  output_dir = c(
    # emolive study
    "results/02_revision_1/emolive study/NED/main/processed/",
    "results/02_revision_1/emolive study/PED/main/processed/",
    "results/02_revision_1/emolive study/NED/check nr of iterations/processed/",
    "results/02_revision_1/emolive study/PED/check nr of iterations/processed/",
    "results/02_revision_1/emolive study/NED/neg ICC handling/setzero/processed/",
    "results/02_revision_1/emolive study/NED/neg ICC handling/exclude/processed/",
    "results/02_revision_1/emolive study/PED/neg ICC handling/setzero/processed/",
    "results/02_revision_1/emolive study/PED/neg ICC handling/exclude/processed/",
    # EMOTIONS study
    "results/02_revision_1/EMOTIONS study/NED/main/processed/",
    "results/02_revision_1/EMOTIONS study/PED/main/processed/",
    "results/02_revision_1/EMOTIONS study/NED/check nr of iterations/processed/",
    "results/02_revision_1/EMOTIONS study/PED/check nr of iterations/processed/",
    "results/02_revision_1/EMOTIONS study/NED/neg ICC handling/setzero/processed/",
    "results/02_revision_1/EMOTIONS study/NED/neg ICC handling/exclude/processed/",
    "results/02_revision_1/EMOTIONS study/PED/neg ICC handling/setzero/processed/",
    "results/02_revision_1/EMOTIONS study/PED/neg ICC handling/exclude/processed/"
    
  ),
  
  object_name = c(
    # emolive study
    "res",
    "res",
    "res2",
    "res2",
    "setzero",
    "excl",
    "setzero",
    "excl",
    # EMOTIONS study
    "res",
    "res",
    "res2",
    "res2",
    "setzero",
    "excl",
    "setzero",
    "excl"
  )
  
)



# Sanity check ------------------------------------------------------------

file.exists(simulation_specifications$input_file)
dir.exists(simulation_specifications$output_dir)



# Run Processing ----------------------------------------------------------

for (i in seq_len(nrow(simulation_specifications))) {
  
  message("Processing: ", simulation_specifications[i, "sim_id"])
  
  process_simulation_results(
    input_file = simulation_specifications[i, "input_file"],
    sim_id = simulation_specifications[i, "sim_id"],
    output_dir = simulation_specifications[i, "output_dir"],
    object_name = simulation_specifications[i, "object_name"]
  )
  
  message("Finished: ", simulation_specifications[i, "sim_id"])
}




sessionInfo()

# R version 4.5.3 (2026-03-11 ucrt)
# Platform: x86_64-w64-mingw32/x64
# Running under: Windows 11 x64 (build 26200)
# 
# Matrix products: default
#   LAPACK version 3.12.1
# 
# locale:
# [1] LC_COLLATE=German_Germany.utf8  LC_CTYPE=German_Germany.utf8   
# [3] LC_MONETARY=German_Germany.utf8 LC_NUMERIC=C                   
# [5] LC_TIME=German_Germany.utf8    
# 
# time zone: Europe/Berlin
# tzcode source: internal
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# loaded via a namespace (and not attached):
# [1] compiler_4.5.3    tictoc_1.2.1      tools_4.5.3       rstudioapi_0.18.0
