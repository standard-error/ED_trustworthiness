###################################################################
#####    Estimating trait negative emotion differentiation:   #####
#####        How many measurement occasions and               #####
#####              emotion items are needed?                  #####
###################################################################

###################################################################
#####             Results Aggregation Function                #####
###################################################################



# Write Helper Function (Fisher's Z-transformation) -----------------------
# For correlation with the benchmark as outcome, the correlations
# need to be Fisher's Z-transformed before averaging, and backtransformed after
# averaging. For this purpose, we want a helper function to keep the code
# below clean.

# natural logarithm = log() function in R

# Fisher's Z-transformation
fisher_z <- function(r) {
  z <- 0.5 * log( (1 + r) / (1 - r) )
  return(z)
}
# fisher_z(0.5)
# fisher_z(c(0.5, 0.5))

# back-transformation
inverse_fisher_z <- function(z) {
  r <- ( exp(2*z) - 1 ) / ( exp(2*z) + 1 )
  return(r)
}

# inverse_fisher_z(0.5)
# 
# inverse_fisher_z(fisher_z(0.5))


# r <- c(0.3, 0.4, 0.5)
# z <- fisher_z(r)
# mean <- mean(z)
# r_mean <- inverse_fisher_z(mean)
# r_mean
# 
# inverse_fisher_z(mean(fisher_z(r)))



# Create Function to Aggregrate Results Across Iterations -----------------
# For each outcome, calculate mean, min and max across all iterations of each
# condition.
# For relative outcomes, the benchmark needs to be excluded because these are
# already in relation to the benchmark. Relative outcomes are: mean difference,
# min difference, max difference, correlation with benchmark, RMSE.
# For absolute outcomes, also use the benchmark and plot it as a reference. 
# Absolute outcomes are: reliability, SD, number of negative ICCs, estimation problems, valid ICCs.
# Plot the outcomes for raw ICCs and Fisher's Z-transformed ICC.z.

# Aggregate across n_items, n_occasions and occasions_drawn.
# If occasions_drawn does not play a role, the results could be
# aggregated across occasions_drawn.


# For group-wise analysis, the results also need to be aggregated
# per group -> aggregate(outcome ~ n_items + n_occasions + occasions_drawn + group).


aggregate_results <- function(data, outcomes, rel_outcomes, abs_outcomes,
                              groupwise = FALSE, group_var = NULL) {
  # data: simulation data frame with results across all conditions and iterations
  # outcomes: chr vector with names of the outcome variables in the simulation data frame (data)
  # rel_outcomes: chr vector indicating names of relative outcomes (-> do not use benchmark, as it
          # already is relative)
  # abs_outcomes: chr vector indicating names of absolute outcomes (-> also use benchmark)
  # groupwise: logical indicating whether or not to apply the function to the overall simulation
        # or the groupwise simulation (high, medium, low NED); default = FALSE
  # group_var = chr indicating name of the grouping variable in the data frame, default = NULL
  
  # results with benchmark
  all <- data
  # results without benchmark
  without_bench <- data[data[ , "condition"] != "benchmark", ]
  
  # apply the function to aggregate outcomes on all outcomes
  results <- lapply(outcomes, function(outcome) {
    
    
    # First, choose correct data to apply function to
    # if outcome is relative, use the results without benchmark
    if (outcome %in%  rel_outcomes) {
      use_data <- without_bench
    } else if (outcome %in% abs_outcomes) {
      use_data <- all
    } else {
      stop(sprintf("Outcome %s is not in relative or absolute outcomes list.", outcome))
    }
      
    
    # Choose factors to aggregate across
    # always use occasions_drawn, n_occasions, and n_items
    # if the simulation was groupwise, also use group as factor
    factors <- c("occasions_drawn", "n_occasions", "n_items")
    
    if (groupwise == TRUE) { 
      # CHECK: is group_var provided and a variable in the data frame?
      if (is.null(group_var) || !(group_var %in% names(use_data))) {
        stop(sprintf("When groupwise == TRUE, a valid group_var must be provided."))
      } else {
        factors <- c(factors, group_var) # add group_var to the factors
      }
    }
    
    
    # create formula to use in aggregate function
    formula <- as.formula(
      paste0(outcome, " ~ ", paste0(factors, collapse = " + "))
    )
    
    
    # aggregate results across iterations
    # if outcome is correlation -> use Fisher's Z-transformation before averaging,
    # and backtransform
    
    tmp <- do.call(
      data.frame,
      aggregate(formula, data = use_data, FUN = function(x) {
        if (outcome == "cor_ICC" | outcome == "cor_ICC.z") { # if outcome is correlation
        c(min(x),
          inverse_fisher_z(mean(fisher_z(x))), # apply Fisher's Z-transformation, average, backtransform
          max(x))          
        } else { # else just calculate mean
        c(min(x), mean(x), max(x))
        }
      })
    )
    
    
    # rename columns
    names(tmp) <- c(factors,
                    paste0(outcome, "_min"),
                    paste0(outcome, "_mean"),
                    paste0(outcome, "_max"))
    list(agg_res = tmp)
  })
  names(results) <- outcomes
  return(results)
}




# 
# # Load Simulation Results -------------------------------------------------
# load("results/sim_results_whole_data_set_Study1.rda")
# load("results/sim_results_subgroups_Study1.rda")
# 
# 
# 
# # Check Missings ----------------------------------------------------------
# any(is.na(res))
# any(is.na(res_group))
# 
# 
# out <-aggregate_results(res, outcomes = c('min_diff_ICC', 'mean_diff_ICC', 'max_diff_ICC',
#                                                 'N_valid_ICC.z',
#                                                 'min_diff_ICC.z', 'mean_diff_ICC.z', 'max_diff_ICC.z',
#                                                 'cor_ICC', 'cor_ICC.z',
#                                                 'RMSE_ICC', 'RMSE_ICC.z',
#                                                 'rel', 'N_rel',
#                                                 'sd_ICC', 'sd_ICC.z',
#                                                 'negICC', 'estimationProbNeg', 'estimationProbPos'),
#                   rel_outcomes = c('min_diff_ICC', 'mean_diff_ICC', 'max_diff_ICC',
#                                                'min_diff_ICC.z', 'mean_diff_ICC.z', 'max_diff_ICC.z',
#                                                'cor_ICC', 'cor_ICC.z',
#                                                'RMSE_ICC', 'RMSE_ICC.z'),
#                   abs_outcomes = c('N_valid_ICC.z',
#                                    'rel', 'N_rel',
#                                    'sd_ICC', 'sd_ICC.z',
#                                    'negICC', 'estimationProbNeg', 'estimationProbPos'),
#                   groupwise = FALSE, group_var = NULL)
# 
# 
# 
# 
# 
# out2 <-aggregate_results(res_group, outcomes = c('min_diff_ICC', 'mean_diff_ICC', 'max_diff_ICC',
#                                                       'N_valid_ICC.z',
#                                                       'min_diff_ICC.z', 'mean_diff_ICC.z', 'max_diff_ICC.z',
#                                                       'cor_ICC', 'cor_ICC.z',
#                                                       'RMSE_ICC', 'RMSE_ICC.z',
#                                                       'rel', 'N_rel',
#                                                       'sd_ICC', 'sd_ICC.z',
#                                                       'negICC', 'estimationProbNeg', 'estimationProbPos'),
#                         rel_outcomes = c('min_diff_ICC', 'mean_diff_ICC', 'max_diff_ICC',
#                                                      'min_diff_ICC.z', 'mean_diff_ICC.z', 'max_diff_ICC.z',
#                                                      'cor_ICC', 'cor_ICC.z',
#                                                      'RMSE_ICC', 'RMSE_ICC.z'),
#                         abs_outcomes = c('N_valid_ICC.z',
#                                          'rel', 'N_rel',
#                                          'sd_ICC', 'sd_ICC.z',
#                                          'negICC', 'estimationProbNeg', 'estimationProbPos'),
#                         groupwise = TRUE, group_var = "group")
# 
# 

