###################################################################
#####    Estimating trait negative emotion differentiation:   #####
#####        How many measurement occasions and               #####
#####              emotion items are needed?                  #####
###################################################################

###################################################################
#####             Results Aggregation Function                #####
###################################################################




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


aggregate_results <- function(data, outcomes, rel_outcomes, abs_outcomes) {
  
  # results with benchmark
  all <- data
  # results without benchmark
  without_bench <- data[data[ , "condition"] != "benchmark", ]
  
  # apply the function to aggregate outcomes on all outcomes
  results <- lapply(outcomes, function(outcome) {
    
    # if outcome is relative, use the results without benchmark
    if (outcome %in% rel_outcomes) {
      tmp <- do.call(data.frame,
                     aggregate(without_bench[ , outcome] ~ without_bench[ , "occasions_drawn"] +
                                 without_bench[ , "n_occasions"] +
                                 without_bench[ , "n_items"],
                               FUN = function(x) {
                                 c(min(x), mean(x), max(x))
                               }))
    } else if (outcome %in% abs_outcomes) {
      tmp <- do.call(data.frame,
                     aggregate(all[ , outcome] ~ all[ , "occasions_drawn"]+
                                 all[ , "n_occasions"] + all[ , "n_items"],
                               FUN = function(x) {
                                 c(min(x), mean(x), max(x))
                               })) 
    } else {
      stop(sprintf("Outcome %s is not in relative or absolute outcomes list.", outcome))
    }
    
    names(tmp) <- c("occasions_drawn", "n_occasions", "n_items",
                    paste0(outcome, "_min"),
                    paste0(outcome, "_mean"),
                    paste0(outcome, "_max"))
    list(agg_res = tmp)
  })
  names(results) <- outcomes
  return(results)
}
