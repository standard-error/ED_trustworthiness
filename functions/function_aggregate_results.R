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




aggregate_results <- function(data, outcomes, rel_outcomes, abs_outcomes) {
  # data: simulation data frame with results across all conditions and iterations
  # outcomes: chr vector with names of the outcome variables in the simulation data frame (data)
  # rel_outcomes: chr vector indicating names of relative outcomes (-> do not use benchmark, as it
          # already is relative)
  # abs_outcomes: chr vector indicating names of absolute outcomes (-> also use benchmark)

  
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
    factors <- c("occasions_drawn", "n_occasions", "n_items")

    
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
        
        # include check -> results may be NA if there was no valid ICC data at all
        if (all(is.na(x))) {
          return(c(NA_real_, NA_real_, NA_real_))
        }
        
        if (outcome == "cor_ICC" | outcome == "cor_ICC.z") { # if outcome is correlation
        c(min(x, na.rm=TRUE),
          inverse_fisher_z(mean(fisher_z(x), na.rm=TRUE)), # apply Fisher's Z-transformation, average, backtransform
          max(x, na.rm=TRUE))          
        } else { # else just calculate mean
        c(min(x, na.rm=TRUE), mean(x, na.rm=TRUE), max(x, na.rm=TRUE))
        }
      },
      na.action = na.pass),
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



