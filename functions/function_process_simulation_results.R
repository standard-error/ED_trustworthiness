###################################################################
#####      Estimating trait emotion differentiation:          #####
#####         How many measurement occasions and              #####
#####               emotion items are needed?                 #####
###################################################################

###################################################################
#####      Wrapper Function to Process Simulation Results     #####
###################################################################




# Wrapper Function to Process All Simulation Results ----------------------

process_simulation_results <- function(input_file,
                                       output_dir,
                                       sim_id,
                                       object_name,
                                       aggregate_function_file = "functions/function_aggregate_results.R") {
  # input_file: file containing the simulation data frame with results (one row for each replication)
  # output_dir: directory to save results in
  # sim_id: ID variable coding which simulation is being processed, will be used as index when
      # saving files with corresponding name
  # object_name: name of the R object related to the simulation results (-> input_file)
  # aggregate_function_file: file containing the aggregate_results function used to aggregate
      # certain outcomes across replications per condition

  
  
  #========================================================================
  # Preparation
  #========================================================================

  if (file.exists(output_dir) == FALSE) { # if output directory does not yet exist, create it
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  
  env <- new.env() # create new environment
  
  load(input_file, envir=env) # load input file into the new environment
  
  if (!object_name %in% ls(env)) { # if the specified object to aggregate results from does not exist, throw error
    stop("Object '", object_name, "' not found in ", input_file)
  }
  
  
  # regardless of the exact object name, we want to use res as the object being processed
  # write input object to res
  res <- get(object_name, envir = env)
  
  
  # determine maximum number of participants
  n_persons <- max(res$n_total_persons)
  
  message("Running processing for ", sim_id, " with n_persons = ", n_persons,
          " and a total of ", nrow(res), " rows in simulation design.")
  
  # determine participant IDs from named person-level variables
  # (to merge participant-level data later)
  person_ids <- as.numeric(names(res$person_estimates_ICC[[1]])) # numeric ids
  if(any(is.na(person_ids))) { stop("At least one person ID could not be converted to numeric.")} # check
  
  person_ids_chr <- as.character(person_ids) # character ids
  
  # check:
  if (length(person_ids) != n_persons) {
    stop("Number of unique person IDs does not match n_persons.")
  }
  
  # check whether all person-level estimates contain ALL participants IN THE SAME ORDER
  # -> needed so that we can simply bind the data later without any matching by ID
  # -> check whether names(outcome_vector_in_row) == person_ids_chr
  # -> same number and order of IDs
  
  # for person_estimates_ICC
  if (!all(unlist(
    # unlist and check whether all checks are TRUE or not
    lapply(
      # apply to list (all rows in simulation data frame)
      res$person_estimates_ICC,
      FUN = function(x) {
        # list of person_estimates_ICC
        identical(names(x), person_ids_chr) # function to apply: check whether names (IDs) are identical
      }
    )
  ))) { # if not all are TRUE (i.e., checks for all rows)
    stop( # give error message
      "Person IDs are not the same and/or not in the same order across all replications for person_estimates_ICC"
    )
  }
  
  
  
  # for person_estimates_ICC.z
  if (!all(unlist(
    # unlist and check whether all checks are TRUE or not
    lapply(
      # apply to list (all rows in simulation data frame)
      res$person_estimates_ICC.z,
      FUN = function(x) {
        # list of person_estimates_ICC.z
        identical(names(x), person_ids_chr) # function to apply: check whether names (IDs) are identical
      }
    )
  ))) { # if not all are TRUE (i.e., checks for all rows)
    stop( # give error message
      "Person IDs are not the same and/or not in the same order across all replications for person_estimates_ICC.z"
    )
  }

  
  
  # for person_diff_ICC
  if (!all(unlist(
    # unlist and check whether all checks are TRUE or not
    lapply(
      # apply to list (all rows in simulation data frame)
      res$person_diff_ICC,
      FUN = function(x) {
        # list of person_diff_ICC
        identical(names(x), person_ids_chr) # function to apply: check whether names (IDs) are identical
      }
    )
  ))) { # if not all are TRUE (i.e., checks for all rows)
    stop( # give error message
      "Person IDs are not the same and/or not in the same order across all replications for person_diff_ICC"
    )
  }
  

  
  # for person_diff_ICC.z
  if (!all(unlist(
    # unlist and check whether all checks are TRUE or not
    lapply(
      # apply to list (all rows in simulation data frame)
      res$person_diff_ICC.z,
      FUN = function(x) {
        # list of person_diff_ICC.z
        identical(names(x), person_ids_chr) # function to apply: check whether names (IDs) are identical
      }
    )
  ))) { # if not all are TRUE (i.e., checks for all rows)
    stop( # give error message
      "Person IDs are not the same and/or not in the same order across all replications for person_diff_ICC.z"
    )
  }
  

  
  # Check missings
  if (any(is.na(res)) == TRUE) {
    message("Simulation results contain NAs.")
  }
  
  
  # Source aggregate_results Function
  source(aggregate_function_file)
  

  #========================================================================
  # Process simulation results 
  #========================================================================
  
  

  # Diagnostic Information on Redraws -------------------------------------

  # Determine Total Number of Redraws
  # diagnostic information -> not handled like the other outcomes, but we
  # want to report this information either way
  redraw_summary <- list(
    total = sum(res$total_redraws, na.rm=TRUE),
    frequency = as.data.frame(table(res$total_redraws)),
    sum_by_condition = aggregate(total_redraws ~ occasions_drawn + n_occasions + n_items,
                                 data = res, FUN = function(x) {
                                 sum(x, na.rm=TRUE)
                                })
  )
  
  save(redraw_summary, file= file.path(output_dir,
                                       paste0("redraw_summary_",
                                              sim_id,
                                              ".rda")))

  
  # Determine Number and Reasons for Skipped Persons
  skipped_summary <- list(
    total = sum(res$n_skipped_persons_var, na.rm=TRUE),
    frequency = as.data.frame(table(res$n_skipped_persons_var)),
    sum_by_condition = aggregate(n_skipped_persons_var ~ occasions_drawn + n_occasions + n_items,
                                 data = res, FUN = function(x) {
                                   sum(x, na.rm=TRUE)
                                 })
  )
  
  save(skipped_summary, file= file.path(output_dir,
                                        paste0("skipped_summary_",
                                               sim_id,
                                               ".rda")))
  
  
  
  # Calculate %negICC -----------------------------------------------------
  # calculate proportion of negative ICCs
  # -> relative to number of calculated ICCs (i.e., not ALL participants,
  # but participants for whom ICC was calculated at all [no zero variances]
  # and before handling of negative ICCs)
  
  res$percnegICC_raw <- ifelse(
    res$N_merged_ICC_raw == 0, # if there were no ICCs across all participants
    NA_real_, # return NA
    res$negICC_raw / res$N_merged_ICC_raw # divide by number of participants for whom a raw ICC was calculated (before handling of negative ICCs)
  )
  
  # if there were no participants without ICC (due to zero variances),
  # this should be the same when n_total_persons is used
  
  
  
  
  # Extract Person-Level ICC Estimates --------------------------------------
  # '' For ICCs -------------------------------------------------------------
  # extract person-level differences for each replication and each condition per participant
  person_level_ICC_estimates <- data.frame(matrix(nrow=nrow(res), ncol=n_persons+4)) # + 4 for simulation conditions
  person_level_ICC_estimates [ , 1:4] <- res[ , c(1,3:5)] # extract simulation conditions (incl. design_row_id)
  names(person_level_ICC_estimates ) <- c("design_row_id", "n_occasions", "occasions_drawn", "n_items", paste0("person_ICC_", 1:n_persons))
  
  ICC_matrix <- do.call(rbind, res$person_estimates_ICC) # extract the N person_estimates_ICC values per row (replication) and bind them
  # -> matrix of N participants (columns) and their values in each replication (rows)


  
  # check dimensions of matrix and stop if the nrow(matrix) != nrow(res) or
  # ncol(matrix) != n_persons
  stopifnot(nrow(ICC_matrix) == nrow(res))
  stopifnot(ncol(ICC_matrix) == n_persons)
  
  # bind with part_dat
  person_level_ICC_estimates [ , 5:(n_persons+4)] <- ICC_matrix
  
  
  # save
  save(person_level_ICC_estimates ,
       file = file.path(output_dir,
                        paste0(
                          "person_level_ICC_per_replication_",
                          sim_id,
                          ".rda"
                        )))
  
  
  
  # '' For ICC.z ------------------------------------------------------------
  # extract person-level differences for each replication and each condition per participant
  person_level_ICC.z_estimates  <- data.frame(matrix(nrow=nrow(res), ncol=n_persons+4))
  person_level_ICC.z_estimates [ , 1:4] <- res[ , c(1,3:5)]
  names(person_level_ICC.z_estimates ) <- c("design_row_id", "n_occasions", "occasions_drawn", "n_items", paste0("person_ICC.z_", 1:n_persons))
  
  ICC.z_matrix <- do.call(rbind, res$person_estimates_ICC.z) # extract the N person_estimates_ICC.z values per row (replication) and bind them
  # -> matrix of N participants (columns) and their values in each replication (rows)
  
  # check dimensions of matrix and stop if the nrow(matrix) != nrow(res) or
  # ncol(matrix) != n_persons
  stopifnot(nrow(ICC.z_matrix) == nrow(res))
  stopifnot(ncol(ICC.z_matrix) == n_persons)
  
  
  # bind with part_dat
  person_level_ICC.z_estimates [ , 5:(n_persons+4)] <- ICC.z_matrix
  
  
  # save
  save(person_level_ICC.z_estimates,
       file = file.path(output_dir,
                        paste0("person_level_ICC.z_per_replication_",
                               sim_id,
                               ".rda")))

  
  
  
  # Calculate Person-Level Deviation ("Bias") Across Replications -----------
  
  
  # '' For ICCs -------------------------------------------------------------
  # extract person-level differences for each replication and each condition per participant
  person_level_diff <- data.frame(matrix(nrow=nrow(res), ncol=n_persons+4))
  person_level_diff[ , 1:4] <- res[ , c(1,3:5)]
  names(person_level_diff) <- c("design_row_id", "n_occasions", "occasions_drawn", "n_items", paste0("person_diff_ICC_", 1:n_persons))
  
  diff_matrix <- do.call(rbind, res$person_diff_ICC) # extract the N person_diff_ICC values per row (replication) and bind them
  # -> matrix of N participants (columns) and their values in each replication (rows)
  
  # check dimensions of matrix and stop if the nrow(matrix) != nrow(res) or
  # ncol(matrix) != n_persons
  stopifnot(nrow(diff_matrix) == nrow(res))
  stopifnot(ncol(diff_matrix) == n_persons)
  
  
  # bind with part_dat
  person_level_diff[ , 5:(n_persons+4)] <- diff_matrix
  
  
  # save
  save(person_level_diff,
       file = file.path(output_dir,
                        paste0("person_level_diff_per_repl_",
                               sim_id,
                               ".rda")))
  
  
  
  
  # '' For ICC.z ------------------------------------------------------------
  # extract person-level differences for each replication and each condition per participant
  person_level_diff.z <- data.frame(matrix(nrow=nrow(res), ncol=n_persons+4))
  person_level_diff.z[ , 1:4] <- res[ , c(1,3:5)]
  names(person_level_diff.z) <- c("design_row_id", "n_occasions", "occasions_drawn", "n_items", paste0("person_diff_ICC.z_", 1:n_persons))
  
  diff.z_matrix <- do.call(rbind, res$person_diff_ICC.z) # extract the N person_diff_ICC.z values per row (replication) and bind them
  # -> matrix of N participants (columns) and their values in each replication (rows)
  
  # check dimensions of matrix and stop if the nrow(matrix) != nrow(res) or
  # ncol(matrix) != n_persons
  stopifnot(nrow(diff.z_matrix) == nrow(res))
  stopifnot(ncol(diff.z_matrix) == n_persons)
  
  
  # bind with part_dat
  person_level_diff.z[ , 5:(n_persons+4)] <- diff.z_matrix
  
  
  # save
  save(person_level_diff.z,
       file = file.path(output_dir,
                        paste0("person_level_diff.z_per_repl_",
                               sim_id,
                               ".rda")))
  
  
  
  
  # '' Aggegrate Across Replications ----------------------------------------
  
  ## for ICC
  vars <- paste0("person_diff_ICC_", 1:n_persons)
  person_diff_agg <- aggregate(person_level_diff[ , vars],
                               by = person_level_diff[ , c("occasions_drawn", "n_occasions", "n_items")],
                               FUN = function(x) {
                                 if (all(is.na(x))) {
                                   NA_real_ # if person does not have ANY valid value -> return NA
                                 } else {
                                   mean(x, na.rm=TRUE) # calculate mean, but remove NAs
                                 }
                               }) 
  
  names(person_diff_agg) <- c("occasions_drawn", "n_occasions", "n_items", paste0("person_difference_", 1:n_persons))
  
  # save
  save(person_diff_agg,
       file = file.path(output_dir,
                        paste0("person_level_diff_agg_all_part_",
                               sim_id,
                               ".rda")))

  

  ### for ICC.z
  vars <- paste0("person_diff_ICC.z_", 1:n_persons)
  person_diff_agg.z <- aggregate(person_level_diff.z[ , vars],
                                 by = person_level_diff.z[ , c("occasions_drawn", "n_occasions", "n_items")],
                                 FUN = function(x) {
                                   if (all(is.na(x))) {
                                     NA_real_ # if person does not have ANY valid value -> return NA
                                   } else {
                                     mean(x, na.rm=TRUE) # calculate mean, but remove NAs
                                   }
                                 }) 
  
  names(person_diff_agg.z) <- c("occasions_drawn", "n_occasions", "n_items", paste0("person_difference.z_", 1:n_persons))
  
  # save
  save(person_diff_agg.z,
       file = file.path(output_dir,
                        paste0("person_level_diff.z_agg_all_part_",
                               sim_id,
                               ".rda")))


  
  # '' Aggregate Across Participants (for Plotting) -------------------------
  ## for ICC
  # calculate overall difference (mean difference across participants)
  person_diff_agg$difference_mean <- rowMeans(person_diff_agg[ , 4:(n_persons+3)], na.rm=T)
  person_diff_agg$difference_median <- apply(person_diff_agg[ ,4:(n_persons+3)], 1, median, na.rm=T) 
  
  # determine which participant deviates most
  person_diff_agg$difference_min_id <- apply(person_diff_agg[ ,4:(n_persons+3)], 1, which.min)
  person_diff_agg$difference_max_id <- apply(person_diff_agg[ ,4:(n_persons+3)], 1, which.max)
  
  # calculate min and max
  person_diff_agg$difference_min <- apply(person_diff_agg[ ,4:(n_persons+3)], 1, min, na.rm=T)
  person_diff_agg$difference_max <- apply(person_diff_agg[ ,4:(n_persons+3)], 1, max, na.rm=T)
  
  
  person_diff_agg <- person_diff_agg[ , c("occasions_drawn", "n_occasions", "n_items",
                                          "difference_mean", "difference_median", "difference_min_id",
                                          "difference_max_id", "difference_min", "difference_max")]
  
  
  ## for ICC.z
  # calculate overall difference (mean difference across participants)
  person_diff_agg.z$difference.z_mean <- rowMeans(person_diff_agg.z[ , 4:(n_persons+3)], na.rm=T)
  person_diff_agg.z$difference.z_median <- apply(person_diff_agg.z[ ,4:(n_persons+3)], 1, median, na.rm=T) 
  
  # determine which participant deviates most
  person_diff_agg.z$difference.z_min_id <- apply(person_diff_agg.z[ ,4:(n_persons+3)], 1, which.min)
  person_diff_agg.z$difference.z_max_id <- apply(person_diff_agg.z[ ,4:(n_persons+3)], 1, which.max)
  
  # calculate min and max
  person_diff_agg.z$difference.z_min <- apply(person_diff_agg.z[ ,4:(n_persons+3)], 1, min, na.rm=T)
  person_diff_agg.z$difference.z_max <- apply(person_diff_agg.z[ ,4:(n_persons+3)], 1, max, na.rm=T)
  
  
  
  person_diff_agg.z <- person_diff_agg.z[ , c("occasions_drawn", "n_occasions", "n_items",
                                              "difference.z_mean", "difference.z_median", "difference.z_min_id",
                                              "difference.z_max_id", "difference.z_min", "difference.z_max")]
  
  
  
  # Calculate RMSE for Each Participant Across Replications -------------------
  
  
  # '' For ICCs -------------------------------------------------------------
  # use person-level differences -> square
  person_level_diff_sq <- person_level_diff[ ,1:4]
  person_level_diff_sq[ , 5:(n_persons+4)] <- (person_level_diff[ ,5:(n_persons+4)])^2
  names(person_level_diff_sq)[5:(n_persons+4)] <- paste0("sq_diff_ICC_", 1:n_persons)
  
  ### aggregate
  
  # do not calculate RMSE for ordered conditions
  # -> not directly comparable to random draw conditions
  # -> in random draws, we have variability across different drawn measurement occasions
  # -> in ordered draws, the occasions are constant and we have variability across item sets
  # however, we are interested in variability across occasions (replications)
  # -> not comparable
  # (see also Monte Carlo standard error)
  
  # for random-draw conditions, we have replications across different drawn occasions
  # CAVE: Participants may have missing in given replications
  # -> use the number of valid occasions
  # -> automatically determine this
  
  person_level_diff_sq.rd <- person_level_diff_sq[person_level_diff_sq$occasions_drawn == "random", ]
  # only select conditions with random draws of occasions

  # RMSE = sqrt(sum(sq_diff_ICC)/n_replication)
  sq_diff_cols <- paste0("sq_diff_ICC_", 1:n_persons)
  
  RMSE <- aggregate(person_level_diff_sq.rd[ , sq_diff_cols], # for each participant-specific sq_diff column
                    by = person_level_diff_sq.rd[ , c("occasions_drawn", "n_occasions", "n_items")], # aggregate across conditions
                    FUN = function(x) {
                      
                      n_valid <- sum(!is.na(x)) # determine the number of valid replications in given condition for participant
                      
                      if (n_valid == 0) { # if there are no valid replications, return NA
                        NA_real_
                      } else { # else, take the square root of the the summed sq_diff divided by number of (valid) replications
                        sqrt(sum(x, na.rm=TRUE) / n_valid)
                      }
                      
                    })
  names(RMSE)[4:(n_persons+3)] <- c(paste0("RMSE_", 1:n_persons))
  # order
  RMSE <- RMSE[order(RMSE$occasions_drawn, RMSE$n_items, RMSE$n_occasions), ]
  
  
  # also create data frame with information on how many replications per participant were used for RMSE calculation
  
  RMSE_N <- aggregate(person_level_diff_sq.rd[ , sq_diff_cols], # for each participant-specific sq_diff column
                      by = person_level_diff_sq.rd[ , c("occasions_drawn", "n_occasions", "n_items")], # aggregate across conditions
                      FUN = function(x) {
                        
                        sum(!is.na(x)) # determine the number of valid replications in given condition for participant
                        # same code as in RMSE calculation, but now stored individually
                      })
  names(RMSE_N)[4:(n_persons+3)] <- c(paste0("RMSE_N_", 1:n_persons))
  
  RMSE_N <- RMSE_N[order(RMSE_N$occasions_drawn, RMSE_N$n_items, RMSE_N$n_occasions), ]
  
  
  # since we only calculate RMSE for random occasions, we do not need to exclude benchmark row here
  # (would be correctly zero)
  
  
  # save 
  save(RMSE,
       file = file.path(output_dir,
                        paste0("RMSE_values_per_participant_",
                               sim_id,
                               ".rda")))
  
  save(RMSE_N,
       file = file.path(output_dir,
                        paste0("RMSE_repl_N_per_participant_",
                               sim_id,
                               ".rda")))
  
  
  
  # Calculate min, mean, and max across participants
  RMSE$RMSE_min <- apply(RMSE[ , 4:(n_persons+3)], 1, FUN = min, na.rm = TRUE)
  RMSE$RMSE_mean <- rowMeans(RMSE[ ,4:(n_persons+3)], na.rm=TRUE)
  RMSE$RMSE_max <- apply(RMSE[ , 4:(n_persons+3)], 1, FUN = max, na.rm = TRUE)
  # subset 
  RMSE <- RMSE[ , c("occasions_drawn", "n_occasions", "n_items",
                    "RMSE_min", "RMSE_mean", "RMSE_max")]
  
  
  RMSE_N$RMSE_N_min <- apply(RMSE_N[ , 4:(n_persons+3)], 1, FUN = min, na.rm = TRUE)
  RMSE_N$RMSE_N_mean <- rowMeans(RMSE_N[ ,4:(n_persons+3)], na.rm=TRUE)
  RMSE_N$RMSE_N_max <- apply(RMSE_N[ , 4:(n_persons+3)], 1, FUN = max, na.rm = TRUE)
  # subset 
  RMSE_N <- RMSE_N[ , c("occasions_drawn", "n_occasions", "n_items",
                        "RMSE_N_min", "RMSE_N_mean", "RMSE_N_max")]
  
  
  
  
  # '' For ICC.z ------------------------------------------------------------
  # use person-level differences -> square
  person_level_diff.z_sq <- person_level_diff.z[ ,1:4]
  person_level_diff.z_sq[ , 5:(n_persons+4)] <- (person_level_diff.z[ ,5:(n_persons+4)])^2
  names(person_level_diff.z_sq)[5:(n_persons+4)] <- paste0("sq_diff_ICC.z_", 1:n_persons)
  
  
  ### aggregate
  # do not calculate RMSE for ordered conditions
  # -> not directly comparable to random draw conditions
  # -> in random draws, we have variability across different drawn measurement occasions
  # -> in ordered draws, the occasions are constant and we have variability across item sets
  # however, we are interested in variability across occasions (replications)
  # -> not comparable
  # (see also Monte Carlo standard error)
  
  # for random-draw conditions, we have replications across different drawn occasions
  # CAVE: Participants may have missing in given replications
  # -> use the number of valid occasions
  # -> automatically determine this
  
  person_level_diff.z_sq.rd <- person_level_diff.z_sq[person_level_diff.z_sq$occasions_drawn == "random", ]
  # only select conditions with random draws of occasions
  
  # RMSE.z = sqrt(sum(sq_diff_ICC)/n_replication)
  sq_diff_cols <- paste0("sq_diff_ICC.z_", 1:n_persons)
  
  
  RMSE.z <- aggregate(person_level_diff.z_sq.rd[ , sq_diff_cols], # for each participant-specific sq_diff column
                      by = person_level_diff.z_sq.rd[ , c("occasions_drawn", "n_occasions", "n_items")], # aggregate across conditions
                      FUN = function(x) {
                        
                        n_valid <- sum(!is.na(x)) # determine the number of valid replications in given condition for participant
                        
                        if (n_valid == 0) { # if there are no valid replications, return NA
                          NA_real_
                        } else { # else, take the square root of the the summed sq_diff divided by number of (valid) replications
                          sqrt(sum(x, na.rm=TRUE) / n_valid)
                        }
                        
                      })
  names(RMSE.z)[4:(n_persons+3)] <- c(paste0("RMSE.z_", 1:n_persons))
  # order
  RMSE.z <- RMSE.z[order(RMSE.z$occasions_drawn, RMSE.z$n_items, RMSE.z$n_occasions), ]
  
  
  # also create data frame with information on how many replications per participant were used for RMSE calculation

  RMSE.z_N <- aggregate(person_level_diff.z_sq.rd[ , sq_diff_cols], # for each participant-specific sq_diff column
                        by = person_level_diff.z_sq.rd[ , c("occasions_drawn", "n_occasions", "n_items")], # aggregate across conditions
                        FUN = function(x) {
                          
                          sum(!is.na(x)) # determine the number of valid replications in given condition for participant
                          # same code as in RMSE.z calculation, but now stored individually
                        })
  names(RMSE.z_N)[4:(n_persons+3)] <- c(paste0("RMSE.z_N_", 1:n_persons))
  
  RMSE.z_N <- RMSE.z_N[order(RMSE.z_N$occasions_drawn, RMSE.z_N$n_items, RMSE.z_N$n_occasions), ]
  
  
  # since we only calculate RMSE for random occasions, we do not need to exclude benchmark row here
  # (would be correctly zero)
  
  
  # save 
  save(RMSE.z,
       file = file.path(output_dir,
                        paste0("RMSE.z_values_per_participant_",
                               sim_id,
                               ".rda")))
  
  
  save(RMSE.z_N,
       file = file.path(output_dir,
                        paste0("RMSE.z_repl_N_per_participant_",
                               sim_id,
                               ".rda")))
  
  
  
  # Calculate min, mean, and max across participants
  RMSE.z$RMSE.z_min <- apply(RMSE.z[ , 4:(n_persons+3)], 1, FUN = min, na.rm = TRUE)
  RMSE.z$RMSE.z_mean <- rowMeans(RMSE.z[ ,4:(n_persons+3)], na.rm=TRUE)
  RMSE.z$RMSE.z_max <- apply(RMSE.z[ , 4:(n_persons+3)], 1, FUN = max, na.rm = TRUE)
  # subset 
  RMSE.z <- RMSE.z[ , c("occasions_drawn", "n_occasions", "n_items",
                        "RMSE.z_min", "RMSE.z_mean", "RMSE.z_max")]
  
  
  RMSE.z_N$RMSE.z_N_min <- apply(RMSE.z_N[ , 4:(n_persons+3)], 1, FUN = min, na.rm = TRUE)
  RMSE.z_N$RMSE.z_N_mean <- rowMeans(RMSE.z_N[ ,4:(n_persons+3)], na.rm=TRUE)
  RMSE.z_N$RMSE.z_N_max <- apply(RMSE.z_N[ , 4:(n_persons+3)], 1, FUN = max, na.rm = TRUE)
  # subset 
  RMSE.z_N <- RMSE.z_N[ , c("occasions_drawn", "n_occasions", "n_items",
                            "RMSE.z_N_min", "RMSE.z_N_mean", "RMSE.z_N_max")]
  
  
  
  
  
  
  # Aggregate Results -------------------------------------------------------
  agg <- aggregate_results(res,
                           outcomes = c('N_merged_ICC_raw', 'N_merged_ICC_handled',
                                        'N_valid_ICC.z_handled',
                                        'N_cor_ICC', 'N_cor_ICC.z',
                                        'N_rel',
                                        'cor_ICC', 'cor_ICC.z',
                                        'rel',
                                        'sd_ICC', 'sd_ICC.z',
                                        'negICC_raw', 'negICC_handled', 'percnegICC_raw',
                                        'estimationProbNeg_raw', 'estimationProbPos_raw',
                                        'total_redraws', 'n_valid_persons_var', 'n_skipped_persons_var'),
                           rel_outcomes = c('cor_ICC', 'cor_ICC.z'),
                           abs_outcomes = c('N_merged_ICC_raw', 'N_merged_ICC_handled',
                                            'N_valid_ICC.z_handled',
                                            'N_cor_ICC', 'N_cor_ICC.z',
                                            'N_rel',
                                            'rel',
                                            'sd_ICC', 'sd_ICC.z',
                                            'negICC_raw', 'negICC_handled', 'percnegICC_raw',
                                            'estimationProbNeg_raw', 'estimationProbPos_raw',
                                            'total_redraws', 'n_valid_persons_var', 'n_skipped_persons_var'))
  
  
  # merge difference to agg
  agg_res <- list(person_diff_agg)
  names(agg_res) <- "agg_res"
  agg$person_diff <- agg_res
  
  agg_res <- list(person_diff_agg.z)
  names(agg_res) <- "agg_res"
  agg$person_diff.z <- agg_res
  
  
  
  # merge RMSE to agg
  agg_res <- list(RMSE) # should be nested as other outcomes so that function works
  names(agg_res) <- "agg_res"
  agg$RMSE_ICC <- agg_res
  
  agg_res <- list(RMSE_N) # should be nested as other outcomes so that function works
  names(agg_res) <- "agg_res"
  agg$RMSE_ICC_N <- agg_res
  
  
  agg_res <- list(RMSE.z) # should be nested as other outcomes so that function works
  names(agg_res) <- "agg_res"
  agg$RMSE_ICC.z <- agg_res
  
  
  agg_res <- list(RMSE.z_N) # should be nested as other outcomes so that function works
  names(agg_res) <- "agg_res"
  agg$RMSE_ICC.z_N <- agg_res
  

  
  
  # Save Aggregated Results -------------------------------------------------
  save(agg,
       file = file.path(output_dir,
                        paste0("aggregated_results_",
                               sim_id,
                               ".rda")))
  
  
  
  
  #----------------------------------------------------------------------------------------
  
  
  
  
  
  # Calculate Monte Carlo Standard Error ------------------------------------
  # for formulas, see Siepe et al. (2024), doi: 10.1037/met0000695
  
  
  # use subset with random draws (ordered draws are not independent
  # and the only "replications" there are are the different item sets)
  rd <- res[which(res$occasions_drawn == "random"),]
  
  
  
  max(rd$n_iteration) # maximum number of iterations
  
  
  
  # write function to calculate MCSE for mean of generic statistic G
  # (used for all outcomes except person-level difference ("bias") and person-level RMSE)
  
  mcse_generic <- function(x) {
    
    r_valid <- sum(!is.na(x)) # determine number of valid replications
    
    if (r_valid < 2) { # if there is only one replication -> no variability -> no MCSE
      return(NA_real_) # return empty value (NA)
    } else {
      
      x_valid <- x[!is.na(x)] # use only those values of the outcome that are not NA
      
      MCSE <- sqrt( ( (sum( ( x_valid - (sum(x_valid)/r_valid ) )^2 )) / (r_valid - 1) ) / r_valid )
      # use r_valid (number of valid replications [i.e., replication with value instead of NA]) to calculate MCSE
      return(MCSE)
      
    }
    
  }
  
  
  # Calculate MCSE:
  
  ## for correlations (ICC)
  # use generic formula
  MCSE1 <- do.call(data.frame,
                   aggregate(cor_ICC ~ n_occasions + n_items, data = rd,
                             FUN = mcse_generic))
  
  names(MCSE1) <- c("n_occasions", "n_items", "cor_ICC_MCSE")
  
  
  
  ## for correlations (ICC.z)
  # use generic formula
  
  MCSE2 <- do.call(data.frame,
                   aggregate(cor_ICC.z ~ n_occasions + n_items, data = rd,
                             FUN = mcse_generic))
  
  names(MCSE2) <- c("n_occasions", "n_items", "cor_ICC.z_MCSE")
  
  
  ## for reliability
  # -> mean of generic statistic G
  
  MCSE3 <- do.call(data.frame,
                   aggregate(rel ~ n_occasions + n_items, data = rd,
                             FUN = mcse_generic))
  
  names(MCSE3) <- c("n_occasions", "n_items", "rel_MCSE")
  
  
  ## for SD (ICC)
  # use formula for mean of generic statistic G
  
  MCSE4 <- do.call(data.frame,
                   aggregate(sd_ICC ~ n_occasions + n_items, data = rd,
                             FUN = mcse_generic))
  
  names(MCSE4) <- c("n_occasions", "n_items", "sd_ICC_MCSE")
  
  
  ## for SD (ICC.z)
  # use formula for mean of generic statistic G
  
  MCSE5 <- do.call(data.frame,
                   aggregate(sd_ICC.z ~ n_occasions + n_items, data = rd,
                             FUN = mcse_generic))
  
  names(MCSE5) <- c("n_occasions", "n_items", "sd_ICC.z_MCSE")
  
  
  ## for % negICC
  # performance measure: mean of generic statistic G
  
  MCSE6 <- do.call(data.frame,
                   aggregate(percnegICC_raw ~ n_occasions + n_items, data = rd,
                             FUN = mcse_generic))
  
  names(MCSE6) <- c("n_occasions", "n_items", "percnegICC_raw_MCSE")
  
  ## for estimProbNeg
  # performance measure: mean of generic statistic G
  
  MCSE7 <- do.call(data.frame,
                   aggregate(estimationProbNeg_raw ~ n_occasions + n_items, data = rd,
                             FUN = mcse_generic))
  
  names(MCSE7) <- c("n_occasions", "n_items", "estimationProbNeg_raw_MCSE")
  
  
  ## for estimProbPos
  # performance measure: mean of generic statistic G
  
  MCSE8 <- do.call(data.frame,
                   aggregate(estimationProbPos_raw ~ n_occasions + n_items, data = rd,
                             FUN = mcse_generic))
  
  names(MCSE8) <- c("n_occasions", "n_items", "estimationProbPos_raw_MCSE")
  
  
  
  # combine
  MCSE <- merge(MCSE1, MCSE2, by = c("n_occasions", "n_items"))
  MCSE <- merge(MCSE, MCSE3, by = c("n_occasions", "n_items"))
  MCSE <- merge(MCSE, MCSE4, by = c("n_occasions", "n_items"))
  MCSE <- merge(MCSE, MCSE5, by = c("n_occasions", "n_items"))
  MCSE <- merge(MCSE, MCSE6, by = c("n_occasions", "n_items"))
  MCSE <- merge(MCSE, MCSE7, by = c("n_occasions", "n_items"))
  MCSE <- merge(MCSE, MCSE8, by = c("n_occasions", "n_items"))
  
  MCSE
  names(MCSE)
  
  
  
  # for person-level differences (person-level "bias")
  # for each participant, calculate sampling variance and mean of differences ("bias") (across replications per condition)
  # -> calculate MCSE per participant and condition
  # for formula, see Siepe et al. (2024), Table 3, formula for bias
  
  # MCSE = sqrt( s^2(estimates) / nsim )
  # sampling variance -> sampling variance of estimates (not difference!)
  # nsim -> here: valid number of replications per participant and condition
  
  ## for ICCs
  # use data from random draws only
  person_level_ICC_estimates.rd <- person_level_ICC_estimates[
    which(person_level_ICC_estimates$occasions_drawn == "random"), 
  ]
  
  
  # automize over participants
  # get ICC columns (one for each participant)
  ICC_cols <- grep("^person_ICC_", names(person_level_ICC_estimates.rd), value=TRUE)
  
  # calculate MCSE
  MCSE_difference <- aggregate(person_level_ICC_estimates.rd[ , ICC_cols],
                               by = person_level_ICC_estimates.rd[ , c("n_occasions", "n_items")],
                               FUN = function(x) {
                                 
                                 r_valid <- sum(!is.na(x)) # determine number of valid replications (without NA)
                                 
                                 if (r_valid < 2) { # if there is only one replication -> no variability -> no MCSE
                                   return(NA_real_)
                                 } else {
                                   
                                   x_valid <- x[!is.na(x)] # use only those values of the outcome that are not NA
                                   
                                   # calculate MCSE across valid values and use number of valid replications (r_valid)
                                   MCSE <- sqrt( ( (sum( ( x_valid - (sum(x_valid)/r_valid) )^2 )) / (r_valid - 1) ) / r_valid )
                                   
                                 }
                                 
                               })
  
  # adjust col names
  names(MCSE_difference) <- c("n_occasions", "n_items", paste0("MCSE_difference_", sub("^person_ICC_", "", ICC_cols)))
  
  # save
  save(MCSE_difference,
       file = file.path(output_dir,
                        paste0("MCSE_difference_per_participant_",
                               sim_id,
                               ".rda")))
  
  
  # calculate mean, min, max per condition
  MCSE_difference$MCSE_difference_mean <- rowMeans(MCSE_difference[ , c(3:(n_persons+2))], na.rm=T)
  MCSE_difference$MCSE_difference_min <- apply(MCSE_difference[ , 3:(n_persons+2)], 1, FUN = min, na.rm = TRUE)
  MCSE_difference$MCSE_difference_max <- apply(MCSE_difference[ , 3:(n_persons+2)], 1, FUN = max, na.rm = TRUE)
  
  
  
  # add to MCSE object
  MCSE <- merge(MCSE, MCSE_difference[ , c("n_occasions", "n_items", "MCSE_difference_min", "MCSE_difference_mean", "MCSE_difference_max")],
                by = c("n_occasions", "n_items"))
  
  
  
  
  ## for ICC.z
  # use data from random draws only
  person_level_ICC.z_estimates.rd <- person_level_ICC.z_estimates[
    which(person_level_ICC.z_estimates$occasions_drawn == "random"),
  ]
  
  
  # automize over participants
  # get ICC columns (one for each participant)
  ICC_cols <- grep("^person_ICC.z_", names(person_level_ICC.z_estimates.rd), value=TRUE)
  
  
  MCSE_difference.z <- aggregate(person_level_ICC.z_estimates.rd[ , ICC_cols],
                                 by = person_level_ICC.z_estimates.rd[ , c("n_occasions", "n_items")],
                                 FUN = function(x) {
                                   
                                   r_valid <- sum(!is.na(x)) # determine number of valid replications (without NA)
                                   
                                   if (r_valid < 2) { # if there is only one replication -> no variability -> no MCSE
                                     return(NA_real_)
                                   } else {
                                     
                                     x_valid <- x[!is.na(x)] # use only those values of the outcome that are not NA
                                     
                                     # calculate MCSE across valid values and use number of valid replications (r_valid)
                                     MCSE <- sqrt( ( (sum( ( x_valid - (sum(x_valid)/r_valid) )^2 )) / (r_valid - 1) ) / r_valid )
                                     
                                   }
                                 })
  # adjust col names
  names(MCSE_difference.z) <- c("n_occasions", "n_items", paste0("MCSE_difference.z_", sub("^person_ICC.z_", "", ICC_cols)))
  
  
  # save
  save(MCSE_difference.z,
       file = file.path(output_dir,
                        paste0("MCSE_difference.z_per_participant_",
                               sim_id,
                               ".rda")))
  
  
  # calculate mean, min, max per condition
  MCSE_difference.z$MCSE_difference.z_mean <- rowMeans(MCSE_difference.z[ , c(3:(n_persons+2))], na.rm=T)
  MCSE_difference.z$MCSE_difference.z_min <- apply(MCSE_difference.z[ , 3:(n_persons+2)], 1, FUN = min, na.rm = TRUE)
  MCSE_difference.z$MCSE_difference.z_max <- apply(MCSE_difference.z[ , 3:(n_persons+2)], 1, FUN = max, na.rm = TRUE)
  
  # add to MCSE object
  MCSE <- merge(MCSE, MCSE_difference.z[ , c("n_occasions", "n_items", "MCSE_difference.z_min", "MCSE_difference.z_mean", "MCSE_difference.z_max")],
                by = c("n_occasions", "n_items"))
  
  
  
  
  
  # for RMSE per participant
  # for each participant, calculate sampling variance of squared errors
  # and mean of squared errors (across replications per condition)
  # -> calculate MCSE per participant and condition
  
  # for formula, see Siepe et al. (2024), Table 3, formula for MCSE of RMSE
  # MSE hat = expected value for squared errors = mean of squared errors across replications
  
  # use data from random draws only
  person_level_diff_sq.rd <- person_level_diff_sq[which(person_level_diff_sq$occasions_drawn == "random"), ]
  
  
  
  ## for ICCs
  # automize over participants
  sq_diff_cols <- grep("^sq_diff_ICC_", names(person_level_diff_sq.rd), value=TRUE)
  
  
  MCSE_RMSE <- aggregate(person_level_diff_sq.rd[ , sq_diff_cols],
                         by = person_level_diff_sq.rd[ , c("n_occasions", "n_items")],
                         FUN = function(x) {
                           
                           r_valid <- sum(!is.na(x)) # determine number of valid replications (without NA)
                           
                           if (r_valid < 2) { # if there is only one replication -> no variability -> no MCSE
                             return(NA_real_)
                           } else {
                             
                             x_valid <- x[!is.na(x)] # use only those values of the outcome that are not NA
                             
                             # calculate MCSE across valid values and use number of valid replications (r_valid)
                             MCSE <- sqrt( ( (sum( ( x_valid - (sum(x_valid)/r_valid) )^2 )) / (r_valid - 1) ) / (4*r_valid*mean(x_valid)))
                           }
                         })
  
  names(MCSE_RMSE) <- c("n_occasions", "n_items", paste0("MCSE_RMSE_", sub("^sq_diff_ICC_", "", sq_diff_cols )))
  
  
  
  # save
  save(MCSE_RMSE, 
       file = file.path(output_dir,
                        paste0("MCSE_RMSE_per_participant_",
                               sim_id,
                               ".rda")))
  
  
  # calculate mean, min, max per condition
  MCSE_RMSE$MCSE_RMSE_mean <- rowMeans(MCSE_RMSE[ , c(3:(n_persons+2))], na.rm=T)
  MCSE_RMSE$MCSE_RMSE_min <- apply(MCSE_RMSE[ , 3:(n_persons+2)], 1, FUN = min, na.rm = TRUE)
  MCSE_RMSE$MCSE_RMSE_max <- apply(MCSE_RMSE[ , 3:(n_persons+2)], 1, FUN = max, na.rm = TRUE)
  
  
  # add to MCSE object
  MCSE <- merge(MCSE, MCSE_RMSE[ , c("n_occasions", "n_items", "MCSE_RMSE_min", "MCSE_RMSE_mean", "MCSE_RMSE_max")],
                by = c("n_occasions", "n_items"))
  
  
  
  
  ## for ICC.z
  person_level_diff.z_sq.rd <- person_level_diff.z_sq[which(person_level_diff.z_sq$occasions_drawn == "random"), ]
  
  
  sq_diff_cols <- grep("^sq_diff_ICC.z_", names(person_level_diff.z_sq.rd), value=TRUE)
  
  MCSE_RMSE.z <- aggregate(person_level_diff.z_sq.rd[ , sq_diff_cols],
                           by = person_level_diff.z_sq.rd[ , c("n_occasions", "n_items")],
                           FUN = function(x) {
                             
                             r_valid <- sum(!is.na(x)) # determine number of valid replications (without NA)
                             
                             if (r_valid < 2) { # if there is only one replication -> no variability -> no MCSE
                               return(NA_real_)
                             } else {
                               
                               x_valid <- x[!is.na(x)] # use only those values of the outcome that are not NA
                               
                               # calculate MCSE across valid values and use number of valid replications (r_valid)
                               MCSE <- sqrt( ( (sum( ( x_valid - (sum(x_valid)/r_valid) )^2 )) / (r_valid - 1) ) / (4*r_valid*mean(x_valid)))
                             }
                             
                           })
  
  names(MCSE_RMSE.z) <- c("n_occasions", "n_items", paste0("MCSE_RMSE.z_", sub("^sq_diff_ICC.z_", "", sq_diff_cols )))
  
  
  # save
  save(MCSE_RMSE.z,
       file = file.path(output_dir,
                        paste0("MCSE_RMSE.z_per_participant_",
                               sim_id,
                               ".rda")))
  
  
  # calculate mean, min, max per condition
  MCSE_RMSE.z$MCSE_RMSE.z_mean <- rowMeans(MCSE_RMSE.z[ , c(3:(n_persons+2))], na.rm=T)
  MCSE_RMSE.z$MCSE_RMSE.z_min <- apply(MCSE_RMSE.z[ , 3:(n_persons+2)], 1, FUN = min, na.rm = TRUE)
  MCSE_RMSE.z$MCSE_RMSE.z_max <- apply(MCSE_RMSE.z[ , 3:(n_persons+2)], 1, FUN = max, na.rm = TRUE)
  
  
  
  # merge to MCSE
  MCSE <- merge(MCSE, MCSE_RMSE.z[ , c("n_occasions", "n_items", "MCSE_RMSE.z_min", "MCSE_RMSE.z_mean", "MCSE_RMSE.z_max")],
                by = c("n_occasions", "n_items"))
  
  
  
  # round and save MCSE as csv
  # round to 3 decimals in this case
  mcse_cols <- grep("MCSE", names(MCSE))
  MCSE[mcse_cols] <- lapply(MCSE[mcse_cols], round, 3)
  
  MCSE <- MCSE[order(MCSE$n_occasions, MCSE$n_items), ] # order
  write.csv(MCSE,
            file = file.path(output_dir,
                             paste0("MCSE_table_",
                                    sim_id,
                                    ".csv")),
            row.names = F)


  #===================================================================
  # Save total output
  
  output <- list(
    sim_id = sim_id,
    input_file = input_file,
    output_dir = output_dir,
    n_persons = n_persons,
    redraw_summary = redraw_summary,
    agg = agg,
    MCSE = MCSE
  )
  
  return(output)
}
