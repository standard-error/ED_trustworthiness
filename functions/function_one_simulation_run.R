###################################################################
#####    Estimating trait negative emotion differentiation:   #####
#####        How many measurement occasions and               #####
#####              emotion items are needed?                  #####
###################################################################

###################################################################
#####            Function for One Simulation Run              #####
###################################################################

# one simulation run: takes the input data, handles it according
# to the parameters in the simulation design matrix,
# calculates the ICCs and calculates outcome statistics on them
# (i.e., mean, SD, ...)
# i.e., one simulation = one row in the simulation matrix
# absolute outcome measures can be calculated within each simulation run
# relative outcome measures need the benchmark data
# -> take benchmark data as argument
# --> split into two parts for clarity


# function takes as input all parameters that we want to vary:
# number of occasions, occasions drawn (random vs. ordered) and
# number of items (and therefore also item content)
# function also takes as input all arguments that we need for
# functions used:
# data, id.var, occ.running.var, nr.of.occasions
# type (for ICC), unit (for ICC)
# function also takes as input the benchmark data (ICCs calculated for benchmark)
# that the relative outcome measures are calculated in comparison to: benchmark_ICCdata

# items (for ICC) -> determined in overall simulation study (constant across replications)
# type (for ICC): default here is consistency, but could be varied in principle in simulation
# unit (for ICC): default here is single measurements, but could be varied in principle in simulation




### PART 1: DATA PREPARATION ACCORDING TO DESIGN CHOICES
one_sim_data_manipulation <- function(data, nr.of.occasions, occasions.drawn,
                                      nr.of.items, items, id.var, occ.running.var) {
  
  # data: takes the data frame with all participants
  # and their occasions as input (long format) = benchmark data
  # nr.of.occasions: number of occasions to draw per participant (for ICC calculation)
  # occasions.drawn: whether occasions per participant are drawn by order or randomly
  # nr.of.items: number of items that shall be used for ICC calculation
        # also determines the type of items
  # items: items that shall be used for ICC calculation
  # id.var: character that indicates name of participant ID variable
  # occ.running.var: character that indicates the name of the occasion running variable

  

  
  # DRAW OCCASIONS FOR EACH PARTICIPANT
  if (occasions.drawn == "random") {
    # for random draws, we have a count variable that reflects how many re-draws we had across all participants
    # -> drawn_data and total_redraws are stored in a list
    # -> use temporary outcome list and extract the data and count variable separately
    drawn_all <- random_occasion_draw(data = data, # insert start data (full data set provided in argument)
                                       id.var = id.var, # pass id.var 
                                       occ.running.var = occ.running.var, # pass occ.running.var
                                       nr.of.occasions = nr.of.occasions,  # pass nr.of.occasions
                                       items = items) # pass items 
    

  } else if (occasions.drawn == "by order") {
    # for ordered draws, we do not have cases with zero variance -> no re-draws
    # -> was checked before running the simulation as a whole
    drawn_all <- ordered_occasion_draw(data = data, # insert start data (full data set provided in argument)
                                        id.var = id.var, # pass id.var 
                                        occ.running.var = occ.running.var, # pass occ.running.var
                                        nr.of.occasions = nr.of.occasions,# pass nr.of.occasions
                                        items = items) # pass items
  }
  
  
  # Extract information from drawings (e.g., number of skipped persons, redraws, ...)
  # -> diagnostic features
  drawn_data <- drawn_all$drawn_data
  total_redraws <- drawn_all$total_redraws
  n_total_persons <- drawn_all$n_total_persons
  n_valid_persons <- drawn_all$n_valid_persons
  n_skipped_persons <- drawn_all$n_skipped_persons
  draw_log <- drawn_all$draw_log # also save person overview of draw_log
  
  # return list with drawn_data, total_redraws and other information
  return(list(
    drawn_data = drawn_data,
    total_redraws = total_redraws,
    n_total_persons = n_total_persons,
    n_valid_persons = n_valid_persons,
    n_skipped_persons = n_skipped_persons,
    draw_log = draw_log
  ))

}



### PART 2: CALCULATE OUTCOMES MEASURES

one_sim_outcome_measures <- function(benchmark_ICCdata, sim_ICCdata_handled,
                                     sim_ICCdata_raw, id.var,
                                     nr.of.items, nr.of.occasions) {
  # benchmark_ICCdata: data frame of the ICC data estimated with benchmark data
      # variable names need to be: id.var, bench_ICC, bench_ICC.z
  # sim_ICCdata_handled: data frame of the ICC data estimated with simulated data (data adjusted
                # according to simulation design features) AND  with negative ICCs handled accordingly
  # sim_ICCdata_raw: data frame of the ICC data estimated with simulated data BEFORE HANDLING of negative ICCs
      # BUT BEFORE HANDLING --> needed to determine estimation problems and negative ICCs
  # id.var: character indicating name of ID variable
  # nr.of.items: number of items (as in simulation design)
  # nr.of.occasions: number of occasions per participant (as in simulation design)
  
  # merge benchmark ICC data and simulated ICC data (with negative ICCs HANDLED) by id.var
  merged <- merge(benchmark_ICCdata, sim_ICCdata_handled, by = id.var, all.x = TRUE)
  
  # restore benchmark person order after merge (fixed order across all simulation runs):
  merged <- merged[match(benchmark_ICCdata[ , id.var], merged[ , id.var]), ]
  rownames(merged) <- NULL # reset row numbers
  
  # also use the raw simulated ICC data (WITHOUT handling of negative ICCs)
  # to determine number of negative ICCs, number of estimation problems
  merged_raw <- merge(benchmark_ICCdata, sim_ICCdata_raw, by = id.var, all.x = TRUE)
  merged_raw <- merged_raw[match(benchmark_ICCdata[ , id.var], merged_raw[ , id.var]), ]
  rownames(merged_raw) <- NULL # reset row numbers
  

  # Store number of participants for whom ICC was calculated in simulation:
  # not nrow(merged) -> contains ALL participants
  # -> use only those that were not skipped in data manipulation
  # -> i.e., those whose comp_ICC is not NA
  # compare later with n_valid_persons
  # use raw data BEFORE handling of negative ICCs here (merged_raw)
  # how many ICCs were - in principle - calculated before we handled them
  N_merged_ICC_raw <- sum(!is.na(merged_raw$comp_ICC))
  
  # also store total number
  N_merged_total_raw <- nrow(merged_raw)
  
  # Now determine how many ICCs are there after handling of negative ICCs
  # in case of exclusion, this + number of negative ICCs should add up to N_merged_ICC
  N_merged_ICC_handled <- sum(!is.na(merged$comp_ICC))
  N_merged_total_handled <- nrow(merged)

  
  # Some participants may have an ICC.z of +/- infinite or NaN
  # -> estimation problems
  # -> only use participants with valid values and store number of 
  # participants used for reliability analysis and other outcomes
  # involving ICC.z
  
  # Explanation:
  # some participants may have NaN for ICC.z because the Fisher's Z-transformation
  # uses the natural logarithm, which is not defined for values <= 0. The
  # expression within log() becomes negative (< 0) if the numerator is positive and the
  # denominator is negative, and vice versa.
  # (for formula, see McGraw & Wong, 1996, Appendix B, doi: 10.1037/1082-989X.1.1.30)
  # K_i = number of items
  # numerator: 1+(K_i - 1)*ICC
  # denominator: 1 - ICC
  
  # Case 1: Denominator negative (and numerator positive)
  # if ICC > 1
  # Case 2: Numerator negative (and denominator positive)
  # if ICC < -1/(K_i - 1)
  
  # Furthermore, the expression within log() becomes 0
  # if ICC == -1/(K_i - 1)
  
  # The expression within log() is also not defined for
  # ICC == 1
  # because the denominator would be 0.
  
  # Therefore, the domain of the function is:
  # D = R \ {<= -1/(K_i - 1), >= 1}
  # D = ]-1/(K_i - 1); 1[
  
  # So, strictly speaking the function is not defined for ICC <= -1/(K_i - 1)
  # and ICC >= 1. However, for ICC = -1/(K_i - 1) and ICC = 1,
  # R returns infinite values, as the function
  # tends towards -inf / inf at the boundaries of its domain.
  
  
  # clean invalid ICC.z from merged (will be used to store participant-specific values)
  merged[ , "comp_ICC.z"][
    is.nan(merged[ , "comp_ICC.z"]) |
      is.infinite(merged[ , "comp_ICC.z"])
  ] <- NA # set NaN and infinite to NA
  
  
  
  # exclude participants with invalid ICC.z
  # (i.e., create clean data set for ICC.z analyses)
  # if ICC.z = NA (if ICC = NA), remove participant from analyses
  # (NA -> stems from ICC = NA or ICC.z is NaN or ICC.z is infinite)
  merged.c <- merged[!is.na(merged[ , "comp_ICC.z"]), ]
  
  # if all participants have valid ICC.z, then merged.c = merged
  
  
  N_valid_ICC.z_handled <- nrow(merged.c)
  #### FOR ICC.Z OUTCOMES, MERGED.C IS USED 
  # except for when we want to store ICCs and ICC.z of ALL participants
  
  
  
  ## ABSOLUTE OUTCOMES ##
  # absolute outcomes are independent of benchmark ICC data
  # -> use the ICCs of the sim_ICCdata only (either ith sim_ICCdata object or with corresponding
  # rows in the merged data frame)
  
  # ESTIMATION PROBLEMS
  # only relevant for raw ICCs (not transformed)
  # USE ICCS BEFORE HANDLING OF NEGATIVE ICCS HERE (merged_raw)
  
  # ESTIMATION PROBLEMS REFER TO THE RAW ICCS BEFORE ANY HANDLING
  
  # define estimation problems:
  # Fisher's Z-transformationuses the natural logarithm,
  # which is not defined for negative values.
  # The expression within log() becomes negative if the numerator is positive
  # and the denominator is negative, and vice versa.
  # (for formula, see McGraw & Wong, 1996, Appendix B, doi: 10.1037/1082-989X.1.1.30)
  # K_i = number of items
  # numerator: 1+(K_i - 1)*ICC
  # denominator: 1 - ICC
  
  # Case 1: Denominator negative (and numerator positive)
  # if ICC > 1
  # Case 2: Numerator negative (and denominator positive)
  # if ICC < -1/(K_i - 1)
  
  # for ICC = 1 -> ICC.z = inf
  # for ICC = -1/(K_i -1) -> ICC.z = -inf
  
  # -> estimationProbNeg -> if ICC =< -1/(K_i - 1)
  # -> estimationProbPos -> if ICC >= 1
  
  # K_i will be defined below (for reliability)
  # plug in nr.of.items here for K_i
  
  # number of ICCs <= -1/(K_i -1)
  estimationProbNeg_raw <- sum(
    merged_raw[ , "comp_ICC"] <= ( (-1) / (nr.of.items - 1) ),
    na.rm = TRUE
  )
  
  
  # number of ICCs >= 1
  estimationProbPos_raw <- sum(
    merged_raw[ , "comp_ICC"] >= 1,
    na.rm = TRUE
  )
  
  
  # number of negative ICCs
  # (theoretically impossible)
  negICC_raw <- sum(
    merged_raw[ , "comp_ICC"] < 0,
    na.rm=TRUE
  )
  
  # sanity check: number of negative ICCs after handling
  negICC_handled <- sum(
    merged[ , "comp_ICC"] < 0,
    na.rm=TRUE
  )
  
  
  # now use the handled ICCs (merged)
  # -> ICCs after handling of negative ICCs
  
  # STANDARD DEVIATION OF ICCs
  sd_ICC <- sd(merged[ , "comp_ICC"], na.rm=TRUE) # raw ICCs (remove NAs -> for participants who had zero variance, we have NA)
  sd_ICC.z <- sd(merged.c[ , "comp_ICC.z"], na.rm=TRUE) # transformed ICCs
  
  
  # RELIABILITY
  # according to Schneider & Junghaenel (2023)
  # can only be calculated for transformed ICCs
  # ICCs are already transformed in data frame (ICC.z)
  
  # some participants may have ICC +/- infinite after transformation (for ICC = 1
  # or ICC = -1/(K_i - 1))
  # or they may have NaN (for ICC > 1 or ICC < -1/(K_i - 1))
  # -> problems for reliability estimation
  # -> only use participants with valid values (merged.c) and store number of 
  # participants used for reliability analysis


  if (N_valid_ICC.z_handled  < 2 ) { # if there are NO or only ONE valid ICC.z (for reliability analysis)
    rel <- NA_real_ # return NA for reliability and the number of N_valid_ICC.z_valid for N_rel
    N_rel <- N_valid_ICC.z_handled # should be 0 or 1
  } else { # else calculate reliability with ICC.z
    
    # calculate sampling variance (the same for all participants -> all have the same
    # number of items and occasions)
    K_i <- nr.of.items
    T_i <- nr.of.occasions
    merged.c[ , "sampvar"] <- K_i / (2*(T_i - 2)*(K_i - 1))
    
    # calculate I² as reliability measure
    meta <- metaSEM::meta(data = merged.c, # use data set with participants with valid ICC.z only
                          y = comp_ICC.z, # use Fisher's Z-transformed values
                          v = sampvar,
                          I2 = "I2am",
                          intervals.type = "LB")
    
    rel <- summary(meta)$I2.values$Estimate # extract reliability estimate
    N_rel <- summary(meta)$no.studies # extract number of participants used for reliability estimation
    
  }
  
  
  
  ## RELATIVE OUTCOMES ##
  
  # PERSON-LEVEL DIFFERENCE OF ICCs FROM BENCHMARK ICCs ("bias")
  # for both raw ICCs and transformed ICCs
  # here, we do not use merged.c for ICC.z since we want vectors with ALL participants that we can store
  
  merged[ , "difference_ICC"] <- merged[ , "comp_ICC"] - merged[ , "bench_ICC"] # difference between raw ICCs
  merged[ , "difference_ICC.z"] <- merged[ , "comp_ICC.z"] - merged[ , "bench_ICC.z"] # difference between transformed ICCs
  
  
  # store differences
  person_diff_ICC <- merged[ , "difference_ICC"] 
  names(person_diff_ICC) <- merged[ , id.var] # use ID variable as names of the vector -> each element (ICC) linked with person ID
  person_diff_ICC.z <- merged[ , "difference_ICC.z"]
  names(person_diff_ICC.z) <- merged[ , id.var] # use ID variable as names of the vector -> each element (ICC) linked with person ID
  
  # also store estimate per person -> needed for MCSE of "bias" (i.e., person-level difference)
  person_estimates_ICC <- merged[ , "comp_ICC"] # use FULL person vector here! (invalid values will be NA)
  names(person_estimates_ICC) <- merged[ , id.var] # use ID variable as names of the vector -> each element (ICC) linked with person ID
  person_estimates_ICC.z <- merged[ , "comp_ICC.z"] # use FULL person vector here! (invalid values will be NA)
  names(person_estimates_ICC.z) <- merged[ , id.var] # use ID variable as names of the vector -> each element (ICC) linked with person ID
  
  # here, we do not use merged.c for ICC.z because we want to store values for ALL participants
  # so that we can later on calculate person-specific outcomes across all replications
  
  
  
  
  # RMSE
  # root mean square error (for both raw ICCs and transformed ICCs)
  # -> compare ICC from comparison condition to benchmark ICC
  # --> deviation of the comp_ICC from the bench_ICC for each participant
  # -> calculate an RMSE for each participant across all replications of a condition
  # since we have a single simulation run (i.e., one replication) here, we cannot yet
  # calculate the person-wise RMSE.
  # instead, calculate difference between replication ICC and benchmark ICC for each participant
  # and store this as a matrix for further calculations later
  # -> later on, for each participant, the squared differences will be summed, divided by number of replications
  # and square root taken (separately for each condition)
  # -> one RMSE per person
  
  # difference: calculated above
  

  
  # CORRELATION WITH BENCHMARK
  # for both raw ICCs and transformed ICCs
  # determine number of participants used for correlation calculation
  N_cor_ICC <- sum(complete.cases(merged[ , c("comp_ICC", "bench_ICC")]))
  N_cor_ICC.z <- sum(complete.cases(merged.c[ , c("comp_ICC.z", "bench_ICC.z")]))
  
  
  # calculate correlations
  # if there are less than 2 participants with valid values, correlation cannot be calculated
  if (N_cor_ICC < 2) {
    cor_ICC <- NA_real_
  } else {
    cor_ICC <- psych::corr.test(merged[ , c("comp_ICC", "bench_ICC")], use="complete")$r[2, 1]
  }
  
  if (N_cor_ICC.z < 2) {
    cor_ICC.z <- NA_real_
  } else {
    cor_ICC.z <- psych::corr.test(merged.c[ , c("comp_ICC.z", "bench_ICC.z")], use="complete")$r[2, 1]
  }


  
  # RETURN ALL OUTCOMES
  return(data.frame(
    N_merged_total_raw, # store information on number of participants
    N_merged_ICC_raw,
    N_merged_total_handled,
    N_merged_ICC_handled,
    N_valid_ICC.z_handled,
    N_cor_ICC,
    N_cor_ICC.z,
    N_rel,
    # relative outcome measures
    cor_ICC, cor_ICC.z,
    person_estimates_ICC = I(list(person_estimates_ICC)),
    person_estimates_ICC.z = I(list(person_estimates_ICC.z)),
    person_diff_ICC = I(list(person_diff_ICC)),
    person_diff_ICC.z = I(list(person_diff_ICC.z)), # store list of differences (with according name, else it will be changed to list.person_diff_ICC.)
    # absolute outcome measures
    rel,
    sd_ICC, sd_ICC.z,
    negICC_raw,
    negICC_handled,
    estimationProbNeg_raw, estimationProbPos_raw
  ))
  
}



# COMBINE TO ONE FUNCTION
one_simulation <- function(data, nr.of.occasions, occasions.drawn,
                           nr.of.items, items, id.var, occ.running.var,
                           type, unit,
                           benchmark_ICCdata,
                           negative_icc_handling = c("keep", "set to zero", "exclude")) {
  # data: takes the data frame with all participants
          # and their occasions as input (long format) = benchmark data
  # nr.of.occasions: number of occasions to draw per participant (for ICC calculation)
  # occasions.drawn: whether occasions per participant are drawn by order or randomly
  # nr.of.items: number of items that shall be used for ICC calculation
                # also determines the type of items
  # items: items that shall be used for ICC calculation
  # id.var: character that indicates name of participant ID variable
  # occ.running.var: character that indicates the name of the occasion running variable
  # type: type for ICC calculation
          # here: default is consistency (but could be varied in principle in simulation)
  # unit: unit for ICC calculation
          # here: default is single measurements (but could be varied in principle in simulation)
  # benchmark_ICCdata: data on ICCs (raw ICC and ICC.z) using benchmark data
  # negative_icc_handling: specifies whether negative ICCs shall be kept, set to zero or excluded (in simulated data)
  
  
  negative_icc_handling <- match.arg(negative_icc_handling, several.ok = FALSE)
  
  
  # CHECK: is length(items) == nr.of.items?
  # i.e., is the number of items / the items vector provided to the function correct?
  if (length(items) != nr.of.items)  {
    stop(
      sprintf(
        "Number of items (nr.of.items) and length of item vector provided for ICC calculation is not equal.
        Called from one_simulation()."
      )
    )
  }
  
  
  # Step 1: Manipulate data according to simulation design (and calculate ICCs)
  # the output is a list of drawn_data and total_redraws
  # -> store separately
  drawn_all <- one_sim_data_manipulation(data = data, nr.of.occasions = nr.of.occasions,
                                          occasions.drawn = occasions.drawn, nr.of.items = nr.of.items,
                                          items = items,
                                          id.var = id.var, occ.running.var = occ.running.var) # pass arguments from outer function

  # extract drawn_data
  drawn_data <- drawn_all$drawn_data
  
  # extract diagnostic information:
  total_redraws <- drawn_all$total_redraws
  n_total_persons <- drawn_all$n_total_persons
  n_valid_persons <- drawn_all$n_valid_persons
  n_skipped_persons <- drawn_all$n_skipped_persons
 
  
  
  # Step 2: Calculate ICCs with drawn data
  sim_ICCdata_raw <- calculate_icc(data  = drawn_data, # insert drawn data: calculate ICCs on data subset (corresponding to design choice)
                               id.var = id.var, # pass id.var
                               items = items, # pass items
                               type = type, # pass type, default here: consistency; could be varied in principle in simulation
                               unit = unit) # pass unit, default here: single; could be varied in principle in simulation
  
  colnames(sim_ICCdata_raw) <- c(id.var, "comp_ICC", "comp_ICC.z") # rename for comparison with benchmark 

  # now handle negative ICCs according to argument:
  sim_ICCdata_handled <- handle_negative_iccs(
    ICCdata = sim_ICCdata_raw, # raw simulation ICC data
    icc_col = "comp_ICC",  # column name
    icc.z_col = "comp_ICC.z", # column name
    negative_icc_handling = negative_icc_handling # pass negative_icc_handling_argument
  )
  
  # this is the ICC data that we want to calculate outcomes measures for
  # e.g., mean, SD, ...
  
  # however, using the raw ICC data (sim_ICCdata_raw), we want to calculate
  # negative ICCs, estimation problems

  # check if the number of rows is consistent with the number of participants that were used (i.e., not skipped)
  if (nrow(sim_ICCdata_raw) != drawn_all$n_total_persons) {
    stop("Mismatch between n_total_persons from data manipulation and sim_ICCdata rows")
  }
    

  # Step 3: Calculate outcome measures based on the manipulated data (step 2) and benchmark data (passed to outer function)
  outcomes <- one_sim_outcome_measures(benchmark_ICCdata = benchmark_ICCdata,
                                       sim_ICCdata_handled,
                                       sim_ICCdata_raw,
                                       id.var,
                                       nr.of.items,
                                       nr.of.occasions)
  
  # Sanity check: merged N should match total persons
  if (outcomes$N_merged_total_raw != n_total_persons) {
    stop("Mismatch: N_merged_total_raw does not equal n_total_persons.")
  }
  
  
  # add total number of re-draws and ohter diagnostics to the outcomes
  outcomes$total_redraws <- total_redraws
  outcomes$n_total_persons <- n_total_persons
  outcomes$n_valid_persons_var <- n_valid_persons
  outcomes$n_skipped_persons_var <- n_skipped_persons
  
  return(outcomes)
}






# # test function
# source("functions/function_calculate_iccs.R")
# source("functions/function_ordered_occasion_draw.R")
# source("functions/function_random_occasion_draw.R")
# 
# 
# load("prepared data/EMOTIONS_benchmark_data.rda")
# 
# benchmark_ICCdata <- calculate_icc(data = bench,
#                                    id.var = "id",
#                                    items = c("angry", "excluded", "envious",
#                                              "resentful", "ashamed", "insecure",
#                                              "anxious", "sad", "lonely"),
#                                    type = "consistency",
#                                    unit = "single")
# colnames(benchmark_ICCdata) <- c("id", "bench_ICC", "bench_ICC.z")
# 
# 
# res.o <- one_simulation(data = bench,
#                         nr.of.occasions = 3,
#                         nr.of.items = 3,
#                         occasions.drawn = "by order",
#                         items = c("angry", "excluded", "envious"),
#                         id.var = "id", benchmark_ICCdata = benchmark_ICCdata,
#                         occ.running.var = "occ_running", type = "consistency", unit="single",
#                         negative_icc_handling = "keep")
# 
# # calculate manually
# comp_ICCdata <- calculate_icc(data = bench[bench$occ_running <=3 ,],
#                               id.var = "id",
#                               items = c("angry", "excluded", "envious"),
#                               type = "consistency",
#                               unit = "single")
# colnames(comp_ICCdata) <- c("id", "comp_ICC", "comp_ICC.z")
# merged <- merge(benchmark_ICCdata, comp_ICCdata, by = "id")
# table(is.nan(merged$comp_ICC)) # 84 not a number, 166 true
# 
# # calculate manually for participant 2 to check:
# sub <- bench[bench$id == 2, ]
# sub2 <- sub[sub$occ_running <= 3, ]
# irr::icc(sub2[ , c("angry", "excluded", "envious")],
#          model="twoway", type="consistency", unit="single")
# # no variance = NaN
# 
# 
# 
# 
# # test how negative ICCs are handled
# bench_iccs <- calculate_icc(data = bench,
#                             id.var = "id",
#                             items = c("angry", "excluded", "envious",
#                                       "resentful", "ashamed", "insecure",
#                                       "anxious", "sad", "lonely"),
#                             type = "consistency",
#                             unit = "single")
# colnames(bench_iccs) <- c("id", "bench_ICC", "bench_ICC.z")
# 
# table(bench_iccs[ , "bench_ICC"] < 0, useNA="always") # 5 negative ICCs
# table(bench_iccs[ , "bench_ICC"] == 0, useNA="always") # 0 ICCs are exactly 0
# 
# handled_bench <- handle_negative_iccs(bench_iccs,
#                                       icc_col = "bench_ICC",
#                                       icc.z_col =  "bench_ICC.z",
#                                       negative_icc_handling = "keep")
# all(identical(handled_bench, bench_iccs))
# # all identical, correct
# 
# zero_bench <- handle_negative_iccs(bench_iccs,
#                                    icc_col = "bench_ICC",
#                                    icc.z_col =  "bench_ICC.z",
#                                    negative_icc_handling = "set to zero")
# 
# table(zero_bench[ , "bench_ICC"] < 0, useNA="always") # 0 negative ICCs
# # -> negative ICCs have been handled
# table(zero_bench[ , "bench_ICC"] == 0, useNA="always") # 5 ICCs exactly zero
# # correct
# 
# excl_bench <- handle_negative_iccs(bench_iccs,
#                                    icc_col = "bench_ICC",
#                                    icc.z_col =  "bench_ICC.z",
#                                    negative_icc_handling = "exclude")
# table(excl_bench[ , "bench_ICC"] < 0, useNA="always") # 0 negative ICCs, but 5 NA
# # -> negative ICCs have been handled
# # correct
# 
# 
# # check whether the handled values refer to the same participants
# colnames(handled_bench) <- c("id", "keep_ICC", "keep_ICC.z")
# colnames(zero_bench) <- c("id", "zero_ICC", "zero_ICC.z")
# colnames(excl_bench) <- c("id", "excl_ICC", "excl_ICC.z")
# 
# all <- merge(bench_iccs, handled_bench, by = "id")
# all <- merge(all, zero_bench, by = "id")
# all <- merge(all, excl_bench, by = "id")
# 
# 
# View(all[all$bench_ICC < 0, ])
# # correct 
# 
# 
# # now test with one_simulation_run
# 
# load("prepared data/EMOTIONS_benchmark_data.rda")
# 
# benchmark_ICCdata <- calculate_icc(data = bench,
#                                    id.var = "id",
#                                    items = c("angry", "excluded", "envious",
#                                              "resentful", "ashamed", "insecure",
#                                              "anxious", "sad", "lonely"),
#                                    type = "consistency",
#                                    unit = "single")
# colnames(benchmark_ICCdata) <- c("id", "bench_ICC", "bench_ICC.z")
# 
# 
# res.o <- one_simulation(data = bench,
#                         nr.of.occasions = 3,
#                         nr.of.items = 3,
#                         occasions.drawn = "by order",
#                         items = c("angry", "excluded", "envious"),
#                         id.var = "id", benchmark_ICCdata = benchmark_ICCdata,
#                         occ.running.var = "occ_running", type = "consistency", unit="single",
#                         negative_icc_handling = "keep")
# # 59 negative ICCs
# 
# res.o_zero <- one_simulation(data = bench,
#                         nr.of.occasions = 3,
#                         nr.of.items = 3,
#                         occasions.drawn = "by order",
#                         items = c("angry", "excluded", "envious"),
#                         id.var = "id", benchmark_ICCdata = benchmark_ICCdata,
#                         occ.running.var = "occ_running", type = "consistency", unit="single",
#                         negative_icc_handling = "set to zero")
# # 59 negative ICCs before handling, still 1 estimation problem before handling
# # BUT we now have one more valid ICC.z because the estimation problem ICC was set to 0 -> now
# # valid transformed value
# # zero negative ICCs after handling -> correct
# 
# 
# res.o_excl <- one_simulation(data = bench,
#                              nr.of.occasions = 3,
#                              nr.of.items = 3,
#                              occasions.drawn = "by order",
#                              items = c("angry", "excluded", "envious"),
#                              id.var = "id", benchmark_ICCdata = benchmark_ICCdata,
#                              occ.running.var = "occ_running", type = "consistency", unit="single",
#                              negative_icc_handling = "exclude")
# # seems plausible

