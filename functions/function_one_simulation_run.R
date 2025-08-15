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

# items (for ICC) -> determined within function according to nr.of.items
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
        # used in random draws
  # id.var: character that indicates name of participant ID variable
  # occ.running.var: character that indicates the name of the occasion running variable

  

  
  # DRAW OCCASIONS FOR EACH PARTICIPANT
  if (occasions.drawn == "random") {
    drawn_data <- random_occasion_draw(data = data, # insert start data (full data set provided in argument)
                                       id.var = id.var, # pass id.var 
                                       occ.running.var = occ.running.var, # pass occ.running.var
                                       nr.of.occasions = nr.of.occasions,  # pass nr.of.occasions
                                       items = items) # pass items 
  } else if (occasions.drawn == "by order") {
    drawn_data <- ordered_occasion_draw(data = data, # insert start data (full data set provided in argument)
                                        id.var = id.var, # pass id.var 
                                        occ.running.var = occ.running.var, # pass occ.running.var
                                        nr.of.occasions = nr.of.occasions) # pass.nr.of.occasions
    
  }
  
  return(drawn_data)

}


# dat <- one_sim_data_manipulation(data = bench,
#                                  nr.of.occasions = 50,
#                                  occasions.drawn = "by order",
#                                  nr.of.items = 15,
#                                  id.var = "SERIAL",
#                                  occ.running.var = "occ_running")
# ICC <- calculate_icc(dat, id.var="SERIAL", items = c('aerger1', 'aerger2', 'aerger3',
#                                                      'traurigkeit1', 'traurigkeit2', 'traurigkeit3',
#                                                      'angst1', 'angst2', 'angst3',
#                                                      'scham1', 'scham2', 'scham3',
#                                                      'schuld1', 'schuld2', 'schuld3'),
#                      type="consistency", unit="single")
# rm(dat, ICC)


### PART 2: CALCULATE OUTCOMES MEASURES

one_sim_outcome_measures <- function(benchmark_ICCdata, sim_ICCdata, id.var,
                                     nr.of.items, nr.of.occasions) {
  # benchmark_ICCdata: data frame of the ICC data estimated with benchmark data
      # variable names need to be: id.var, bench_ICC, bench_ICC.z
  # sim_ICCdata: data frame of the ICC data estimated with simulated data (data adjusted
                # according to simulation design features)
  # id.var: character indicating name of ID variable
  # nr.of.items: number of items (as in simulation design)
  # nr.of.occasions: number of occasions per participant (as in simulation design)
  
  # merge benchmark ICC data and simulated ICC data by id.var
  merged <- merge(benchmark_ICCdata, sim_ICCdata, by = id.var)
  
  
  # some participants may have ICC.z +/- infinite after transformation
  # -> problems for reliability estimation and other outcomes involving ICC.z
  # -> only use participants with valid values and store number of 
  # participants used for reliability analysis and other outcomes
  # involving ICC.z
  merged.c <- merged[!is.infinite(merged[ , "comp_ICC.z"]), ]
  # if all participants have valid ICC.z, then merged.c = merged
  # 
  N_valid_ICC.z <- nrow(merged.c)
  #### FOR ICC.Z OUTCOMES, MERGED.C IS USED 

  
  
  ## ABSOLUTE OUTCOMES ##
  # absolute outcomes are independent of benchmark ICC data
  # -> use the ICCs of the sim_ICCdata only (either ith sim_ICCdata object or with corresponding
  # rows in the merged data frame)
  
  # ESTIMATION PROBLEMS
  # only relevant for raw ICCs
  estimationProbNeg <- nrow(merged[merged[ , "comp_ICC"] <= -1, ]) # number of ICCs <= -1
  
  estimationProbPos <- nrow(merged[merged[ , "comp_ICC"] >= 1, ]) # number of ICCs >= 1
  
  negICC <- nrow(merged[merged[ , "comp_ICC"] < 0, ]) # number of negative ICCs
  
  
  # STANDARD DEVIATION OF ICCs
  sd_ICC <- sd(merged[ , "comp_ICC"]) # raw ICCs
  sd_ICC.z <- sd(merged.c[ , "comp_ICC.z"]) # transformed ICCs
  
  
  # RELIABILITY
  # according to Schneider & Junghaenel (2023)
  # can only be calculated for transformed ICCs
  # ICCs are already transformed in data frame (ICC.z)
  
  # some participants may have ICC +/- infinite after transformation
  # -> problems for reliability estimation
  # -> only use participants with valid values (merged.c) and store number of 
  # participants used for reliability analysis

  
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
  
  
  
  ## RELATIVE OUTCOMES ##
  
  # DIFFERENCE OF ICCs TO BENCHMARK ICCs
  # for both raw ICCs and transformed ICCs
  
  merged[ , "difference_ICC"] <- merged[ , "comp_ICC"] - merged[ , "bench_ICC"] # difference between raw ICCs
  merged.c[ , "difference_ICC.z"] <- merged.c[ , "comp_ICC.z"] - merged.c[ , "bench_ICC.z"] # difference between transformed ICCs
  
  # raw ICCs
  mean_diff_ICC <- mean(merged[ , "difference_ICC"])
  min_diff_ICC <- min(merged[ , "difference_ICC"]) # minimum of difference (i.e., most negative deviation)
  max_diff_ICC <- max(merged[ , "difference_ICC"]) # maximum of difference (i.e., most positive deviation)

  # transformed ICCs
  mean_diff_ICC.z <- mean(merged.c[ , "difference_ICC.z"])
  min_diff_ICC.z <- min(merged.c[ , "difference_ICC.z"]) # minimum of difference (i.e., most negative deviation)
  max_diff_ICC.z <- max(merged.c[ , "difference_ICC.z"]) # maximum of difference (i.e., most positive deviation)
  
  
  # RMSE
  # root mean square error (for both raw ICCs and transformed ICCs)
  # -> compare ICC from comparison condition to benchmark ICC
  # --> deviation of the comp_ICC from the bench_ICC for each participant
  # here: RMSE across participants in a single replication
  # sum up all squared differences between comp_ICC and bench_ICC, divide by number of participants, take square root
  
  # differences were already calculated
  
  RMSE_ICC <- sqrt( sum( merged[ , "difference_ICC"]^2 ) / nrow(merged) )
  RMSE_ICC.z <- sqrt( sum( merged.c[ , "difference_ICC.z"]^2 ) / nrow(merged.c) )
  
  
  # CORRELATION WITH BENCHMARK
  # for both raw ICCs and transformed ICCs
  
  cor_ICC <- psych::corr.test(merged[ , c("comp_ICC", "bench_ICC")])$r[2, 1]
  cor_ICC.z <- psych::corr.test(merged.c[ , c("comp_ICC.z", "bench_ICC.z")])$r[2, 1]
  
  
  # RETURN ALL OUTCOMES
  return(cbind(
    # relative outcome measures
    min_diff_ICC, mean_diff_ICC, max_diff_ICC,
    N_valid_ICC.z,
    min_diff_ICC.z, mean_diff_ICC.z, max_diff_ICC.z,
    cor_ICC, cor_ICC.z,
    RMSE_ICC, RMSE_ICC.z, 
    # absolute outcome measures
    rel, N_rel,
    sd_ICC, sd_ICC.z,
    negICC, estimationProbNeg, estimationProbPos
  ))
  
}


# # Test
# benchmark_data <- calculate_icc(bench, id.var="SERIAL", items = c("aerger1", "aerger2", "aerger3",
#                                                                   "traurigkeit1", "traurigkeit2", "traurigkeit3",
#                                                                   "angst1", "angst2", "angst3",
#                                                                   "scham1", "scham2", "scham3",
#                                                                   "schuld1", "schuld2", "schuld3"),
#                                 type = "consistency", unit = "single")
# 
# 
# 
# colnames(benchmark_data) <- c("SERIAL", "bench_ICC", "bench_ICC.z")
# 
# sim_data <- one_sim_data_manipulation(data = bench, nr.of.occasions = 50, occasions.drawn = "by order",
#                                       nr.of.items = 15, id.var = "SERIAL", occ.running.var = "occ_running")
# 
# sim_data_ICC <- calculate_icc(sim_data, id.var = "SERIAL", items = c("aerger1", "aerger2", "aerger3",
#                                                                      "traurigkeit1", "traurigkeit2", "traurigkeit3",
#                                                                      "angst1", "angst2", "angst3",
#                                                                      "scham1", "scham2", "scham3",
#                                                                      "schuld1", "schuld2", "schuld3"),
#                               type="consistency", unit="single")
# colnames(sim_data_ICC) <- c("SERIAL", "comp_ICC", "comp_ICC.z")
# 
# 
# out <- one_sim_outcome_measures(benchmark_ICCdata = benchmark_data, sim_ICCdata = sim_data_ICC,
#                                 id.var = "SERIAL", nr.of.items = 15, nr.of.occasions = 50)
# 
# out
# 
# rm(out, benchmark_data, sim_data, sim_data_ICC)




# COMBINE TO ONE FUNCTION
one_simulation <- function(data, nr.of.occasions, occasions.drawn,
                           nr.of.items, items, id.var, occ.running.var,
                           type, unit,
                           benchmark_ICCdata) {
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
  drawn_data <- one_sim_data_manipulation(data = data, nr.of.occasions = nr.of.occasions,
                                          occasions.drawn = occasions.drawn, nr.of.items = nr.of.items,
                                          id.var = id.var, occ.running.var = occ.running.var) # pass arguments from outer function


  
  # Step 2: Calculate ICCs with drawn data
  sim_ICCdata <- calculate_icc(data  = drawn_data, # insert drawn data: calculate ICCs on data subset (corresponding to design choice)
                               id.var = id.var, # pass id.var
                               items = items, # use items that were used for drawing data
                               type = type, # pass type, default here: consistency; could be varied in principle in simulation
                             unit = unit) # pass unit, default here: single; could be varied in principle in simulation
  
  colnames(sim_ICCdata) <- c(id.var, "comp_ICC", "comp_ICC.z") # rename for comparison with benchmark 
  
  # this is the ICC data that we want to calculate outcomes measures for
  # e.g., mean, SD, ...
  

  # Step 3: Calculate outcome measures based on the manipulated data (step 2) and benchmark data (passed to outer function)
  outcomes <- one_sim_outcome_measures(benchmark_ICCdata = benchmark_ICCdata, sim_ICCdata, id.var, nr.of.items, nr.of.occasions)
  
  return(outcomes)
}



# # Test:
# source("functions/function_calculate_iccs.R")
# source("functions/function_ordered_occasion_draw.R")
# source("functions/function_random_occasion_draw.R")
# load("prepared data/benchmark_data_Study1.rda")
# 
# # create benchmark ICC data (needs to be defined once in overall simulation)
# benchmark_data <- calculate_icc(bench, id.var="SERIAL", items = c("aerger1", "aerger2", "aerger3",
#                                                                   "traurigkeit1", "traurigkeit2", "traurigkeit3",
#                                                                   "angst1", "angst2", "angst3",
#                                                                   "scham1", "scham2", "scham3",
#                                                                   "schuld1", "schuld2", "schuld3"),
#                                 type = "consistency", unit = "single")
# 
# 
# 
# colnames(benchmark_data) <- c("SERIAL", "bench_ICC", "bench_ICC.z")
# 
# out <- one_simulation(data = bench,
#                       nr.of.occasions = 10, nr.of.items = 6,
#                       items = c("schuld1", "schuld2", "schuld3",
#                                 "scham1", "scham2", "scham3"),
#                       occasions.drawn = "by order",
#                       id.var = "SERIAL",
#                       occ.running.var = "occ_running",
#                       type = "consistency", unit = "single",
#                       benchmark_ICCdata = benchmark_data)
# out
# 
# # Test one simulation with 100 replications
# 
# res <- matrix(NA, ncol=18, nrow=100)
# colnames(res) <- c('min_diff_ICC', 'mean_diff_ICC', 'max_diff_ICC', 'min_diff_ICC.z', 'mean_diff_ICC.z', 'max_diff_ICC.z', 'cor_ICC', 'cor_ICC.z', 'RMSE_ICC', 'RMSE_ICC.z', 'rel', 'N_rel', 'sd_ICC', 'sd_ICC.z', 'negICC', 'estimationProbNeg', 'estimationProbPos')
# for (i in 1:100) {
#   res[i, ] <- one_simulation(data = bench,
#                              nr.of.occasions = 10, nr.of.items = 15,
#                              occasions.drawn = "random",
#                              id.var = "SERIAL",
#                              occ.running.var = "occ_running",
#                              type = "consistency", unit = "single",
#                              benchmark_ICCdata = benchmark_data)
# 
# }
# 
# res[1:10, ]
# rm(list=ls())
