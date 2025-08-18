###################################################################
#####    Estimating trait negative emotion differentiation:   #####
#####        How many measurement occasions and               #####
#####              emotion items are needed?                  #####
###################################################################

###################################################################
#####              Simulation Study Function                  #####
###################################################################



# Source Functions --------------------------------------------------------
source("functions/function_ordered_occasion_draw.R")
source("functions/function_random_occasion_draw.R")
source("functions/function_calculate_iccs.R")
source("functions/function_draw_items.R")
source("functions/function_one_simulation_run.R")



# Librabry ----------------------------------------------------------------
library(future)
library(future.apply)




# Write Function for Simulation -------------------------------------------
simulation_study <- function(data, n_occasions, occasions_drawn, n_items, n_iteration,
                             id.var, all_items, categories = NULL,
                             type = "consistency", unit = "single", occ.running.var,
                             seed = NULL, cores = 1) {
  # data: takes the data frame with all participants
          # and their occasions as input (long format) = benchmark data
  # n_occasions: number of occasions to draw per participant for ICC calculation
          # -> design factor in simulation study
          # highest value = benchmark
  # occasions_drawn: chr vector whether occasions per participant are drawn by order or randomly
          # -> design factor in simulation study
  # n_items: number of items that shall be used for ICC calculation
          # -> design factor in simulation study
  # n_iteration: number of iterations (for only relevant for random draws)
  # id.var: character that indicates name of participant ID variable
  # all_items: character vector indicating emotion item names (of all items assessed)
  # categories: optional vector (same length as all_items) indicating category
              # --> needed if multiple items per emotion category were assessed 
              # -> draw items per category
  # type: type for ICC calculation
          # here: default is consistency (but could be varied in principle in simulation)
  # unit: unit for ICC calculation
          # here: default is single measurements (but could be varied in principle in simulation)
  # occ.running.var: character that indicates the name of the occasion running variable
  # seed: seed set for reproducibility
  # cores: number of cores to use for parallelized simulation

  
  ## INCLUDE CHECKS BEFORE RUNNING SIMULATION  
  
  # CHECK: is id.var numeric in data frame?
  if (!is.numeric(data[ , id.var])) {
    stop(
      sprintf(
        "Data type of ID variable (id.var) in data frame needs to be numeric."
      )
    )
  }
  
  
  # CHECK: maximum number of items == length of all_items vector?
  # -> else the benchmark will not be calculated correctly (using the maximum of 
  # n_items)
  if (max(n_items) != length(all_items)) {
    stop(
      sprintf(
        "Maximum number of items (length(n_items)) does not match the length of all_items vector.\nExpected (all_items): %i\nGot (max(n_items)): %i",
        length(all_items),
        max(n_items)
      )
    )
  }
  
  
  # CHECK: maximum number of occasions in simulation == actual maximum number of occasions in data frame?
  # -> else the benchmark will not be calculated correctly (using the maximum of n_occasions)
  if (max(n_occasions) != max(data[ , occ.running.var])) {
    stop(
      sprintf(
        "Maximum number of occasions in simulation (max(n_occasions)) does not match actual maximum number
        of occasions in data frame (max(data[ , occ.running.var])). \nExpected (max(data[ , occ.running.var])): %i\nGot (max(n_occasions)): %i",
        max(data[ , occ.running.var]),
        max(n_occasions)
      )
    )
  }
  
  

  # SET UP SIMULATION DESIGN
  design <- expand.grid(
    n_occasions = n_occasions,
    occasions_drawn = occasions_drawn,
    n_items = n_items,
    n_iteration = 1:n_iteration # number of iterations, only relevant for random draws
  )
  
  # remove iterations for occasions_drawn == "by order" -> there is only one option
  # on how to draw by order --> only one replication
  # also remove conditions with random draw of maximum nr. of occasions: as they are drawn
  # without replacement, the "randomly" drawn occasions (i.e., the maximum
  # number, benchmark) will always be the same
  design$flag <- 0
  design$flag[design$occasions_drawn == "by order" & design$n_iteration >= 2] <- 1
  design$flag[design$n_occasions == max(n_occasions) & design$occasions_drawn == "random"] <- 1
  
  design <- design[design$flag == 0, ]
  design$flag <- NULL
  
  # for clarity: add variable that codes whether condition is benchmark or
  # a comparison condition
  # benchmark: maximum number of occasions drawn by order, maximum number of items
  design$condition <- "comparison"
  design$condition[design$n_occasions == max(n_occasions) &
                     design$occasions_drawn == "by order" &
                     design$n_items == max(n_items)] <- "benchmark"
  
  
  
  # DRAW ITEMS ONCE FOR EACH n_items CONDITION
  # -> draw once randomly, but keep constant across simulation
  # seed:
  if (!is.null(seed)) {
    set.seed(seed)
  }
  drawn_items <- draw_items(all_items = all_items,
                            n_items = n_items,
                            categories = categories)
  
  # append design df by the items drawn according to number of items
  design <- merge(design, drawn_items, by = "n_items")
  # -> in each row in the simulation, the items can be read

  # adjust variable order
  design <- design[ , c("condition", "n_occasions", "occasions_drawn", "n_items", "n_iteration")]
  
  
  # PREPARE BENCHMARK DATA
  benchmark_ICCdata <- calculate_icc(data, id.var=id.var,
                                     items = all_items,
                                     type = type,
                                     unit = unit)
  colnames(benchmark_ICCdata) <- c(id.var, "bench_ICC", "bench_ICC.z")
  # benchmark_ICCdata: data on ICCs (raw ICC and ICC.z) using benchmark data
  # -> conditions are compared to this
  
  
  # CREATE RESULTS STORAGE
  res <- data.frame(matrix(NA, nrow=nrow(design), ncol=18))
  # ncol = 18 -> we have 18 outcomes (6 outcome measures, but some for raw ICCs
  # and also for Fisher's Z-transformed ICCs)
  
  # rename columns; order as in the one_simulation_outcome_measures-function
  colnames(res) <- c( 
    # relative outcome measures
    "min_diff_ICC",
    "mean_diff_ICC",
    "max_diff_ICC",
    "N_valid_ICC.z",
    "min_diff_ICC.z",
    "mean_diff_ICC.z",
    "max_diff_ICC.z",
    "cor_ICC",
    "cor_ICC.z",
    "RMSE_ICC",
    "RMSE_ICC.z",
    # absolute outcome measures
    "rel",
    "N_rel",
    "sd_ICC",
    "sd_ICC.z",
    "negICC",
    "estimationProbNeg",
    "estimationProbPos"
  )

  
  

  # SET FUTURE PLAN FOR PARALLELIZATION
  if (cores == 1) { # if cores == 1 (default)
    plan(sequential)  # run simulation sequentially
  } else { # else (cores != 1)
    plan(multisession, workers = cores) # run simulation in parallalized manner
  }
  
  # set seed
  future_seed <- !is.null(seed) # if is.null -> FALSE, if not null -> TRUE
  if (future_seed==TRUE) { # if true -> set seed
    set.seed(seed)
  } else {
    future_seed = NULL # set future_seed = NULL -> needed for future_sapply function
  }
  

  # RUN SIMULATION
  res[] <- t(future_sapply(seq_len(nrow(design)),# apply function to row dimension of design matrix (i.e.,
                           # "loop" over rows) and then transpose to the results matrix
                           # seq_len(nrow(design))) -> sequence along row numbers of the
                           # design matrix (column vector of row numbers)
                           
                           FUN = function(design_row) { # function that runs one_simulation row-wise
                             one_result <- one_simulation(
                               data = data, # input data = benchmark data
                               nr.of.occasions = design[design_row, "n_occasions"],
                               occasions.drawn = design[design_row, "occasions_drawn"],
                               nr.of.items = design[design_row, "n_items"],
                               items = strsplit(design[design_row,"items"], ", ")[[1]], # pass items (but as chr vector!)
                               # strsplit splits the single string of items into one string per item -> chr vector
                               id.var = id.var,
                               occ.running.var = occ.running.var,
                               type = type,
                               unit = unit,
                               benchmark_ICCdata = benchmark_ICCdata) # calculated before
                             
                             # include a check whether the variable names are the same
                             # in the same order for the one_result output
                             # and the res object -> so that everything is correctly stored
                             # also works if the order of the names is swapped
                             if (!identical(colnames(one_result), colnames(res))) {
                               stop(
                                 sprintf(
                                   "Column names of simulation output don't match results object.\nExpected: %s\nGot: %s",
                                   paste(colnames(res), collapse = ", "),
                                   paste(colnames(one_result), collapse = ", ")
                                 )
                               )
                             }
                             as.vector(one_result)
                           },
                           future.seed=future_seed)) # set TRUE/FALSE for future seed from above
  # combine design and results
  res <- cbind(design, res)
  return(res)
}





# # Test function
# load("prepared data/benchmark_data_Study1.rda")
# 
# # Make sure that ID variable is numeric
# # (relevant for ICC calculation: matrix can only store one data type)
# is.numeric(bench$SERIAL)
# 
# test <- simulation_study(data = bench, n_occasions = c(3,5,10,100), occasions_drawn = c("random", "by order"),
#                          n_items = c(5,15), n_iteration = 5,
#                          id.var = "SERIAL", all_items = c('aerger1', 'aerger2', 'aerger3',
#                                                           'traurigkeit1', 'traurigkeit2', 'traurigkeit3',
#                                                           'angst1', 'angst2', 'angst3',
#                                                           'scham1', 'scham2', 'scham3',
#                                                           'schuld1', 'schuld2', 'schuld3'),
#                          categories = c(rep("aerger",3), rep("traurig", 3), rep("angst",3),
#                                         rep("scham", 3), rep("schuld", 3)), 
#                          type = "consistency", unit = "single",
#                          occ.running.var = "occ_running",
#                          seed = NULL, cores = 1)
# #
# 
# # without categories -> could draw multiple items from the same category
# # should also work with number of items that would not lead to equal number per category (e.g., 7)
# test <- simulation_study(data = bench, n_occasions = c(3,5,10,100), occasions_drawn = c("random", "by order"),
#                          n_items = c(5,7,15), n_iteration = 5,
#                          id.var = "SERIAL", all_items = c('aerger1', 'aerger2', 'aerger3',
#                                                           'traurigkeit1', 'traurigkeit2', 'traurigkeit3',
#                                                           'angst1', 'angst2', 'angst3',
#                                                           'scham1', 'scham2', 'scham3',
#                                                           'schuld1', 'schuld2', 'schuld3'),
#                          categories = NULL, 
#                          type = "consistency", unit = "single",
#                          occ.running.var = "occ_running",
#                          seed = NULL, cores = 1)
# 
# # however, 7 items should not work when specifying categories:
# test <- simulation_study(data = bench, n_occasions = c(3,5,10,100), occasions_drawn = c("random", "by order"),
#                          n_items = c(5,7,15), n_iteration = 5,
#                          id.var = "SERIAL", all_items = c('aerger1', 'aerger2', 'aerger3',
#                                                           'traurigkeit1', 'traurigkeit2', 'traurigkeit3',
#                                                           'angst1', 'angst2', 'angst3',
#                                                           'scham1', 'scham2', 'scham3',
#                                                           'schuld1', 'schuld2', 'schuld3'),
#                          categories = c(rep("aerger",3), rep("traurig", 3), rep("angst",3),
#                                         rep("scham", 3), rep("schuld", 3)), 
#                          type = "consistency", unit = "single",
#                          occ.running.var = "occ_running",
#                          seed = NULL, cores = 1)
# # correct

# # test with different ID variable
# names(bench)[1] <- "IDVAR"
# test2 <- simulation_study(data = bench, n_occasions = c(3,5,10,100), occasions_drawn = c("random", "by order"),
#                           n_items = c(5,15), n_iteration = 5,
#                           id.var = "IDVAR", all_items = c('aerger1', 'aerger2', 'aerger3',
#                                                            'traurigkeit1', 'traurigkeit2', 'traurigkeit3',
#                                                            'angst1', 'angst2', 'angst3',
#                                                            'scham1', 'scham2', 'scham3',
#                                                            'schuld1', 'schuld2', 'schuld3'),
#                           type = "consistency", unit = "single",
#                           occ.running.var = "occ_running",
#                           seed = NULL, cores = 1)
# 
# # test with wrong maximum number of items
# names(bench)[1] <- "SERIAL"
# test3 <- simulation_study(data = bench, n_occasions = c(3,5,100), occasions_drawn = c("random", "by order"),
#                           n_items = c(5,10), n_iteration = 5,
#                           id.var = "SERIAL", all_items = c('aerger1', 'aerger2', 'aerger3',
#                                                           'traurigkeit1', 'traurigkeit2', 'traurigkeit3',
#                                                           'angst1', 'angst2', 'angst3',
#                                                           'scham1', 'scham2', 'scham3',
#                                                           'schuld1', 'schuld2', 'schuld3'),
#                           type = "consistency", unit = "single",
#                           occ.running.var = "occ_running",
#                           seed = NULL, cores = 1)
# 
# 
# # test with non-numeric ID variable
# bench$ID.chr <- rep(c(LETTERS[1:26], letters[1:10]), each = 100)
# test4 <- simulation_study(data = bench, n_occasions = c(3,5,100), occasions_drawn = c("random", "by order"),
#                           n_items = c(5,15), n_iteration = 5,
#                           id.var = "ID.chr", all_items = c('aerger1', 'aerger2', 'aerger3',
#                                                           'traurigkeit1', 'traurigkeit2', 'traurigkeit3',
#                                                           'angst1', 'angst2', 'angst3',
#                                                           'scham1', 'scham2', 'scham3',
#                                                           'schuld1', 'schuld2', 'schuld3'),
#                           type = "consistency", unit = "single",
#                           occ.running.var = "occ_running",
#                           seed = NULL, cores = 1)
# 
# # test with wrong maximum number of occasions
# test5 <- simulation_study(data = bench, n_occasions = c(3,5,10), occasions_drawn = c("random", "by order"),
#                           n_items = c(5,15), n_iteration = 5,
#                           id.var = "SERIAL", all_items = c('aerger1', 'aerger2', 'aerger3',
#                                                            'traurigkeit1', 'traurigkeit2', 'traurigkeit3',
#                                                            'angst1', 'angst2', 'angst3',
#                                                            'scham1', 'scham2', 'scham3',
#                                                            'schuld1', 'schuld2', 'schuld3'),
#                           type = "consistency", unit = "single",
#                           occ.running.var = "occ_running",
#                           seed = NULL, cores = 1)
# 
# rm(test, test2, test3, test4, test5)
