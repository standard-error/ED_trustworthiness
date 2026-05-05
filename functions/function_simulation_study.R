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
source("functions/function_determine_all_possible_item_sets.R")
source("functions/function_one_simulation_run.R")



# Library -----------------------------------------------------------------
library(future)
library(future.apply)



# Write Function for Simulation -------------------------------------------
simulation_study <- function(data, n_occasions, occasions_drawn = c("random", "by order"), n_items, n_iteration,
                             id.var, all_items, categories = NULL,
                             type = "consistency", unit = "single",
                             negative_icc_handling = c("keep", "set to zero", "exclude"),
                             occ.running.var,
                             seed_item = global.seed.item.set,
                             item_sets_across_replications = c("balanced", "fixed", "random"),
                             seed_sim = NULL, cores = 1) {
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
  # negative_icc_handling: specifies whether negative ICCs shall be kept, set to zero or excluded (in benchmark and simulated data)
  # occ.running.var: character that indicates the name of the occasion running variable
  # seed_item: seed for drawing item sets -> global seed so that item sets are equal across simulations
              # i.e., for check whether number of replications is sufficient
  # item_sets_across_replications = c("fixed", "balanced", "random"):
    # whether item sets shall be fixed ("fixed") across replications (for each item-number condition),
    # drawn randomly in each simulation run (replication) ("random") or whether they 
    # shall be balanced across replications (i.e., across the replications for a given condition,
    # all possible item sets are drawn equally frequent) ("balanced")
  # seed_sim: seed set for reproducibility of simulation
    # -> separate seeds so that item sets are constant across simulations, but occasions may differ
  # cores: number of cores to use for parallelized simulation

  ## MATCH ARGUMENTS
  occasions_drawn <- match.arg(occasions_drawn, several.ok = TRUE)
  item_sets_across_replications <- match.arg(item_sets_across_replications, several.ok = FALSE)
  negative_icc_handling <- match.arg(negative_icc_handling, several.ok = FALSE)
  
  
  
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
  
  
  # CHECK: If ONLY ordered draws -> only balanced and fixed drawing of items possible
  # random not possible because there is only one option on how to draw occasions -> no replications
  # beyond different item sets
  if (!("random" %in% occasions_drawn) & "random" %in% item_sets_across_replications) {
    stop(
      sprintf(
        "Using only ordered draws for measurement occasions does not allow replications with  
        different drawn measurement occasions, as there is only one order of occasions. Thus,
        ordered draws can be combined with different item sets, but these cannot be drawn randomly,
        only fixed (i.e., there is one replication with one item set for each number of measurement 
        occasions) or balanced (i.e., all possible item sets are implemented for each number of measurement 
        occasions. Please choose 'fixed' or 'balanced' for the argument item_sets_across_replications, or 
        consider using 'random' for the drawing of measurement occasions (occasions_drawn) as well."
      )
    )
  }
  
  

  # SET UP SIMULATION DESIGN
  # set up separately for random and ordered draws -> different approaches
  
  # set seed for item drawing:
  # seed:
  if (!is.null(seed_item)) {
    set.seed(seed_item)
  }
  
  
  # 1. FOR RANDOM OCCASION DRAW
  if ("random" %in% occasions_drawn) {
    # build random setup
    
    design_random <- expand.grid(
      n_occasions = n_occasions,
      occasions_drawn = "random",
      n_items = n_items,
      n_iteration = 1:n_iteration # number of iterations, only relevant for random draws
    )
     
    # remove conditions with random draw of maximum nr. of occasions: as they are drawn
    # without replacement, the "randomly" drawn occasions (i.e., the maximum
    # number, benchmark) will always be the same
    design_random$flag <- 0
    design_random$flag[design_random$n_occasions == max(n_occasions) & design_random$occasions_drawn == "random"] <- 1

    design_random <- design_random[design_random$flag == 0, ]
    design_random$flag <- NULL
    
    
    
    
    # draw / assign item sets for each simulation run:
    # DRAW ITEMS FOR EACH n_items CONDITION
    # -> draw randomly for each simulation run (i.e., the 5-item set will differ across replications)

    if (item_sets_across_replications == "fixed") { # if item sets shall be fixed across replications
      # for each item-number condition -> draw once and add to data frame
      
      drawn_items <- draw_items(all_items = all_items, # draw items once for each item-number condition
                                n_items = n_items,
                                categories = categories)
      # item sets are automatically ordered
      
      
      # append design_random df by the items drawn according to number of items
      design_random <- merge(design_random, drawn_items, by = "n_items")
      
    } else if (item_sets_across_replications == "random") { # if item set shall be drawn random for each replication
      # draw n = n_iteration item sets for each item-number condition and add to design_random df
      # --> equivalent to drawing item set random in each run later
      # add drawn items directly to the simulation design_random data frame
      design_random$items <- sapply(design_random$n_items, function(design_random_row) { # for each row & value of design_random, draw items according to n_items
        draw_items(
          all_items = all_items,
          n_items = design_random_row, # inser the number of items (n_items) from the design_random row here (design_random$n_items is actually vector, but design_random_row'th element is the same as the number of the row)
          categories = categories
        )$items # extract items from output of draw_items function
      })
      # -> in each row in the simulation, the items can be read
    } else if (item_sets_across_replications == "balanced") { # if item sets shall be drawn in balanced manner
      # then use all possible item sets (use generate_all_possible_item_sets() function)
      # if n_iterations cannot be divided by number of item sets without remainder
      # -> draw item sets from all possible item sets for remaining iterations WITHOUT REPLACEMENT
      # -> maximum difference of 1 in frequency
      
      design_random$items <- NA_character_
      
      # determine conditions (without iterations) -> for each condition, all possible item sets shall be drawn
      # and then distributed across iterations as evenly as possible
      # -> if the total number of iterations is not divisible by number of item sets without remainder:
      # -> distribute evenly and draw randomly (without replacement) for the remaining conditions

      # only select one row for each condition = determine conditions
      # and only use the condition factors n_items and n_occasions
      
      design_random <- do.call( # assign to design_random
      rbind, # bind across unique conditions
      lapply( # apply to each unique condition (combination of n_items, n_occasions)
        split(design_random, list(design_random$n_occasions, design_random$n_items), drop=TRUE), # split design
        # according to each unique condition (combination of n_items, n_occasions)
        
        function(condition_cell) { # for each unique condition, do the following:
          
          n_iter_condition <- nrow(condition_cell) # how many iterations for this unique condition?
          n_items_condition <- unique(condition_cell$n_items) # how many items for this condition?
          
          all_item_sets <- generate_all_item_sets( # generate all possible item sets for this condition
            all_items = all_items,  # pass all items from simulation study
            n_items = n_items_condition, # pass item number for this condition
            categories = categories # pass categories for items from simulation study
          )
          
          n_item_sets <- length(all_item_sets) # determine number of all possible unique item sets
          n_full <- n_iter_condition %/% n_item_sets # divide number of iterations by number of item sets without remainder
          # i.e., how often should each item set be used?
          n_rest <- n_iter_condition %% n_item_sets # determine remainder
          
          
          # now assign item sets -> each item set n_full times
          # for the remainder, draw from all possible item sets without replacement
          assigned_item_sets <- c(
            rep(all_item_sets, each = n_full), # assign each possible item set n_full times
            sample(all_item_sets, size = n_rest, replace=FALSE) # for the remainder, sample from all item sets without replacement
          )
          
          # now distribute these item sets across the replications for this condition RANDOMLY (without replacement)
          # -> no fixed order
          condition_cell$items <- sample(assigned_item_sets, size=length(assigned_item_sets), replace=FALSE)
          condition_cell
        }
        )
      
      )
      rownames(design_random) <- NULL
      # adjust variable order
      
    }
    
    design_random <- design_random[ , c("n_occasions", "occasions_drawn", "n_items", "items", "n_iteration")]
    
    
    
  }
 
  

  
  # CONSTRUCT DESIGN FOR ODERED DRAWS
  
  if ("by order" %in% occasions_drawn) {
    # build ordered setup
    design_ordered <- expand.grid(
      n_occasions = n_occasions,
      occasions_drawn = "by order", 
      n_items = n_items 
      # n_iteration will be determined via number of item sets
    )
    
    # draw / assign item sets for each simulation run:

    
    if (item_sets_across_replications == "fixed") { # if item sets shall be fixed across replications
      # for each item-number condition -> draw once and add to data frame
      
      # if occasions are also drawn randomly -> use the fixed sets from design_random
      # if not, draw one item set for each item-number condition
      
      if("random" %in% occasions_drawn) { # if there is already a random design, use the same fixed item sets
        fixed_items <- unique(design_random[ , c("n_items", "items")])
        # add to design data frame
        design_ordered <- merge(design_ordered, fixed_items, by="n_items")
        design_ordered$n_iteration <- 1 # only one iteration
      } else if (!"random" %in% occasions_drawn) { # if only ordered draws are used, draw items once
        
        drawn_items <- draw_items(all_items = all_items, # draw items once for each item-number condition
                                  n_items = n_items,
                                  categories = categories)
        # item sets are automatically ordered
        
        # append design_ordered df by the items drawn according to number of items
        design_ordered <- merge(design_ordered, drawn_items, by = "n_items")
        design_ordered$n_iteration <- 1 # only one iteration
      }
  
    } else if (item_sets_across_replications == "balanced") { # if item sets across replications are balanced
      
      # construct all possible item sets
      # equivalent to random draws
      

      # determine conditions (without iterations) -> for each condition, all possible item sets shall be drawn
      # since there are no iterations/replications (for drawing of measurement occasions),
      # the item sets do not need to be distributed evenly across iterations.
      # Instead, implement all item sets for each condition once.
      # -> iterations for ordered draws stem from different item sets, not different measurement occasions
  
      # since all item sets will be implemented for all item-number conditions and will be appended to the
      # design_ordered data frame (each item set will be used once for each condition, as we do not have 
      # iterations for drawing different measurement occasions), we can simply determine all possible
      # item sets for all unique numbers of items
      # (we do not need to distribute them evenly across iterations)
      
      # use all numbers of items
      # -> n_items in simulation study function
      
      all_item_sets_df <- do.call( # store results for different item-number conditions in an overall df
        rbind,
        lapply(n_items, function(n_items_condition) { # for every item-number condition, function is applied
          all_item_sets_condition <- generate_all_item_sets(
            all_items = all_items,
            categories = categories,
            n_items = n_items_condition # for every item-number condition, function is applied; here, specific number is used
          )
          
          data.frame(n_items = n_items_condition, # turn results into a data frame
                     items = all_item_sets_condition,
                     n_iteration = seq_along(all_item_sets_condition))
        })
      )
      
      # now merge to design_ordered df
      # -> for each unique condition, all possible item sets shall be used
      design_ordered <- merge(design_ordered, all_item_sets_df, by = "n_items")
      
      # sanity check: are there as many rows as possible item sets for each unique condition?
      # (e.g., 243 rows for 5 items and 14 occasions, 243 rows for 5 items and 20 occasions, ...)
      # check: number of item sets per condition
      check_df <- aggregate(items ~ n_occasions + n_items, design_ordered, length)
      names(check_df) <- c("n_occasions", "n_items", "n_rows_in_design")
      # determine number of rows for each unique condition (combination of n_items and n_occasions)
      
      # expected number of item sets per n_items
      expected_number_of_rows <- sapply(unique(design_ordered$n_items), function(n) {
        length(generate_all_item_sets(all_items = all_items, categories = categories, n_items = n))
        # determine number of possible item sets (with length function applied to vector of all item sets)
      })
      names(expected_number_of_rows) <- unique(design_ordered$n_items)
      # use the n_items conditions as names, so that it is clear which condition should have how many rows
      
      # check for all conditions, whether the number of rows in design (check_df$n_rows_in_design)
      # is the same as the expected number of rows for this given item-number condition
      # (expected_number_of_rows[as.character(check_df$n_items)] -> use number of items as name of
      # the element in the expected_number vector)
      if (!all(check_df$n_rows_in_design == expected_number_of_rows[as.character(check_df$n_items)])) {
        stop("Mismatch in number of item sets per condition in ordered design.")
        # if not all of these are TRUE (i.e., actual and expected number are the same), throw error message
      } else { # else remove these checks to save space
        rm(expected_number_of_rows, check_df)
      }

      
    }

    rownames(design_ordered) <- NULL
    # adjust variable order
    design_ordered <- design_ordered[ , c("n_occasions", "occasions_drawn", "n_items", "items", "n_iteration")]
  }
  
  
  
  # COMBINE DESIGN_RANDOM AND DESIGN_ORDERED IF NECESSARY
  # or relabel the corresponding design df to design
  if("random" %in% occasions_drawn & "by order" %in% occasions_drawn) {
    
    # combine both design data frames
    
    # check whether variables are the same in both data frames (and in the same order)
    if (!all(names(design_random) == names(design_ordered)) ) {
      stop("Variables are  not the same or not in the same order in the design_random and the
           design_ordered data frames")
    }
    
    # now combine the data frames
    design <- rbind(design_random, design_ordered)
    
    # for clarity: add variable that codes whether condition is benchmark or
    # a comparison condition
    # benchmark: maximum number of occasions drawn by order, maximum number of items
    design$condition <- "comparison"
    design$condition[design$n_occasions == max(n_occasions) &
                       design$occasions_drawn == "by order" &
                       design$n_items == max(n_items)] <- "benchmark"
    
    
  } else if (!"random" %in% occasions_drawn & "by order" %in% occasions_drawn) {
    # if only ordered draws, but not random draws are in simulation design
    # add a variable specifying benchmark and comparison conditions
    # and relabel design_ordered to design
    
    design <- design_ordered
    
    # for clarity: add variable that codes whether condition is benchmark or
    # a comparison condition
    # benchmark: maximum number of occasions drawn by order, maximum number of items
    design$condition <- "comparison"
    design$condition[design$n_occasions == max(n_occasions) &
                       design$occasions_drawn == "by order" &
                       design$n_items == max(n_items)] <- "benchmark"
    

  } else if ("random" %in% occasions_drawn & !"by order" %in% occasions_drawn) {
    # if only random draws, but not ordered draws are in simulation design
    # add benchmark condition
    # add a variable specifying benchmark and comparison conditions
    # and relabel design_random to design
    
    # relabel
    design <- design_random
    
    # occasions_drawn variable needs to be adjusted so that we can add "by order" as condition
    # add "by order" as a possible factor level
    levels(design$occasions_drawn) <- c(levels(design$occasions_drawn), "by order")
    
    # add benchmark
    benchmark_row <- design[0, ] # create benchmark_row with exact same variables as design
    # (use row 0 so that there is no data in here)
    # now fill benchmark row
    benchmark_row[1, "n_occasions"] <- max(n_occasions) # use maximum number of occasions
    benchmark_row[1 , "n_items"] <- max(n_items) # use maximum number of items
    benchmark_row[1 , "occasions_drawn"] <- "by order" # benchmark occasions are drawn by order (the maximum number anyways)
    benchmark_row[1 , "n_iteration"] <- 1 # only one iteration
    benchmark_row[1, "items"] <- order_item_sets(all_items) # all items as benchmark, but order them
    
    # now add benchmark_row to design
    design <- rbind(design, benchmark_row)
    
    # add variable specifying benchmark and comparison conditions
    design$condition <- "comparison"
    design$condition[design$n_occasions == max(n_occasions) &
                       design$occasions_drawn == "by order" &
                       design$n_items == max(n_items)] <- "benchmark"
    
  }
  
   # order design data frame
   design <- design[order(
    design$occasions_drawn,
    design$n_occasions,
    design$n_items,
    design$n_iteration
  ), ]
  
  rownames(design) <- NULL
  
  # add design_row_id variable so that each design row has a unique ID
  design$design_row_id <- seq_len(nrow(design))
  
  # adjust variable order
  design <- design[ , c("design_row_id", "condition", "n_occasions", "occasions_drawn", "n_items", "items", "n_iteration")]
  
  
  
  # ---------------------------------------------------
  
  # PREPARE BENCHMARK DATA
  benchmark_ICCdata <- calculate_icc(data=data, id.var=id.var,
                                     items = all_items,
                                     type = type,
                                     unit = unit)
  colnames(benchmark_ICCdata) <- c(id.var, "bench_ICC", "bench_ICC.z")
  # benchmark_ICCdata: data on ICCs (raw ICC and ICC.z) using benchmark data
  # -> conditions are compared to this
  
  # HANDLING OF NEGATIVE ICC IN BENCHMARK DATA
  benchmark_ICCdata <- handle_negative_iccs(
    ICCdata = benchmark_ICCdata, # pass benchmark ICC data
    icc_col = "bench_ICC",
    icc.z_col = "bench_ICC.z",
    negative_icc_handling = negative_icc_handling # pass negative ICC handling argument
  )
  # no need to determine number of negative ICCs here, because the benchmark will be
  # an individual row in the design data frame
  # and the benchmark can be easily reproduced outside the function
  
  #----------------------------------------------------
  
  # CREATE RESULTS STORAGE
  # already create columns that we will store the results in
  # name columns; order as in the one_simulation_outcome_measures-function
  res <- data.frame(
    # information on N
    N_merged_total_raw = rep(NA, nrow(design)),
    N_merged_ICC_raw = rep(NA, nrow(design)),
    N_merged_total_handled = rep(NA, nrow(design)),
    N_merged_ICC_handled = rep(NA, nrow(design)),
    N_valid_ICC.z_handled = rep(NA, nrow(design)),
    N_cor_ICC = rep(NA, nrow(design)),
    N_cor_ICC.z = rep(NA, nrow(design)),
    N_rel = rep(NA, nrow(design)),
    # relative outcome measures
    cor_ICC = rep(NA, nrow(design)),
    cor_ICC.z = rep(NA, nrow(design)),
    person_estimates_ICC = I(vector("list", nrow(design))),
    person_estimates_ICC.z = I(vector("list", nrow(design))),
    person_diff_ICC = I(vector("list", nrow(design))),
    person_diff_ICC.z = I(vector("list", nrow(design))), 
    # absolute outcome measures
    rel = rep(NA, nrow(design)),
    sd_ICC = rep(NA, nrow(design)),
    sd_ICC.z = rep(NA, nrow(design)),
    negICC_raw = rep(NA, nrow(design)),
    negICC_handled = rep(NA, nrow(design)),
    estimationProbNeg_raw = rep(NA, nrow(design)),
    estimationProbPos_raw = rep(NA, nrow(design)),
    # information on total, valid, and skipped persons (we can change the order later)
    total_redraws = rep(NA, nrow(design)),
    n_total_persons = rep(NA, nrow(design)),
    n_valid_persons_var = rep(NA, nrow(design)),
    n_skipped_persons_var = rep(NA, nrow(design)),
    # add design row id
    design_row_id = rep(NA, nrow(design)) # add design_row_id to merge results and design later
  )
  
  #----------------------------------------------------
  

  # SET FUTURE PLAN FOR PARALLELIZATION
  if (cores == 1) { # if cores == 1 (default)
    plan(sequential)  # run simulation sequentially
  } else { # else (cores != 1)
    plan(multisession, workers = cores) # run simulation in parallalized manner
  }
  
  # set seed
  future_seed <- !is.null(seed_sim) # if is.null -> FALSE, if not null -> TRUE
  if (future_seed==TRUE) { # if true -> set seed
    set.seed(seed_sim)
  } else {
    future_seed = NULL # set future_seed = NULL -> needed for future_sapply function
  }
  

  #----------------------------------------------------
  
  
  # RUN SIMULATION
  res_list <- future_lapply(seq_len(nrow(design)),# apply function to row dimension of design matrix (i.e.,
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
                               benchmark_ICCdata = benchmark_ICCdata, # calculated before (and negative ICCs handled!)
                               negative_icc_handling = negative_icc_handling) 
                             
                             
                             # add design_row_id to merge with design later
                             one_result$design_row_id <- design[design_row, "design_row_id"]
                             
                             
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
                             
                             
                             return(one_result)
                           },
                           future.seed=future_seed) # set TRUE/FALSE for future seed from above
  
  # store results from list in pre-allocated res data frame
  res <- do.call(rbind, res_list)
  # future_lapply from future.apply preserves order of input and output -> design row 1 = res row 1
  
  # combine design and results
  # merge by design_row_id
  output <- merge(design, res, by="design_row_id", sort=FALSE)
  # add column specifying negative ICC handling in case that simulations shall be combined
  output$negative_icc_handling <- negative_icc_handling 
  output <- output[order(output$design_row_id), ] # restore original order
  return(output)
}




### Test function up to design
# load("prepared data/benchmark_data.rda")
#
# data = bench
# n_occasions = c(14, seq(20, 70, 10))
# occasions_drawn = c("random", "by order")
# n_items = c(5, 10, 15)
# n_iteration = 5000
# id.var = "SERIAL"
# all_items = c('aerger1', 'aerger2', 'aerger3',
#                                  'traurigkeit1', 'traurigkeit2', 'traurigkeit3',
#                                  'angst1', 'angst2', 'angst3',
#                                  'scham1', 'scham2', 'scham3',
#                                  'schuld1', 'schuld2', 'schuld3')
# categories = c("aerger", "aerger", "aerger",
#                "traurigkeit", "traurigkeit", "traurigkeit",
#                "angst", "angst", "angst",
#                "scham", "scham", "scham",
#                "schuld", "schuld", "schuld")
# # look at subsets whether all unique item combinations are drawn (for 5 items)
# sub <- design[design$n_items == 5, ]
# length(unique(sub$items)) # all unique item sets (= 243)
# # look how they are distributed (all item sets almost equally frequent?)
# by(sub, sub$n_occasions, function(df) {
#   freq <- table(df$items)
#   c(
#     min = min(freq),
#     max = max(freq),
#     sd = sd(freq)
#   )
# })
# # relatively uneven
#
#
# # look at subsets whether all unique item combinations are drawn (for 10 items)
# sub <- design[design$n_items == 10, ]
# # item sets in different order are still the same item set -> use only one order
# # canon_items <- sapply(sub$items, function(x) {
# #   items <- trimws(strsplit(x, ",")[[1]])
# #   paste(sort(items), collapse = ", ")
# # })
#
# sub$items_ordered <- sapply(sub$items, function(x) {
#   items <- trimws(strsplit(x, ",")[[1]])
#   paste(sort(items), collapse = ", ")
# })
#
# length(unique(sub$items_ordered)) # 243, correct (all unique item sets)
#
#
# # look how they are distributed (all item sets almost equally frequent?)
# by(sub, sub$n_occasions, function(df) {
#   freq <- table(df$items_ordered)
#   c(
#     min = min(freq),
#     max = max(freq),
#     sd = sd(freq)
#   )
# })
# # relatively uneven
#
# test <- simulation_study(data = bench, n_occasions = c(14, seq(50, 70, 10)),
#                          occasions_drawn = c("random", "by order"), n_items = c(5, 10, 15),
#                          n_iteration = 500,
#                          id.var = "SERIAL", all_items = c('aerger1', 'aerger2', 'aerger3',
#                                                           'traurigkeit1', 'traurigkeit2', 'traurigkeit3',
#                                                           'angst1', 'angst2', 'angst3',
#                                                           'scham1', 'scham2', 'scham3',
#                                                           'schuld1', 'schuld2', 'schuld3'),
#                          categories = c("aerger", "aerger", "aerger",
#                                         "traurigkeit", "traurigkeit", "traurigkeit",
#                                         "angst", "angst", "angst",
#                                         "scham", "scham", "scham",
#                                         "schuld", "schuld", "schuld"),
#                          type = "consistency", unit = "single", occ.running.var = "occ_running",
#                          item_sets_across_replications = "balanced",
#                          seed_item = 123, seed_sim = 456, cores = 5)
# by(test[test$n_items == 5 & test$occasions_drawn=="random", ], test[test$n_items == 5 & test$occasions_drawn=="random", ]$n_occasions, function(df) {
#   freq <- table(df$items)
#   c(
#     min = min(freq),
#     max = max(freq),
#     sd = sd(freq)
#   )
# })
#
#
# by(test[test$n_items == 5 & test$occasions_drawn=="by order", ], test[test$n_items == 5 & test$occasions_drawn=="by order", ]$n_occasions, function(df) {
#   freq <- table(df$items)
#   c(
#     min = min(freq),
#     max = max(freq),
#     sd = sd(freq)
#   )
# })



# # Check for second data set
# # e.g., 5-item condition:
# choose(9, 5) # 126 unique 5-item sets
# 5000 %/% 126  # each item set should appear 39 times
# 5000 %% 126 # 86 item sets will appear 40 times (remainder of 5000 %/% 126 is 86 -> these rows will be filled
# # with one item set each)
#
# # create subset:
# sub <- design_random[design_random$n_items == 5, ]
#
# length(unique(sub$items)) # all unique item sets (= 126), correct
# # look how they are distributed (all item sets almost equally frequent?)
# # -> 39 to 40 times for each item set
# by(sub, sub$n_occasions, function(df) {
#   freq <- table(df$items)
#   c(
#     min = min(freq),
#     max = max(freq),
#     sd = sd(freq)
#   )
# })
# # correct, each item set appears 39 to 40 times in each occasion-number condition
#
# # check 7-item condition
# choose(9, 7) # 36 unique 7-item sets
# 5000 %/% 36  # each item set should appear 138 times
# 5000 %% 36 # 32 item sets will appear 139 times (remainder of 5000 %/% 36 is 32 -> these rows will be filled
# # with one item set each)
#
# # create subset:
# sub <- design_random[design_random$n_items == 7, ]
#
# length(unique(sub$items)) # all unique item sets (= 36), correct
# # look how they are distributed (all item sets almost equally frequent?)
# # -> 138 to 139 times for each item set
# by(sub, sub$n_occasions, function(df) {
#   freq <- table(df$items)
#   c(
#     min = min(freq),
#     max = max(freq),
#     sd = sd(freq)
#   )
# })
# # correct, all item sets appear 138 to 139 times
#
#
# # check 3-item condition
# choose(9, 3) # 84 unique 3-item sets
# 5000 %/% 84  # each item set should appear 59 times
# 5000 %% 84 # 44 item sets will appear 60 times (remainder of 5000 %/% 84 is 44 -> these rows will be filled
# # with one item set each)
#
# # create subset:
# sub <- design_random[design_random$n_items == 3, ]
#
# length(unique(sub$items)) # all unique item sets (= 84), correct
# # look how they are distributed (all item sets almost equally frequent?)
# # -> 59 to 60 times for each item set
# by(sub, sub$n_occasions, function(df) {
#   freq <- table(df$items)
#   c(
#     min = min(freq),
#     max = max(freq),
#     sd = sd(freq)
#   )
# })
# # correct, all item sets appear 59 to 60 times




# ## Test function with regard to negative_icc_handling
# load("prepared data/EMOTIONS_benchmark_data.rda")
# 
# 
# # check whether there are negative ICCs beforehand (in benchmark ICCdata)
# bench_ICC <- calculate_icc(data = bench,
#                            id.var = "id",
#                            items = c("angry", "excluded", "envious", "resentful",
#                                      "ashamed", "insecure", "anxious", "sad", "lonely"),
#                            type = "consistency",
#                            unit = "single")
# table(bench_ICC[ , "ICC"] < 0)
# # 5 negative ICCs beforehand
# 
# # set up small simulation to check negative_icc_handling argument
# 
# # keep negative ICCs
# keep <- simulation_study(data = bench, n_occasions = c(14, 70),
#                          occasions_drawn = c("random", "by order"), 
#                          n_items = 9,
#                          n_iteration = 1,
#                          id.var = "id",
#                          all_items = c("angry", "excluded", "envious", "resentful",
#                                        "ashamed", "insecure", "anxious", "sad", "lonely"),
#                          categories = NULL,
#                          type = "consistency", 
#                          unit = "single",
#                          occ.running.var = "occ_running",
#                          negative_icc_handling = "keep",
#                          seed_item = 1,
#                          item_sets_across_replications = "balanced",
#                          seed_sim = 1, cores = 1)
# View(keep)
# # in 70 occasions and 9 items, there are 5 negative ICCs -> correct
# # compare
# sim_ICCkeep <- keep[3, "person_estimates_ICC"][[1]]
# merged1 <- cbind(bench_ICC, sim_ICCkeep)
# isTRUE(all.equal(merged1[ ,"ICC"], merged1[ , "sim_ICCkeep"], tolerance = 1e-10, check.attributed=FALSE)) # TRUE
# # keep works correctly
# 
# # check whether numbers are plausible
# # number of negative ICCs before and after handling is the same
# # persons skipped due to missing variance in line with the persons for whom an ICC was calculated
# # one positive estimation problem -> one valid ICC.z less than valid ICC
# 
# 
# 
# # set negative ICCs to zero
# # keep negative ICCs
# setzero <- simulation_study(data = bench, n_occasions = c(14, 70),
#                          occasions_drawn = c("random", "by order"), 
#                          n_items = 9,
#                          n_iteration = 1,
#                          id.var = "id",
#                          all_items = c("angry", "excluded", "envious", "resentful",
#                                        "ashamed", "insecure", "anxious", "sad", "lonely"),
#                          categories = NULL,
#                          type = "consistency", 
#                          unit = "single",
#                          occ.running.var = "occ_running",
#                          negative_icc_handling = "set to zero",
#                          seed_item = 1,
#                          item_sets_across_replications = "balanced",
#                          seed_sim = 1, cores = 1)
# View(setzero)
# # in 70 occasions and 9 items, there are 5 negative ICCs before handling -> correct
# # and there are zero negative ICCs after handling
# # number of ICCs that were calculated = number of ICCs after handling (not excluded but set to zero)
# # compare ICCs
# # -> those with negative ICCs in benchmark should now have an ICC of zero
# sim_ICCzero <- setzero[3, "person_estimates_ICC"][[1]]
# merged2 <- cbind(bench_ICC, sim_ICCzero)
# isTRUE(all.equal(merged2[ ,"ICC"], merged2[ , "sim_ICCzero"], tolerance = 1e-10, check.attributed=FALSE)) 
# # FALSE -> some are not equal (this is what we would expect)
# 
# merged2 <- cbind(merged2, diff = merged2[ , "ICC"] - merged2[ , "sim_ICCzero"])
# View(merged2[merged2[ , "diff"] != 0, ])
# # the 5 participants with negative ICCs and one with a difference really close to zero (5.551115e-17)
# 
# # matching über named vector
# merged_test <- cbind(bench_ICC, sim_ICCzero = sim_ICCzero[as.character(bench_ICC[, "id"])])
# merged_test <- cbind(merged_test, diff = merged_test[ , "ICC"] - merged_test[ , "sim_ICCzero"])
# View(merged_test[merged_test[ , "diff"] != 0, ])
# # exact same result
# 
# 
# # now try excluding negative ICCs -> N_merged_ICC_handled should be lower
# excl <- simulation_study(data = bench, n_occasions = c(14, 70),
#                             occasions_drawn = c("random", "by order"), 
#                             n_items = 9,
#                             n_iteration = 1,
#                             id.var = "id",
#                             all_items = c("angry", "excluded", "envious", "resentful",
#                                           "ashamed", "insecure", "anxious", "sad", "lonely"),
#                             categories = NULL,
#                             type = "consistency", 
#                             unit = "single",
#                             occ.running.var = "occ_running",
#                             negative_icc_handling = "exclude",
#                             seed_item = 1,
#                             item_sets_across_replications = "balanced",
#                             seed_sim = 1, cores = 1)
# View(excl)
# # 5 negative ICCs in benchmark before handling, zero after handling
# # N_merged_total = 250, N_merged_ICC_handled = 245 -> correct
# 
# 
# # -> check ICCs
# sim_ICCexcl <- excl[3, "person_estimates_ICC"][[1]]
# merged3 <- cbind(bench_ICC, sim_ICCexcl)
# isTRUE(all.equal(merged3[ ,"ICC"], merged3[ , "sim_ICCexcl"], tolerance = 1e-10, check.attributed=FALSE)) 
# # FALSE -> some are not equal (this is what we would expect)
# 
# merged3 <- cbind(merged3, diff = merged3[ , "ICC"] - merged3[ , "sim_ICCexcl"])
# View(merged3[merged3[ , "diff"] != 0 | is.na(merged3[ , "diff"]), ])
# # the 5 participants with negative ICCs in benchmark (now NAs) and one with a difference really close to zero (5.551115e-17)
# # same participants for set to zero
# 
# # matching über named vector
# merged_test2 <- cbind(bench_ICC, sim_ICCexcl = sim_ICCexcl[as.character(bench_ICC[, "id"])])
# merged_test2 <- cbind(merged_test2, diff = merged_test2[ , "ICC"] - merged_test2[ , "sim_ICCexcl"])
# View(merged_test2[merged_test2[ , "diff"] != 0, ])
# # exact same result
# 
# # row 2:
# # 216 valid ICCs after handling; one positive estim prob -> 215 valid ICC.z
# # fewer ICCs for correlation --> possibly due to persons with negative ICCs not being identical with
# # persons having negative ICCs in benchmark
# row2 <- excl[2 , "person_estimates_ICC"][[1]]
# merged4 <- cbind(bench_ICC, row2)
# merged4 <- as.data.frame(merged4)
# 
# merged4$invalid_ICC <- ifelse(merged4[ , "ICC"] < 0, 1, 0)
# merged4$invalid_row2_ICC <- ifelse(is.na(merged4[ , "row2"]), 1, 0)
#                                    
# merged4$invalid_sum <- rowSums(merged4[ ,c("invalid_ICC", "invalid_row2_ICC")])
# table(merged4$invalid_sum)
# # 33 participants have one invalid value, 3 have 2 invalid values
# # -> 214 have no invalid values
# # --> i.e., some participants have only ONE invalid value (either benchmark OR simulation ICC)
# # -> therefore, the sample size used for ICC is 214
# table(merged4$invalid_ICC == 1 & merged4$invalid_row2_ICC == 0) # 2 participants for whom the
# # benchmark ICC is invalid (negative), but the simulated ICC is not
# 
# # 250 (all persons) - 9 (without variance) - 25 (negative simulation ICC)
# # = 216
# # - 2 (negative ICC in benchmark but not in simulation ICC)
# # = 214
# # -> correct, 214 used for correlation
# # 213 for cor ICC.z -> due to one positive estimation problem (no transformed ICC estimable)
# # for relaibility analysis again 215 (213 + the 2 who had negative benchmark ICC but no negative simulation ICC)
# # -> plausible
# 
# 
# # appears to work correctly