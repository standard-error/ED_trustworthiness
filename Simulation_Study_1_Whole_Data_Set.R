###################################################################
#####    Estimating trait negative emotion differentiation:   #####
#####        How many measurement occasions and               #####
#####              emotion items are needed?                  #####
###################################################################

###################################################################
#####              Simulation: Whole Data Set                 #####
###################################################################



# Preparation: Load Data --------------------------------------------------
load("prepared data/benchmark_data_Study1.rda")

# Make sure that ID variable is numeric
# (relevant for ICC calculation: matrix can only store one data type)
is.numeric(bench$SERIAL)


# Source Functions --------------------------------------------------------
source("functions/function_ordered_occasion_draw.R")
source("functions/function_random_occasion_draw.R")
source("functions/function_calculate_iccs.R")
source("functions/one_simulation_run.R")



# Set Up Simulation Design ------------------------------------------------
design <- expand.grid(
  n_occasions = c(3, 5, seq(10, 100, 10)),
  occasions_drawn = c("by order", "random"),
  n_items = c(15, 5),
  n_iteration = 1:100 # number of iterations, only relevant for random draws
)

# remove iterations for occasions_drawn == "by order" -> there is only one option
# on how to draw by order --> only one replication
# also remove conditions with random draw of 100 occasions: as they are drawn
# without replacement, the 100 "randomly" drawn occasions (i.e., the maximum
# number, benchmark) will always be the same
design$flag <- 0
design$flag[design$occasions_drawn == "by order" & design$n_iteration >= 2] <- 1
design$flag[design$n_occasions == 100 & design$occasions_drawn == "random"] <- 1

design <- design[design$flag == 0, ]
design$flag <- NULL


# for clarity: add variable that codes whether condition is benchmark or
# a comparison condition
# benchmark: 100 occasions drawn by order, 15 items
which(design$n_occasions == 100 &
        design$occasions_drawn == "by order" &
        design$n_items == 15) # row number of benchmark

design$condition <- "comparison"
design$condition[design$n_occasions == 100 &
                   design$occasions_drawn == "by order" &
                   design$n_items == 15] <- "benchmark"
# table(design$condition)

# adjust variable order
design <- design[ , c(5, 1:4)]
# design




# Prepare Benchmark Data that Conditions Are Compared To ------------------
benchmark_ICCdata <- calculate_icc(bench, id.var="SERIAL",
                                   items = c("aerger1", "aerger2", "aerger3",
                                             "traurigkeit1", "traurigkeit2", "traurigkeit3",
                                             "angst1", "angst2", "angst3",
                                             "scham1", "scham2", "scham3",
                                             "schuld1", "schuld2", "schuld3"),
                                   type = "consistency",
                                   unit = "single")

# rename variables
colnames(benchmark_ICCdata) <- c("SERIAL", "bench_ICC", "bench_ICC.z")



# Create Results Storage --------------------------------------------------
res <- data.frame(matrix(NA, nrow=nrow(design), ncol=17))
# ncol = 17 -> we have 17 outcomes (6 outcome measures, but some for raw ICCs
# and also for Fisher's Z-transformed ICCs)

# rename columns; order as in the one_simulation_outcome_measures-function
colnames(res) <- c( 
  # relative outcome measures
  "min_diff_ICC",
  "mean_diff_ICC",
  "max_diff_ICC",
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



# Run Simulation ----------------------------------------------------------
tictoc::tic()
set.seed(230694)
for (design_row in 1:nrow(design)) {
  res[design_row, ] <- one_simulation(data = bench,
                                      nr.of.occasions = design[design_row, "n_occasions"],
                                      occasions.drawn = design[design_row, "occasions_drawn"],
                                      nr.of.items = design[design_row, "n_items"],
                                      id.var = "SERIAL",
                                      occ.running.var = "occ_running",
                                      type = "consistency",
                                      unit = "single",
                                      benchmark_ICCdata = benchmark_ICCdata)
}
tictoc::toc()
# combine design and results
res <- cbind(design, res)



# Save Results ------------------------------------------------------------
save(res, file="results/sim_results_whole_data_set_Study1.rda")

