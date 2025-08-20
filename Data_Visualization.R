###################################################################
#####    Estimating trait negative emotion differentiation:   #####
#####        How many measurement occasions and               #####
#####              emotion items are needed?                  #####
###################################################################

###################################################################
#####                  Data Visualization                     #####
###################################################################


# Source Function ---------------------------------------------------------
source("functions/function_plot_outcomes.R")



# Load Packages -----------------------------------------------------------
library(ggpubr)
# ggplot is loaded when sourcing the function


# Load Aggregated Results Data --------------------------------------------
load("results/aggregated_whole_data_set_Study1.rda")


# Extract Data ------------------------------------------------------------

## CORRELATION
cor <- agg[["cor_ICC"]][["agg_res"]]

## DIFFERENCE IN ICC
# for difference in ICC, do NOT report minimum of MEAN difference across replications
# but the minimum difference on person level -> i.e., minimum of minimum difference, mean of
# mean difference, and maximum of maxium difference
# -> merge relevant data

# read data
sub1 <- agg[["min_diff_ICC"]][["agg_res"]]
sub2 <- agg[["mean_diff_ICC"]][["agg_res"]]  
sub3 <- agg[["max_diff_ICC"]][["agg_res"]]

# subset
sub1 <- sub1[ , c("occasions_drawn", "n_occasions", "n_items", "min_diff_ICC_min")]
sub2 <- sub2[ , c("occasions_drawn", "n_occasions", "n_items", "mean_diff_ICC_mean")]
sub3 <- sub3[ , c("occasions_drawn", "n_occasions", "n_items", "max_diff_ICC_max")]

diff <- merge(sub1, sub2, by = c("occasions_drawn", "n_occasions", "n_items"))
diff <- merge(diff, sub3, by = c("occasions_drawn", "n_occasions", "n_items"))

rm(sub1,sub2,sub3)

## RMSE
rmse <- agg[["RMSE_ICC"]][["agg_res"]]

## SD
sd <- agg[["sd_ICC"]][["agg_res"]]

## RELIABILITY
rel <- agg[["rel"]][["agg_res"]]

## NUMBER OF NEGATIVE ICCS
nnegICC <- agg[["negICC"]][["agg_res"]]

## ESTIMATION PROBLEMS
estimProbNeg <- agg[["estimationProbNeg"]][["agg_res"]]
estimProbPos <- agg[["estimationProbPos"]][["agg_res"]]


## VALID VALUES
N_valid_ICC.z <- agg[["N_valid_ICC.z"]][["agg_res"]]
N_rel <- agg[["N_rel"]][["agg_res"]]





# Plot Outcomes -----------------------------------------------------------
# convert data frames to list so that lapply can be used to apply function
# automatically to all outcomes

data_list <- list(cor = cor,
                  diff = diff,
                  rmse = rmse,
                  sd = sd,
                  rel = rel,
                  nnegICC = nnegICC,
                  estimProbNeg = estimProbNeg,
                  estimProbPos = estimProbPos,
                  N_valid_ICC.z = N_valid_ICC.z,
                  N_rel = N_rel)

# define the y label for each outcome plot
ylabels <- list("Correlation with Benchmark",
             "Difference in ICCs to Benchmkark",
             "RMSE",
             "SD of ICCs",
             "Reliability of ICCs",
             "Number of Negative ICCs",
             "Number of Estimation Problems (Negative)",
             "Number of Estimation Problems (Positive)",
             "Number of Valid ICC.z",
             "Number of ICCs for Reliability") 
names(ylabels) <- names(data_list)

# Check minimum and maximum for y limits of each plot and define
# min(cor$min_cor_ICC_min)
# max(cor$max_cor_ICC_max)
# min(diff$min_diff_ICC_min)
# max(diff$max_diff_ICC_max)
# min(rmse$RMSE_ICC_min)
# max(rmse$RMSE_ICC_max)
# min(sd$sd_ICC_min)
# max(sd$sd_ICC_max)
# min(rel$rel_min)
# max(rel$rel_max)
# min(nnegICC$negICC_min)
# max(nnegICC$negICC_max)
# min(estimProbNeg$estimationProbNeg_min) 
# max(estimProbNeg$estimationProbNeg_max)
# min(estimProbPos$estimationProbPos_min)
# max(estimProbPos$estimationProbPos_max)
# # in general: no estimation problems
# min(N_valid_ICC.z$N_valid_ICC.z_min)
# max(N_valid_ICC.z$N_valid_ICC.z_max)
# min(N_rel$N_rel_min)
# max(N_rel$N_rel_max)

ylim_list <- list(
  c(-0.3, 1), # correlation with benchmark
  c(-0.7, 0.85), # difference in ICCs (compared to benchmark)
  c(0, 0.35), # RMSE
  c(0, 0.32), # SD of ICCs
  c(0, 1), # Reliability
  c(0, 22), # number of negative ICCs
  c(0, 1), # number of estimation problems (negative)
  c(0, 1), # number of estimation problems (positive)
  c(35, 36), # number of valid ICC.z
  c(35, 36) # number of ICCs used for reliability
)
names(ylim_list) <- names(data_list)

plot_list <- lapply(names(data_list), function(outcome) {
  df <- data_list[[outcome]]
  plot_outcome(df, ylabel = ylabels[[outcome]], ylims=ylim_list[[outcome]],
               x_breaks = seq(0, 100, 20), theme_custom = my_theme)
})

names(plot_list) <- names(data_list)





# Look At Plots -----------------------------------------------------------

plot_list[["cor"]]
plot_list[["diff"]]
plot_list[["rmse"]]
plot_list[["sd"]]
plot_list[["rel"]]
plot_list[["nnegICC"]]
plot_list[["estimProbNeg"]]
plot_list[["estimProbPos"]]
plot_list[["N_valid_ICC.z"]]
plot_list[["N_rel"]]



# Combine Plots -----------------------------------------------------------
# do not use estimation problems as there were none
# number of valid ICC.z and number of participants for reliability can be 
# reported easily -> do not plot

combined <- ggpubr::ggarrange(plot_list[["cor"]],
                      plot_list[["diff"]],
                      plot_list[["rmse"]],
                      plot_list[["sd"]],
                      plot_list[["rel"]],
                      plot_list[["nnegICC"]], ncol=3, nrow=2, common.legend = TRUE, legend="top")
combined
