###################################################################
#####      Estimating trait emotion differentiation:          #####
#####          How many measurement occasions and             #####
#####              emotion items are needed?                  #####
###################################################################

###################################################################
#####           Plots for Manuscript and Supplement           #####
###################################################################

##### Sensitivity Analysis: Negative ICC Handling (Exclude) #######


# Create a plot with multiple panels for the outcomes for 
# both NED and PED for a given study
# -> use random draws for main manuscript and ordered draws
# for supplement (sensitivity analysis)


# Source Function ---------------------------------------------------------
source("functions/function_plot_outcomes.R")


# Load Packages -----------------------------------------------------------
library(ggpubr)
library(tidyverse) # reshape data (for participant-level plots)
# ggplot is loaded when sourcing the function


# EMOTIONS Data PED + NED --------------------------------------------------

load("results/02_revision_1/EMOTIONS study/NED/neg ICC handling/exclude/processed/aggregated_results_NED_EMOTIONS_exclude.rda")
agg_ned <- agg
rm(agg)

load("results/02_revision_1/EMOTIONS study/PED/neg ICC handling/exclude/processed/aggregated_results_PED_EMOTIONS_exclude.rda")
agg_ped <- agg
rm(agg)


# add variable to all outcomes in agg_ned and agg_ped specifying NED or PED (will be used as facet in plots)
# str(agg_ned)
# names(agg_ned)

for (out in names(agg_ned)) {
  agg_ned[[out]][["agg_res"]]$diff_type <- "NED"
}

for (out in names(agg_ped)) {
  agg_ped[[out]][["agg_res"]]$diff_type <- "PED"
}


# now merge agg_ned and agg_ped into one data frame for each outcome
both <- list()
for (out in names(agg_ned)) {
  both[[out]][["agg_res"]] <- rbind(agg_ned[[out]][["agg_res"]], agg_ped[[out]][["agg_res"]])
}




# for each outcome, split into two data frames, one for the main manuscript (random draws)
# and one for the supplement (ordered draws)
rd <- list() # random draws
od <- list() # ordered draws

for (out in names(both)) {
  rd[[out]] <- both[[out]][["agg_res"]][both[[out]][["agg_res"]]$occasions_drawn == "random", ]
  od[[out]] <- both[[out]][["agg_res"]][both[[out]][["agg_res"]]$occasions_drawn == "by order", ]
}





# '' Random Draw Figures for Manuscript -----------------------------------

# '''' Check Number of Participants ---------------------------------------
# First, check whether sample size differed across analyses/outcomes,
# whether there were participants who were skipped due to lack of variance,
# whether there were differences in the number of replications used to calculate RMSE
# and whether there were any estimation problems
min(rd[["N_merged_ICC_raw"]]["N_merged_ICC_raw_min"])
max(rd[["N_merged_ICC_raw"]]["N_merged_ICC_raw_max"])

min(rd[["N_merged_ICC_handled"]]["N_merged_ICC_handled_min"])
max(rd[["N_merged_ICC_handled"]]["N_merged_ICC_handled_max"])

min(rd[["N_valid_ICC.z_handled"]]["N_valid_ICC.z_handled_min"])
max(rd[["N_valid_ICC.z_handled"]]["N_valid_ICC.z_handled_max"])

min(rd[["N_cor_ICC"]]["N_cor_ICC_min"])
max(rd[["N_cor_ICC"]]["N_cor_ICC_max"])

min(rd[["N_cor_ICC.z"]]["N_cor_ICC.z_min"])
max(rd[["N_cor_ICC.z"]]["N_cor_ICC.z_max"])

min(rd[["N_rel"]]["N_rel_min"])
max(rd[["N_rel"]]["N_rel_max"])

min(rd[["n_valid_persons_var"]]["n_valid_persons_var_min"])
max(rd[["n_valid_persons_var"]]["n_valid_persons_var_max"])

min(rd[["n_skipped_persons_var"]]["n_skipped_persons_var_min"])
max(rd[["n_skipped_persons_var"]]["n_skipped_persons_var_max"])

min(rd[["estimationProbNeg_raw"]]["estimationProbNeg_raw_min"])
max(rd[["estimationProbNeg_raw"]]["estimationProbNeg_raw_max"])

min(rd[["estimationProbPos_raw"]]["estimationProbPos_raw_min"])
max(rd[["estimationProbPos_raw"]]["estimationProbPos_raw_max"])

min(rd[["RMSE_ICC_N"]]["RMSE_N_min"])
max(rd[["RMSE_ICC_N"]]["RMSE_N_max"])

min(rd[["RMSE_ICC.z_N"]]["RMSE.z_N_min"])
max(rd[["RMSE_ICC.z_N"]]["RMSE.z_N_max"])





# '''' Plot Outcomes ------------------------------------------------------

# data frames already stored in a list
# but use only those that we want to plot

data_list <- list(cor = rd[["cor_ICC"]],
                  cor.z = rd[["cor_ICC.z"]],
                  diff = rd[["person_diff"]],
                  diff.z = rd[["person_diff.z"]],
                  rmse = rd[["RMSE_ICC"]],
                  rmse.z = rd[["RMSE_ICC.z"]],
                  sd = rd[["sd_ICC"]],
                  sd.z = rd[["sd_ICC.z"]],
                  rel = rd[["rel"]],
                  percnegICC = rd[["percnegICC_raw"]],
                  estimProbNeg = rd[["estimationProbNeg_raw"]],
                  estimProbPos = rd[["estimationProbPos_raw"]],
                  N_valid_ICC.z = rd[["N_valid_ICC.z_handled"]],
                  N_cor_ICC.z = rd[["N_cor_ICC.z"]],
                  N_rel = rd[["N_rel"]],
                  N_RMSE.z = rd[["RMSE_ICC.z_N"]])

# define the y label for each outcome plot
ylabels <- list("Correlation with Benchmark",
                "Correlation with Benchmark (ICC.z)",
                "Difference in ICCs to Benchmark",
                "Difference in ICCs to Benchmark (ICC.z)",
                "RMSE",
                "RMSE (ICC.z)",
                "SD of ICCs",
                "SD of ICC.z",
                "Reliability of ICCs",
                "Proportion of Negative ICCs",
                "Number of Estimation Problems (Lower Bound)",
                "Number of Estimation Problems (Upper Bound)",
                "Number of valid ICC.z",
                "Number of ICC.z used for Correlation",
                "Number of ICC.z used for Reliability",
                "Number of Replications used for RMSE (ICC.z)") 
names(ylabels) <- names(data_list)

# Check minimum and maximum to define y-axis limits for each outcome
# or use theoretical range, respectively
# min(data_list[["cor"]][["cor_ICC_min"]])
# max(data_list[["cor"]][["cor_ICC_max"]])
# min(data_list[["cor.z"]][["cor_ICC.z_min"]])
# max(data_list[["cor.z"]][["cor_ICC.z_max"]])
# min(data_list[["diff"]][["difference_min"]])
# max(data_list[["diff"]][["difference_max"]])
# min(data_list[["diff.z"]][["difference.z_min"]])
# max(data_list[["diff.z"]][["difference.z_max"]])
# min(data_list[["rmse"]][["RMSE_min"]])
# max(data_list[["rmse"]][["RMSE_max"]])
# min(data_list[["rmse.z"]][["RMSE.z_min"]])
# max(data_list[["rmse.z"]][["RMSE.z_max"]])
# min(data_list[["sd"]][["sd_ICC_min"]])
# max(data_list[["sd"]][["sd_ICC_max"]])
# min(data_list[["sd.z"]][["sd_ICC.z_min"]])
# max(data_list[["sd.z"]][["sd_ICC.z_max"]])
# min(data_list[["rel"]][["rel_min"]])
# max(data_list[["rel"]][["rel_max"]])
# min(data_list[["estimProbNeg"]][["estimationProbNeg_raw_min"]])
# max(data_list[["estimProbNeg"]][["estimationProbNeg_raw_max"]])
# min(data_list[["estimProbPos"]][["estimationProbPos_raw_min"]])
# max(data_list[["estimProbPos"]][["estimationProbPos_raw_max"]])
# min(data_list[["percnegICC"]][["percnegICC_raw_min"]])
# max(data_list[["percnegICC"]][["percnegICC_raw_max"]])
# min(data_list[["N_valid_ICC.z"]][["N_valid_ICC.z_handled_min"]])
# max(data_list[["N_valid_ICC.z"]][["N_valid_ICC.z_handled_max"]])
# min(data_list[["N_cor_ICC.z"]][["N_cor_ICC.z_min"]])
# max(data_list[["N_cor_ICC.z"]][["N_cor_ICC.z_max"]])
# min(data_list[["N_rel"]][["N_rel_min"]])
# max(data_list[["N_rel"]][["N_rel_max"]])
# min(data_list[["N_RMSE.z"]][["RMSE.z_N_min"]])
# max(data_list[["N_RMSE.z"]][["RMSE.z_N_max"]])



# for correlation, reliability, proportions, ... -> use theoretical range (e.g., difference max and min: 1 and -1)
ylim_list <- list(
  c(0, 1), # correlation with benchmark, theoretical range
  c(0, 1), # correlation with benchmark (ICC.z), theoretical range
  c(-1, 1), # difference in ICCs (compared to benchmark), theoretical range
  c(-1.5, 3.5), # difference in ICCs (compared to benchmark) for ICC.z
  c(0, 0.6), # RMSE, theoretical lower bound
  c(0, 8.5), # RMSE (ICC.z), theoretical lower bound
  c(0, 0.4), # SD of ICCs, theoretical lower bound
  c(0, 3), # SD of ICC.z, theoretical lower bound
  c(0, 1), # Reliability
  c(0, 1), # proportion of negative ICCs
  c(0, 1), # number of estimation problems (negative)
  c(0, 11), # number of estimation problems (positive)
  c(160, 176), # number of valid ICC.z
  c(160, 176), # number of ICC.z used for correlation
  c(160, 176), # number of ICC.z used for reliability
  c(0, 5000) # number of replications used for RMSE
)
names(ylim_list) <- names(data_list)


# plot outcomes
plot_list <- lapply(names(data_list), function(outcome) {
  df <- data_list[[outcome]]
  plot_outcome(df,
               ylabel = ylabels[[outcome]],
               ylims=ylim_list[[outcome]],
               x_breaks = seq(0, 70, 10),
               theme_custom = my_theme,
               dodge_width = 3,
               split_facets = FALSE,
               facet_var = "diff_type",
               facet_order = c("NED", "PED"))
})

names(plot_list) <- names(data_list)

plot_list[["cor"]]
plot_list[["cor.z"]]
plot_list[["diff"]]
plot_list[["diff.z"]]
plot_list[["rmse"]]
plot_list[["rmse.z"]]
plot_list[["sd"]]
plot_list[["sd.z"]]
plot_list[["rel"]]
plot_list[["percnegICC"]]
plot_list[["estimProbNeg"]]
plot_list[["estimProbPos"]]
plot_list[["N_valid_ICC.z"]]
plot_list[["N_cor_ICC.z"]]
plot_list[["N_rel"]]
plot_list[["N_RMSE.z"]]




# '''' Combine Plots ------------------------------------------------------

# '''''' for raw ICCs -----------------------------------------------------

# adjust the plots a little (e.g., no y-axis lable but title, no x-axis label)
a <- plot_list[["cor"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                axis.text.y = element_text(hjust=1),
                                axis.text = element_text(size=10)) + ggtitle("(A) Correlation with Benchmark") +
  geom_hline(yintercept=0.80, linetype="twodash", color = "black") + force_panelsizes(rows=1, cols = c(1,1))
# a
b <- plot_list[["diff"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                 plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                 axis.text.y = element_text(hjust=1),
                                 axis.text = element_text(size=10)) + ggtitle("(B) Difference from Benchmark") + force_panelsizes(rows=1, cols = c(1,1))
# b
c <- plot_list[["rmse"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                 plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                 axis.text.y = element_text(hjust=1),
                                 axis.text = element_text(size=10)) + ggtitle("(C) RMSE")+ force_panelsizes(rows=1, cols = c(1,1))


# c

# get benchmark SD of ICCs for raw ICCs
# for NED:
bench_ned_ICC_sd <- od[["sd_ICC"]][od[["sd_ICC"]]$diff_type == "NED" &
                                     od[["sd_ICC"]]$n_items == 9 &
                                     od[["sd_ICC"]]$n_occasions == 70, ]$sd_ICC_mean

bench_ped_ICC_sd <- od[["sd_ICC"]][od[["sd_ICC"]]$diff_type == "PED" &
                                     od[["sd_ICC"]]$n_items == 5 &
                                     od[["sd_ICC"]]$n_occasions == 70, ]$sd_ICC_mean

bench_ICC_sd <- data.frame(
  diff_type = c("NED", "PED"),
  benchmark_sd = c(bench_ned_ICC_sd, bench_ped_ICC_sd)
)


d <- plot_list[["sd"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                               plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                               axis.text.y = element_text(hjust=1),
                               axis.text = element_text(size=10)) + ggtitle("(D) SD of ICCs") +
  geom_hline(data=bench_ICC_sd, aes(yintercept=benchmark_sd), # use benchmark SD
             linetype="twodash", color = "black")+ force_panelsizes(rows=1, cols = c(1,1))

# d
e <- plot_list[["rel"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                axis.text.y = element_text(hjust=1),
                                axis.text = element_text(size=10)) + ggtitle("(E) Reliability of ICCs") +
  geom_hline(yintercept=0.80, linetype="twodash", color = "black")+ force_panelsizes(rows=1, cols = c(1,1))
# e
f <- plot_list[["percnegICC"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                       plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                       axis.text.y = element_text(hjust=1),
                                       axis.text = element_text(size=10)) + ggtitle("(F) Proportion of Negative ICCs")+ force_panelsizes(rows=1, cols = c(1,1))

g <- plot_list[["estimProbNeg"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                         plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                         axis.text.y = element_text(hjust=1),
                                         axis.text = element_text(size=10)) + ggtitle("(G) Estimation Problems (LB)") +
  force_panelsizes(rows=1, cols = c(1,1))

# g

h <- plot_list[["estimProbPos"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                         plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                         axis.text.y = element_text(hjust=1),
                                         axis.text = element_text(size=10)) + ggtitle("(H) Estimation Problems (UB)") +
  force_panelsizes(rows=1, cols = c(1,1))

# h


combined <- ggpubr::ggarrange(a,b,c,d,e,f,g,h, ncol=3, nrow=3, common.legend = TRUE, legend="top",
                              align = "hv", widths = c(1,1,1), heights = c(1, 1, 1)) # equal panel sizes


combined <- annotate_figure(combined,
                            bottom = text_grob("Number of Measurement Occasions", size = 12))

combined


ggsave("plots/02_revision_1/for publication/neg ICC handling/EMOTIONS_plots_exclude_random_draws_NED_PED_combined.pdf",plot = combined, device="pdf", height = 222, width = 222, unit="mm")

ggsave("plots/02_revision_1/for publication/neg ICC handling/EMOTIONS_plots_exclude_random_draws_NED_PED_combined.svg",plot = combined, device="svg", height = 222, width = 222, unit="mm")

# as tiff
tiff("plots/02_revision_1/for publication/neg ICC handling/EMOTIONS_plots_exclude_random_draws_NED_PED_combined.tiff", units="mm", width=222, height=222, res=1200)
combined
dev.off()




# '''''' for ICC.z --------------------------------------------------------
# adjust the plots a little (e.g., no y-axis lable but title, no x-axis label)
a <- plot_list[["cor.z"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                  plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                  axis.text = element_text(size=10)) + ggtitle("(A) Correlation with Benchmark") +
  geom_hline(yintercept=0.80, linetype="twodash", color = "black")
# a
b <- plot_list[["diff.z"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                   plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                   axis.text = element_text(size=10)) + ggtitle("(B) Difference from Benchmark")
# b
c <- plot_list[["rmse.z"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                   plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                   axis.text = element_text(size=10)) + ggtitle("(C) RMSE")
# c


# get benchmark SD of ICCs for raw ICCs
# for NED:
bench_ned_ICC.z_sd <- od[["sd_ICC.z"]][od[["sd_ICC.z"]]$diff_type == "NED" &
                                         od[["sd_ICC.z"]]$n_items == 9 &
                                         od[["sd_ICC.z"]]$n_occasions == 70, ]$sd_ICC.z_mean

bench_ped_ICC.z_sd <- od[["sd_ICC.z"]][od[["sd_ICC.z"]]$diff_type == "PED" &
                                         od[["sd_ICC.z"]]$n_items == 5 &
                                         od[["sd_ICC.z"]]$n_occasions == 70, ]$sd_ICC.z_mean

bench_ICC.z_sd <- data.frame(
  diff_type = c("NED", "PED"),
  benchmark_sd.z = c(bench_ned_ICC.z_sd, bench_ped_ICC.z_sd)
)


d <- plot_list[["sd.z"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                 plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                 axis.text = element_text(size=10)) + ggtitle("(D) SD of ICCs") +
  geom_hline(data=bench_ICC.z_sd, aes(yintercept=benchmark_sd.z), # use benchmark SD
             linetype="twodash", color = "black")
# d

e <- plot_list[["rel"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                axis.text = element_text(size=10)) + ggtitle("(E) Reliability of ICCs")  +
  geom_hline(yintercept=0.80, linetype="twodash", color = "black")
# e
f <- plot_list[["percnegICC"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                       plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                       axis.text = element_text(size=10)) + ggtitle("(F) Proportion of Negative ICCs")

g <- plot_list[["estimProbNeg"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                         plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                         axis.text.y = element_text(hjust=1),
                                         axis.text = element_text(size=10)) + ggtitle("(G) Estimation Problems (LB)") +
  force_panelsizes(rows=1, cols = c(1,1))

# g

h <- plot_list[["estimProbPos"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                         plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                         axis.text.y = element_text(hjust=1),
                                         axis.text = element_text(size=10)) + ggtitle("(H) Estimation Problems (UB)") +
  force_panelsizes(rows=1, cols = c(1,1))

# h



combined <- ggpubr::ggarrange(a,b,c,d,e,f,g,h , ncol=3, nrow=3, common.legend = TRUE, legend="top",
                              align = "hv", widths = c(1,1,1), heights = c(1, 1)) # equal panel sizes


combined <- annotate_figure(combined,
                            bottom = text_grob("Number of Measurement Occasions", size = 12))

combined



ggsave("plots/02_revision_1/for publication/neg ICC handling/EMOTIONS_plots_exclude_random_draws_NED_PED_Z-transformed_combined.pdf",plot = combined, device="pdf", height = 222, width = 222, unit="mm")

ggsave("plots/02_revision_1/for publication/neg ICC handling/EMOTIONS_plots_exclude_random_draws_NED_PED_Z-transformed_combined.svg",plot = combined, device="svg", height = 222, width = 222, unit="mm")

# as tiff
tiff("plots/02_revision_1/for publication/neg ICC handling/EMOTIONS_plots_exclude_random_draws_NED_PED_Z-transformed_combined.tiff", units="mm", width=222, height=222, res=1200)
combined
dev.off()




# '' Ordered Draw Figures for Supplement ----------------------------------

# use ordered draws here (sensitivity analysis)


# '''' Check Number of Participants ---------------------------------------
# First, check whether sample size differed across analyses/outcomes,
# whether there were participants who were skipped due to lack of variance,
# whether there were differences in the number of replications used to calculate RMSE
# and whether there were any estimation problems
min(od[["N_merged_ICC_raw"]]["N_merged_ICC_raw_min"])
max(od[["N_merged_ICC_raw"]]["N_merged_ICC_raw_max"])

min(od[["N_merged_ICC_handled"]]["N_merged_ICC_handled_min"])
max(od[["N_merged_ICC_handled"]]["N_merged_ICC_handled_max"])

min(od[["N_valid_ICC.z_handled"]]["N_valid_ICC.z_handled_min"])
max(od[["N_valid_ICC.z_handled"]]["N_valid_ICC.z_handled_max"])

min(od[["N_cor_ICC"]]["N_cor_ICC_min"])
max(od[["N_cor_ICC"]]["N_cor_ICC_max"])

min(od[["N_cor_ICC.z"]]["N_cor_ICC.z_min"])
max(od[["N_cor_ICC.z"]]["N_cor_ICC.z_max"])

min(od[["N_rel"]]["N_rel_min"])
max(od[["N_rel"]]["N_rel_max"])

min(od[["n_valid_persons_var"]]["n_valid_persons_var_min"])
max(od[["n_valid_persons_var"]]["n_valid_persons_var_max"])

min(od[["n_skipped_persons_var"]]["n_skipped_persons_var_min"])
max(od[["n_skipped_persons_var"]]["n_skipped_persons_var_max"])

min(od[["estimationProbNeg_raw"]]["estimationProbNeg_raw_min"])
max(od[["estimationProbNeg_raw"]]["estimationProbNeg_raw_max"])

min(od[["estimationProbPos_raw"]]["estimationProbPos_raw_min"])
max(od[["estimationProbPos_raw"]]["estimationProbPos_raw_max"])


# some participants skipped due to lack of variance (in given item set across occasions) 
# and estimation problems
# -> varying numbers of participants with valid values and participants used for analyses

# -> plot and/or report in tables



# '''' Plot Outcomes ------------------------------------------------------

# data frames already stored in a list
# but use only those that we want to plot

data_list <- list(cor = od[["cor_ICC"]],
                  cor.z = od[["cor_ICC.z"]],
                  diff = od[["person_diff"]],
                  diff.z = od[["person_diff.z"]],
                  sd = od[["sd_ICC"]],
                  sd.z = od[["sd_ICC.z"]],
                  rel = od[["rel"]],
                  percnegICC = od[["percnegICC_raw"]],
                  estimProbNeg = od[["estimationProbNeg_raw"]],
                  estimProbPos = od[["estimationProbPos_raw"]],
                  N_valid_persons_var = od[["n_valid_persons_var"]],
                  N_skipped_persons_var = od[["n_skipped_persons_var"]],
                  N_merged_ICC_raw = od[["N_merged_ICC_raw"]],
                  N_merged_ICC_handled = od[["N_merged_ICC_handled"]],
                  N_cor_ICC = od[["N_cor_ICC"]],
                  N_valid_ICC.z = od[["N_valid_ICC.z_handled"]],
                  N_cor_ICC.z = od[["N_cor_ICC.z"]],
                  N_rel = od[["N_rel"]])

# define the y label for each outcome plot
ylabels <- list("Correlation with Benchmark",
                "Correlation with Benchmark (ICC.z)",
                "Difference in ICCs to Benchmark",
                "Difference in ICCs to Benchmark (ICC.z)",
                "SD of ICCs",
                "SD of ICC.z",
                "Reliability of ICCs",
                "Proportion of Negative ICCs",
                "Number of Estimation Problems (Lower Bound)",
                "Number of Estimation Problems (Upper Bound)",
                "Number of Valid Persons",
                "Number of Skipped Persons",
                "Number of Valid Raw ICCs (Before Negative ICC Treatment)",
                "Number of Valid Raw ICCs (After Negative ICC Treatment)",
                "Number of ICCs used for Correlation",
                "Number of Valid ICC.z",
                "Number of ICC.z used for Correlation",
                "Number of ICC.z used for Reliability") 
names(ylabels) <- names(data_list)

# # Check minimum and maximum to define y-axis limits for each outcome
# # or use theoretical range, respectively
# min(data_list[["cor"]][["cor_ICC_min"]])
# max(data_list[["cor"]][["cor_ICC_max"]])
# min(data_list[["cor.z"]][["cor_ICC.z_min"]])
# max(data_list[["cor.z"]][["cor_ICC.z_max"]])
# min(data_list[["diff"]][["difference_min"]])
# max(data_list[["diff"]][["difference_max"]])
# min(data_list[["diff.z"]][["difference.z_min"]])
# max(data_list[["diff.z"]][["difference.z_max"]])
# min(data_list[["sd"]][["sd_ICC_min"]])
# max(data_list[["sd"]][["sd_ICC_max"]])
# min(data_list[["sd.z"]][["sd_ICC.z_min"]])
# max(data_list[["sd.z"]][["sd_ICC.z_max"]])
# min(data_list[["rel"]][["rel_min"]])
# max(data_list[["rel"]][["rel_max"]])
# min(data_list[["percnegICC"]][["percnegICC_raw_min"]])
# max(data_list[["percnegICC"]][["percnegICC_raw_max"]])
# min(data_list[["estimProbNeg"]][["estimationProbNeg_raw_min"]])
# max(data_list[["estimProbNeg"]][["estimationProbNeg_raw_max"]])
# min(data_list[["estimProbPos"]][["estimationProbPos_raw_min"]])
# max(data_list[["estimProbPos"]][["estimationProbPos_raw_max"]])
# min(data_list[["N_valid_persons_var"]][["n_valid_persons_var_min"]])
# max(data_list[["N_valid_persons_var"]][["n_valid_persons_var_max"]])
# min(data_list[["N_skipped_persons_var"]][["n_skipped_persons_var_min"]])
# max(data_list[["N_skipped_persons_var"]][["n_skipped_persons_var_max"]])
# min(data_list[["N_merged_ICC_raw"]][["N_merged_ICC_raw_min"]])
# max(data_list[["N_merged_ICC_raw"]][["N_merged_ICC_raw_max"]])
# min(data_list[["N_merged_ICC_handled"]][["N_merged_ICC_handled_min"]])
# max(data_list[["N_merged_ICC_handled"]][["N_merged_ICC_handled_max"]])
# min(data_list[["N_cor_ICC"]][["N_cor_ICC_min"]])
# max(data_list[["N_cor_ICC"]][["N_cor_ICC_max"]])
# min(data_list[["N_valid_ICC.z"]][["N_valid_ICC.z_handled_min"]])
# max(data_list[["N_valid_ICC.z"]][["N_valid_ICC.z_handled_max"]])
# min(data_list[["N_cor_ICC.z"]][["N_cor_ICC.z_min"]])
# max(data_list[["N_cor_ICC.z"]][["N_cor_ICC.z_max"]])
# min(data_list[["N_rel"]][["N_rel_min"]])
# max(data_list[["N_rel"]][["N_rel_max"]])


# for correlation, reliability, proportions, ... -> use theoretical range (e.g., difference max and min: 1 and -1)
ylim_list <- list(
  c(0, 1), # correlation with benchmark, theoretical range
  c(0, 1), # correlation with benchmark (ICC.z), theoretical range
  c(-1, 1), # difference in ICCs (compared to benchmark), theoretical range
  c(-1.5, 3.5), # difference in ICCs (compared to benchmark) for ICC.z
  c(0, 0.4), # SD of ICCs, theoretical lower bound
  c(0, 3), # SD of ICC.z, theoretical lower bound
  c(0, 1), # Reliability
  c(0, 1), # proportion of negative ICCs
  c(0, 1), # number of estimation problems (negative)
  c(0, 11), # number of estimation problems (positive)
  c(120, 176), # number of valid persons
  c(0, 50), # number of skipped persons
  c(120, 176), # number of valid raw ICCs (before negative ICC treatment)
  c(120, 176), # number of valid raw ICCs (after negative ICC treatment)
  c(120, 176), # number of ICCs used for correlation
  c(120, 176), # number of valid ICC.z
  c(120, 176), # number of ICC.z used for correlation
  c(120, 176) # number of ICC.z used for reliability
)
names(ylim_list) <- names(data_list)


# plot outcomes
plot_list <- lapply(names(data_list), function(outcome) {
  df <- data_list[[outcome]]
  plot_outcome(df,
               ylabel = ylabels[[outcome]],
               ylims=ylim_list[[outcome]],
               x_breaks = seq(0, 70, 10),
               theme_custom = my_theme,
               dodge_width = 3,
               split_facets = FALSE,
               facet_var = "diff_type",
               facet_order = c("NED", "PED"))
})

names(plot_list) <- names(data_list)

plot_list[["cor"]]
plot_list[["cor.z"]]
plot_list[["diff"]]
plot_list[["diff.z"]]
plot_list[["sd"]]
plot_list[["sd.z"]]
plot_list[["rel"]]
plot_list[["percnegICC"]]
plot_list[["estimProbNeg"]]
plot_list[["estimProbPos"]]
plot_list[["N_valid_persons_var"]]
plot_list[["N_skipped_persons_var"]]
plot_list[["N_merged_ICC_raw"]]
plot_list[["N_merged_ICC_handled"]]
plot_list[["N_cor_ICC"]]
plot_list[["N_valid_ICC.z"]]
plot_list[["N_cor_ICC.z"]]
plot_list[["N_rel"]]




# '''' Combine Plots ------------------------------------------------------

# '''''' for raw ICCs -----------------------------------------------------

# adjust the plots a little (e.g., no y-axis lable but title, no x-axis label)
a <- plot_list[["cor"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                axis.text.y = element_text(hjust=1),
                                axis.text = element_text(size=10)) + ggtitle("(A) Correlation with Benchmark") +
  geom_hline(yintercept=0.80, linetype="twodash", color = "black") + force_panelsizes(rows=1, cols = c(1,1))
# a
b <- plot_list[["diff"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                 plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                 axis.text.y = element_text(hjust=1),
                                 axis.text = element_text(size=10)) + ggtitle("(B) Difference from Benchmark") + force_panelsizes(rows=1, cols = c(1,1))
# b
c <- ggplot() + theme_void() + force_panelsizes(rows=1, cols = c(1,1)) # empty panel for RMSE for ordered draws

# c

# get benchmark SD of ICCs for raw ICCs
# for NED:
bench_ned_ICC_sd <- od[["sd_ICC"]][od[["sd_ICC"]]$diff_type == "NED" &
                                     od[["sd_ICC"]]$n_items == 9 &
                                     od[["sd_ICC"]]$n_occasions == 70, ]$sd_ICC_mean

bench_ped_ICC_sd <- od[["sd_ICC"]][od[["sd_ICC"]]$diff_type == "PED" &
                                     od[["sd_ICC"]]$n_items == 5 &
                                     od[["sd_ICC"]]$n_occasions == 70, ]$sd_ICC_mean

bench_ICC_sd <- data.frame(
  diff_type = c("NED", "PED"),
  benchmark_sd = c(bench_ned_ICC_sd, bench_ped_ICC_sd)
)


d <- plot_list[["sd"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                               plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                               axis.text.y = element_text(hjust=1),
                               axis.text = element_text(size=10)) + ggtitle("(D) SD of ICCs") +
  geom_hline(data=bench_ICC_sd, aes(yintercept=benchmark_sd), # use benchmark SD
             linetype="twodash", color = "black")+ force_panelsizes(rows=1, cols = c(1,1))

# d
e <- plot_list[["rel"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                axis.text.y = element_text(hjust=1),
                                axis.text = element_text(size=10)) + ggtitle("(E) Reliability of ICCs") +
  geom_hline(yintercept=0.80, linetype="twodash", color = "black")+ force_panelsizes(rows=1, cols = c(1,1))
# e
f <- plot_list[["percnegICC"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                       plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                       axis.text.y = element_text(hjust=1),
                                       axis.text = element_text(size=10)) + ggtitle("(F) Proportion of Negative ICCs")+ force_panelsizes(rows=1, cols = c(1,1))


g <- plot_list[["estimProbNeg"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                         plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                         axis.text.y = element_text(hjust=1),
                                         axis.text = element_text(size=10)) + ggtitle("(G) Estimation Problems (LB)") +
  force_panelsizes(rows=1, cols = c(1,1))

# g

h <- plot_list[["estimProbPos"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                         plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                         axis.text.y = element_text(hjust=1),
                                         axis.text = element_text(size=10)) + ggtitle("(H) Estimation Problems (UB)") +
  force_panelsizes(rows=1, cols = c(1,1))

# h


combined <- ggpubr::ggarrange(a,b,c,d,e,f,g,h , ncol=3, nrow=3, common.legend = TRUE, legend="top",
                              align = "hv", widths = c(1,1,1), heights = c(1,1,1)) # equal panel sizes


combined <- annotate_figure(combined,
                            bottom = text_grob("Number of Measurement Occasions", size = 12))

combined


ggsave("plots/02_revision_1/for publication/neg ICC handling/EMOTIONS_plots_exclude_ordered_draws_NED_PED_combined.pdf",plot = combined, device="pdf", height = 222, width = 222, unit="mm")

ggsave("plots/02_revision_1/for publication/neg ICC handling/EMOTIONS_plots_exclude_ordered_draws_NED_PED_combined.svg",plot = combined, device="svg", height = 222, width = 222, unit="mm")

# as tiff
tiff("plots/02_revision_1/for publication/neg ICC handling/EMOTIONS_plots_exclude_ordered_draws_NED_PED_combined.tiff", units="mm", width=222, height=222, res=1200)
combined
dev.off()



# '''''' for ICC.z --------------------------------------------------------
# adjust the plots a little (e.g., no y-axis lable but title, no x-axis label)
a <- plot_list[["cor.z"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                  plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                  axis.text = element_text(size=10)) + ggtitle("(A) Correlation with Benchmark") +
  geom_hline(yintercept=0.80, linetype="twodash", color = "black")
# a
b <- plot_list[["diff.z"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                   plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                   axis.text = element_text(size=10)) + ggtitle("(B) Difference from Benchmark")
# b
c <- ggplot() + theme_void() +force_panelsizes(rows=1, cols = c(1,1)) # empty panel for RMSE for ordered draws
# c


# get benchmark SD of ICCs for raw ICCs
# for NED:
bench_ned_ICC.z_sd <- od[["sd_ICC.z"]][od[["sd_ICC.z"]]$diff_type == "NED" &
                                         od[["sd_ICC.z"]]$n_items == 9 &
                                         od[["sd_ICC.z"]]$n_occasions == 70, ]$sd_ICC.z_mean

bench_ped_ICC.z_sd <- od[["sd_ICC.z"]][od[["sd_ICC.z"]]$diff_type == "PED" &
                                         od[["sd_ICC.z"]]$n_items == 5 &
                                         od[["sd_ICC.z"]]$n_occasions == 70, ]$sd_ICC.z_mean

bench_ICC.z_sd <- data.frame(
  diff_type = c("NED", "PED"),
  benchmark_sd.z = c(bench_ned_ICC.z_sd, bench_ped_ICC.z_sd)
)


d <- plot_list[["sd.z"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                 plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                 axis.text = element_text(size=10)) + ggtitle("(D) SD of ICCs") +
  geom_hline(data=bench_ICC.z_sd, aes(yintercept=benchmark_sd.z), # use benchmark SD
             linetype="twodash", color = "black")
# d

e <- plot_list[["rel"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                axis.text = element_text(size=10)) + ggtitle("(E) Reliability of ICCs")  +
  geom_hline(yintercept=0.80, linetype="twodash", color = "black")
# e
f <- plot_list[["percnegICC"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                       plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                       axis.text = element_text(size=10)) + ggtitle("(F) Proportion of Negative ICCs")

g <- plot_list[["estimProbNeg"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                         plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                         axis.text.y = element_text(hjust=1),
                                         axis.text = element_text(size=10)) + ggtitle("(G) Estimation Problems (LB)") +
  force_panelsizes(rows=1, cols = c(1,1))

# g

h <- plot_list[["estimProbPos"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                         plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                         axis.text.y = element_text(hjust=1),
                                         axis.text = element_text(size=10)) + ggtitle("(H) Estimation Problems (UB)") +
  force_panelsizes(rows=1, cols = c(1,1))

# h



combined <- ggpubr::ggarrange(a,b,c,d,e,f,g,h , ncol=3, nrow=3, common.legend = TRUE, legend="top",
                              align = "hv", widths = c(1,1,1), heights = c(1,1,1)) # equal panel sizes


combined <- annotate_figure(combined,
                            bottom = text_grob("Number of Measurement Occasions", size = 12))

combined



ggsave("plots/02_revision_1/for publication/neg ICC handling/EMOTIONS_plots_exclude_ordered_draws_NED_PED_Z-transformed_combined.pdf",plot = combined, device="pdf", height = 222, width = 222, unit="mm")

ggsave("plots/02_revision_1/for publication/neg ICC handling/EMOTIONS_plots_exclude_ordered_draws_NED_PED_Z-transformed_combined.svg",plot = combined, device="svg", height = 222, width = 222, unit="mm")

# as tiff
tiff("plots/02_revision_1/for publication/neg ICC handling/EMOTIONS_plots_exclude_ordered_draws_NED_PED_Z-transformed_combined.tiff", units="mm", width=222, height=222, res=1200)
combined
dev.off()



rm(list=ls())

