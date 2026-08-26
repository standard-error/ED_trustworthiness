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


# emolive Data PED + NED --------------------------------------------------
load("results/02_revision_1/emolive study/NED/neg ICC handling/exclude/processed/aggregated_results_NED_emolive_exclude.rda")
agg_ned <- agg
rm(agg)

load("results/02_revision_1/emolive study/PED/neg ICC handling/exclude/processed/aggregated_results_PED_emolive_exclude.rda")
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


# now add a new n_items grouping variable
# -> 4 (PED) and 5 (NED) items shall be plotted using the same visualization (ltype, shape, ...)
add_new_item_group <- function(df) {
  
  df$n_items_original <- df$n_items
  
  df$n_items[ # if NED and n_items == 5 or PED and n_items == 4, use label "4 (PED) or 5 (NED)"
    (df$diff_type == "NED" & df$n_items_original == 5) |
      (df$diff_type == "PED" & df$n_items_original == 4) ] <- "4 (PED) or 5 (NED)"
  
  
  df$n_items[ # if NED and n_items == 10 or PED and n_items == 8, use label "8 (PED) or 10 (NED)
    (df$diff_type == "NED" & df$n_items_original == 10) |
      (df$diff_type == "PED" & df$n_items_original == 8) ] <- "8 (PED) or 10 (NED)"
  
  
  df$n_items[ # if NED and n_items == 15 or PED and n_items == 12, use label "12 (PED) or 15 (NED)
    (df$diff_type == "NED" & df$n_items_original == 15) |
      (df$diff_type == "PED" & df$n_items_original == 12) ] <- "12 (PED) or 15 (NED)"
  
  df$n_items <- factor(
    df$n_items,
    levels = c(
      "4 (PED) or 5 (NED)",
      "8 (PED) or 10 (NED)",
      "12 (PED) or 15 (NED)"
    )
  )
  
  df
}


for(out in names(both)) {
  both[[out]][["agg_res"]] <- add_new_item_group(both[[out]][["agg_res"]])
}



# for each outcome, split into two data frames, one for the main manuscript (random draws)
# and one for the supplement (ordered draws)
rd <- list() # random draws
od <- list() # ordered draws

for (out in names(both)) {
  rd[[out]] <- both[[out]][["agg_res"]][both[[out]][["agg_res"]]$occasions_drawn == "random", ]
  od[[out]] <- both[[out]][["agg_res"]][both[[out]][["agg_res"]]$occasions_drawn == "by order", ]
}





# '' Random Draw Figures --------------------------------------------------

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


# number of participants varies due to exclusion of negative ICCs



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
                  percnegICC = rd[["percnegICC_raw"]])

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
                "Proportion of Negative ICCs") 
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
# min(data_list[["percnegICC"]][["percnegICC_raw_min"]])
# max(data_list[["percnegICC"]][["percnegICC_raw_max"]])


# for correlation, reliability, proportions, ... -> use theoretical range (e.g., difference max and min: 1 and -1)
ylim_list <- list(
  c(0, 1), # correlation with benchmark, theoretical range
  c(0, 1), # correlation with benchmark (ICC.z), theoretical range
  c(-1, 1), # difference in ICCs (compared to benchmark), theoretical range
  c(-1.4, 0.2), # difference in ICCs (compared to benchmark) for ICC.z
  c(0, 0.6), # RMSE, theoretical lower bound
  c(0, 1.5), # RMSE (ICC.z)
  c(0, 0.3), # SD of ICCs, theoretical lower bound
  c(0, 0.5), # SD of ICC.z, theoretical lower bound
  c(0, 1), # Reliability
  c(0, 1) # proportion of negative ICCs
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

# plot_list[["cor"]]
# plot_list[["cor.z"]]
# plot_list[["diff"]]
# plot_list[["diff.z"]]
# plot_list[["rmse"]]
# plot_list[["rmse.z"]]
# plot_list[["sd"]]
# plot_list[["sd.z"]]
# plot_list[["rel"]]
# plot_list[["percnegICC"]]




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
                                     od[["sd_ICC"]]$n_items_original == 15 &
                                     od[["sd_ICC"]]$n_occasions == 70, ]$sd_ICC_mean

bench_ped_ICC_sd <- od[["sd_ICC"]][od[["sd_ICC"]]$diff_type == "PED" &
                                     od[["sd_ICC"]]$n_items_original == 12 &
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


combined <- ggpubr::ggarrange(a,b,c,d,e,f , ncol=3, nrow=2, common.legend = TRUE, legend="top",
                              align = "hv", widths = c(1,1,1), heights = c(1, 1)) # equal panel sizes


combined <- annotate_figure(combined,
                            bottom = text_grob("Number of Measurement Occasions", size = 12))

combined


ggsave("plots/02_revision_1/for publication/neg ICC handling/emolive_plots_exclude_random_draws_NED_PED_combined.pdf",plot = combined, device="pdf", height = 148, width = 220, unit="mm")

ggsave("plots/02_revision_1/for publication/neg ICC handling/emolive_plots_exclude_random_draws_NED_PED_combined.svg",plot = combined, device="svg", height = 148, width = 220, unit="mm")

# as tiff
tiff("plots/02_revision_1/for publication/neg ICC handling/emolive_plots_exclude_random_draws_NED_PED_combined.tiff", units="mm", width=220, height=148, res=1200)
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
                                         od[["sd_ICC.z"]]$n_items_original == 15 &
                                         od[["sd_ICC.z"]]$n_occasions == 70, ]$sd_ICC.z_mean

bench_ped_ICC.z_sd <- od[["sd_ICC.z"]][od[["sd_ICC.z"]]$diff_type == "PED" &
                                         od[["sd_ICC.z"]]$n_items_original == 12 &
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



combined <- ggpubr::ggarrange(a,b,c,d,e,f , ncol=3, nrow=2, common.legend = TRUE, legend="top",
                              align = "hv", widths = c(1,1,1), heights = c(1, 1)) # equal panel sizes


combined <- annotate_figure(combined,
                            bottom = text_grob("Number of Measurement Occasions", size = 12))

combined



ggsave("plots/02_revision_1/for publication/neg ICC handling/emolive_plots_exclude_random_draws_NED_PED_Z-transformed_combined.pdf",plot = combined, device="pdf", height = 148, width = 220, unit="mm")

ggsave("plots/02_revision_1/for publication/neg ICC handling/emolive_plots_exclude_random_draws_NED_PED_Z-transformed_combined.svg",plot = combined, device="svg", height = 148, width = 220, unit="mm")

# as tiff
tiff("plots/02_revision_1/for publication/neg ICC handling/emolive_plots_exclude_random_draws_NED_PED_Z-transformed_combined.tiff", units="mm", width=220, height=148, res=1200)
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


# number of participants differs due to exclusion of negative ICCs


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
                  percnegICC = od[["percnegICC_raw"]])

# define the y label for each outcome plot
ylabels <- list("Correlation with Benchmark",
                "Correlation with Benchmark (ICC.z)",
                "Difference in ICCs to Benchmark",
                "Difference in ICCs to Benchmark (ICC.z)",
                "SD of ICCs",
                "SD of ICC.z",
                "Reliability of ICCs",
                "Proportion of Negative ICCs") 
names(ylabels) <- names(data_list)

# Check minimum and maximum to define y-axis limits for each outcome
# or use theoretical range, respectively
min(data_list[["cor"]][["cor_ICC_min"]])
max(data_list[["cor"]][["cor_ICC_max"]])
min(data_list[["cor.z"]][["cor_ICC.z_min"]])
max(data_list[["cor.z"]][["cor_ICC.z_max"]])
min(data_list[["diff"]][["difference_min"]])
max(data_list[["diff"]][["difference_max"]])
min(data_list[["diff.z"]][["difference.z_min"]])
max(data_list[["diff.z"]][["difference.z_max"]])
min(data_list[["sd"]][["sd_ICC_min"]])
max(data_list[["sd"]][["sd_ICC_max"]])
min(data_list[["sd.z"]][["sd_ICC.z_min"]])
max(data_list[["sd.z"]][["sd_ICC.z_max"]])
min(data_list[["rel"]][["rel_min"]])
max(data_list[["rel"]][["rel_max"]])
min(data_list[["percnegICC"]][["percnegICC_raw_min"]])
max(data_list[["percnegICC"]][["percnegICC_raw_max"]])


# for correlation, reliability, proportions, ... -> use theoretical range (e.g., difference max and min: 1 and -1)
ylim_list <- list(
  c(0, 1), # correlation with benchmark, theoretical range
  c(0, 1), # correlation with benchmark (ICC.z), theoretical range
  c(-1, 1), # difference in ICCs (compared to benchmark), theoretical range
  c(-1.5, 0.8), # difference in ICCs (compared to benchmark) for ICC.z
  c(0, 0.3), # SD of ICCs, theoretical lower bound
  c(0, 0.5), # SD of ICC.z, theoretical lower bound
  c(0, 1), # Reliability
  c(0, 1) # proportion of negative ICCs
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
# 
# plot_list[["cor"]]
# plot_list[["cor.z"]]
# plot_list[["diff"]]
# plot_list[["diff.z"]]
# plot_list[["sd"]]
# plot_list[["sd.z"]]
# plot_list[["rel"]]
# plot_list[["percnegICC"]]

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
                                     od[["sd_ICC"]]$n_items_original == 15 &
                                     od[["sd_ICC"]]$n_occasions == 70, ]$sd_ICC_mean

bench_ped_ICC_sd <- od[["sd_ICC"]][od[["sd_ICC"]]$diff_type == "PED" &
                                     od[["sd_ICC"]]$n_items_original == 12 &
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


combined <- ggpubr::ggarrange(a,b,c,d,e,f , ncol=3, nrow=2, common.legend = TRUE, legend="top",
                              align = "hv", widths = c(1,1,1), heights = c(1, 1)) # equal panel sizes


combined <- annotate_figure(combined,
                            bottom = text_grob("Number of Measurement Occasions", size = 12))

combined


ggsave("plots/02_revision_1/for publication/neg ICC handling/emolive_plots_exclude_ordered_draws_NED_PED_combined.pdf",plot = combined, device="pdf", height = 148, width = 220, unit="mm")

ggsave("plots/02_revision_1/for publication/neg ICC handling/emolive_plots_exclude_ordered_draws_NED_PED_combined.svg",plot = combined, device="svg", height = 148, width = 220, unit="mm")

# as tiff
tiff("plots/02_revision_1/for publication/neg ICC handling/emolive_plots_exclude_ordered_draws_NED_PED_combined.tiff", units="mm", width=220, height=148, res=1200)
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
                                         od[["sd_ICC.z"]]$n_items_original == 15 &
                                         od[["sd_ICC.z"]]$n_occasions == 70, ]$sd_ICC.z_mean

bench_ped_ICC.z_sd <- od[["sd_ICC.z"]][od[["sd_ICC.z"]]$diff_type == "PED" &
                                         od[["sd_ICC.z"]]$n_items_original == 12 &
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



combined <- ggpubr::ggarrange(a,b,c,d,e,f , ncol=3, nrow=2, common.legend = TRUE, legend="top",
                              align = "hv", widths = c(1,1,1), heights = c(1, 1)) # equal panel sizes


combined <- annotate_figure(combined,
                            bottom = text_grob("Number of Measurement Occasions", size = 12))

combined



ggsave("plots/02_revision_1/for publication/neg ICC handling/emolive_plots_exclude_ordered_draws_NED_PED_Z-transformed_combined.pdf",plot = combined, device="pdf", height = 148, width = 220, unit="mm")

ggsave("plots/02_revision_1/for publication/neg ICC handling/emolive_plots_exclude_ordered_draws_NED_PED_Z-transformed_combined.svg",plot = combined, device="svg", height = 148, width = 220, unit="mm")

# as tiff
tiff("plots/02_revision_1/for publication/neg ICC handling/emolive_plots_exclude_ordered_draws_NED_PED_Z-transformed_combined.tiff", units="mm", width=220, height=148, res=1200)
combined
dev.off()



rm(list=ls())


# Session Info ------------------------------------------------------------
# R version 4.5.3 (2026-03-11 ucrt)
# Platform: x86_64-w64-mingw32/x64
# Running under: Windows 11 x64 (build 26200)
# 
# Matrix products: default
#   LAPACK version 3.12.1
# 
# locale:
# [1] LC_COLLATE=German_Germany.utf8  LC_CTYPE=German_Germany.utf8   
# [3] LC_MONETARY=German_Germany.utf8 LC_NUMERIC=C                   
# [5] LC_TIME=German_Germany.utf8    
# 
# time zone: Europe/Berlin
# tzcode source: internal
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#  [1] lubridate_1.9.4   forcats_1.0.1     stringr_1.6.0     dplyr_1.1.4      
#  [5] purrr_1.1.0       readr_2.1.5       tidyr_1.3.1       tibble_3.3.0     
#  [9] tidyverse_2.0.0   ggpubr_0.6.3      ggh4x_0.3.1       scales_1.4.0     
# [13] viridis_0.6.5     viridisLite_0.4.3 ggplot2_4.0.2    
# 
# loaded via a namespace (and not attached):
#  [1] gtable_0.3.6       compiler_4.5.3     ggsignif_0.6.4     tidyselect_1.2.1  
#  [5] gridExtra_2.3      R6_2.6.1           generics_0.1.4     Formula_1.2-5     
#  [9] backports_1.5.1    car_3.1-5          tzdb_0.5.0         pillar_1.11.1     
# [13] RColorBrewer_1.1-3 rlang_1.2.0        stringi_1.8.7      broom_1.0.12      
# [17] S7_0.2.0           timechange_0.3.0   cli_3.6.5          withr_3.0.2       
# [21] magrittr_2.0.3     grid_4.5.3         rstudioapi_0.18.0  hms_1.1.4         
# [25] cowplot_1.2.0      lifecycle_1.0.5    vctrs_0.6.5        rstatix_0.7.3     
# [29] glue_1.8.0         farver_2.1.2       abind_1.4-8        carData_3.0-6     
# [33] tools_4.5.3        pkgconfig_2.0.3  

