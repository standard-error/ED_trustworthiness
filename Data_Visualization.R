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



##### STUDY 1: OVERALL DATA SET #####

# Load Aggregated Results Data --------------------------------------------
load("results/aggregated_whole_data_set_Study1.rda")


# Extract Data ------------------------------------------------------------

## CORRELATION
cor <- agg[["cor_ICC"]][["agg_res"]]
cor.z <- agg[["cor_ICC.z"]][["agg_res"]]


## DIFFERENCE IN ICC
# for difference in ICC, do NOT report minimum of MEAN difference across replications
# but the minimum difference on person level -> i.e., minimum of minimum difference, mean of
# mean difference, and maximum of maximum difference
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


# for ICC.z
# read data
sub1 <- agg[["min_diff_ICC.z"]][["agg_res"]]
sub2 <- agg[["mean_diff_ICC.z"]][["agg_res"]]  
sub3 <- agg[["max_diff_ICC.z"]][["agg_res"]]

# subset
sub1 <- sub1[ , c("occasions_drawn", "n_occasions", "n_items", "min_diff_ICC.z_min")]
sub2 <- sub2[ , c("occasions_drawn", "n_occasions", "n_items", "mean_diff_ICC.z_mean")]
sub3 <- sub3[ , c("occasions_drawn", "n_occasions", "n_items", "max_diff_ICC.z_max")]

diff.z <- merge(sub1, sub2, by = c("occasions_drawn", "n_occasions", "n_items"))
diff.z <- merge(diff.z, sub3, by = c("occasions_drawn", "n_occasions", "n_items"))

rm(sub1,sub2,sub3)



## RMSE
rmse <- agg[["RMSE_ICC"]][["agg_res"]]
rmse.z <- agg[["RMSE_ICC.z"]][["agg_res"]]

## SD
sd <- agg[["sd_ICC"]][["agg_res"]]
sd.z <- agg[["sd_ICC.z"]][["agg_res"]]

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
                  cor.z = cor.z,
                  diff = diff,
                  diff.z = diff.z,
                  rmse = rmse,
                  rmse.z = rmse.z,
                  sd = sd,
                  sd.z = sd.z,
                  rel = rel,
                  nnegICC = nnegICC,
                  estimProbNeg = estimProbNeg,
                  estimProbPos = estimProbPos,
                  N_valid_ICC.z = N_valid_ICC.z,
                  N_rel = N_rel)

# define the y label for each outcome plot
ylabels <- list("Correlation with Benchmark",
                "Correlation with Benchmark (ICC.z)",
             "Difference in ICCs to Benchmkark",
             "Difference in ICCs to Benchmkark (ICC.z)",
             "RMSE",
             "RMSE (ICC.z)",
             "SD of ICCs",
             "SD of ICC.z",
             "Reliability of ICCs",
             "Number of Negative ICCs",
             "Number of Estimation Problems (Negative)",
             "Number of Estimation Problems (Positive)",
             "Number of Valid ICC.z",
             "Number of ICCs for Reliability") 
names(ylabels) <- names(data_list)

# Check minimum and maximum for y limits of each plot and define
# min(cor$cor_ICC_min)
# max(cor$cor_ICC_max)
# min(cor.z$cor_ICC.z_min)
# max(cor.z$cor_ICC.z_max)
# min(diff$min_diff_ICC_min)
# max(diff$max_diff_ICC_max)
# min(diff.z$min_diff_ICC.z_min)
# max(diff.z$max_diff_ICC.z_max)
# min(rmse$RMSE_ICC_min)
# max(rmse$RMSE_ICC_max)
# min(rmse.z$RMSE_ICC.z_min)
# max(rmse.z$RMSE_ICC.z_max)
# min(sd$sd_ICC_min)
# max(sd$sd_ICC_max)
# min(sd.z$sd_ICC.z_min)
# max(sd.z$sd_ICC.z_max)
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
  c(-0.35, 1), # correlation with benchmark
  c(-0.35, 1), # correlation with benchmark (ICC.z)
  c(-0.7, 0.85), # difference in ICCs (compared to benchmark)
  c(-5.5, 3.5), # difference in ICCs (compared to benchmark) for ICC.z
  c(0, 0.35), # RMSE
  c(0, 1.5), # RMSE (ICC.z)
  c(0, 0.32), # SD of ICCs
  c(0, 1.2), # SD of ICC.z
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
               x_breaks = seq(0, 100, 20), theme_custom = my_theme,
               groupwise = FALSE)
})

names(plot_list) <- names(data_list)





# Look At Plots -----------------------------------------------------------

plot_list[["cor"]]
plot_list[["cor.z"]]
plot_list[["diff"]]
plot_list[["diff.z"]]
plot_list[["rmse"]]
plot_list[["rmse.z"]]
plot_list[["sd"]]
plot_list[["sd.z"]]
plot_list[["rel"]]
plot_list[["nnegICC"]]
plot_list[["estimProbNeg"]]
plot_list[["estimProbPos"]]
plot_list[["N_valid_ICC.z"]]
plot_list[["N_rel"]]



# Save Single Plots -------------------------------------------------------
ggsave("plots/Study 1/overall data set/single plots/correlation.pdf",plot = plot_list[["cor"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/overall data set/single plots/correlation.z.pdf",plot = plot_list[["cor.z"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/overall data set/single plots/difference.pdf",plot = plot_list[["diff"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/overall data set/single plots/difference.z.pdf",plot = plot_list[["diff.z"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/overall data set/single plots/rmse.pdf",plot = plot_list[["rmse"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/overall data set/single plots/rmse.z.pdf",plot = plot_list[["rmse.z"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/overall data set/single plots/sd.pdf",plot = plot_list[["sd"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/overall data set/single plots/sd.z.pdf",plot = plot_list[["sd.z"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/overall data set/single plots/reliability.pdf",plot = plot_list[["rel"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/overall data set/single plots/NnegICC.pdf",plot = plot_list[["nnegICC"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/overall data set/single plots/EstimProbNeg.pdf",plot = plot_list[["estimProbNeg"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/overall data set/single plots/EstimProbPos.pdf",plot = plot_list[["estimProbPos"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/overall data set/single plots/N_ValidICC.z.pdf",plot = plot_list[["N_valid_ICC.z"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/overall data set/single plots/N_rel.pdf",plot = plot_list[["N_Rel"]], device="pdf", height = 148, width = 210, unit="mm")


# as svg (for presentations)

ggsave("plots/Study 1/overall data set/single plots/correlation.svg",plot = plot_list[["cor"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/overall data set/single plots/correlation.z.svg",plot = plot_list[["cor.z"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/overall data set/single plots/difference.svg",plot = plot_list[["diff"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/overall data set/single plots/difference.z.svg",plot = plot_list[["diff.z"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/overall data set/single plots/rmse.svg",plot = plot_list[["rmse"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/overall data set/single plots/rmse.z.svg",plot = plot_list[["rmse.z"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/overall data set/single plots/sd.svg",plot = plot_list[["sd"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/overall data set/single plots/sd.z.svg",plot = plot_list[["sd.z"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/overall data set/single plots/reliability.svg",plot = plot_list[["rel"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/overall data set/single plots/NnegICC.svg",plot = plot_list[["nnegICC"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/overall data set/single plots/EstimProbNeg.svg",plot = plot_list[["estimProbNeg"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/overall data set/single plots/EstimProbPos.svg",plot = plot_list[["estimProbPos"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/overall data set/single plots/N_ValidICC.z.svg",plot = plot_list[["N_valid_ICC.z"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/overall data set/single plots/N_rel.svg",plot = plot_list[["N_Rel"]], device="svg", height = 148, width = 210, unit="mm")





# Combine Plots -----------------------------------------------------------
# do not use estimation problems as there were none
# number of valid ICC.z and number of participants for reliability can be 
# reported easily -> do not plot


# for raw ICCs

# adjust the plots a little (e.g., no y-axis lable but title, no x-axis label)
a <- plot_list[["cor"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                axis.text = element_text(size=10)) + ggtitle("(A) Correlation with Benchmark")
# a
b <- plot_list[["diff"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                 plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                 axis.text = element_text(size=10)) + ggtitle("(B) Difference to Benchmark")
# b
c <- plot_list[["rmse"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                 plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                 axis.text = element_text(size=10)) + ggtitle("(C) RMSE")
# c
d <- plot_list[["sd"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                               plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                               axis.text = element_text(size=10)) + ggtitle("(D) SD of ICCs")
# d
e <- plot_list[["rel"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                axis.text = element_text(size=10)) + ggtitle("(E) Reliability of ICCs")
# e
f <- plot_list[["nnegICC"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                    plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                    axis.text = element_text(size=10)) + ggtitle("(F) Number of Negative ICCs")



combined <- ggpubr::ggarrange(a,b,c,d,e,f , ncol=3, nrow=2, common.legend = TRUE, legend="top",
                              align = "hv", widths = c(1,1,1), heights = c(1, 1)) # equal panel sizes


combined <- annotate_figure(combined,
                            bottom = text_grob("Number of Occasions", size = 12))

combined



ggsave("plots/Study 1/overall data set/plots_whole_data_set_Study1.pdf",plot = combined, device="pdf", height = 148, width = 210, unit="mm")
# save in DIN A5 format

ggsave("plots/Study 1/overall data set/plots_whole_data_set_Study1.svg",plot = combined, device="svg", height = 148, width = 210, unit="mm")
# save in DIN A5 format




# for ICC.z

# adjust the plots a little (e.g., no y-axis lable but title, no x-axis label)
a <- plot_list[["cor.z"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                axis.text = element_text(size=10)) + ggtitle("(A) Correlation with Benchmark")
# a
b <- plot_list[["diff.z"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                 plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                 axis.text = element_text(size=10)) + ggtitle("(B) Difference to Benchmark")
# b
c <- plot_list[["rmse.z"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                 plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                 axis.text = element_text(size=10)) + ggtitle("(C) RMSE")
# c
d <- plot_list[["sd.z"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                               plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                               axis.text = element_text(size=10)) + ggtitle("(D) SD of ICCs")
# d
e <- plot_list[["rel"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                axis.text = element_text(size=10)) + ggtitle("(E) Reliability of ICCs")
# e
f <- plot_list[["nnegICC"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                    plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                    axis.text = element_text(size=10)) + ggtitle("(F) Number of Negative ICCs")



combined <- ggpubr::ggarrange(a,b,c,d,e,f , ncol=3, nrow=2, common.legend = TRUE, legend="top",
                              align = "hv", widths = c(1,1,1), heights = c(1, 1)) # equal panel sizes


combined <- annotate_figure(combined,
                            bottom = text_grob("Number of Occasions", size = 12))

combined



ggsave("plots/Study 1/overall data set/plots_Z-transformed_whole_data_set_Study1.pdf",plot = combined, device="pdf", height = 148, width = 210, unit="mm")
# save in DIN A5 format


ggsave("plots/Study 1/overall data set/plots_Z-transformed_whole_data_set_Study1.svg",plot = combined, device="svg", height = 148, width = 210, unit="mm")
# save in DIN A5 format


# Build .csv Table --------------------------------------------------------

# for raw ICC
# bind all results
all_agg_results <- merge(cor, diff, by = c("occasions_drawn", "n_occasions", "n_items"))
all_agg_results <- merge(all_agg_results, rmse, by = c("occasions_drawn", "n_occasions", "n_items"))
all_agg_results <- merge(all_agg_results, sd, by = c("occasions_drawn", "n_occasions", "n_items"))
all_agg_results <- merge(all_agg_results, rel, by = c("occasions_drawn", "n_occasions", "n_items"))
all_agg_results <- merge(all_agg_results, nnegICC, by = c("occasions_drawn", "n_occasions", "n_items"))
all_agg_results <- merge(all_agg_results, estimProbNeg, by = c("occasions_drawn", "n_occasions", "n_items"))
all_agg_results <- merge(all_agg_results, estimProbPos, by = c("occasions_drawn", "n_occasions", "n_items"))
all_agg_results <- merge(all_agg_results, N_rel, by = c("occasions_drawn", "n_occasions", "n_items"))
all_agg_results <- merge(all_agg_results, N_valid_ICC.z, by = c("occasions_drawn", "n_occasions", "n_items"))
# round to 3 decimals
all_agg_results[4:33] <- round(all_agg_results[4:33], 3) 
# sort
all_agg_results <- all_agg_results[order(all_agg_results$occasions_drawn, all_agg_results$n_occasions, all_agg_results$n_items), ]
# reset row names
rownames(all_agg_results) <- NULL
# save
write.csv(all_agg_results, "results/results_table_whole_data_set_Study1.csv", row.names = F)

# round to 2 decimals and save again
all_agg_results <- round(all_agg_results[4:33], 2)
write.csv(all_agg_results, "results/results_table_whole_data_set_Study1_two_decimals.csv", row.names = F)

# for ICC.z
# bind all results
all_agg_results <- merge(cor.z, diff.z, by = c("occasions_drawn", "n_occasions", "n_items"))
all_agg_results <- merge(all_agg_results, rmse.z, by = c("occasions_drawn", "n_occasions", "n_items"))
all_agg_results <- merge(all_agg_results, sd.z, by = c("occasions_drawn", "n_occasions", "n_items"))
all_agg_results <- merge(all_agg_results, rel, by = c("occasions_drawn", "n_occasions", "n_items"))
all_agg_results <- merge(all_agg_results, nnegICC, by = c("occasions_drawn", "n_occasions", "n_items"))
all_agg_results <- merge(all_agg_results, estimProbNeg, by = c("occasions_drawn", "n_occasions", "n_items"))
all_agg_results <- merge(all_agg_results, estimProbPos, by = c("occasions_drawn", "n_occasions", "n_items"))
all_agg_results <- merge(all_agg_results, N_rel, by = c("occasions_drawn", "n_occasions", "n_items"))
all_agg_results <- merge(all_agg_results, N_valid_ICC.z, by = c("occasions_drawn", "n_occasions", "n_items"))
# round to 3 decimals
all_agg_results[4:33] <- round(all_agg_results[4:33], 3) 
# sort
all_agg_results <- all_agg_results[order(all_agg_results$occasions_drawn, all_agg_results$n_occasions, all_agg_results$n_items), ]
# reset row names
rownames(all_agg_results) <- NULL
# save
write.csv(all_agg_results, "results/results_table_Z-transformed_whole_data_set_Study1.csv", row.names = F)





##### STUDY 1: GROUPWISE #####

rm(list=ls())
source("functions/function_plot_outcomes.R")


# Load Aggregated Results Data --------------------------------------------
load("results/aggregated_subgroups_Study1.rda")


# Extract Data ------------------------------------------------------------

## CORRELATION
cor <- agg_grp[["cor_ICC"]][["agg_res"]]
cor.z <- agg_grp[["cor_ICC.z"]][["agg_res"]]

## DIFFERENCE IN ICC
# for difference in ICC, do NOT report minimum of MEAN difference across replications
# but the minimum difference on person level -> i.e., minimum of minimum difference, mean of
# mean difference, and maximum of maximum difference
# -> merge relevant data

# for raw ICC
# read data
sub1 <- agg_grp[["min_diff_ICC"]][["agg_res"]]
sub2 <- agg_grp[["mean_diff_ICC"]][["agg_res"]]  
sub3 <- agg_grp[["max_diff_ICC"]][["agg_res"]]

# subset
sub1 <- sub1[ , c("occasions_drawn", "n_occasions", "n_items", "group", "min_diff_ICC_min")]
sub2 <- sub2[ , c("occasions_drawn", "n_occasions", "n_items", "group", "mean_diff_ICC_mean")]
sub3 <- sub3[ , c("occasions_drawn", "n_occasions", "n_items", "group", "max_diff_ICC_max")]

diff <- merge(sub1, sub2, by = c("occasions_drawn", "n_occasions", "n_items", "group"))
diff <- merge(diff, sub3, by = c("occasions_drawn", "n_occasions", "n_items", "group"))

rm(sub1,sub2,sub3)

# for ICC.z
# read data
sub1 <- agg_grp[["min_diff_ICC.z"]][["agg_res"]]
sub2 <- agg_grp[["mean_diff_ICC.z"]][["agg_res"]]  
sub3 <- agg_grp[["max_diff_ICC.z"]][["agg_res"]]

# subset
sub1 <- sub1[ , c("occasions_drawn", "n_occasions", "n_items", "group", "min_diff_ICC.z_min")]
sub2 <- sub2[ , c("occasions_drawn", "n_occasions", "n_items", "group", "mean_diff_ICC.z_mean")]
sub3 <- sub3[ , c("occasions_drawn", "n_occasions", "n_items", "group", "max_diff_ICC.z_max")]

diff.z <- merge(sub1, sub2, by = c("occasions_drawn", "n_occasions", "n_items", "group"))
diff.z <- merge(diff.z, sub3, by = c("occasions_drawn", "n_occasions", "n_items", "group"))

rm(sub1,sub2,sub3)

## RMSE
rmse <- agg_grp[["RMSE_ICC"]][["agg_res"]]
rmse.z <- agg_grp[["RMSE_ICC.z"]][["agg_res"]]

## SD
sd <- agg_grp[["sd_ICC"]][["agg_res"]]
sd.z <- agg_grp[["sd_ICC.z"]][["agg_res"]]

## RELIABILITY
rel <- agg_grp[["rel"]][["agg_res"]]

## NUMBER OF NEGATIVE ICCS
nnegICC <- agg_grp[["negICC"]][["agg_res"]]

## ESTIMATION PROBLEMS
estimProbNeg <- agg_grp[["estimationProbNeg"]][["agg_res"]]
estimProbPos <- agg_grp[["estimationProbPos"]][["agg_res"]]


## VALID VALUES
N_valid_ICC.z <- agg_grp[["N_valid_ICC.z"]][["agg_res"]]
N_rel <- agg_grp[["N_rel"]][["agg_res"]]





# Plot Outcomes -----------------------------------------------------------
# convert data frames to list so that lapply can be used to apply function
# automatically to all outcomes

data_list <- list(cor = cor,
                  cor.z = cor.z,
                  diff = diff,
                  diff.z = diff.z,
                  rmse = rmse,
                  rmse.z = rmse.z,
                  sd = sd,
                  sd.z = sd.z,
                  rel = rel,
                  nnegICC = nnegICC,
                  estimProbNeg = estimProbNeg,
                  estimProbPos = estimProbPos,
                  N_valid_ICC.z = N_valid_ICC.z,
                  N_rel = N_rel)

# define the y label for each outcome plot
ylabels <- list("Correlation with Benchmark",
                "Correlation with Benchmark (ICC.z)",
                "Difference in ICCs to Benchmkark",
                "Difference in ICCs to Benchmkark (ICC.z)",
                "RMSE",
                "RMSE.z",
                "SD of ICCs",
                "SD of ICC.z",
                "Reliability of ICCs",
                "Number of Negative ICCs",
                "Number of Estimation Problems (Negative)",
                "Number of Estimation Problems (Positive)",
                "Number of Valid ICC.z",
                "Number of ICCs for Reliability") 
names(ylabels) <- names(data_list)

# Check minimum and maximum for y limits of each plot and define
# min(cor$cor_ICC_min)
# max(cor$cor_ICC_max)
# min(cor.z$cor_ICC.z_min)
# max(cor.z$cor_ICC.z_max)
# min(diff$min_diff_ICC_min)
# max(diff$max_diff_ICC_max)
# min(diff.z$min_diff_ICC.z_min)
# max(diff.z$max_diff_ICC.z_max)
# min(rmse$RMSE_ICC_min)
# max(rmse$RMSE_ICC_max)
# min(rmse.z$RMSE_ICC.z_min)
# max(rmse.z$RMSE_ICC.z_max)
# min(sd$sd_ICC_min)
# max(sd$sd_ICC_max)
# min(sd.z$sd_ICC.z_min)
# max(sd.z$sd_ICC.z_max)
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
  c(-1, 1), # correlation with benchmark
  c(-1, 1), # correlation with benchmark (ICC.z)
  c(-0.7, 0.85), # difference in ICCs (compared to benchmark)
  c(-5, 3.5), # difference in ICCs (compared to benchmark) for ICC.z
  c(0, 0.5), # RMSE
  c(0, 1.8), # RMSE (ICC.z)
  c(0, 0.45), # SD of ICCs
  c(0, 1.4), # SD of ICCs (ICC.z)
  c(0, 1), # Reliability
  c(0, 10), # number of negative ICCs
  c(0, 1), # number of estimation problems (negative)
  c(0, 1), # number of estimation problems (positive)
  c(11, 12), # number of valid ICC.z
  c(11, 12) # number of ICCs used for reliability
)
names(ylim_list) <- names(data_list)

plot_list <- lapply(names(data_list), function(outcome) {
  df <- data_list[[outcome]]
  plot_outcome(df, ylabel = ylabels[[outcome]], ylims=ylim_list[[outcome]],
               x_breaks = seq(0, 100, 20), theme_custom = my_theme,
               groupwise = TRUE)
})

names(plot_list) <- names(data_list)





# Look At Plots -----------------------------------------------------------

plot_list[["cor"]]
plot_list[["cor.z"]]
plot_list[["diff"]]
plot_list[["diff.z"]]
plot_list[["rmse"]]
plot_list[["rmse.z"]]
plot_list[["sd"]]
plot_list[["sd.z"]]
plot_list[["rel"]]
plot_list[["nnegICC"]]
plot_list[["estimProbNeg"]]
plot_list[["estimProbPos"]]
plot_list[["N_valid_ICC.z"]]
plot_list[["N_rel"]]



# Save Single Plots -------------------------------------------------------
ggsave("plots/Study 1/groupwise/single plots/correlation.pdf",plot = plot_list[["cor"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/groupwise/single plots/correlation.z.pdf",plot = plot_list[["cor.z"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/groupwise/single plots/difference.pdf",plot = plot_list[["diff"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/groupwise/single plots/difference.z.pdf",plot = plot_list[["diff.z"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/groupwise/single plots/rmse.pdf",plot = plot_list[["rmse"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/groupwise/single plots/rmse.z.pdf",plot = plot_list[["rmse.z"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/groupwise/single plots/sd.pdf",plot = plot_list[["sd"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/groupwise/single plots/sd.z.pdf",plot = plot_list[["sd.z"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/groupwise/single plots/reliability.pdf",plot = plot_list[["rel"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/groupwise/single plots/NnegICC.pdf",plot = plot_list[["nnegICC"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/groupwise/single plots/EstimProbNeg.pdf",plot = plot_list[["estimProbNeg"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/groupwise/single plots/EstimProbPos.pdf",plot = plot_list[["estimProbPos"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/groupwise/single plots/N_ValidICC.z.pdf",plot = plot_list[["N_valid_ICC.z"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/groupwise/single plots/N_rel.pdf",plot = plot_list[["N_Rel"]], device="pdf", height = 148, width = 210, unit="mm")


# as svg (for presentations)
ggsave("plots/Study 1/groupwise/single plots/correlation.svg",plot = plot_list[["cor"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/groupwise/single plots/correlation.z.svg",plot = plot_list[["cor.z"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/groupwise/single plots/difference.svg",plot = plot_list[["diff"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/groupwise/single plots/difference.z.svg",plot = plot_list[["diff.z"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/groupwise/single plots/rmse.svg",plot = plot_list[["rmse"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/groupwise/single plots/rmse.z.svg",plot = plot_list[["rmse.z"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/groupwise/single plots/sd.svg",plot = plot_list[["sd"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/groupwise/single plots/sd.z.svg",plot = plot_list[["sd.z"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/groupwise/single plots/reliability.svg",plot = plot_list[["rel"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/groupwise/single plots/NnegICC.svg",plot = plot_list[["nnegICC"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/groupwise/single plots/EstimProbNeg.svg",plot = plot_list[["estimProbNeg"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/groupwise/single plots/EstimProbPos.svg",plot = plot_list[["estimProbPos"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/groupwise/single plots/N_ValidICC.z.svg",plot = plot_list[["N_valid_ICC.z"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/Study 1/groupwise/single plots/N_rel.svg",plot = plot_list[["N_Rel"]], device="svg", height = 148, width = 210, unit="mm")





# Build .csv Table --------------------------------------------------------

# for raw ICCs

# bind all results
all_agg_results_grp <- merge(cor, diff, by = c("group", "occasions_drawn", "n_occasions", "n_items"))
all_agg_results_grp <- merge(all_agg_results_grp, rmse, by = c("group", "occasions_drawn", "n_occasions", "n_items"))
all_agg_results_grp <- merge(all_agg_results_grp, sd, by = c("group", "occasions_drawn", "n_occasions", "n_items"))
all_agg_results_grp <- merge(all_agg_results_grp, rel, by = c("group", "occasions_drawn", "n_occasions", "n_items"))
all_agg_results_grp <- merge(all_agg_results_grp, nnegICC, by = c("group", "occasions_drawn", "n_occasions", "n_items"))
all_agg_results_grp <- merge(all_agg_results_grp, estimProbNeg, by = c("group", "occasions_drawn", "n_occasions", "n_items"))
all_agg_results_grp <- merge(all_agg_results_grp, estimProbPos, by = c("group", "occasions_drawn", "n_occasions", "n_items"))
all_agg_results_grp <- merge(all_agg_results_grp, N_rel, by = c("group", "occasions_drawn", "n_occasions", "n_items"))
all_agg_results_grp <- merge(all_agg_results_grp, N_valid_ICC.z, by = c("group", "occasions_drawn", "n_occasions", "n_items"))
# round to 3 decimals
all_agg_results_grp[5:34] <- round(all_agg_results_grp[5:34], 3) 
# sort
all_agg_results_grp <- all_agg_results_grp[order(all_agg_results_grp$group, all_agg_results_grp$occasions_drawn, all_agg_results_grp$n_occasions, all_agg_results_grp$n_items), ]
# reset row names
rownames(all_agg_results_grp) <- NULL
# save
write.csv(all_agg_results_grp, "results/results_table_subgroups_Study1.csv", row.names = F)



# for ICC.z

# bind all results
all_agg_results_grp <- merge(cor.z, diff.z, by = c("group", "occasions_drawn", "n_occasions", "n_items"))
all_agg_results_grp <- merge(all_agg_results_grp, rmse.z, by = c("group", "occasions_drawn", "n_occasions", "n_items"))
all_agg_results_grp <- merge(all_agg_results_grp, sd.z, by = c("group", "occasions_drawn", "n_occasions", "n_items"))
all_agg_results_grp <- merge(all_agg_results_grp, rel, by = c("group", "occasions_drawn", "n_occasions", "n_items"))
all_agg_results_grp <- merge(all_agg_results_grp, nnegICC, by = c("group", "occasions_drawn", "n_occasions", "n_items"))
all_agg_results_grp <- merge(all_agg_results_grp, estimProbNeg, by = c("group", "occasions_drawn", "n_occasions", "n_items"))
all_agg_results_grp <- merge(all_agg_results_grp, estimProbPos, by = c("group", "occasions_drawn", "n_occasions", "n_items"))
all_agg_results_grp <- merge(all_agg_results_grp, N_rel, by = c("group", "occasions_drawn", "n_occasions", "n_items"))
all_agg_results_grp <- merge(all_agg_results_grp, N_valid_ICC.z, by = c("group", "occasions_drawn", "n_occasions", "n_items"))
# round to 3 decimals
all_agg_results_grp[5:34] <- round(all_agg_results_grp[5:34], 3) 
# sort
all_agg_results_grp <- all_agg_results_grp[order(all_agg_results_grp$group, all_agg_results_grp$occasions_drawn, all_agg_results_grp$n_occasions, all_agg_results_grp$n_items), ]
# reset row names
rownames(all_agg_results_grp) <- NULL
# save
write.csv(all_agg_results_grp, "results/results_table_Z-transformed_subgroups_Study1.csv", row.names = F)



