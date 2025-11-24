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

# Note: Function could, in principle, handle if simulation study was run
# for separate groups (e.g., high, medium, and low NED) -> groupwise argument.
# However, this was not done here.



# Load Packages -----------------------------------------------------------
library(ggpubr)
library(tidyverse) # reshape data (for participant-level plots)
# ggplot is loaded when sourcing the function



# Person-Level Difference Plot --------------------------------------------

## for ICC
load("results/person_level_difference_aggregated_all_participants.rda")

# plot
# reshape data
long <- person_diff_agg %>%
  pivot_longer(
    cols = starts_with("person_difference_"),   
    names_to = "participant",
    values_to = "person_difference"
  )



diff_plot <- ggplot(long, aes(x = n_occasions, y = person_difference, group = participant)) +
  geom_line(alpha = 0.4, aes(col=participant), linewidth=0.6) +
  facet_grid(rows=vars(n_items), cols=vars(occasions_drawn), labeller = labeller(n_items = function(x) paste0(x, " items"))) +
  my_theme +
  theme(legend.position = "none") +
  ylab("Difference in ICC from Benchmark") +
  xlab("Number of Measurement Occasions") +
  expand_limits(x = 70) 

diff_plot

ggsave("plots/person_specific_diff_plot.pdf",plot = diff_plot, device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/person_specific_diff_plot.svg",plot = diff_plot, device="svg", height = 148, width = 210, unit="mm")

tiff("plots/person_specific_diff_plot.tiff", units="mm", width=210, height=148, res=1200)
diff_plot
dev.off()

rm(diff_plot, long, person_diff_agg)


## for ICC.z
load("results/person_level_difference.z_aggregated_all_participants.rda")

# plot
# reshape data
long.z <- person_diff_agg.z %>%
  pivot_longer(
    cols = starts_with("person_difference.z_"),   
    names_to = "participant",
    values_to = "person_difference.z"
  )



diff_plot.z <- ggplot(long.z, aes(x = n_occasions, y = person_difference.z, group = participant)) +
  geom_line(alpha = 0.4, aes(col=participant), linewidth=0.6) +
  facet_grid(rows=vars(n_items), cols=vars(occasions_drawn), labeller = labeller(n_items = function(x) paste0(x, " items"))) +
  my_theme +
  theme(legend.position = "none") +
  ylab("Difference in ICC.z from Benchmark") +
  xlab("Number of Measurement Occasions") +
  expand_limits(x = 70) 

diff_plot.z

ggsave("plots/person_specific_diff.z_plot.pdf",plot = diff_plot.z, device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/person_specific_diff.z_plot.svg",plot = diff_plot.z, device="svg", height = 148, width = 210, unit="mm")

tiff("plots/person_specific_diff.z_plot.tiff", units="mm", width=210, height=148, res=1200)
diff_plot.z
dev.off()

rm(diff_plot.z, long.z, person_diff_agg.z)



# Load Aggregated Results Data --------------------------------------------
load("results/aggregated_results.rda")


# Extract Data ------------------------------------------------------------

## CORRELATION
cor <- agg[["cor_ICC"]][["agg_res"]]
cor.z <- agg[["cor_ICC.z"]][["agg_res"]]


## DIFFERENCE IN ICC
# for ICC
diff <- agg[["person_diff"]][["agg_res"]]

# for ICC.z
diff.z <- agg[["person_diff.z"]][["agg_res"]]



## RMSE
rmse <- agg[["RMSE_ICC"]][["agg_res"]]
rmse.z <- agg[["RMSE_ICC.z"]][["agg_res"]]
# for RMSE, only plot values for random draws -> set by order to NA
rmse <- rmse[which(rmse$occasions_drawn == "random"), ]
rmse.z <- rmse.z[which(rmse.z$occasions_drawn == "random"), ]



## SD
sd <- agg[["sd_ICC"]][["agg_res"]]
sd.z <- agg[["sd_ICC.z"]][["agg_res"]]

## RELIABILITY
rel <- agg[["rel"]][["agg_res"]]


## PROPORTION OF NUMBER OF NEGATIVE ICCS
percnegICC <- agg[["percnegICC"]][["agg_res"]]


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
                  percnegICC = percnegICC,
                  estimProbNeg = estimProbNeg,
                  estimProbPos = estimProbPos,
                  N_valid_ICC.z = N_valid_ICC.z,
                  N_rel = N_rel)

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
             "Number of Estimation Problems (Negative)",
             "Number of Estimation Problems (Positive)",
             "Number of Valid ICC.z",
             "Number of ICCs for Reliability") 
names(ylabels) <- names(data_list)

# # Check minimum and maximum for y limits of each plot and define
# min(cor$cor_ICC_min)
# max(cor$cor_ICC_max)
# min(cor.z$cor_ICC.z_min)
# max(cor.z$cor_ICC.z_max)
# min(diff$difference_min)
# max(diff$difference_max)
# min(diff.z$difference.z_min)
# max(diff.z$difference.z_max)
# min(rmse$RMSE_min)
# max(rmse$RMSE_max)
# min(rmse.z$RMSE.z_min)
# max(rmse.z$RMSE.z_max)
# min(sd$sd_ICC_min)
# max(sd$sd_ICC_max)
# min(sd.z$sd_ICC.z_min)
# max(sd.z$sd_ICC.z_max)
# min(rel$rel_min)
# max(rel$rel_max)
# min(percnegICC$percnegICC_min)
# max(percnegICC$percnegICC_max)
# min(estimProbNeg$estimationProbNeg_min)
# max(estimProbNeg$estimationProbNeg_max)
# min(estimProbPos$estimationProbPos_min)
# max(estimProbPos$estimationProbPos_max)
# # in general: no estimation problems
# min(N_valid_ICC.z$N_valid_ICC.z_min)
# max(N_valid_ICC.z$N_valid_ICC.z_max)
# min(N_rel$N_rel_min)
# max(N_rel$N_rel_max)

# for correlation, reliability, proportions, ... -> use theoretical range (e.g., difference max and min: 1 and -1)
ylim_list <- list(
  c(0, 1), # correlation with benchmark
  c(0, 1), # correlation with benchmark (ICC.z)
  c(-1, 1), # difference in ICCs (compared to benchmark)
  c(-1.6, 0.8), # difference in ICCs (compared to benchmark) for ICC.z
  c(0, 0.6), # RMSE
  c(0, 1.5), # RMSE (ICC.z)
  c(0, 0.2), # SD of ICCs
  c(0, 0.45), # SD of ICC.z
  c(0, 1), # Reliability
  c(0, 1), # proportion of negative ICCs
  c(0, 1), # number of estimation problems (negative)
  c(0, 1), # number of estimation problems (positive)
  c(105, 109), # number of valid ICC.z
  c(105, 109) # number of ICCs used for reliability
)
names(ylim_list) <- names(data_list)

plot_list <- lapply(names(data_list), function(outcome) {
  df <- data_list[[outcome]]
  plot_outcome(df, ylabel = ylabels[[outcome]], ylims=ylim_list[[outcome]],
               x_breaks = seq(0, 70, 10), theme_custom = my_theme,
               dodge_width = 3,
               groupwise = FALSE, split_facets = FALSE)
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
plot_list[["percnegICC"]]
plot_list[["estimProbNeg"]]
plot_list[["estimProbPos"]]
plot_list[["N_valid_ICC.z"]]
plot_list[["N_rel"]]



# Save Single Plots -------------------------------------------------------
ggsave("plots/single plots/facet per plot/correlation.pdf",plot = plot_list[["cor"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/single plots/facet per plot/correlation.z.pdf",plot = plot_list[["cor.z"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/single plots/facet per plot/difference.pdf",plot = plot_list[["diff"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/single plots/facet per plot/difference.z.pdf",plot = plot_list[["diff.z"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/single plots/facet per plot/rmse.pdf",plot = plot_list[["rmse"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/single plots/facet per plot/rmse.z.pdf",plot = plot_list[["rmse.z"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/single plots/facet per plot/sd.pdf",plot = plot_list[["sd"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/single plots/facet per plot/sd.z.pdf",plot = plot_list[["sd.z"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/single plots/facet per plot/reliability.pdf",plot = plot_list[["rel"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/single plots/facet per plot/percnegICC.pdf",plot = plot_list[["percnegICC"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/single plots/facet per plot/EstimProbNeg.pdf",plot = plot_list[["estimProbNeg"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/single plots/facet per plot/EstimProbPos.pdf",plot = plot_list[["estimProbPos"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/single plots/facet per plot/N_ValidICC.z.pdf",plot = plot_list[["N_valid_ICC.z"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/single plots/facet per plot/N_rel.pdf",plot = plot_list[["N_Rel"]], device="pdf", height = 148, width = 210, unit="mm")


# as svg (for presentations)

ggsave("plots/single plots/facet per plot/correlation.svg",plot = plot_list[["cor"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/single plots/facet per plot/correlation.z.svg",plot = plot_list[["cor.z"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/single plots/facet per plot/difference.svg",plot = plot_list[["diff"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/single plots/facet per plot/difference.z.svg",plot = plot_list[["diff.z"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/single plots/facet per plot/rmse.svg",plot = plot_list[["rmse"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/single plots/facet per plot/rmse.z.svg",plot = plot_list[["rmse.z"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/single plots/facet per plot/sd.svg",plot = plot_list[["sd"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/single plots/facet per plot/sd.z.svg",plot = plot_list[["sd.z"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/single plots/facet per plot/reliability.svg",plot = plot_list[["rel"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/single plots/facet per plot/percnegICC.svg",plot = plot_list[["percnegICC"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/single plots/facet per plot/EstimProbNeg.svg",plot = plot_list[["estimProbNeg"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/single plots/facet per plot/EstimProbPos.svg",plot = plot_list[["estimProbPos"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/single plots/facet per plot/N_ValidICC.z.svg",plot = plot_list[["N_valid_ICC.z"]], device="svg", height = 148, width = 210, unit="mm")
ggsave("plots/single plots/facet per plot/N_rel.svg",plot = plot_list[["N_Rel"]], device="svg", height = 148, width = 210, unit="mm")





# Combine Plots -----------------------------------------------------------
# do not use estimation problems as there were none
# number of valid ICC.z and number of participants for reliability can be 
# reported easily -> do not plot


# for raw ICCs

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

d <- plot_list[["sd"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                               plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                               axis.text.y = element_text(hjust=1),
                               axis.text = element_text(size=10)) + ggtitle("(D) SD of ICCs") +
                          geom_hline(yintercept=agg[["sd_ICC"]][["agg_res"]][["sd_ICC_mean"]][[39]], # use benchmark SD
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




ggsave("plots/plots_combined.pdf",plot = combined, device="pdf", height = 148, width = 220, unit="mm")

ggsave("plots/plots_combined.svg",plot = combined, device="svg", height = 148, width = 220, unit="mm")


# as tiff
tiff("plots/plots_combined.tiff", units="mm", width=220, height=148, res=1200)
combined
dev.off()


# for ICC.z

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
d <- plot_list[["sd.z"]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                               plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                               axis.text = element_text(size=10)) + ggtitle("(D) SD of ICCs") +
                           geom_hline(yintercept=agg[["sd_ICC.z"]][["agg_res"]][["sd_ICC.z_mean"]][[39]], linetype="twodash", color = "black")
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



ggsave("plots/plots_Z-transformed_combined.pdf",plot = combined, device="pdf", height = 148, width = 220, unit="mm")



ggsave("plots/plots_Z-transformed_combined.svg",plot = combined, device="svg", height = 148, width = 220, unit="mm")


# as tiff
tiff("plots/plots_Z-transformed_combined.tiff", units="mm", width=220, height=148, res=1200)
combined
dev.off()






# Single Plots Split by occasions_drawn -----------------------------------

plot_list_split <- lapply(names(data_list), function(outcome) {
  dat <- data_list[[outcome]]
  plot_outcome(dat, ylabel=ylabels[[outcome]], ylims=ylim_list[[outcome]],
               x_breaks = seq(0, 100, 20), theme_custom = my_theme,
               dodge_width = 5,
               groupwise = FALSE,
               split_facets = TRUE)
})
names(plot_list_split) <- names(data_list)

plot_list_split[["cor"]][[1]]
plot_list_split[["cor"]][[2]]
plot_list_split[["cor.z"]][[1]]
plot_list_split[["cor.z"]][[2]]
plot_list_split[["diff"]][[1]]
plot_list_split[["diff"]][[2]]
plot_list_split[["diff.z"]][[1]]
plot_list_split[["diff.z"]][[2]]
plot_list_split[["rmse"]][[1]]
plot_list_split[["rmse"]][[2]]
plot_list_split[["rmse.z"]][[1]]
plot_list_split[["rmse.z"]][[2]]
plot_list_split[["sd"]][[1]]
plot_list_split[["sd"]][[2]]
plot_list_split[["sd.z"]][[1]]
plot_list_split[["sd.z"]][[2]]
plot_list_split[["rel"]][[1]]
plot_list_split[["rel"]][[2]]
plot_list_split[["percnegICC"]][[1]]
plot_list_split[["percnegICC"]][[2]]
plot_list_split[["estimProbNeg"]][[1]]
plot_list_split[["estimProbNeg"]][[2]]
plot_list_split[["estimProbPos"]][[1]]
plot_list_split[["estimProbPos"]][[2]]
plot_list_split[["N_valid_ICC.z"]][[1]]
plot_list_split[["N_valid_ICC.z"]][[2]]
plot_list_split[["N_rel"]][[1]]
plot_list_split[["N_rel"]][[2]]




# Combine Plots Split by occasions_drawn ----------------------------------


# for ICC, random draws
# adjust the plots a little (e.g., no y-axis lable but title, no x-axis label)
a <- plot_list_split[["cor"]][[1]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                  plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                  axis.text = element_text(size=10)) + ggtitle("Correlation with Benchmark") +
                                  geom_hline(yintercept=0.80, linetype="twodash", color = "black")
# a
b <- plot_list_split[["diff"]][[1]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                   plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                   axis.text = element_text(size=10)) + ggtitle("Difference from Benchmark")
# b
c <- plot_list_split[["rmse"]][[1]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                   plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                   axis.text = element_text(size=10)) + ggtitle("RMSE")
# c
d <- plot_list_split[["sd"]][[1]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                 plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                 axis.text = element_text(size=10)) + ggtitle("SD of ICCs")+
                                geom_hline(yintercept=agg[["sd_ICC"]][["agg_res"]][["sd_ICC_mean"]][[39]], linetype="twodash", color = "black")


# d
e <- plot_list_split[["rel"]][[1]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                axis.text = element_text(size=10)) + ggtitle("Reliability of ICCs") +
                                geom_hline(yintercept=0.80, linetype="twodash", color = "black")
# e
f <- plot_list_split[["percnegICC"]][[1]]+ theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                    plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                    axis.text = element_text(size=10)) + ggtitle("Proportion of Negative ICCs")



panel_random <- ggpubr::ggarrange(a,b,c,d,e,f , ncol=3, nrow=2, common.legend = TRUE, legend = "top",
                                  align = "hv", widths = c(1,1,1), heights = c(1, 1)) # equal panel sizes

# 
panel_random <- annotate_figure(panel_random,
                                top = text_grob("Occasions drawn randomly", face="bold"),
                                bottom = text_grob("Number of measurement occasions"))

panel_random



# for ICC, ordered draws
# adjust the plots a little (e.g., no y-axis lable but title, no x-axis label)
a <- plot_list_split[["cor"]][[2]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                           plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                           axis.text = element_text(size=10)) + ggtitle("Correlation with Benchmark")+
                                    geom_hline(yintercept=0.80, linetype="twodash", color = "black")

# a
b <- plot_list_split[["diff"]][[2]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                            plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                            axis.text = element_text(size=10)) + ggtitle("Difference from Benchmark")
# b
c <- plot_list_split[["rmse"]][[2]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                            plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                            axis.text = element_text(size=10)) + ggtitle("RMSE")
# c
d <- plot_list_split[["sd"]][[2]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                          plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                          axis.text = element_text(size=10)) + ggtitle("SD of ICCs")+
  geom_hline(yintercept=agg[["sd_ICC"]][["agg_res"]][["sd_ICC_mean"]][[39]], linetype="twodash", color = "black")

# d
e <- plot_list_split[["rel"]][[2]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                           plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                           axis.text = element_text(size=10)) + ggtitle("Reliability of ICCs")+
                                      geom_hline(yintercept=0.80, linetype="twodash", color = "black")

# e
f <- plot_list_split[["percnegICC"]][[2]]+ theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                              plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                              axis.text = element_text(size=10)) + ggtitle("Proprtion of Negative ICCs")



panel_order <- ggpubr::ggarrange(a,b,c,d,e,f , ncol=3, nrow=2, common.legend=TRUE, legend = "top",
                                  align = "hv", widths = c(1,1,1), heights = c(1, 1)) # equal panel sizes


panel_order <- annotate_figure(panel_order,
                               top = text_grob("Occasions drawn by order", face="bold"),
                               bottom = text_grob("Number of measurement occasions"))

panel_order


combined <- ggpubr::ggarrange(panel_random, panel_order, nrow=2, ncol=1,
                             align="hv",widths = c(1,1), heights = c(1, 1))

combined


# save panels
ggsave("plots/single plots/split by facet/ICC_random_draws.pdf", plot = panel_random,
       device="pdf", height=148, width=210, unit="mm")

ggsave("plots/single plots/split by facet/ICC_random_draws.svg", plot = panel_random,
       device="svg", height=148, width=210, unit="mm")

ggsave("plots/single plots/split by facet/ICC_ordered_draws.pdf", plot = panel_order,
       device="pdf", height=148, width=210, unit="mm")

ggsave("plots/single plots/split by facet/ICC_ordered_draws.svg", plot = panel_order,
       device="svg", height=148, width=210, unit="mm")


ggsave("plots/single plots/split by facet/ICC_combined.pdf", plot = combined,
       device="pdf", height=250, width=210, unit="mm")

ggsave("plots/single plots/split by facet/ICC_combined.svg", plot = combined,
       device="svg", height=250, width=210, unit="mm")



# for ICC.z, random draws
# adjust the plots a little (e.g., no y-axis lable but title, no x-axis label)
a <- plot_list_split[["cor.z"]][[1]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                           plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                           axis.text = element_text(size=10)) + ggtitle("Correlation with Benchmark")+
                                         geom_hline(yintercept=0.80, linetype="twodash", color = "black")

# a
b <- plot_list_split[["diff.z"]][[1]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                            plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                            axis.text = element_text(size=10)) + ggtitle("Difference from Benchmark")
# b
c <- plot_list_split[["rmse.z"]][[1]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                            plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                            axis.text = element_text(size=10)) + ggtitle("RMSE")
# c
d <- plot_list_split[["sd.z"]][[1]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                          plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                          axis.text = element_text(size=10)) + ggtitle("SD of ICCs")+
  geom_hline(yintercept=agg[["sd_ICC.z"]][["agg_res"]][["sd_ICC.z_mean"]][[39]], linetype="twodash", color = "black")

# d
e <- plot_list_split[["rel"]][[1]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                           plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                           axis.text = element_text(size=10)) + ggtitle("Reliability of ICCs")+
                                      geom_hline(yintercept=0.80, linetype="twodash", color = "black")

# e
f <- plot_list_split[["percnegICC"]][[1]]+ theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                              plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                              axis.text = element_text(size=10)) + ggtitle("Proportion of Negative ICCs")



panel_random <- ggpubr::ggarrange(a,b,c,d,e,f , ncol=3, nrow=2, common.legend = TRUE, legend = "top",
                                  align = "hv", widths = c(1,1,1), heights = c(1, 1)) # equal panel sizes

# 
panel_random <- annotate_figure(panel_random,
                                top = text_grob("Occasions drawn randomly", face="bold"),
                                bottom = text_grob("Number of measurement occasions"))

panel_random



# for ICC.z, ordered draws
# adjust the plots a little (e.g., no y-axis lable but title, no x-axis label)
a <- plot_list_split[["cor.z"]][[2]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                           plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                           axis.text = element_text(size=10)) + ggtitle("Correlation with Benchmark")+
                                       geom_hline(yintercept=0.80, linetype="twodash", color = "black")

# a
b <- plot_list_split[["diff.z"]][[2]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                            plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                            axis.text = element_text(size=10)) + ggtitle("Difference from Benchmark")
# b
c <- plot_list_split[["rmse.z"]][[2]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                            plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                            axis.text = element_text(size=10)) + ggtitle("RMSE")
# c
d <- plot_list_split[["sd.z"]][[2]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                          plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                          axis.text = element_text(size=10)) + ggtitle("SD of ICCs")+
  geom_hline(yintercept=agg[["sd_ICC.z"]][["agg_res"]][["sd_ICC.z_mean"]][[39]], linetype="twodash", color = "black")

# d
e <- plot_list_split[["rel"]][[2]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                           plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                           axis.text = element_text(size=10)) + ggtitle("Reliability of ICCs")+
                                    geom_hline(yintercept=0.80, linetype="twodash", color = "black")

# e
f <- plot_list_split[["percnegICC"]][[2]]+ theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                              plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                              axis.text = element_text(size=10)) + ggtitle("Proportion of Negative ICCs")



panel_order <- ggpubr::ggarrange(a,b,c,d,e,f , ncol=3, nrow=2, common.legend=TRUE, legend = "top",
                                 align = "hv", widths = c(1,1,1), heights = c(1, 1)) # equal panel sizes


panel_order <- annotate_figure(panel_order,
                               top = text_grob("Occasions drawn by order", face="bold"),
                               bottom = text_grob("Number of measurement occasions"))

panel_order


combined <- ggpubr::ggarrange(panel_random, panel_order, nrow=2, ncol=1,
                              align="hv",widths = c(1,1), heights = c(1, 1))

combined


# save panels
ggsave("plots/single plots/split by facet/ICC.z_random_draws.pdf", plot = panel_random,
       device="pdf", height=148, width=210, unit="mm")

ggsave("plots/single plots/split by facet/ICC.z_random_draws.svg", plot = panel_random,
       device="svg", height=148, width=210, unit="mm")

ggsave("plots/single plots/split by facet/ICC.z_ordered_draws.pdf", plot = panel_order,
       device="pdf", height=148, width=210, unit="mm")

ggsave("plots/single plots/split by facet/ICC.z_ordered_draws.svg", plot = panel_order,
       device="svg", height=148, width=210, unit="mm")


ggsave("plots/single plots/split by facet/ICC.z_combined.pdf", plot = combined,
       device="pdf", height=250, width=210, unit="mm")

ggsave("plots/single plots/split by facet/ICC.z_combined.svg", plot = combined,
       device="svg", height=250, width=210, unit="mm")






# '' Colored Plots for Presentations --------------------------------------

plot_list_split_col <- lapply(names(data_list), function(outcome) {
  dat <- data_list[[outcome]]
  plot_outcome(dat, ylabel=ylabels[[outcome]], ylims=ylim_list[[outcome]],
               x_breaks = seq(0, 100, 20), theme_custom = my_theme,
               scale_color = scale_colour_viridis_d(option="magma", begin=0.2, end=0.85),
               dodge_width = 5,
               groupwise = FALSE,
               split_facets = TRUE)
})
names(plot_list_split_col) <- names(data_list)

plot_list_split_col[["cor"]][[1]]
plot_list_split_col[["cor"]][[2]]
plot_list_split_col[["cor.z"]][[1]]
plot_list_split_col[["cor.z"]][[2]]
plot_list_split_col[["diff"]][[1]]
plot_list_split_col[["diff"]][[2]]
plot_list_split_col[["diff.z"]][[1]]
plot_list_split_col[["diff.z"]][[2]]
plot_list_split_col[["rmse"]][[1]]
plot_list_split_col[["rmse"]][[2]]
plot_list_split_col[["rmse.z"]][[1]]
plot_list_split_col[["rmse.z"]][[2]]
plot_list_split_col[["sd"]][[1]]
plot_list_split_col[["sd"]][[2]]
plot_list_split_col[["sd.z"]][[1]]
plot_list_split_col[["sd.z"]][[2]]
plot_list_split_col[["rel"]][[1]]
plot_list_split_col[["rel"]][[2]]
plot_list_split_col[["percnegICC"]][[1]]
plot_list_split_col[["percnegICC"]][[2]]
plot_list_split_col[["estimProbNeg"]][[1]]
plot_list_split_col[["estimProbNeg"]][[2]]
plot_list_split_col[["estimProbPos"]][[1]]
plot_list_split_col[["estimProbPos"]][[2]]
plot_list_split_col[["N_valid_ICC.z"]][[1]]
plot_list_split_col[["N_valid_ICC.z"]][[2]]
plot_list_split_col[["N_rel"]][[1]]
plot_list_split_col[["N_rel"]][[2]]




# for ICC, random draws
# adjust the plots a little (e.g., no y-axis lable but title, no x-axis label)
a <- plot_list_split_col[["cor"]][[1]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                           plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                           axis.text = element_text(size=10)) + ggtitle("Correlation with Benchmark")+
                                        geom_hline(yintercept=0.80, linetype="twodash", color = "red")

# a
b <- plot_list_split_col[["diff"]][[1]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                            plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                            axis.text = element_text(size=10)) + ggtitle("Difference from Benchmark")
# b
c <- plot_list_split_col[["rmse"]][[1]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                            plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                            axis.text = element_text(size=10)) + ggtitle("RMSE")
# c
d <- plot_list_split_col[["sd"]][[1]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                          plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                          axis.text = element_text(size=10)) + ggtitle("SD of ICCs")+
  geom_hline(yintercept=agg[["sd_ICC"]][["agg_res"]][["sd_ICC_mean"]][[39]], linetype="twodash", color = "red")

# d
e <- plot_list_split_col[["rel"]][[1]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                           plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                           axis.text = element_text(size=10)) + ggtitle("Reliability of ICCs")+
                                         geom_hline(yintercept=0.80, linetype="twodash", color = "red")

# e
f <- plot_list_split_col[["percnegICC"]][[1]]+ theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                              plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                              axis.text = element_text(size=10)) + ggtitle("Proportion of Negative ICCs")



panel_random <- ggpubr::ggarrange(a,b,c,d,e,f , ncol=3, nrow=2, common.legend = TRUE, legend = "top",
                                  align = "hv", widths = c(1,1,1), heights = c(1, 1)) # equal panel sizes

# 
panel_random <- annotate_figure(panel_random,
                                top = text_grob("Occasions drawn randomly", face="bold"),
                                bottom = text_grob("Number of measurement occasions"))

panel_random



# for ICC, ordered draws
# adjust the plots a little (e.g., no y-axis lable but title, no x-axis label)
a <- plot_list_split_col[["cor"]][[2]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                           plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                           axis.text = element_text(size=10)) + ggtitle("Correlation with Benchmark")+
                                        geom_hline(yintercept=0.80, linetype="twodash", color = "red")

# a
b <- plot_list_split_col[["diff"]][[2]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                            plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                            axis.text = element_text(size=10)) + ggtitle("Difference from Benchmark")
# b
c <- plot_list_split_col[["rmse"]][[2]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                            plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                            axis.text = element_text(size=10)) + ggtitle("RMSE")
# c
d <- plot_list_split_col[["sd"]][[2]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                          plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                          axis.text = element_text(size=10)) + ggtitle("SD of ICCs")+
  geom_hline(yintercept=agg[["sd_ICC"]][["agg_res"]][["sd_ICC_mean"]][[39]], linetype="twodash", color = "red")

# d
e <- plot_list_split_col[["rel"]][[2]] + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                           plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                           axis.text = element_text(size=10)) + ggtitle("Reliability of ICCs")+
                                         geom_hline(yintercept=0.80, linetype="twodash", color = "red")

# e
f <- plot_list_split_col[["percnegICC"]][[2]]+ theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                              plot.title = element_text(size=12), plot.margin=margin(t=5,r=5,b=10,l=5),
                                              axis.text = element_text(size=10)) + ggtitle("Proportion of Negative ICCs")



panel_order <- ggpubr::ggarrange(a,b,c,d,e,f , ncol=3, nrow=2, common.legend=TRUE, legend = "top",
                                 align = "hv", widths = c(1,1,1), heights = c(1, 1)) # equal panel sizes


panel_order <- annotate_figure(panel_order,
                               top = text_grob("Occasions drawn by order", face="bold"),
                               bottom = text_grob("Number of measurement occasions"))

panel_order


combined <- ggpubr::ggarrange(panel_random, panel_order, nrow=2, ncol=1,
                              align="hv",widths = c(1,1), heights = c(1, 1))

combined


# save panels
ggsave("plots/single plots/split by facet/FGME/ICC_random_draws.pdf", plot = panel_random,
       device="pdf", height=148, width=210, unit="mm")

ggsave("plots/single plots/split by facet/FGME/ICC_random_draws.svg", plot = panel_random,
       device="svg", height=148, width=210, unit="mm")

ggsave("plots/single plots/split by facet/FGME/ICC_ordered_draws.pdf", plot = panel_order,
       device="pdf", height=148, width=210, unit="mm")

ggsave("plots/single plots/split by facet/FGME/ICC_ordered_draws.svg", plot = panel_order,
       device="svg", height=148, width=210, unit="mm")


ggsave("plots/single plots/split by facet/FGME/ICC_combined.pdf", plot = combined,
       device="pdf", height=250, width=210, unit="mm")

ggsave("plots/single plots/split by facet/FGME/ICC_combined.svg", plot = combined,
       device="svg", height=250, width=210, unit="mm")






# Build .csv Table --------------------------------------------------------

# for raw ICC
# bind all results
all_agg_results <- merge(cor, diff, by = c("occasions_drawn", "n_occasions", "n_items"), all = TRUE)
all_agg_results <- merge(all_agg_results, rmse, by = c("occasions_drawn", "n_occasions", "n_items"), all = TRUE)
all_agg_results <- merge(all_agg_results, sd, by = c("occasions_drawn", "n_occasions", "n_items"), all = TRUE)
all_agg_results <- merge(all_agg_results, rel, by = c("occasions_drawn", "n_occasions", "n_items"), all = TRUE)
all_agg_results <- merge(all_agg_results, percnegICC, by = c("occasions_drawn", "n_occasions", "n_items"), all = TRUE)
all_agg_results <- merge(all_agg_results, estimProbNeg, by = c("occasions_drawn", "n_occasions", "n_items"), all = TRUE)
all_agg_results <- merge(all_agg_results, estimProbPos, by = c("occasions_drawn", "n_occasions", "n_items"), all = TRUE)
all_agg_results <- merge(all_agg_results, N_rel, by = c("occasions_drawn", "n_occasions", "n_items"), all = TRUE)
all_agg_results <- merge(all_agg_results, N_valid_ICC.z, by = c("occasions_drawn", "n_occasions", "n_items"), all = TRUE)
# round to 3 decimals
all_agg_results[4:33] <- round(all_agg_results[4:33], 3) 
# sort
all_agg_results <- all_agg_results[order(all_agg_results$occasions_drawn, all_agg_results$n_occasions, all_agg_results$n_items), ]
# reset row names
rownames(all_agg_results) <- NULL
# save
write.csv(all_agg_results, "results/results_table.csv", row.names = F)

# for ICC.z
# bind all results
all_agg_results <- merge(cor.z, diff.z, by = c("occasions_drawn", "n_occasions", "n_items"), all = TRUE)
all_agg_results <- merge(all_agg_results, rmse.z, by = c("occasions_drawn", "n_occasions", "n_items"), all = TRUE)
all_agg_results <- merge(all_agg_results, sd.z, by = c("occasions_drawn", "n_occasions", "n_items"), all = TRUE)
all_agg_results <- merge(all_agg_results, rel, by = c("occasions_drawn", "n_occasions", "n_items"), all = TRUE)
all_agg_results <- merge(all_agg_results, percnegICC, by = c("occasions_drawn", "n_occasions", "n_items"), all = TRUE)
all_agg_results <- merge(all_agg_results, estimProbNeg, by = c("occasions_drawn", "n_occasions", "n_items"), all = TRUE)
all_agg_results <- merge(all_agg_results, estimProbPos, by = c("occasions_drawn", "n_occasions", "n_items"), all = TRUE)
all_agg_results <- merge(all_agg_results, N_rel, by = c("occasions_drawn", "n_occasions", "n_items"), all = TRUE)
all_agg_results <- merge(all_agg_results, N_valid_ICC.z, by = c("occasions_drawn", "n_occasions", "n_items"), all = TRUE)
# round to 3 decimals
all_agg_results[4:33] <- round(all_agg_results[4:33], 3) 
# sort
all_agg_results <- all_agg_results[order(all_agg_results$occasions_drawn, all_agg_results$n_occasions, all_agg_results$n_items), ]
# reset row names
rownames(all_agg_results) <- NULL
# save
write.csv(all_agg_results, "results/results_table_Z-transformed.csv", row.names = F)




# Check Sufficient Nr of Iterations ---------------------------------------

rm(list=ls())

source("functions/function_plot_outcomes.R")



# Person-Level Difference Plot

## for ICC
load("results/check nr of iterations/person_level_difference_aggregated_all_participants.rda")

# plot
# reshape data
long2 <- person_diff_agg2 %>%
  pivot_longer(
    cols = starts_with("person_difference_"),   
    names_to = "participant",
    values_to = "person_difference"
  )



diff_plot2 <- ggplot(long2, aes(x = n_occasions, y = person_difference, group = participant)) +
  geom_line(alpha = 0.4, aes(col=participant), linewidth=0.6) +
  facet_grid(rows=vars(n_items), cols=vars(occasions_drawn), labeller = labeller(n_items = function(x) paste0(x, " items"))) +
  my_theme +
  theme(legend.position = "none") +
  ylab("Difference in ICC from Benchmark") +
  xlab("Number of Measurement Occasions") +
  expand_limits(x = 70) 

diff_plot2

ggsave("plots/check nr of iterations/person_specific_diff_plot.pdf",plot = diff_plot2, device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/check nr of iterations/person_specific_diff_plot.svg",plot = diff_plot2, device="svg", height = 148, width = 210, unit="mm")

tiff("plots/check nr of iterations/person_specific_diff_plot.tiff", units="mm", width=210, height=148, res=1200)
diff_plot2
dev.off()

rm(diff_plot2, long2, person_diff_agg2)


## for ICC.z
load("results/check nr of iterations/person_level_difference.z_aggregated_all_participants.rda")

# plot
# reshape data
long.z2 <- person_diff_agg.z2 %>%
  pivot_longer(
    cols = starts_with("person_difference.z_"),   
    names_to = "participant",
    values_to = "person_difference.z"
  )



diff_plot.z2 <- ggplot(long.z2, aes(x = n_occasions, y = person_difference.z, group = participant)) +
  geom_line(alpha = 0.4, aes(col=participant), linewidth=0.6) +
  facet_grid(rows=vars(n_items), cols=vars(occasions_drawn), labeller = labeller(n_items = function(x) paste0(x, " items"))) +
  my_theme +
  theme(legend.position = "none") +
  ylab("Difference in ICC.z from Benchmark") +
  xlab("Number of Measurement Occasions") +
  expand_limits(x = 70) 

diff_plot.z2

ggsave("plots/check nr of iterations/person_specific_diff.z_plot.pdf",plot = diff_plot.z2, device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/check nr of iterations/person_specific_diff.z_plot.svg",plot = diff_plot.z2, device="svg", height = 148, width = 210, unit="mm")

tiff("plots/check nr of iterations/person_specific_diff.z_plot.tiff", units="mm", width=210, height=148, res=1200)
diff_plot.z2
dev.off()

rm(diff_plot.z2, long.z2, person_diff_agg.z2)





# Load Aggregated Results Data 
load("results/check nr of iterations/aggregated_results.rda")


# Extract Data 

## CORRELATION
cor <- agg2[["cor_ICC"]][["agg_res"]]
cor.z <- agg2[["cor_ICC.z"]][["agg_res"]]


## DIFFERENCE IN ICC
# for ICC
diff <- agg2[["person_diff"]][["agg_res"]]

# for ICC.z
diff.z <- agg2[["person_diff.z"]][["agg_res"]]



## RMSE
rmse <- agg2[["RMSE_ICC"]][["agg_res"]]
rmse.z <- agg2[["RMSE_ICC.z"]][["agg_res"]]
# for RMSE, only plot values for random draws -> set by order to NA
rmse <- rmse[which(rmse$occasions_drawn == "random"), ]
rmse.z <- rmse.z[which(rmse.z$occasions_drawn == "random"), ]



## SD
sd <- agg2[["sd_ICC"]][["agg_res"]]
sd.z <- agg2[["sd_ICC.z"]][["agg_res"]]

## RELIABILITY
rel <- agg2[["rel"]][["agg_res"]]


## PROPORTION OF NUMBER OF NEGATIVE ICCS
percnegICC <- agg2[["percnegICC"]][["agg_res"]]


## ESTIMATION PROBLEMS
estimProbNeg <- agg2[["estimationProbNeg"]][["agg_res"]]
estimProbPos <- agg2[["estimationProbPos"]][["agg_res"]]

## VALID VALUES
N_valid_ICC.z <- agg2[["N_valid_ICC.z"]][["agg_res"]]
N_rel <- agg2[["N_rel"]][["agg_res"]]





# Plot Outcomes 
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
                  percnegICC = percnegICC,
                  estimProbNeg = estimProbNeg,
                  estimProbPos = estimProbPos,
                  N_valid_ICC.z = N_valid_ICC.z,
                  N_rel = N_rel)

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
# min(diff$difference_min)
# max(diff$difference_max)
# min(diff.z$difference.z_min)
# max(diff.z$difference.z_max)
# min(rmse$RMSE_min)
# max(rmse$RMSE_max)
# min(rmse.z$RMSE.z_min)
# max(rmse.z$RMSE.z_max)
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

# for correlation, reliability, proportions, ... -> use theoretical range (e.g., difference max and min: 1 and -1)
ylim_list <- list(
  c(0, 1), # correlation with benchmark
  c(0, 1), # correlation with benchmark (ICC.z)
  c(-1, 1), # difference in ICCs (compared to benchmark)
  c(-1.6, 0.8), # difference in ICCs (compared to benchmark) for ICC.z
  c(0, 0.6), # RMSE
  c(0, 1.5), # RMSE (ICC.z)
  c(0, 0.2), # SD of ICCs
  c(0, 0.45), # SD of ICC.z
  c(0, 1), # Reliability
  c(0, 1), # proportion of negative ICCs
  c(0, 1), # number of estimation problems (negative)
  c(0, 1), # number of estimation problems (positive)
  c(105, 109), # number of valid ICC.z
  c(105, 109) # number of ICCs used for reliability
)
names(ylim_list) <- names(data_list)

plot_list <- lapply(names(data_list), function(outcome) {
  df <- data_list[[outcome]]
  plot_outcome(df, ylabel = ylabels[[outcome]], ylims=ylim_list[[outcome]],
               x_breaks = seq(0, 70, 10), theme_custom = my_theme,
               dodge_width = 3,
               groupwise = FALSE, split_facets = FALSE)
})

names(plot_list) <- names(data_list)





# Look At Plots & Compare With "Original" Results
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
plot_list[["N_rel"]]



# Save Single Plots 
ggsave("plots/check nr of iterations/correlation.pdf",plot = plot_list[["cor"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/check nr of iterations/correlation.z.pdf",plot = plot_list[["cor.z"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/check nr of iterations/difference.pdf",plot = plot_list[["diff"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/check nr of iterations/difference.z.pdf",plot = plot_list[["diff.z"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/check nr of iterations/rmse.pdf",plot = plot_list[["rmse"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/check nr of iterations/rmse.z.pdf",plot = plot_list[["rmse.z"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/check nr of iterations/sd.pdf",plot = plot_list[["sd"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/check nr of iterations/sd.z.pdf",plot = plot_list[["sd.z"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/check nr of iterations/reliability.pdf",plot = plot_list[["rel"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/check nr of iterations/percnegICC.pdf",plot = plot_list[["percnegICC"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/check nr of iterations/EstimProbNeg.pdf",plot = plot_list[["estimProbNeg"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/check nr of iterations/EstimProbPos.pdf",plot = plot_list[["estimProbPos"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/check nr of iterations/N_ValidICC.z.pdf",plot = plot_list[["N_valid_ICC.z"]], device="pdf", height = 148, width = 210, unit="mm")
ggsave("plots/check nr of iterations/N_rel.pdf",plot = plot_list[["N_Rel"]], device="pdf", height = 148, width = 210, unit="mm")





# Session Info ------------------------------------------------------------
sessionInfo()

# R version 4.5.1 (2025-06-13 ucrt)
# Platform: x86_64-w64-mingw32/x64
# Running under: Windows 11 x64 (build 26200)
# 
# Matrix products: default
#   LAPACK version 3.12.1
# 
# locale:
# [1] LC_COLLATE=German_Germany.utf8  LC_CTYPE=German_Germany.utf8    LC_MONETARY=German_Germany.utf8
# [4] LC_NUMERIC=C                    LC_TIME=German_Germany.utf8    
# 
# time zone: Europe/Berlin
# tzcode source: internal
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#  [1] ggpubr_0.6.1      ggh4x_0.3.1       scales_1.4.0      viridis_0.6.5     viridisLite_0.4.2 lubridate_1.9.4  
#  [7] forcats_1.0.0     stringr_1.5.1     dplyr_1.1.4       purrr_1.1.0       readr_2.1.5       tidyr_1.3.1      
# [13] tibble_3.3.0      ggplot2_3.5.2     tidyverse_2.0.0  
# 
# loaded via a namespace (and not attached):
#  [1] generics_0.1.4     rstatix_0.7.2      stringi_1.8.7      hms_1.1.3          magrittr_2.0.3    
#  [6] grid_4.5.1         timechange_0.3.0   RColorBrewer_1.1-3 backports_1.5.0    Formula_1.2-5     
# [11] gridExtra_2.3      textshaping_1.0.1  abind_1.4-8        cli_3.6.5          rlang_1.1.6       
# [16] cowplot_1.2.0      withr_3.0.2        tools_4.5.1        tzdb_0.5.0         ggsignif_0.6.4    
# [21] broom_1.0.9        vctrs_0.6.5        R6_2.6.1           lifecycle_1.0.4    car_3.1-3         
# [26] ragg_1.4.0         pkgconfig_2.0.3    pillar_1.11.0      gtable_0.3.6       glue_1.8.0        
# [31] systemfonts_1.2.3  tidyselect_1.2.1   rstudioapi_0.17.1  farver_2.1.2       carData_3.0-5     
# [36] svglite_2.2.1      labeling_0.4.3     compiler_4.5.1   