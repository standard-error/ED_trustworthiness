###################################################################
#####    Estimating trait negative emotion differentiation:   #####
#####        How many measurement occasions and               #####
#####              emotion items are needed?                  #####
###################################################################

###################################################################
#####            Data Visualization Function                  #####
###################################################################



# Load Packages -----------------------------------------------------------
library(ggplot2)
library(viridis) # for colors
library(scales) # for "pretty" breaks in y axis



# Define Theme ------------------------------------------------------------
my_theme <- theme_bw() +
  theme(panel.grid.minor = element_blank(),
        text = element_text(size=12),
        legend.text = element_text(size = 12))



# Function for Data Visualization (Overall) -------------------------------
plot_outcome <- function(data, ylims=NULL, ylabel=NULL, x_breaks = seq(0, 100, 10), theme_custom = my_theme,
                         groupwise = FALSE) {
  # data : data frame with the results
  # ylims: optional y-limit per outcome 
  # ylabel: optional y-axis label
  # x_breaks: breaks to use on x-axis, e.g., seq(0, 100, 10)
  # theme_custom : ggplot theme
  # groupwise: logical indicating whether or not the data contains high/medium/low NED group 
          # and if it should be plotted groupwise
  

  # Identify mean, min, max columns automatically from data frame
  col_mean <- grep("_mean$", names(data), value = TRUE)
  col_min  <- grep("_min$", names(data), value = TRUE)
  col_max  <- grep("_max$", names(data), value = TRUE)
  
  # read the outcome name from the last column in data (should be outcome_max)
  # last column = length(data)
  outcome_name <- sub("_max$", "", names(data)[length(data)])
  
  

  # Check if groupwise = TRUE
  # adjust facets accordingly
  # for groupwise = FALSE: facet only by occasions drawn
  # for groupwise = TRUE: also facet by NED group (after ordering by high, medium, low NED)
  if (groupwise == TRUE & !("group" %in% names(data))) {
    stop(sprintf("There is no grouping variable for NED group in data."))
  } else if (groupwise == TRUE & "group" %in% names(data)) {
    data[ , "group"] <- factor(data[ , "group"], levels = c("high NED", "medium NED", "low NED"))
    facet_formula <- facet_grid(rows = vars(group), cols = vars(occasions_drawn))
  } else if (groupwise == FALSE ) {
    facet_formula <- facet_wrap(~factor(occasions_drawn))
  }
  
  # Build plot
  p <- ggplot(data, aes(
      x = n_occasions, # x axis: n_occasions
      y = .data[[col_mean]], # y axis: mean outcome
      color = factor(n_items), # different lines for n_items
      shape = factor(n_items),
      linetype = factor(n_items),
      group = factor(n_items)
    )) +
    geom_point(position = position_dodge(width = 2)) +
    geom_line(linewidth = 0.3, position = position_dodge(width = 2)) +
    geom_errorbar(aes(ymin = .data[[col_min]], ymax = .data[[col_max]]),
                  position = position_dodge(width = 2)) + # error bar: min and max outcome
    scale_x_continuous(breaks = x_breaks) +
    xlab("Number of Occasions") +
    #  if y label is provided, use it; else, use the outcome name extracted from column names of data 
    ylab(ifelse(!is.null(ylabel), ylabel, outcome_name)) +
    labs(color = "Number of Items", shape = "Number of Items", linetype = "Number of Items") +
    facet_formula +
    scale_color_grey(start = 0.30, end = 0.00) +
    theme_custom
  
  if (!is.null(ylims)) {
    p <- p + scale_y_continuous(limits = ylims, breaks = scales::breaks_pretty(n = 5))
  }
  
  return(p)
  
}



# # test function
# load("results/aggregated_whole_data_set_Study1.rda")
# # extract examplary data
# dat <- agg[["cor_ICC"]][["agg_res"]]
# 
# plot_outcome(data=dat, ylims = c(-0.20, 1.0), ylabel = NULL, x_breaks = seq(0,100,10), theme_custom = my_theme,
#              groupwise = FALSE)
# rm(dat, agg)
# 
# load("results/aggregated_subgroups_Study1.rda")
# dat <- agg_grp[["cor_ICC"]][["agg_res"]]
# plot_outcome(data=dat, ylims=c(-0.20, 1.0), ylabel=NULL, x_breaks = seq(0,100,10), theme_custom=my_theme,
#              groupwise = TRUE)


# # Notes (plot without function)
# test <-
#   ggplot(data = dat) +
#   aes(x = n_occasions , y = cor_ICC_mean) +
#   geom_point(aes(shape=factor(n_items), color=factor(n_items)), position=position_dodge(width=2)) +
#   ylim(-0.20, 1.0) +
#   scale_x_continuous(breaks = seq(0, 100, 10)) +
#   geom_line(aes(color=factor(n_items)), linewidth=0.3) +
#   geom_errorbar(aes(x = n_occasions, ymin = cor_ICC_min, ymax  = cor_ICC_max,
#                     color = factor(n_items)), position=position_dodge(width=2)) +
#   labs(shape = "number of items", color = "number of items") +
#   xlab("Number of Occasions") +
#   ylab("Outcome") +
#   facet_wrap(~factor(occasions_drawn)) +
#   scale_color_viridis_d(option="magma", begin = 0.20, end=0.80) +
#   my_theme
#
# test



# # Note: Group as additional grouping variable, but not faceted
# plot_outcome <- function(data, ylims=NULL, ylabel=NULL, x_breaks = seq(0, 100, 10), theme_custom = my_theme,
#                          groupwise = FALSE) {
#   # data : data frame with the results
#   # ylims: optional y-limit per outcome 
#   # ylabel: optional y-axis label
#   # x_breaks: breaks to use on x-axis, e.g., seq(0, 100, 10)
#   # theme_custom : ggplot theme
#   # groupwise: logical indicating whether or not the data contains high/medium/low NED group 
#   # and if it should be plotted groupwise
#   
#   
#   # Identify mean, min, max columns automatically from data frame
#   col_mean <- grep("_mean$", names(data), value = TRUE)
#   col_min  <- grep("_min$", names(data), value = TRUE)
#   col_max  <- grep("_max$", names(data), value = TRUE)
#   
#   # read the outcome name from the last column in data (should be outcome_max)
#   # last column = length(data)
#   outcome_name <- sub("_max$", "", names(data)[length(data)])
#   
#   
#   # Workaround so that we have a title above "random" and "by order" for occasions
#   # drawn as split by facet_wrap
#   # -> however, do not use facet_wrap but ggh4x::facet_nested, so that the 
#   # title spans across both plots
#   data[ , "facet_group"] <- "Occasions Drawn"
#   
#   
#   
#   # Check if groupwise = TRUE
#   if (groupwise == TRUE & "group" %in% names(data)) {
#     aes_settings <- aes(
#       x = n_occasions,
#       y = .data[[col_mean]],
#       color = factor(group), #  use both group and n_items as grouping variables
#       shape = factor(n_items),
#       group = interaction(group, n_items) # "interaction" between groups and items
#       # -> each group with each number of items
#     )
#     legend.labels <- labs(color = "NED Group", shape = "Number of Items")
#   } else if (groupwise == TRUE & !("group" %in% names(data))) {
#     stop(sprintf("There is no grouping variable for NED group in data."))
#   } else if (groupwise == FALSE ) {
#     aes_settings <- aes(
#       x = n_occasions,
#       y = .data[[col_mean]],
#       color = factor(n_items),
#       shape = factor(n_items),
#       group = factor(n_items)
#     ) 
#     legend.labels <- labs(color = "Number of Items", shape = "Number of Items")
#   }
#   
#   # Build plot
#   p <- ggplot(data, aes_settings) +
#     geom_point(position = position_dodge(width = 2)) +
#     geom_line(linewidth = 0.3) +
#     geom_errorbar(aes(ymin = .data[[col_min]], ymax = .data[[col_max]]),
#                   position = position_dodge(width = 2)) +
#     scale_x_continuous(breaks = x_breaks) +
#     xlab("Number of Occasions") +
#     #  if y label is provided, use it; else, use the outcome name extracted from column names of data 
#     ylab(ifelse(!is.null(ylabel), ylabel, outcome_name)) +
#     legend.labels +
#     ggh4x::facet_nested(~facet_group + factor(occasions_drawn)) +
#     scale_color_viridis_d(option = "magma", begin = 0.20, end = 0.80) +
#     theme_custom
#   
#   if (!is.null(ylims)) {
#     p <- p + scale_y_continuous(limits = ylims, breaks = scales::breaks_pretty(n = 5))
#   }
#   
#   return(p)
#   
# }
