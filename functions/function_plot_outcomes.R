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
library(ggh4x) # for facet_nested (instead of facet_wrap)
library(scales) # for "pretty" breaks in y axis



# Define Theme ------------------------------------------------------------
my_theme <- theme_bw() +
  theme(panel.grid.minor = element_blank(),
        text = element_text(size=12))


# Write Function for Data Visualization -----------------------------------
plot_outcome <- function(data, ylims=NULL, ylabel=NULL, theme_custom = my_theme) {
  # data : data frame with the results
  # ylims: optional y-limit per outcome 
  # ylabel: optional y-axis label 
  # theme_custom : ggplot theme
  

  # Identify mean, min, max columns automatically from data frame
  col_mean <- grep("_mean$", names(data), value = TRUE)
  col_min  <- grep("_min$", names(data), value = TRUE)
  col_max  <- grep("_max$", names(data), value = TRUE)
  
  # read the outcome name from the last column in data (should be outcome_max)
  # last column = length(data)
  outcome_name <- sub("_max$", "", names(data)[length(data)])
  
  
  # Workaround so that we have a title above "random" and "by order" for occasions
  # drawn as split by facet_wrap
  # -> however, do not use facet_wrap but ggh4x::facet_nested, so that the 
  # title spans across both plots
  data[ , "facet_group"] <- "Occasions Drawn"
  
  
  # plot the data
  p <- ggplot(data, aes(x = n_occasions, y = .data[[col_mean]])) +
    geom_point(aes(shape = factor(n_items), color = factor(n_items)),
               position = position_dodge(width = 2)) +
    geom_line(aes(color = factor(n_items)), linewidth = 0.3) +
    geom_errorbar(aes(ymin = .data[[col_min]], ymax = .data[[col_max]],
                      color = factor(n_items)),
                  position = position_dodge(width = 2)) +
    scale_x_continuous(breaks = seq(0, 100, 10)) +
    labs(shape = "Number of Items", color = "Number of Items") +
    xlab("Number of Occasions") +
    #  if y label is provided, use it; else, use the outcome name extracted from column names of data 
    ylab(ifelse(!is.null(ylabel), ylabel, outcome_name)) +
    ggh4x::facet_nested(~facet_group + factor(occasions_drawn)) +
    scale_color_viridis_d(option = "magma", begin = 0.20, end = 0.80) +
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
# plot_outcome(data=dat, ylims = c(-0.20, 1.0), ylabel = NULL, theme_custom = my_theme)
# rm(dat, agg)


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




