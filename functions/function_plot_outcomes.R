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
library(ggh4x) # for facet_manual


# Define Theme ------------------------------------------------------------
my_theme <- theme_bw() +
  theme(panel.grid.minor = element_blank(),
        text = element_text(size=12),
        legend.text = element_text(size = 12)) 




# Function for Data Visualization (Overall) -------------------------------
plot_outcome <- function(data, ylims=NULL, ylabel=NULL, x_breaks = seq(0, 70, 10), theme_custom = my_theme,
                         dodge_width = 2,
                         scale_color = scale_color_grey(start = 0.45, end = 0.00),
                         groupwise = FALSE, split_facets = FALSE,
                         facet_var = "occasions_drawn",
                         facet_order = c("random", "by order")) {
  # data : data frame with the results
  # ylims: optional y-limit per outcome 
  # ylabel: optional y-axis label
  # x_breaks: breaks to use on x-axis, e.g., seq(0, 100, 10)
  # theme_custom : ggplot theme
  # scale_color: define color theme
  # dodge_width: argument indicating how much to jitter points from different grouping variables
  # groupwise: logical indicating whether or not the data contains high/medium/low NED group 
          # and if it should be plotted groupwise
  # split_facets: logical indicating whether the two facets (occasions_drawn) should be
          # plotted in one plot or in separate plots
  # facet_var: chr indicating name of the facet variable to split by
  # facet_order: chr defining order of the facet levels so that order is the same across plots
  

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
    facet_formula <- ggh4x::facet_manual(~factor(occasions_drawn), design=matrix(c(1,2), nrow=1, ncol=2, byrow=TRUE), drop=FALSE)
      # do not drop unused factor levels
  }
  
  # Build function for base plot
  base_plot <- function(data) {
  p <- ggplot(data, aes(
      x = n_occasions, # x axis: n_occasions
      y = .data[[col_mean]], # y axis: mean outcome
      color = factor(n_items), # different lines for n_items
      shape = factor(n_items),
      linetype = factor(n_items),
      group = factor(n_items)
    )) +
    geom_point(position = position_dodge(width = dodge_width)) +
    geom_line(linewidth = 0.3, position = position_dodge(width = dodge_width)) +
    geom_errorbar(aes(ymin = .data[[col_min]], ymax = .data[[col_max]]),
                  position = position_dodge(width = dodge_width),
                  linewidth = 0.3) + # error bar: min and max outcome
    scale_x_continuous(breaks = x_breaks) +
    expand_limits(x = 70) + # make sure that 70 is always covered
    xlab("Number of Occasions") +
    #  if y label is provided, use it; else, use the outcome name extracted from column names of data 
    ylab(ifelse(!is.null(ylabel), ylabel, outcome_name)) +
    scale_color +
    labs(color = "Number of Items", shape = "Number of Items", linetype = "Number of Items") +
    guides(color = guide_legend(title = "Number of Items"),
           shape = guide_legend(title = "Number of Items"),
           linetype = guide_legend(title = "Number of Items")) +
    theme_custom
  
  if (!is.null(ylims)) {
    p <- p + scale_y_continuous(limits = ylims, breaks = scales::breaks_pretty(n = 5), labels = function(x) {
      ifelse(x < 0, sprintf("%6.2f", x), sprintf(" %6.2f", x))
    }
    )
  }
  
  return(p)
  }
  
  
  # Plot according to split_facet == TRUE or FALSE
  if (split_facets == FALSE) {
    return(base_plot(data) + facet_formula+
             force_panelsizes(rows=1, cols=c(1,1)))
  } else if (split_facets == TRUE) {
    
    split_plots <- lapply(facet_order, # apply to each unique facet of the facet_var to split by
                          function(facet) { # function of facet
                            data_sub <- data[which(data[ , facet_var] == facet), ] # subset data according to facet
                            p <- base_plot(data_sub)
                            p <- p + ggtitle(paste0("Occasions drawn: ", facet))
                            return(p)
                          })
    return(split_plots)
    
  }
  
}


