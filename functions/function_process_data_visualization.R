###################################################################
#####       Estimating trait emotion differentiation:         #####
#####          How many measurement occasions and             #####
#####               emotion items are needed?                 #####
###################################################################

###################################################################
#####       Function: Visualize Simulation Results            #####
###################################################################


# Source Function ---------------------------------------------------------
source("functions/function_plot_outcomes.R")



# Load Packages -----------------------------------------------------------
library(ggpubr)
library(tidyverse) # reshape data (for participant-level plots)
# ggplot is loaded when sourcing the function



# Define Outcome Labels ---------------------------------------------------
get_outcome_labels <- function() {
  list(
    cor_ICC = "Correlation with Benchmark",
    cor_ICC.z = "Correlation with Benchmark (ICC.z)",
    person_diff = "Difference in ICCs to Benchmark",
    person_diff.z = "Difference in ICCs to Benchmark (ICC.z)",
    RMSE_ICC = "RMSE",
    RMSE_ICC.z = "RMSE (ICC.z)",
    sd_ICC = "SD of ICCs",
    sd_ICC.z = "SD of ICC.z",
    rel = "Reliability of ICCs",
    percnegICC_raw = "Proportion of Negative ICCs (Before Negative ICC Treatment)",
    estimProbNeg_raw = "Number of Estimation Problems (Negative)",
    estimProbPos_raw = "Number of Estimation Problems (Positive)",
    n_skipped_persons_var = "Number of Participants Skipped Due to Lack of Variance",
    N_valid_ICC.z_handled = "Number of Valid ICC.z",
    N_rel = "Number of ICCs for Reliability",
    N_merged_ICC_handled = "Number of ICCs (After Negative ICC Treatment)",
    N_cor_ICC = "Number of ICCs for Correlation",
    N_cor_ICC.z = "Number of ICC.z for Correlation",
    RMSE_ICC_N = "Number of Replications Used for RMSE Calculation (ICC)",
    RMSE_ICC.z_N = "Number of Replications Used for RMSE Calculation (ICC.z)"
  )
}



# Y-Axis Limits -----------------------------------------------------------

get_ylim_list <- function() {
  list(
    cor_ICC = c(0, 1),  # for some outcomes, use theoretical range
    cor_ICC.z = c(0, 1),
    rel = c(0, 1),
    percnegICC_raw = c(0, 1),

    
    RMSE_ICC = c(0, NA), # no upper limits for rmse, sd, ... but lower limit of 0
    RMSE_ICC.z = c(0, NA),
    sd_ICC = c(0, NA),
    sd_ICC.z = c(0, NA),
    n_skipped_persons_var = c(0, NA),
    N_valid_ICC.z_handled = c(0, NA),
    N_rel = c(0, NA),
    N_merged_ICC_handled = c(0, NA),
    N_cor_ICC = c(0, NA),
    N_cor_ICC.z = c(0, NA),
    RMSE_ICC_N = c(0, NA),
    RMSE_ICC.z_N = c(0,NA),

    person_diff = c(NA, NA), # handle some outcomes most flexibly (diff, count variables, ...)
    person_diff.z = c(NA, NA),
    estimProbNeg_raw = c(NA, NA),
    estimProbPos_raw = c(NA, NA)
    
  )
}




# Extract Outcomes --------------------------------------------------------

extract_outcomes <- function(agg) {
  
  data_list <- list(
    
    ## CORRELATION
    cor_ICC = agg[["cor_ICC"]][["agg_res"]],
    cor_ICC.z = agg[["cor_ICC.z"]][["agg_res"]],
    
    
    ## DIFFERENCE IN ICC
    # for ICC
    person_diff = agg[["person_diff"]][["agg_res"]],
    # for ICC.z
    person_diff.z = agg[["person_diff.z"]][["agg_res"]],
    
    
    ## RMSE
    RMSE_ICC = agg[["RMSE_ICC"]][["agg_res"]],
    RMSE_ICC.z = agg[["RMSE_ICC.z"]][["agg_res"]],

    
    ## SD
    sd_ICC = agg[["sd_ICC"]][["agg_res"]],
    sd_ICC.z = agg[["sd_ICC.z"]][["agg_res"]],
    
    ## RELIABILITY
    rel = agg[["rel"]][["agg_res"]],
    
    
    ## PROPORTION OF NUMBER OF NEGATIVE ICCS
    percnegICC_raw = agg[["percnegICC_raw"]][["agg_res"]],
    
    
    ## ESTIMATION PROBLEMS
    estimProbNeg_raw = agg[["estimationProbNeg_raw"]][["agg_res"]],
    estimProbPos_raw = agg[["estimationProbPos_raw"]][["agg_res"]],
    
    ## VALID VALUES
    n_skipped_persons_var = agg[["n_skipped_persons_var"]][["agg_res"]],
    N_valid_ICC.z_handled = agg[["N_valid_ICC.z_handled"]][["agg_res"]],
    N_rel = agg[["N_rel"]][["agg_res"]],
    N_merged_ICC_handled = agg[["N_merged_ICC_handled"]][["agg_res"]],
    N_cor_ICC = agg[["N_cor_ICC"]][["agg_res"]],
    N_cor_ICC.z = agg[["N_cor_ICC.z"]][["agg_res"]],
    RMSE_ICC_N = agg[["RMSE_ICC_N"]][["agg_res"]],
    RMSE_ICC.z_N = agg[["RMSE_ICC.z_N"]][["agg_res"]]
    
  )
  
  
  # for RMSE, only plot values for random draws -> set by order to NA
  data_list$RMSE_ICC <- data_list$RMSE_ICC[which(data_list$RMSE_ICC$occasions_drawn == "random"), ]
  data_list$RMSE_ICC.z <- data_list$RMSE_ICC.z[which(data_list$RMSE_ICC.z$occasions_drawn == "random"), ]
  
  data_list
  
  
}




# Save Plot Function (Multiple Formats) -----------------------------------
save_plot_formats <- function(plot,
                              filename,
                              plot_dir = "plots",
                              width = 210,
                              height = 148,
                              formats = c("pdf", "svg", "tiff")) {

  dir.create(plot_dir, recursive=TRUE, showWarnings=FALSE) # create folder if it does not yet exist
  
  if ("pdf" %in% formats) {
    ggsave(
      filename = file.path(plot_dir, paste0(filename, ".pdf")),
      plot = plot,
      device = "pdf",
      width = width,
      height = height,
      unit = "mm"
    )
  }
  
  if ("svg" %in% formats) {
    ggsave(
      filename = file.path(plot_dir, paste0(filename, ".svg")),
      plot = plot,
      device = "svg",
      width = width,
      height = height,
      unit = "mm"
    )
  }
  
  if ("tiff" %in% formats) {
    tiff(
      filename = file.path(plot_dir, paste0(filename, ".tiff")),
      units = "mm",
      width = width,
      height = height,
      res = 1200
    )
    print(plot)
    dev.off()
  }
}
  



# Create Outcome Plots ----------------------------------------------------

make_outcome_plots <- function(data_list,
                               ylim_list = get_ylim_list(),
                               x_breaks = seq(0, 70, 10),
                               dodge_width = 3,
                               split_facets = FALSE,
                               scale_color = scale_color_grey(start = 0.45, end = 0.00)) {
  
  ylabels <- get_outcome_labels()
  
  plot_list <- lapply(names(data_list), function(outcome) {
    
    ylims <- NULL
    
    if (!is.null(ylim_list) && outcome %in% names(ylim_list)) {
      ylims <- ylim_list[[outcome]]
    }
    
    
    plot_outcome(
      data = data_list[[outcome]],
      ylabel = ylabels[[outcome]],
      ylims = ylims,
      x_breaks = x_breaks,
      theme_custom = my_theme,
      dodge_width = dodge_width,
      scale_color = scale_color,
      split_facets = split_facets
    )
    
  })
  
  names(plot_list) <- names(data_list)
  
  plot_list
  
}




# Build Results Tables ----------------------------------------------------

make_results_table <- function(data_list, z_transformed = FALSE) {
  
  if (z_transformed == TRUE) {
    selected <- data_list[c("cor_ICC.z", "person_diff.z", "RMSE_ICC.z", "sd_ICC.z", "rel",
                            "percnegICC_raw", "estimProbNeg_raw", "estimProbPos_raw",
                            "N_rel", "N_cor_ICC.z", "N_valid_ICC.z_handled",
                            "n_skipped_persons_var", "RMSE_ICC.z_N"
                            )]
  } else {
    selected <- data_list[c("cor_ICC", "person_diff", "RMSE_ICC", "sd_ICC", "rel",
                            "percnegICC_raw", "estimProbNeg_raw", "estimProbPos_raw",
                            "N_rel", "N_cor_ICC", "N_merged_ICC_handled",
                            "n_skipped_persons_var", "RMSE_ICC_N"
    )]
  }
  
  all_results <- purrr::reduce(
    selected,
    dplyr::full_join,
    by = c("occasions_drawn", "n_occasions", "n_items")
  )
  
  # round  
  all_results[ , 4:ncol(all_results)] <- round(all_results[ , 4:ncol(all_results)], 3)
  # sort
  all_results <- all_results[order(all_results$occasions_drawn, all_results$n_occasions, all_results$n_items), ]
  # reset row names
  rownames(all_results) <- NULL

  all_results
}



# Wrapper Function --------------------------------------------------------
visualize_simulation_results <- function(results_dir,
                                         plot_dir,
                                         agg_file,
                                         agg_object = "agg",
                                         x_breaks = seq(0,70,10),
                                         formats = c("pdf", "svg")) {
  
  dir.create(plot_dir, recursive=TRUE, showWarnings = FALSE)
  
  # Load aggregated results
  env <- new.env()
  load(file.path(results_dir, agg_file), envir = env)
  
  agg <- env[[agg_object]]
  
  if (is.null(agg)) {
    stop(sprintf("object '%s' not found in '%s'.", agg_object, agg_file))
  }
 
  
  # Extract outcomes
  data_list <- extract_outcomes(agg)
  
  # Single Outcome Plots
  plot_list <- make_outcome_plots(
    data_list = data_list,
    x_breaks = x_breaks,
    dodge_width = 3,
    split_facets = FALSE,
    ylim_list = get_ylim_list()
  )
  
  # save single plots:
  for (outcome in names(plot_list)) {
    save_plot_formats(
      plot = plot_list[[outcome]],
      filename = paste0("plot_", outcome),
      plot_dir = plot_dir,
      formats = formats
    )
  }
  
  
  # Results tables
  table_raw <- make_results_table(data_list, z_transformed = FALSE)
  table_z <- make_results_table(data_list, z_transformed = TRUE)
 
  write.csv(table_raw, 
            file.path(results_dir, "results_table.csv"),
            row.names = FALSE)
 
  write.csv(table_z, 
            file.path(results_dir, "results_table_Z_transformed.csv"),
            row.names = FALSE)
  
  
  # Return
  invisible(
    list(
      data_list = data_list,
      plot_list = plot_list,
      table_raw = table_raw,
      table_z = table_z
    )
  )
   
}

