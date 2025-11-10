###################################################################
#####    Estimating trait negative emotion differentiation:   #####
#####        How many measurement occasions and               #####
#####              emotion items are needed?                  #####
###################################################################

###################################################################
#####            Function to Calculate ICC                    #####
###################################################################




# Create Function to Calculate ICC ----------------------------------------
# function to calculate intraclass correlation as a between-person variable

calculate_icc <- function(data,
                          id.var,
                          items,
                          type = "consistency",
                          unit = "single") { 
  # data: takes the data frame with all participants
          # and their occasions as input (long format)
          # needs to be data frame (not tibble):
          # data[ , id.var] extracts vector for data frames (length(ids) = number
          # of ids),
          # but column for tibbles (length(ids) would be 1)
  # id.var: character indicating name of participant ID variable
          # NOTE: ID needs to be numeric (matrices can only contain one
          # data type and ICC needs to be numeric)
  # items: character vector of specifying which emotion items
          # shall be used for the calculation (variable names
          # as used in data frame)
  # type: character indicating whether to use consistency ("consistency", default)
          # or absolute agreement ("agreement")
  # unit: character indicating whether to use single measurements ("single", default)
          # or average of k measurements ("average")
  
  # Note: Formula for Fisher's Z-transformation differs depending on whether you apply
  # it to a correlation or an intraclass correlation (see McGraw & Wong, 1996, Appendix B,
  # doi: 10.1037/1082-989X.1.1.30).
  # This function transforms the ICCs, but it does not check whether values are infinite 
  # (which is the case for ICC = 1 and ICC = -1/(K_i - 1)) or whether the transformation
  # cannot be calculated (which is the case for ICC > 1 and ICC < -1/(K_i - 1)).
  # The former case results in -Inf or Inf, the latter case in NaN (because the natural
  # logarithm is not defined for negative numbers).
  # Strictly speaking, the function is not defined for ICC <= -1/(K_i - 1)
  # and ICC >= 1. However, for ICC = -1/(K_i - 1) and ICC = 1,
  # R returns infinite values, as the function
  # tends towards -inf / inf at the boundaries of its domain.
  
  # This check is performed in the one_simulation_run()-function.
  

  # extract (unique) participant IDs as vector
  ids <- unique(data[ , id.var])
  # create empty matrix as storage for ICC data
  ICCdata <- matrix(NA, nrow = length(ids), ncol=3)
  colnames(ICCdata) <- c(id.var, "ICC", "ICC.z") # rename cols according to id.var, and to ICC and ICC.z
  # ICC: raw ICC
  # ICC.z: Fisher's Z-transformed ICC using the formula in Schneider & Junghaenel (2023)


  # use apply to calculate ICC for each participant
  ICCdata[] <- t(apply(matrix(ids), MARGIN = 1, 
        FUN = function(id) {
          # select all rows belonging to this participant and only the
          # relevant emotion items (indicated by items)
          iccsubdat <- data[which(data[ ,id.var] == id), items]
          
          # calculate ICC using the participant's data and ICC(3, 1) measuring consistency
          ICC <- irr::icc(iccsubdat, model="twoway", type=type,
                          unit = unit)$value 
          
          # Fisher's Z-transform the ICCs (according to formula in Schneider & Junghaenel, 2023)
          # -> transformed values needed for reliability estimation using I²
          # Table 1 (p. 3877) in Schneider & Junghaenel (2022):
          # sample estimator of emotion differentiation = 0.5 * log( (1 + (K_i - 1)*ICC_i) / (1 - ICC_i) )
          # log = natural logarithm (see Table 1, emotion variability
          # -> "natural logarithm" and then log is used in formula)
          # log() in R = natural logarithm
          # ICC_i = ICC for person i, here: ICC just calculated above
          # K_i = (average) number of items per occasion
          # -> calculate from length of item vector 
          K_i <- length(items)
          
          ICC.z <- 0.5 * log( (1 + (K_i - 1)*ICC) / (1 - ICC) )
          
          cbind(id, ICC, ICC.z) # return ID, ICC and ICC.z for each participant
          # t() transposes output from apply() to a two-dimensional matrix (3 cols, N rows)
          
        }))
  return(ICCdata) # return ICCdata matrix
}



