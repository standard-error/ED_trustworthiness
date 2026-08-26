###################################################################
#####      Estimating trait emotion differentiation:          #####
#####          How many measurement occasions and             #####
#####               emotion items are needed?                 #####
###################################################################

###################################################################
#####            Function to Calculate ICC                    #####
###################################################################

# two functions:
# function to calculate ICC and function to handle negative ICCs


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
          
          
          # If person had zero variance in emotion ratings,
          # their ratings are set to NA (in ordered_occasion_draw and random_occasion_draw function)
          # -> do not calculate ICC for these participants (would not work anyway)
          if (all(is.na(iccsubdat))) {
            return(cbind(id, NA_real_, NA_real_))
          }
          
          # In all other cases,
          # calculate ICC using the participant's data and ICC(3, 1) measuring consistency (default)
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
          
          ICC.z <- suppressWarnings(0.5 * log( (1 + (K_i - 1)*ICC) / (1 - ICC) ))
          # suppress warnings here -> NaN will be handled in one_simulation function!
          
          cbind(id, ICC, ICC.z) # return ID, ICC and ICC.z for each participant
          # t() transposes output from apply() to a two-dimensional matrix (3 cols, N rows)
          
        }))
  return(ICCdata) # return ICCdata matrix
}




# Create Function to Handle Negative ICCs ---------------------------------
handle_negative_iccs <- function(ICCdata,
                                 icc_col,
                                 icc.z_col,
                                 negative_icc_handling = c("keep", "set to zero", "exclude")) {
  # helper function to handle negative ICCs after calculation
  # ICCdata: matrix of person IDs, ICCs and transformed ICCs (ICC.z)
  # icc_col: character of the variable name of the ICC variable
  # icc.z_col: character of the variable name of the ICC.z variable
  # negative_icc_handling: argument specifying whether to keep negative ICCs, set them to 0 or to exclude them
  
  negative_icc_handling <- match.arg(negative_icc_handling, several.ok = FALSE)
  
  # sanity check:
  if (!all(c(icc_col, icc.z_col) %in% colnames(ICCdata))) {
    stop("icc_col or icc.z_col not found in ICCdata")
  }
  
  # create index of negative ICCs (rows in ICCdata)
  neg_icc_idx <- ICCdata[ , icc_col] < 0 & !is.na(ICCdata[ , icc_col])
  # if ICC < 0 and is not missing -> TRUE
  # neg_icc_idx with TRUE will be chosen to be manipulated
  
  
  if (negative_icc_handling == "exclude") { # if negative ICCs shall be excluded, set them to NA
    ICCdata[neg_icc_idx, icc_col] <- NA
    ICCdata[neg_icc_idx, icc.z_col] <- NA # also set transformed values to NA
    return(ICCdata) # return handled ICCdata set
    
  } else if (negative_icc_handling == "set to zero") {
    # else, if the negative ICCs shall be set to zero:

    # now set raw ICCs to 0 and determine Fisher's Z-transformed ICCs
    ICCdata[neg_icc_idx, icc_col] <- 0 # set the negative ICCs to 0 (by their index)
    ICCdata[neg_icc_idx, icc.z_col] <- 0
    # Fisher's Z-transformed values are as well zero:
    # formula for transformation: 0.5 * log( (1 + (K_i - 1)*ICC_i) / (1 - ICC_i) )
    # use 0 for ICC_i:
    # 0.5 * log( (1 + (K_i - 1)*0) / (1 - 0) ) # --> (K_i - 1)*0 = 0 
    # = 0.5 * log( (1 + 0) / (1 - 0) )
    # = 0.5 * log( 1 / 1 )
    # = 0.5 * log(1)
    # = 0.5 * 0 
    # = 0
    # regardless of K_i, the formula will result in zero if ICC_i = 0
    
    return(ICCdata) # return handled data set
    
  } else if (negative_icc_handling == "keep") {
    # if negative ICCs shall be kept, just return ICCdata
    return(ICCdata)
  }
  
}


