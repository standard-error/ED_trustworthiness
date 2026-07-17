###################################################################
#####       Estimating trait emotion differentiation:         #####
#####          How many measurement occasions and             #####
#####               emotion items are needed?                 #####
###################################################################

###################################################################
#####          Function to Draw Occasions By Order            #####
###################################################################



# Create Function for Ordered Draws of Occasions --------------------------

ordered_occasion_draw <- function(data, id.var, occ.running.var, nr.of.occasions, items) {
  # data: takes the data frame with all participants
          # and their occasions as input (long format)
  # id.var: character that indicates name of participant ID variable
  # occ.running.var: character that indicates the name of the occasion running variable
  # nr.of.occasions: number of occasions to draw per participant by order
  # items: character vector with emotion item variable names that will later be used for calculation of ICC
    # used to check potential variance
  
  
  # control sequence: if the number of occasions is greater than the number of occasions
  # per participant in the data set, print a warning message
  occ_per_part <- sapply(split(data, data[ , id.var]), nrow)
  
  if (any(occ_per_part < nr.of.occasions)) {
   print("Error in ordered_occasions_draw: nr.of.occasions is greater than the number of occasions per participant for at least one participant.")
    drawn_data <- data.frame(matrix(NA, ncol=ncol(data), nrow=nrow(data)))
    names(drawn_data) <- names(data)
    
   return(list(
     drawn_data = drawn_data, # return an empty data frame (NA) of size of input df,
     n_total_persons = length(unique(data[ , id.var])),
     n_valid_persons = NA_integer_,
     n_skipped_persons = NA_integer_,
     draw_log = NA
   )
   )  
  }
  
  ids <- unique(data[ , id.var])
  
  drawn_all <- lapply(ids, function(id_part) { # for each participant in ID vector
    
    participant_data <- data[data[ , id.var] == id_part, ] # use participant data
    
    participant_sub <- participant_data[
      participant_data[ , occ.running.var] <= nr.of.occasions, # select subset with given number of occasions
    ]
    
    # Check variance in emotion ratings
    has_variance <- !all(var(participant_sub[, items])==0) # has_variance = TRUE, if at least one variance != 0 (not ALL variance == 0)
    
    # If participant does not have variance in the emotion ratings across occasions,
    # set their ratings to NA (will be handled later)
    # -> all participants included in all simulation runs (same number and order)
    # -> but not all will contribute valid ICCs
    if (has_variance == FALSE) {
      participant_sub[ , items] <- NA # set emotion item ratings to NA if there is zero variance
    }
    
    
    list(
      drawn_data_sub = participant_sub, 
      skipped = !has_variance, # skipped = TRUE, if has_variance = FALSE (!FALSE = TRUE), skipped = FALSE if has_variance = TRUE (!TRUE = FALSE)
      skip_reason = if (has_variance == FALSE) {
        "no_variance_in_given_item_set_across_occasions"
        } else {
          NA_character_ # if there is no variance (has_variance == FALSE), skip reason is no variance, else, skip reason NA
          } 
    )
  })
  
  # draw_log:
  draw_log <- data.frame(
    id = ids,
    skipped = sapply(drawn_all, `[[`, "skipped"), # read whether participants were skipped
    skip_reason = sapply(drawn_all, `[[`, "skip_reason") # read whether there was a skip reason
  )
  
  # extract data
  drawn_list <- lapply(drawn_all, `[[`, "drawn_data_sub") # extract data for each participant, store in a list
  
  # combine data into single data frame
  drawn_data <- do.call(rbind, drawn_list) # combine data from list in a data frame
  drawn_data <- drawn_data[order(drawn_data[ , id.var], drawn_data[ , occ.running.var]), ] # sort data by participant and occ_running
  rownames(drawn_data) <- NULL # reset row numbers

  
  # Extract summary statistics
  # read out number of skipped persons etc.
  n_total_persons <- length(ids) # number of unique participants in overall data
  n_skipped_persons <- sum(draw_log$skipped, na.rm=TRUE) # number of participants who were skipped / have NAs
  n_valid_persons <- n_total_persons - n_skipped_persons # number of participants with valid data
  
  total_redraws <- 0
  # to be consistent with random occasion draw, here we do not have any redraws
  
  # return data frame and number of skipped persons etc. in a list
  return(list(
    drawn_data = drawn_data,
    total_redraws = total_redraws, 
    n_total_persons = n_total_persons,
    n_valid_persons = n_valid_persons,
    n_skipped_persons = n_skipped_persons,
    draw_log = draw_log
  ))
}




