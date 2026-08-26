###################################################################
#####       Estimating trait emotion differentiation:         #####
#####          How many measurement occasions and             #####
#####               emotion items are needed?                 #####
###################################################################

###################################################################
#####          Function to Draw Occasions Randomly            #####
###################################################################



# Create Function for One Participant (INNER Function) --------------------

draw_for_participant <- function(data, id.var, id.part, nr.of.occasions, items) {
  # data: data frame of all participants with their measurement occasions (long format)
  # id.var: character indicating name of ID variable
  # id.part: indicating id of single participant for whom occasions should be drawn
  # nr.of.occasions: number of occasions to draw randomly
  # items: character vector with emotion item variable names that will later be used for calculation of ICC
  # occ.running.var: character indicating name of running occasions variable
  
  # Draw data of participant
  participant_data <- data[data[ , id.var] == id.part, ]
  
  
  # Check: Does the person have NO variance on given item set across ALL occasions?
  # -> then, redrawing occasions will do nothing if there is no variance overall
  # check these cases and save as outcome
  has_possible_variance <- !all(var(participant_data[, items])== 0)
  
  if (has_possible_variance != TRUE) { # if there is no possible variance
    
    # create empty data for this person (NAs), with random occasions -> does not make any difference though
    na_data <- participant_data[sample(1:nrow(participant_data),
                                       nr.of.occasions, replace = FALSE), , drop=FALSE]
    # create empty data frame of the specified number of occasions and all variables
    # -> same dimensions as other participants will have
    
    
    na_data[ , items] <- NA # set all emotion variables as NA
    
    
    return(list(
      drawn_data_sub = na_data, # return NA data and store skipping reason
      redraws = NA_integer_,
      skipped = TRUE,
      skip_reason = "no_variance_in_given_item_set_across_ALL_occasions"
    ))
  }


  # Initialize draw
  random_sub <- participant_data[sample(1:nrow(participant_data),
                                        nr.of.occasions, replace = FALSE), ]
  # draw nr.of.occasions occasions randomly from all occasions of this participant without replacing (occasions can only be drawn once)
  
  j <- 0 # so far, we have zero re-draws 
  # Repeat drawing as long as variance in emotion ratings is 0
  while (all(var(random_sub[, items])== 0)) {
    # draw again
    random_sub <- participant_data[sample(1:nrow(participant_data),
                                          nr.of.occasions, replace = FALSE), ]
    j <- j + 1
    print(paste0("re-draw nr. ", j))
    
  }
  return(list(
    drawn_data_sub = random_sub, # store randomly drawn data
    redraws = j, # and store number of re-draws for this participant in list
    skipped = FALSE, # participant was not skipped (overall, there was possible variance and re-draws went on until variance was reached)
    skip_reason = NA_character_ # no reason for skipping
    ))

}






# Create Function for All Participants (OUTER Function) -------------------
random_occasion_draw <- function(data, id.var, occ.running.var, nr.of.occasions, items) {
  # data: takes the data frame with all participants
          # and their occasions as input (long format)
  # id.var: character that indicates name of participant ID variable
  # occ.running.var: character that indicates the name of the occasion running variable
  # nr.of.occasions: number of occasions to draw per participant randomly
  # items: character vector indicating the names of emotion items that will later be
           # used for calcuation of ICCs (here: checked whether there is variance)

  
  # control sequence: if the number of occasions to draw is greater than the number of occasions
  # per participant in the data set, print a warning message
  occ_per_part <- sapply(split(data, data[ , id.var]), nrow)
  
  if (any(occ_per_part < nr.of.occasions)) {
    stop("Error in random_occasions_draw: nr.of.occasions is greater than the number of occasions per participant for at least one participant.")
  }
  
  
  # extract unique participant IDs from id.var
  ids <- unique(data[ , id.var])

  # Use lapply over all participant IDs to apply the drawing per participant function
  # NOTE: pass the arguments for the inner function (draw for participant) here
  # The ID list, the function is applied to, should be inserted in id.part -> the ID
  # per participant -> The draw_for_participant function is applied to every ID in the
  # ids vector, and each of these IDs is plugged into id.part
  drawn_all <- lapply(ids, 
                       FUN = function(x) draw_for_participant(data = data,
                                                              id.var = id.var,
                                                              id.part = x,
                                                              nr.of.occasions = nr.of.occasions,
                                                              items = items))
  # results are stored in a list (both data and number of re-draws of occasions)
  
  draw_log <- data.frame( # read out the ids, whether persons were skipped, the skip reason and the number of redraws
    id = ids,
    skipped = sapply(drawn_all, `[[`, "skipped"),
    skip_reason = sapply(drawn_all, `[[`, "skip_reason"),
    redraws = sapply(drawn_all, `[[`, "redraws")
  )

  # extract drawn data
  drawn_list <- lapply(drawn_all, `[[`, "drawn_data_sub") # extract drawn data per participant
  # participants may still have NAs in items -> keep them so that sample is consistent across all simulation runs
  
  # Combine results from list into a single data frame
  drawn_data <- do.call(rbind, drawn_list)
  # Order data frame
  drawn_data <- drawn_data[order(drawn_data[ , id.var], drawn_data[ , occ.running.var]), ]
  # Reset row values
  rownames(drawn_data) <- NULL
  
  # Summary statistics
  n_total_persons <- length(ids) # number of unique IDs in data frame
  n_skipped_persons <- sum(draw_log$skipped, na.rm=TRUE) # number of participants who were skipped / have NAs
  n_valid_persons <- n_total_persons - n_skipped_persons # number of pariticpants with valid data
  
  # sum of re-draws across all participants
  total_redraws <- sum(draw_log$redraws, na.rm=TRUE) # extract redraws per participant and sum these
  
  # Return drawn data and re-draws as a list
  return(list(drawn_data = drawn_data,
              total_redraws = total_redraws,
              n_total_persons = n_total_persons,
              n_valid_persons = n_valid_persons,
              n_skipped_persons = n_skipped_persons,
              draw_log = draw_log))

}

