###################################################################
#####    Estimating trait negative emotion differentiation:   #####
#####        How many measurement occasions and               #####
#####              emotion items are needed?                  #####
###################################################################

###################################################################
#####          Function to Draw Occasions Randomly            #####
###################################################################


# Note: For the ordered draw, the items argument is not necessary
# because the items will be selected in the function to calculate
# the ICCs (i.e., the data frame will be subsetted later). 
# In contrast, the items argument is needed in the random draw
# function because it checks whether there is variance in the 
# emotion ratings so that an ICC can be calculated. However,
# the subsetting (i.e., selection of relevant variables)
# is done in the ICC calculation function.



# Create Function for One Participant (INNER Function) --------------------

draw_for_participant <- function(data, id.var, id.part, nr.of.occasions, items) {
  # data: data frame of all participants with their measurement occasions (long format)
  # id.var: character indicating name of ID variable
  # id.part: indicating id of single participant for whom occasions should be drawn
  # nr.of.occasions: number of occasions to draw randomly
  # items: character vector with emotion item variable names that will later be used for calculation of ICC
  
  # Draw data of participant
  participant_data <- data[data[ , id.var] == id.part, ]
  
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
  return(list(drawn_data_sub = random_sub, # store randomly drawn data
              redraws = j)) # and store number of re-draws for this participant in list

}



# # Test function
# sim <- data.frame(SERIAL = rep(1, times=6),
#                   aerger1 = rep(c(0,1), each=3),
#                   aerger2 = rep(c(1,0), each=3))
# 
# test <- draw_for_participant(data=sim, id.var="SERIAL", id.part = 1,nr.of.occasions = 2, items = c("aerger1", "aerger2"))




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
    print("Error in ordered_occasions_draw: nr.of.occasions is greater than the number of occasions per participant for at least one participant.")
    drawn_data <- data.frame(matrix(NA, ncol=ncol(data), nrow=nrow(data)))
    names(drawn_data) <- names(data)
    return(list(drawn_data = drawn_data,
                total_redraws = NA)) # return list of an empty data frame (NA) of size of input df and of number of re-draws (NA)
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

  # extract drawn data
  drawn_list <- lapply(drawn_all, `[[`, "drawn_data_sub") # extract drawn data per participant
  
  # Combine results from list into a single data frame
  drawn_data <- do.call(rbind, drawn_list)
  # Order data frame
  drawn_data <- drawn_data[order(drawn_data[ , id.var], drawn_data[ , occ.running.var]), ]
  # Reset row values
  rownames(drawn_data) <- NULL
  
  
  # sum of re-draws across all participants
  total_redraws <- sum(sapply(drawn_all, `[[`, "redraws")) # extract redraws per participant and sum these
  
  # Return drawn data and re-draws as a list
  return(list(drawn_data = drawn_data,
              total_redraws = total_redraws))

}
