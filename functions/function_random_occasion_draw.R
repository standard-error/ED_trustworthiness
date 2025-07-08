###################################################################
#####    Estimating trait negative emotion differentiation:   #####
#####        How many measurement occasions and               #####
#####              emotion items are needed?                  #####
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
  
  # Draw data of participant
  participant_data <- data[data[ , id.var] == id.part, ]
  
  # Initialize draw
  random_sub <- participant_data[sample(1:nrow(participant_data),
                                        nr.of.occasions, replace = FALSE), ]
  # draw nr.of.occasions occasions randomly from all occasions of this participant without replacing (occasions can only be drawn once)
  
  j <- 1
  # Repeat drawing as long as variance in emotion ratings is 0
  while (all(var(random_sub[, items])== 0)) {
    # print message
    message(paste0("Random draw of occasions yielded ratings' variances of 0, ICC not estimable - repeating draw. Repetitition nr.: ", j))
    # draw again
    random_sub <- participant_data[sample(1:nrow(participant_data),
                                          nr.of.occasions, replace = FALSE), ]
    j <- j+ 1
    
  }
  return(random_sub)

}


# # test function
# # create data
# df <- data.frame(ID = rep(LETTERS[1:5], each=10), # 5 participants with 10 occasions each
#                  occ_running = rep(1:10, times=5),
#                  val1 = rnorm(50, mean=0, sd=1), # two random variables
#                  val2 = rnorm(50, mean=1, sd=2),
#                  val3 = rnorm(50, mean=0, sd=3))
# 
# draw_for_participant(data = df, id.var="ID", id.part = "A", nr.of.occasions=3, items=c("val1", "val2"))
# 
# ## test again with zero variance in ratings
# df <- data.frame(ID = rep(LETTERS[1:5], each=10), # 5 participants with 10 occasions each
#                  occ_running = rep(1:10, times=5),
#                  val1 = 0, # two random variables
#                  val2 = 0,
#                  val3 = 0)
# draw_for_participant(data = df, id.var="ID", id.part = "C", nr.of.occasions=5, items=c("val1", "val2"))
# # correct, function would repeat over and over again
# # here, it is not possible for it to exit because we really have 0 variance
# 
# ## test with little variance
# df <- data.frame(ID = rep(LETTERS[1:5], each=10), # 5 participants with 10 occasions each
#                  occ_running = rep(1:10, times=5),
#                  val1 = rep(c(1,0), times=25), # two random variables
#                  val2 = rep(c(0,1), times=25),
#                  val3 = 0)
# 
# draw_for_participant(data = df, id.var="ID", id.part = "E", nr.of.occasions=2, items=c("val1", "val2"))





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
    return(drawn_data) # return an empty data frame (NA) of size of input df
  }
  
  
  # extract unique participant IDs from id.var
  ids <- unique(data[ , id.var])

  # Use lapply over all participant IDs to apply the drawing per participant function
  # NOTE: pass the arguments for the inner function (draw for participant) here
  # The ID list, the function is applied to, should be inserted in id.part -> the ID
  # per participant -> The draw_for_participant function is applied to every ID in the
  # ids vector, and each of these IDs is plugged into id.part
  drawn_list <- lapply(ids, 
                       FUN = function(x) draw_for_participant(data = data,
                                                              id.var = id.var,
                                                              id.part = x,
                                                              nr.of.occasions = nr.of.occasions,
                                                              items = items))
  # results are stored in a list

  # Combine results from list into a single data frame
  drawn_data <- do.call(rbind, drawn_list)
  # Order data frame
  drawn_data <- drawn_data[order(drawn_data[ , id.var], drawn_data[ , occ.running.var]), ]
  # Reset row values
  rownames(drawn_data) <- NULL
  
  # Return drawn data
  return(drawn_data)

}


# ## Testing function
# df <- data.frame(ID = rep(LETTERS[1:10], each=10), # 10 participants with 10 occasions each
#                  occ_running = rep(1:10, times=10),
#                  val1 = rnorm(100, mean=0, sd=1), # two random variables
#                  val2 = rnorm(100, mean=1, sd=2),
#                  val3 = rnorm(100, mean=0, sd=3))
# 
# new <- random_occasion_draw(data=df,
#                             id.var="ID",
#                             occ.running.var = "occ_running",
#                             nr.of.occasions = 3,
#                             items=c("val1", "val2", "val3"))
# 
# 
# df <- data.frame(ID = rep(LETTERS[1:5], each=10), # 5 participants with 10 occasions each
#                  occ_running = rep(1:10, times=5),
#                  val1 = rnorm(50, mean=0, sd=1), # two random variables
#                  val2 = rnorm(50, mean=1, sd=2),
#                  val3 = rnorm(50, mean=0, sd=3))
# 
# 
# new2 <- random_occasion_draw(data=df,
#                              id.var="ID",
#                              occ.running.var = "occ_running",
#                              nr.of.occasions = 10,
#                              items=c("val2", "val3"))
# 
# 
# new3 <- random_occasion_draw(data=df,
#                              id.var="ID",
#                              occ.running.var = "occ_running",
#                              nr.of.occasions = 20,
#                              items=c("val1", "val2", "val3"))
# rm(list=ls())

# # TESTING SINGLE PARTS (during construction of outer function)
# df <- data.frame(ID = rep(LETTERS[1:10], each=10), # 10 participants with 10 occasions each
#                  occ_running = rep(1:10, times=10),
#                  val1 = rnorm(100, mean=0, sd=1), # two random variables
#                  val2 = rnorm(100, mean=1, sd=2),
#                  val3 = rnorm(100, mean=0, sd=3))
# 
# id.var <- "ID"
# data <- df
# ids <- unique(data[ , id.var])
# nr.of.occasions <- 7
# items <- c("val1", "val2")
# 
# # Use lapply over all participant IDs
# drawn_list <- lapply(ids, FUN = function(x) draw_for_participant(data = data,
#                                                      id.var = id.var,
#                                                      id.part = x,
#                                                      nr.of.occasions = nr.of.occasions, items = items))
# 
# 
# 
# drawn_data <- do.call(rbind, drawn_list)
# rownames(drawn_data) <- NULL
# 
# 
# drawn_list <- lapply(ids, FUN = function(x) draw_for_participant(data = data,
#                                                                  id.var = id.var,
#                                                                  id.part = x,
#                                                                  nr.of.occasions = 9, items = items))
# 
# 
# 
# drawn_data <- do.call(rbind, drawn_list)
# rownames(drawn_data) <- NULL
# 


# Test whole function with real data
load("C:/Users/ecker/Seafile/Meine Bibliothek/Studien/2) ED Reliability/Data Analysis/data_analysis_git/prepared data/benchmark_data_Study1.rda")

36*25 # 26 participants * 25 occasions -> should be 900
set.seed(123)
test <- random_occasion_draw(data=bench, id.var="SERIAL", occ.running.var = "occ_running",
                             nr.of.occasions = 25, items=c("aerger1", "aerger2", "aerger3",
                                                           "schuld1", "schuld2", "schuld3"))
nrow(test) # correct
# calculate occasions per participant
library(tidyverse)
test %>% 
  group_by(SERIAL) %>% 
  summarise(n()) %>% 
  print(n=36)
