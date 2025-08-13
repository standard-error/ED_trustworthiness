###################################################################
#####    Estimating trait negative emotion differentiation:   #####
#####        How many measurement occasions and               #####
#####              emotion items are needed?                  #####
###################################################################

###################################################################
#####          Function to Draw Occasions By Order            #####
###################################################################


# Note: For the ordered draw, the items argument is not necessary
# because the items will be selected in the function to calculate
# the ICCs (i.e., the data frame will be subsetted later). 
# In contrast, the items argument is needed in the random draw
# function because it checks whether there is variance in the 
# emotion ratings so that an ICC can be calculated. However,
# the subsetting (i.e., selection of relevant variables)
# is done in the ICC calculation function.



# Create Function for Ordered Draws of Occasions --------------------------

ordered_occasion_draw <- function(data, id.var, occ.running.var, nr.of.occasions) {
  # data: takes the data frame with all participants
          # and their occasions as input (long format)
  # id.var: character that indicates name of participant ID variable
  # occ.running.var: character that indicates the name of the occasion running variable
  # nr.of.occasions: number of occasions to draw per participant by order
  
  
  # control sequence: if the number of occasions is greater than the number of occasions
  # per participant in the data set, print a warning message
  occ_per_part <- sapply(split(data, data[ , id.var]), nrow)
  
  if (any(occ_per_part < nr.of.occasions)) {
   print("Error in ordered_occasions_draw: nr.of.occasions is greater than the number of occasions per participant for at least one participant.")
    drawn_data <- data.frame(matrix(NA, ncol=ncol(data), nrow=nrow(data)))
    names(drawn_data) <- names(data)
   return(drawn_data) # return an empty data frame (NA) of size of input df
  }
  
  # measurement occasions are ordered by occ.running.var
  # if we want to draw 10 occasions by order, we want to drawn occ_running 1:10
  # -> we do not need to do this participant-wise
  drawn_data <- data[data[ , occ.running.var] <= nr.of.occasions, ]
  # order data frame by ID variable and occ_running variable
  drawn_data <- drawn_data[order(drawn_data[ , id.var], drawn_data[ , "occ_running"]), ]
  # return data
  return(drawn_data)
}


# # test function
# # create some random data
# df <- data.frame(ID = rep(LETTERS[1:5], each=10), # 5 participants with 10 occasions each
#                  occ_running = rep(1:10, times=5),
#                  val1 = rnorm(50, mean=0, sd=1), # two random variables
#                  val2 = rnorm(50, mean=1, sd=2))
# 
# 
# out <- ordered_occasion_draw(data=df, id.var="ID", occ.running.var = "occ_running", nr.of.occasions = 3)
# out <- ordered_occasion_draw(data=df, id.var="ID", occ.running.var = "occ_running", nr.of.occasions = 9)
# out <- ordered_occasion_draw(data=df, id.var="ID", occ.running.var = "occ_running", nr.of.occasions = 11)
# rm(list=ls())
