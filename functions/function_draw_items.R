###################################################################
#####    Estimating trait negative emotion differentiation:   #####
#####        How many measurement occasions and               #####
#####              emotion items are needed?                  #####
###################################################################

###################################################################
#####               Function to Draw Items                    #####
###################################################################


# Note: In order to subset data frames by variable (item) names as
# stored in the output of the function, the items need to be extracted
# from the output as vector:
# item_vec <- strsplit(output[row,"items"], ", ")[[1]] # extract items as vector
# item_vec



# Function to Draw Items --------------------------------------------------

draw_items <- function(all_items, n_items) {
  # all_items: chr vector indicating the variable names of all items as used in the data frame
  # n_items: number of items to draw
  
  # create storage
  items_drawn <- as.data.frame(matrix(data=NA, nrow=length(n_items), ncol = 2))
  colnames(items_drawn) <- c("n_items", "items")
  
  # draw items and store them in the df
  items_drawn[] <- t(apply(matrix(n_items), 1, # apply the function to all numbers of items (and transpose output)
                           FUN = function(nr.items) { # function is a function of every single item number
                             # draw number of items (nr.items) from all items (all_items) without replacement
                             drawn <- sample(all_items, nr.items, replace = FALSE)
                             # bind the number of items and the drawn items
                             cbind(nr.items, paste(drawn, collapse = ", "))
                           }))
  return(items_drawn)
}



# # Test function
# df <- draw_items(all_items = c("item1", "item2", "item3", "item4"), n_items = c(1,2,3))
# df
# 
# set.seed(123)
# draw_items(all_items = c("item1", "item2", "item3", "item4"), n_items = c(1,2,3))
# df[3, 2]
# 
# rm(df)
#
#
# # Test whether data frame can be subsetted based on the items drawn
# load("prepared data/benchmark_data_Study1.rda")
# 
# output <- draw_items(all_items = c('aerger1', 'aerger2', 'aerger3',
#                                   'traurigkeit1', 'traurigkeit2', 'traurigkeit3',
#                                   'angst1', 'angst2', 'angst3',
#                                   'scham1', 'scham2', 'scham3',
#                                   'schuld1', 'schuld2', 'schuld3'),
#                     n_items = c(3, 5, 10))
# 
# output[1, 2]
# 
# item_vec <- strsplit(output[1,2], ", ")[[1]] # extract items as vector
# item_vec
# 
# sub <- bench[ , c("SERIAL", item_vec)] # first row of items: 3 items -> sub should have 4 columns (with SERIAL)
# # works
# 
# rm(list=ls())
