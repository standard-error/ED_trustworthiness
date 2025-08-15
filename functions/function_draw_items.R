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

draw_items <- function(all_items, n_items, categories = NULL) {
  # all_items: chr vector indicating the variable names of all items as used in the data frame
  # n_items: number of items to draw
  # categories: optional vector (same length as all_items) indicating category
        # --> needed if multiple items per emotion category were assessed 
        # -> draw items per category
  
  # create storage
  items_drawn <- as.data.frame(matrix(data=NA, nrow=length(n_items), ncol = 2))
  colnames(items_drawn) <- c("n_items", "items")
  
  # draw items and store them in the df
  # if categories == NULL, draw from all items, else draw per category
  items_drawn[] <- t(apply(matrix(n_items), 1, # apply the function to all numbers of items (and transpose output)
                           FUN = function(nr.items) { # function is a function of every single item number
                             
                             # if categories == NULL, draw from all itesms
                             if(is.null(categories)) {
                               # draw number of items (nr.items) from all items (all_items) without replacement
                               drawn <- sample(all_items, nr.items, replace = FALSE)
                             } else {
                               # if categories != NULL, draw per category
                               
                               # check: equal vector length -> every item belongs to one category
                               if (length(all_items) != length(categories)) {
                                 stop(
                                   sprintf(
                                     "all_items and categories must be of the same length.
                                     That is, every item needs to belong to one emotion category."
                                   )
                                 )
                               }
                               
                               
                               # extract unique categories
                               unique_categories <- unique(categories)
                               
                               
                               # calculate how many items per category should be drawn
                               items_per_category <- nr.items / length(unique_categories)
                              
                               
                               # check: total number must be divisible by number of categories
                               # for equal number of items per category
                               # floor(): largest integer not greater than x
                               # i.e., if items_per_cat = floor(items_per_cat) -> integer, equal number per category
                               # if items_per_cat != floor(items_per_cat) -> decimal; from some categories,
                               # more items must be drawn than from others
                               if (items_per_category != floor(items_per_category)) {
                                 stop("n_items must be divisible by number of categories for equal number of draws
                                      per category.")
                               }
                              
                               # draw items per category:
                               # apply the sample function to each category
                               drawn <- apply(matrix(unique_categories), 1,
                                              FUN = function(category) {
                                                # subset all_items per category
                                                items_c <- all_items[categories == category] 
                                                sample(items_c, items_per_category, replace = FALSE)
                                                })
                             }
                             
                             # CHECK: if length(unique(drawn)) != nr.items -> something went wrong
                             if (length(unique(drawn)) != nr.items) {
                               stop(
                                 sprintf(
                                   "Number of uniquely drawn items does not match the number of items to draw
                                   for n_items = %i. Something went wrong in the sample() function.",
                                   nr.items
                                 )
                               )
                             }
                             
                             
                             
                             # checked for every nr.items (i.e., all elements in n_items)
                
                             
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
# 
# # Test whether function can draw per category:
# test <- draw_items(all_items = c('aerger1', 'aerger2', 'aerger3',
#                                  'traurigkeit1', 'traurigkeit2', 'traurigkeit3',
#                                  'angst1', 'angst2', 'angst3',
#                                  'scham1', 'scham2', 'scham3',
#                                  'schuld1', 'schuld2', 'schuld3'),
#                    n_items = c(5, 10, 15),
#                    categories = c("aerger", "aerger", "aerger",
#                                   "traurigkeit", "traurigkeit", "traurigkeit",
#                                   "angst", "angst", "angst",
#                                   "scham", "scham", "scham",
#                                   "schuld", "schuld", "schuld"))
# test
# 
# 
# test2 <- draw_items(all_items = c('A1', 'A2', 'A3',
#                                   'B1', 'B2', 'B3',
#                                   'C1', 'C2', 'C3'),
#                    n_items = c(3, 6, 9),
#                    categories = c("A", "A", "A",
#                                   "B", "B", "B",
#                                   "C", "C", "C"))
# test2
# rm(list=ls())
