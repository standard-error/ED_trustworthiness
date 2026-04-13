###################################################################
#####    Estimating trait negative emotion differentiation:   #####
#####        How many measurement occasions and               #####
#####              emotion items are needed?                  #####
###################################################################

###################################################################
#####    Function for Generating all Possible Item Sets       #####
###################################################################

# function is needed if all possible item sets should be balanced across replications
# for each item-number condition
# e.g., total of 15 items (3 for each of 5 emotion categories)
# -> 5-item total set should include 1 item from each emotion category
# -> total number of possible combinations:
# 3^5 (for each category, there are three options on drawing one item,
# and there are 5 categories in total -> 3 * 3 * 3 * 3 * 3)



# Write Helper Function to Order Item Sets --------------------------------

# Helper function: Order item sets by name and number (e.g., aerger1, angst1, angst2, ...)
order_item_sets <- function(items) {
  paste(sort(trimws(items)), collapse=", ")
}




# Write Function for Generating all Possible Item Sets --------------------

generate_all_item_sets <- function(all_items, categories, n_items) {
  
  #### CASE 1: No categories ####
  if (is.null(categories)) {
    
    combis <- combn(all_items, n_items, simplify = FALSE)
    all_item_sets <- sapply(combis, order_item_sets)
    return(all_item_sets)
    
  }
  
  #### CASE 2: Categories ####
  
  if (!is.null(categories)) { # if categories are supplied
    
    n_categories <- length(unique(categories)) # extract number of categories
    n_per_category <- n_items / n_categories # determine item number to draw per category (all should have the same number!)
    category_names <- unique(categories) # extract category names
    
    # Check whether all categories have the same number of items
    # i.e., total number should be divisible by number of categories
    if (n_per_category != floor(n_per_category)) {
      stop("n_items must be divisible by number of categories for equal number of draws
                                      per category.")
    }
    
    # get all possible combinations within each category
    set_by_category <- lapply(category_names, function(category) {
      items_category <- all_items[categories == category] # extract those items from all_items that belong
      # to the current category (same order!)
      combn(items_category, n_per_category, simplify = FALSE)
    })
    
    # now cross all categories to generate all item sets
    crossed <- expand.grid(set_by_category, stringsAsFactors = FALSE)
    
    # now construct a
    all_item_sets <- apply(crossed, 1, function(row) { # for each row in the crossed object...
      items <- unlist(row, use.names=FALSE) # extract the items from different columns
      order_item_sets(items)  # order item set
    })
    
    # now use all unique item sets
    return(unique(all_item_sets))
    
  }
  
}




# # generate_all_item_sets(all_items=all_items, categories=categories, n_items=15)
# # generate_all_item_sets(all_items=all_items, categories=categories, n_items=5)
# # # if we do not have categories: 15 over 5 = 3003 combination
# # choose(15, 5)
# # # test function without categories:
# # generate_all_item_sets(all_items=all_items, categories=NULL, n_items=5)
# # length(generate_all_item_sets(all_items=all_items, categories=NULL, n_items=5))
# # # correct
# # generate_all_item_sets(all_items=all_items, categories=NULL, n_items=15)
# # generate_all_item_sets(all_items=all_items, categories=categories, n_items=15)
# 
# generate_all_item_sets(all_items=c("freude1", "freude2", "freude3",                   # positive emotion items
#                                    "interesse1", "interesse2", "interesse3",
#                                    "liebe1", "liebe2", "liebe3",
#                                    "stolz1", "stolz2", "stolz3"),
#                        categories=c("freude", "freude", "freude",                   # positive emotion categories
#                                     "interesse", "interesse", "interesse",
#                                     "liebe", "liebe", "liebe",
#                                     "stolz", "stolz", "stolz"), n_items=4)
# # if we want a total of 4 items (for a total of 4 categories) -> 1 item per category
# # options per category: 3
# # 3 * 3 * 3 * 3 = 3^4 
# 3^4 # 81
# length(generate_all_item_sets(all_items=c("freude1", "freude2", "freude3",                   # positive emotion items
#                                           "interesse1", "interesse2", "interesse3",
#                                           "liebe1", "liebe2", "liebe3",
#                                           "stolz1", "stolz2", "stolz3"),
#                               categories=c("freude", "freude", "freude",                   # positive emotion categories
#                                            "interesse", "interesse", "interesse",
#                                            "liebe", "liebe", "liebe",
#                                            "stolz", "stolz", "stolz"), n_items=4))
# # correct
# 
# # if we want a total of 8 items (for a total of 4 categories) -> 2 items per category
# # options per category: choose(3, 2) = 3 (3 options on choosing 2 items per category)
# # 3 * 3 * 3 * 3 = 3^4 # for a total of 4 categories (all crossed)
# 3^4 # 81
# choose(3, 2)*choose(3, 2)*choose(3, 2)*choose(3, 2)
# length(generate_all_item_sets(all_items=c("freude1", "freude2", "freude3",                   # positive emotion items
#                                           "interesse1", "interesse2", "interesse3",
#                                           "liebe1", "liebe2", "liebe3",
#                                           "stolz1", "stolz2", "stolz3"),
#                               categories=c("freude", "freude", "freude",                   # positive emotion categories
#                                            "interesse", "interesse", "interesse",
#                                            "liebe", "liebe", "liebe",
#                                            "stolz", "stolz", "stolz"), n_items=8))
# # correct
# generate_all_item_sets(all_items=c("freude1", "freude2", "freude3",                   # positive emotion items
#                                    "interesse1", "interesse2", "interesse3",
#                                    "liebe1", "liebe2", "liebe3",
#                                    "stolz1", "stolz2", "stolz3"),
#                        categories=c("freude", "freude", "freude",                   # positive emotion categories
#                                     "interesse", "interesse", "interesse",
#                                     "liebe", "liebe", "liebe",
#                                     "stolz", "stolz", "stolz"), n_items=8)
# 
# 
# # without categories:
# choose(12, 8) # 495 options for choosing 8 out of 12 items
# length(generate_all_item_sets(all_items=c("freude1", "freude2", "freude3",                   # positive emotion items
#                                           "interesse1", "interesse2", "interesse3",
#                                           "liebe1", "liebe2", "liebe3",
#                                           "stolz1", "stolz2", "stolz3"),
#                               categories=NULL, n_items=8))
# # correct