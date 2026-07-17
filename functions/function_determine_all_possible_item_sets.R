###################################################################
#####      Estimating trait emotion differentiation:          #####
#####           How many measurement occasions and            #####
#####               emotion items are needed?                 #####
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

