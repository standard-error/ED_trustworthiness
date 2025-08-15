###################################################################
#####    Estimating trait negative emotion differentiation:   #####
#####        How many measurement occasions and               #####
#####              emotion items are needed?                  #####
###################################################################

###################################################################
#####               Function to Draw Items                    #####
###################################################################


load("prepared data/benchmark_data_Study1.rda")

all_items <- c('aerger1', 'aerger2', 'aerger3',
               'traurigkeit1', 'traurigkeit2', 'traurigkeit3',
               'angst1', 'angst2', 'angst3',
               'scham1', 'scham2', 'scham3',
               'schuld1', 'schuld2', 'schuld3')


sample(all_items, 5, replace=FALSE)

n_items <- 5
sample(all_items, n_items, replace=FALSE)


n_items <- c(5,10)
# sample(all_items, n_items, replace=FALSE)
apply(matrix(n_items), MARGIN = 1, FUN = function(n_item) {
  sample(all_items, n_item, replace=FALSE)
})



draws <- t(apply(matrix(n_items), MARGIN = 1, FUN = function(n_item) {
  as.vector(sample(all_items, n_item, replace=FALSE))
}))
draws
draws[1,1]

#### 
n_items <- c(5, 10)

items_drawn <- matrix(data=NA, nrow=length(n_items), ncol=2)
colnames(items_drawn) <- c("n_items", "items")

items_drawn[] <- t(apply(matrix(n_items), 1, FUN = function(nr.items) {
  drawn <- sample(all_items, nr.items, replace = FALSE)
  cbind(nr.items, paste(sprintf("'%s'", drawn), collapse = ", "))
}))

####

as.data.frame(t(apply(matrix(n_items), 1, FUN = function(nr.items) {
  drawn <- sample(all_items, nr.items, replace = FALSE)
  cbind(nr.items, paste(sprintf("'%s'", drawn), collapse = ", "))
})))

######

n_items <- c(5, 10)

items_drawn <- as.data.frame(matrix(data=NA, nrow=length(n_items), ncol=2))
colnames(items_drawn) <- c("n_items", "items")

items_drawn[] <- t(apply(matrix(n_items), 1, FUN = function(nr.items) {
  drawn <- sample(all_items, nr.items, replace = FALSE)
  cbind(nr.items, paste(sprintf("'%s'", drawn), collapse = ", "))
}))

items_drawn[ , "n_items"] <- as.numeric(items_drawn[ , "n_items"])
items_drawn[ , "items"] <- as.character(items_drawn[ , "items"])


