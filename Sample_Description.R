###################################################################
#####    Estimating trait negative emotion differentiation:   #####
#####        How many measurement occasions and               #####
#####              emotion items are needed?                  #####
###################################################################

###################################################################
#####                  Sample Description                     #####
###################################################################


# for description of data cleaning, see data preparation scripts


# Load Data ---------------------------------------------------------------
load("internal use/prepared data/emolive_clean_all_participants.rda")


names(AA.c)
table(dplyr::distinct(AA.c, SERIAL, n_occ)$n_occ >= 100)


# Select Benchmark Sample -------------------------------------------------
samp <- AA.c[which(AA.c$n_occ >= 100), ]
# length(unique(samp$SERIAL))

