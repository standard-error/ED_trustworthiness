###################################################################
#####    Estimating trait negative emotion differentiation:   #####
#####        How many measurement occasions and               #####
#####              emotion items are needed?                  #####
###################################################################

###################################################################
#####            Function to Calculate ICC                    #####
###################################################################




# Create Function to Calculate ICC ----------------------------------------
# function to calculate intraclass correlation as a between-person variable

calculate_icc <- function(data,
                          id.var,
                          items,
                          type = "consistency",
                          unit = "single") { 
  # data: takes the data frame with all participants
          # and their occasions as input (long format)
  # id.var: character indicating name of participant ID variable
  # items: character vector of specifying which emotion items
          # shall be used for the calculation (variable names
          # as used in data frame)
  # type: character indicating whether to use consistency ("consistency", default)
          # or absolute agreement ("agreement")
  # unit: character indicating whether to use single measurements ("single", default)
          # or average of k measurements ("average")
  

  # extract (unique) participant IDs as vector
  ids <- unique(data[ , id.var])
  # create empty matrix as storage for ICC data
  ICCdata <- matrix(NA, nrow = length(ids), ncol=2)
  colnames(ICCdata) <- c(id.var, "ICC") # rename cols according to id.var and ICC
  ICCdata[ , id.var] <- ids # store ids in id.var

  for (id in ids) { # now loop over the ID vector -> for each participant, ...
    
    # select all rows belonging to this participant and only the
    # relevant emotion items (indicated by items)
    iccsubdat <- data[which(data[ ,id.var] == id), items]
    
    # calculate ICC using the participant's data and ICC(3, 1) measuring consistency
    ICC <- irr::icc(iccsubdat, model="twoway", type=type,
                    unit = unit)$value 
    
    ICCdata[ICCdata[ , id.var] == id, "ICC"] <- ICC
    # save ICC in row in matrix that corresponds to current ID
  }
  
  return(ICCdata) # return ICCdata matrix
}





# # Test Function 
# load("C:/Users/ecker/Seafile/Meine Bibliothek/Studien/2) ED Reliability/Data Analysis/data_analysis_git/prepared data/benchmark_data_Study1.rda")# 
# 
# test <- calculate_icc(data=bench, id.var="SERIAL",
#                       items = c("aerger1", "aerger2", "aerger3",
#                                 "traurigkeit1", "traurigkeit2", "traurigkeit3"))
# test
# 
# 
# test2 <- calculate_icc(data=bench, id.var="SERIAL",
#                        items = c("angst1", "angst2", "angst3",
#                                  "schuld1", "schuld2", "schuld3"))
# test2
# 
# identical(test, test2)
# test3 <- merge(test, test2, by="SERIAL")
# rm(test, test2, test3)
# 
# 
# # Generate data with different N, different items, and different id.var
# # 20 participants with 10 occasions each, 5 items
# dat <- data.frame(PART_ID = rep(LETTERS[1:20], each=10),
#                   item1 = rnorm(200, mean=0, sd=1),
#                   item2 = rnorm(200, mean=1, sd=1),
#                   item3 = rnorm(200, mean=0, sd=2),
#                   item4 = rnorm(200, mean=-1, sd=2),
#                   item5 = rnorm(200, mean =0.5, sd=0.5))
# 
# 
# test <- calculate_icc(data = dat,
#                       id.var= "PART_ID",
#                       items = c("item1", "item2", "item3", "item4", "item5"))
# test
# 
# rm(list=ls())
