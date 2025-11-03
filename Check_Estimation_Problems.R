res <- read.csv("results/results_table_whole_data_set_Study1.csv")
range(res$estimationProbNeg_min)
range(res$estimationProbNeg_mean)
range(res$estimationProbNeg_max)
# no negative estimation problems

range(res$estimationProbPos_min)
range(res$estimationProbPos_mean)
range(res$estimationProbPos_max)
# no positive estimation problems

range(res$N_rel_min)
range(res$N_rel_mean)
range(res$N_rel_max)
# always 109 valid ICCs