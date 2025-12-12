screened <- readxl::read_excel("C:/Users/ecker/Seafile/Meine Bibliothek/Studien/2) ED Reliability/Manuscript/Screened_Studies_all_coded.xlsx")

# remove comments from file
screened <- screened[1:74, ]


range(screened$final_samp_size)
range(screened$nr_occasions_total)
range(screened$nr_nemo_items)

quantile(screened$nr_occasions_total)
which.max(table(screened$nr_occasions_total))

quantile(screened$nr_nemo_items)
which.max(table(screened$nr_nemo_items))
table(screened$nr_nemo_items)

plot(screened$nr_occasions_total, screened$nr_nemo_items)
library(ggplot2)

median_occ <- median(screened$nr_occasions_total)
median_item <- median(screened$nr_nemo_items)

ggplot(data=screened, aes(x=nr_occasions_total, y=nr_nemo_items)) +
  geom_point(col="#507189", size=2.5) +
  theme_bw() +
  ylab("Number of Items") +
  xlab("Number of Occasions") +
  theme(text = element_text(size=16)) +
  scale_x_continuous(breaks = seq(0,225, 25)) +
  scale_y_continuous(breaks = seq(0,24,2)) + 
  annotate('point',
           x = median_occ, y = median_item,  color="red", size = 3) +
  annotate('label',
           x = median_occ, y = median_item,
           label=" Average study:\n35.5 occasions, 6 items",
           hjust= -0.05, vjust=-0.2, color="red")
  

