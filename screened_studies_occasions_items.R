screened <- readxl::read_excel("C:/Users/ecker/Seafile/Meine Bibliothek/Forschung/2) ED Reliability/Manuscript/2026-04-09_Screened_Studies_all_coded.xlsx",
                               sheet = 1)

# remove comments from file (rows and column 15)
screened <- screened[1:75, 1:14]

# define NA
screened[screened == "NA"] <- NA

# check variable types
str(screened)

# adjust variable types
screened$nr_nemo_items <- as.integer(screened$nr_nemo_items)
screened$nr_pemo_items <- as.integer(screened$nr_pemo_items)

# Sample Sizes ------------------------------------------------------------
range(screened$final_samp_size)
median(screened$final_samp_size)
mean(screened$final_samp_size)


# Response Format ---------------------------------------------------------
table(screened$response_format, useNA="always")

# extract slider / Likert scale from text

screened$scale_type <- stringr::str_extract(screened$response_format, "(slider|Likert) scale")
# View(screened[ , c("response_format", "scale_type")])

table(screened$scale_type, useNA="always")
prop.table(table(screened$scale_type,  useNA="always"))




# Occasions and Items -----------------------------------------------------
range(screened$nr_occasions_total)
range(screened$nr_nemo_items, na.rm=T)
range(screened$nr_pemo_items, na.rm=T)

quantile(screened$nr_occasions_total)
which.max(table(screened$nr_occasions_total))

quantile(screened$nr_nemo_items, na.rm=T)
which.max(table(screened$nr_nemo_items))
table(screened$nr_nemo_items)

quantile(screened$nr_pemo_items, na.rm=T)
which.max(table(screened$nr_pemo_items))
table(screened$nr_pemo_items)



# Plot --------------------------------------------------------------------

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


