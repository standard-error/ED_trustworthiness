screened <- readxl::read_excel("C:/Users/ecker/Seafile/Meine Bibliothek/Studien/2) ED Reliability/Manuscript/Screened_Studies_all_coded.xlsx")

# remove comments from file
screened <- screened[1:74, ]


range(screened$final_samp_size)
range(screened$nr_occasions_total)
range(screened$nr_nemo_items)

quantile(screened$nr_occasions_total)
which.max(table(screened$nr_occasions_total))

quantile(screened$nr_nemo_items)


