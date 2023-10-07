# gsutil-speedtest

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)


# 
# leer_df <- tibble(n=c(1,2,3),val=c("a","b","c"))
# 
if (!dir.exists("test")) dir.create("test")
# for (i in 1:1000) {
#   write_csv(leer_df,paste0("sttmp/test",
#                            formatC(i, width = 3,format="fg", flag="0"),
#                            ".csv"))
# }

n <- now()
system('gsutil -m -h "Cache-Control:no-cache, max_age=0" cp test/* gs://d.data.gcp.cloud.hr.de/test/')
copy_time <- now()-n
n <- now()
cat("Operation took ",copy_time)
#file.remove("sttmp/*.*")

# Erstes Experiment: 1000 Files mit ca. 20k --> 20s (mit -m)
# Löschen der 1000 Files vom Bucket: 7,5s



