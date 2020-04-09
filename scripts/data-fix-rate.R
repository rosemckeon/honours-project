# fixes count data files so they include a column for ploidy rate
# not required for simulations run on sploidy 0.0.8 onwards
rm(list=ls())
library(tidyverse)
wd <- getwd()
setwd("data")
# loop through the sub directories
for(dir in list.dirs(full.names = F, recursive = F)){
  wd_data <- getwd()
  setwd(dir)
  # loop through any simulation replicate subfolders
  for(sim in list.dirs(full.names = F, recursive = F)){
    wd_dir <- getwd()
    setwd(sim)
    # get the ploidy_rate from log file _sploidy-log.txt
    logfile <- file("_sploidy-log.txt", "r")
    log <- readLines(logfile, n = 1) %>% strsplit(" = ")
    rate <- log[[1]][2]
    # transform the count data to include the rate
    counts <- readRDS("_counts.rds") %>%
      dplyr::mutate(ploidy_rate = rate)
    # overwrite the data file
    saveRDS(counts, "_counts.rds")
    close(logfile) # hmm this not working
    message("Counts for ", dir, "/", sim, " updated with rate: ", rate)
    setwd(wd_dir)
  }
  setwd(wd_data)
}
setwd(wd)
