# fixes count data files so that the total column is correct (not zero!)
# not required for simulations run on sploidy 0.0.9 onwards
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
    # transform the count data
    counts <- readRDS("_counts.rds") %>%
      dplyr::mutate(total = sum(seeds, seedlings, rosettes))
    # overwrite the data file
    saveRDS(counts, "_counts.rds")
    message("Counts for ", dir, "/", sim, " updated with new totals.")
    setwd(wd_dir)
  }
  setwd(wd_data)
}
setwd(wd)
