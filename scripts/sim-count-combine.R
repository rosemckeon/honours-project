rm(list=ls())
library(tidyverse)
# loop through the all the random ploidy_rate data
wd <- getwd()
setwd("data")
# make sure we exclude the null data
null_dirs <- c("_NULL", "_NULL-2", "_NULL-3", "_NULL-4")
sim_dirs <- list.dirs(full.names = F, recursive = F)
sim_dirs <- ran_dirs[-which(ran_dirs %in% null_dirs)]
# setup some storage and replicate ID variables
all_counts <- NULL
i <- 1
for(dir in null_dirs){
  wd_data <- getwd()
  setwd(dir)
  # loop through any simulation replicate subfolders
  for(sim in list.dirs(full.names = F, recursive = F)){
    wd_dir <- getwd()
    setwd(sim)
    # get the count data
    counts <- readRDS("_counts.rds") %>%
      mutate(ID = i)
    # increase i for next replicate ID
    i <- i + 1
    # combine data
    all_counts <- dplyr::bind_rows(all_counts, counts)
    # free memory
    rm(counts)
    # reset working dir
    setwd(wd_dir)
  }
  setwd(wd_data)
}
setwd(wd)
# store combined counts
saveRDS(all_counts, "thesis/_data/sim_counts.rds")
rm(all_counts)
