rm(list=ls())
library(tidyverse)
# loop through the all the random ploidy_rate data
wd <- getwd()
setwd("data/germination-benefit")
# make sure we exclude the null data
sim_dirs <- list.dirs(full.names = F, recursive = F)
# setup some storage and replicate ID variables
all_counts <- NULL
i <- 1
for(dir in sim_dirs){
  wd_data <- getwd()
  setwd(dir)
  # loop through any simulation replicate subfolders
  for(sim in list.dirs(full.names = F, recursive = F)){
    wd_dir <- getwd()
    setwd(sim)
    # get the count data
    counts <- readRDS("_counts.rds") %>%
      mutate(ID = i, ploidy_rate = as.numeric(ploidy_rate))
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
saveRDS(all_counts, "thesis/_data/germination_counts.rds")
rm(list=ls())