rm(list=ls())
library(tidyverse)
# loop through the NULL data
wd <- getwd()
setwd("data")
null_dirs <- c("_NULL", "_NULL-2", "_NULL-3", "_NULL-4")
#null_dirs <- c("_NULL")
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
saveRDS(all_counts, "thesis/_data/null_counts.rds")
