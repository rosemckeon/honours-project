rm(list=ls())
library(tidyverse)
library(popbio)

# load the matrix
trans <- readRDS("scripts/data/mimulus.RDS") %>%
  dplyr::filter(group == "Low-elevation perennials") %>%
  dplyr::filter(year == 2013) %>%
  pull(matrix) %>%
  magrittr::extract2(1)

# name the dimensions
stages <- c("seed", "seedling", "rosette")
dimnames(trans)[1:2] <- list(stages)

# set starting population size by stage
N <- c(20, 20, 20)
# make predictions
predictions <- popbio::pop.projection(trans, N, 1000)
predictions %>% str()

# plot life stages over time proportionally
predictions$stage.vectors %>%
  popbio::stage.vector.plot(col = 2:4)

# get the maximum population size (all life stages combined)
predictions[["pop.sizes"]] %>% max()

# look at how fast the population size increases
predictions[["pop.sizes"]][1:30] %>% plot()
