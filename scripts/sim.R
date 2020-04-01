rm(list=ls())
library(tidyverse)
library(disturploidy)

devtools::install_github("rosemckeon/honours-project", auth_token = "60bcd5244537f523313ffae0b061b4ba00873f5d")
library(sploidy)

# create a range of ploidy rates with more values close to min than max (following exponential distribution).
ploidy_rates <- runif(
  n = 100000, 
  min = log(0.001), 
  max = log(0.01)
) %>%
  exp()

# run the model with a random ploidy_rate
sploidy(
  pop_size = 100,
  grid_size = 100,
  ploidy_rate = sample(ploidy_rates, 1, F),
  trans = readRDS("scripts/data/trans-matrix.RDS")
)

sink(type = "message")