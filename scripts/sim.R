rm(list=ls())
library(tidyverse)
library(disturploidy)

#devtools::install_github("rosemckeon/honours-project", auth_token = "60bcd5244537f523313ffae0b061b4ba00873f5d")
library(sploidy)

trans <- readRDS("scripts/data/trans-matrix.RDS")

# create a range of ploidy rates with more values close to min than max (following exponential distribution).
ploidy_rates <- runif(
  n = 100000, 
  min = log(0.001), 
  max = log(0.01)
) %>%
  exp()

sploidy::sploidy(
  ploidy_rate = sample(ploidy_rates, 1, F)
)