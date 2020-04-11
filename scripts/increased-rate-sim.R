rm(list=ls())
library(tidyverse)
library(magrittr)
library(disturploidy)
devtools::install_github("rosemckeon/honours-project", auth_token = "60bcd5244537f523313ffae0b061b4ba00873f5d")
library(sploidy)

# create a range of ploidy rates with more values close to min than max (following exponential distribution).
ploidy_rates <- runif(100000, 0.01, 0.5)

# pull in the data
mimulus <- readRDS("scripts/data/mimulus.RDS") %>%
  dplyr::filter(group == "Low-elevation perennials") %>%
  dplyr::filter(year == 2013)

# set a roughly 6 hr process going to run the simulations
# each named randomly with a random ploidy_rate
for(rate in sample(ploidy_rates, 1, F)){
  sploidy(
    pop_size = c(20, 20, 20),
    grid_size = 100,
    simulations = 1,
    generations = 1000,
    ploidy_rate = rate,
    filepath = "data/_ploidy_rate_0.10-0.50",
    trans = mimulus %>%
      dplyr::pull(matrix) %>%
      magrittr::extract2(1),
    D = 0.534,
    G = mimulus %>%
      dplyr::pull(G),
    seed_longevity = 0
  )
}

# make sure console message print to console if a simulation has benn interupted by an error
#sink(type = "message")
