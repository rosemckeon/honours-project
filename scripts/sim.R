rm(list=ls())
library(tidyverse)
library(magrittr)
library(disturploidy)
devtools::install_github("rosemckeon/honours-project", auth_token = "60bcd5244537f523313ffae0b061b4ba00873f5d")
library(sploidy)

# create a range of ploidy rates 
ploidy_rates <- seq(0, 1, length.out = 100000)
# create a range of starting population sizes
N <- 1:100000

# pull in transition the data
mimulus <- readRDS("scripts/data/mimulus.RDS") %>%
  dplyr::filter(group == "Low-elevation perennials") %>%
  dplyr::filter(year == 2013)

# roughly a 10 hr process
for(rate in sample(ploidy_rates, 50, F)){
  #  get a starting pop size
  n <- sample(N, 1) 
  sploidy(
    pop_size = c(n, n, 0),
    grid_size = 100,
    simulations = 1,
    generations = 1000,
    filepath = "data/results-final",
    ploidy_rate = rate,
    trans = mimulus %>%
      dplyr::pull(matrix) %>%
      magrittr::extract2(1),
    D = 0.534,
    G = mimulus %>%
      dplyr::pull(G),
    G_modifier = 1,
    seed_longevity = 0
  )
}

# make sure console message print to console if a simulation has been interupted by an error
#sink(type = "message")
rm(list=ls())