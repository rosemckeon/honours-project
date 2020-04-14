rm(list=ls())
library(tidyverse)
library(magrittr)
library(disturploidy)
devtools::install_github("rosemckeon/honours-project", auth_token = "60bcd5244537f523313ffae0b061b4ba00873f5d")
library(sploidy)

# create a range of ploidy rates 
ploidy_rates <- runif(100000, 0.15, 0.25)

# create a range of germination modifiers
G_modifiers <- runif(100000, 1, 1.5)

# create a range of starting population sizes
N <- 1:100000

# pull in transition the data
mimulus <- readRDS("scripts/data/mimulus.RDS") %>%
  dplyr::filter(group == "Low-elevation perennials") %>%
  dplyr::filter(year == 2013)

# roughly a 10 hr process
for(rate in sample(ploidy_rates, 12, F)){
  #  get a starting pop size
  n <- sample(N, 1) 
  # do a control test
  sploidy(
    pop_size = c(n, n, 0),
    grid_size = 100,
    simulations = 1,
    generations = 1000,
    filepath = "data/results",
    ploidy_rate = 0,
    trans = mimulus %>%
      dplyr::pull(matrix) %>%
      magrittr::extract2(1),
    D = 0.534,
    G = mimulus %>%
      dplyr::pull(G),
    G_modifier = 1,
    seed_longevity = 0
  )
  # and a ploidy_rate test for that same starting N
  sploidy(
    pop_size = c(n, n, 0),
    grid_size = 100,
    simulations = 1,
    generations = 1000,
    filepath = "data/results",
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
  # and a germination benefit test for the same rate and starting N
  # sploidy(
  #   pop_size = c(n, n, 0),
  #   grid_size = 100,
  #   simulations = 1,
  #   generations = 1000,
  #   filepath = "data/results",
  #   ploidy_rate = rate,
  #   trans = mimulus %>%
  #     dplyr::pull(matrix) %>%
  #     magrittr::extract2(1),
  #   D = 0.534,
  #   G = mimulus %>%
  #     dplyr::pull(G),
  #   G_modifier = sample(G_modifiers, 1),
  #   seed_longevity = 0
  # )
}

# make sure console message print to console if a simulation has benn interupted by an error
#sink(type = "message")
rm(list=ls())