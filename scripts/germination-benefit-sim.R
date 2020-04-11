rm(list=ls())
library(tidyverse)
library(magrittr)
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

G_modifiers <- runif(100000, 1, 1.5)

mimulus <- readRDS("scripts/data/mimulus.RDS") %>%
  dplyr::filter(group == "Low-elevation perennials") %>%
  dplyr::filter(year == 2013)

for(modifier in sample(G_modifiers, 20, F)){
  # run germination benefit simulations
  sploidy(
    pop_size = c(20, 20, 20),
    grid_size = 100,
    simulations = 1,
    generations = 1000,
    ploidy_rate = 0.5,
    filepath = "data/germination-benefit/",
    trans = mimulus %>%
      dplyr::pull(matrix) %>%
      magrittr::extract2(1),
    D = 0.534,
    G = mimulus %>%
      dplyr::pull(G),
    G_modifier = sample(ploidy_rates, 1, F),
    seed_longevity = 0
  )
}


# make sure console message print to console if a simulation has benn interupted by an error
sink(type = "message")
