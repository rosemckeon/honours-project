rm(list=ls())
library(tidyverse)
library(magrittr)
library(disturploidy)
devtools::install_github("rosemckeon/honours-project", auth_token = "60bcd5244537f523313ffae0b061b4ba00873f5d")
library(sploidy)

mimulus <- readRDS("scripts/data/mimulus.RDS") %>%
  dplyr::filter(group == "Low-elevation perennials") %>%
  dplyr::filter(year == 2013)

# run null simulations
sploidy(
  pop_size = c(20, 20, 20),
  grid_size = 100,
  simulations = 10,
  generations = 1000,
  ploidy_rate = 0,
  name = "_NULL-3",
  trans = mimulus %>%
    dplyr::pull(matrix) %>%
    magrittr::extract2(1),
  D = 0.534,
  G = mimulus %>%
    dplyr::pull(G),
  seed_longevity = 0
)

# make sure console message print to console if a simulation has benn interupted by an error
#sink(type = "message")
