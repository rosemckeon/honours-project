rm(list=ls())
library(tidyverse)
library(magrittr)
library(disturploidy)
devtools::install_github("rosemckeon/honours-project", auth_token = "60bcd5244537f523313ffae0b061b4ba00873f5d")
library(sploidy)

# run null simulations
sploidy(
  pop_size = 10000,
  grid_size = 100,
  simulations = 10,
  generations = 100,
  ploidy_rate = 0,
  name = "null-ploidy_rate-0",
  trans = readRDS("scripts/data/mimulus.RDS") %>%
    dplyr::filter(group == "Low-elevation perennials") %>%
    dplyr::filter(year == 2013) %>%
    pull(matrix) %>%
    magrittr::extract2(1)
)

# make sure console message print to console if a simulation has benn interupted by an error
# sink(type = "message")