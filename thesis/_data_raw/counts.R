# wrangling the simulation data from data/results/*
rm(list=ls())
# dependencies
library(tidyverse)
library(sploidy)
# the data ---------
#sploidy::count_combine("data/results", "thesis/_data_raw/counts")
counts <- readRDS("thesis/_data_raw/counts.rds")


K <- 100000
counts <- counts %>%
  dplyr::mutate(
    # make all the counts proportional
    seeds = seeds / K,
    seedlings = seedlings / K,
    rosettes = rosettes / K,
    adults = adults / K,
    diploid_adults = diploid_adults / K,
    polyploid_adults = polyploid_adults / K,
    sterile_polyploid_adults = sterile_polyploid_adults / K
  ) %>%
  # cherry pick the columns we need
  dplyr::select(
    ID, gen, ploidy_rate, starting_N, 
    seeds, adults, diploid_adults, polyploid_adults, sterile_polyploid_adults, 
    total
  ) %>%
  dplyr::mutate(
    # correct adult subset sumation for plotting
    diploid_adults = replace_na(diploid_adults, 0),
    polyploid_adults = replace_na(polyploid_adults, 0),
    sterile_polyploid_adults = replace_na(sterile_polyploid_adults, 0)
  ) 


# store for posterity
saveRDS(counts, "thesis/_data/counts.rds")

# set the control data aside
control <- counts %>%
  dplyr::filter(ploidy_rate == 0) %>%
  group_by(ID, ploidy_rate, starting_N) %>%
  nest()

# nest the rest by simulation
simulations <- counts %>%
  dplyr::filter(ploidy_rate > 0) %>%
  dplyr::group_by(ID, ploidy_rate, starting_N) %>%
  tidyr::nest()

# see how many extra low sims we have
#simulations$ploidy_rate %>% round(2) %>% hist(breaks = 50)

# then trim this number down so these rates aren't over represented
ran_low <- simulations %>%
  dplyr::filter(ploidy_rate < 0.01, ploidy_rate > 0) %>%
  dplyr::ungroup()

ran_high <- simulations %>%
  dplyr::filter(ploidy_rate >= 0.01)  %>%
  dplyr::ungroup()

simulations <- dplyr::bind_rows(
  ran_low <- ran_low %>% sample_n(20), 
  ran_high
)

# now let's just keep 1000, instead of 1070 o to make our life with percentages easier
simulations <- simulations %>%
  dplyr::sample_n(1000) 

# did that work nicely ?
simulations$ID %>% unique() %>% length()
# how's the spread of rates look?
#simulations$ploidy_rate %>% round(2) %>% hist(breaks = 50)
# how's the spread of starting N look?
#simulations$starting_N %>% round(2) %>% hist(breaks = 50)

# store for posterity
saveRDS(control, "thesis/_data/control.rds")
saveRDS(simulations, "thesis/_data/simulations.rds")
# clean up
rm(list=ls())
