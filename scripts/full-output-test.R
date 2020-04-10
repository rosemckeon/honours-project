# shorter generations with full output to test whether polyploids do sometimes mate successfully.
rm(list=ls())
library(tidyverse)
library(magrittr)
library(disturploidy)
devtools::install_github("rosemckeon/honours-project", auth_token = "60bcd5244537f523313ffae0b061b4ba00873f5d")
library(sploidy)

my_palette = c("#000000", "#30a068")

mimulus <- readRDS("scripts/data/mimulus.RDS") %>%
  dplyr::filter(group == "Low-elevation perennials") %>%
  dplyr::filter(year == 2013)

# run simulations
# sploidy(
#   pop_size = c(20, 20, 20),
#   generations = 250,
#   simulations = 1,
#   grid_size = 100,
#   ploidy_rate = 0.1, # ramped up high
#   name = "_test",
#   trans = mimulus %>%
#     dplyr::pull(matrix) %>%
#     magrittr::extract2(1),
#   seed_longevity = 0,
#   output_gen_data = T
# )

# make sure console message print to console if a simulation has been interupted by an error
#sink(type = "message")

# get a feel for the count data
counts <- readRDS("data/_test/sim-001/_counts.rds") %>%
  tidyr::gather("population", "count", seeds:total) %>%
  dplyr::filter(population %in% c("diploid_adults", "polyploid_adults"))

counts %>%
  ggplot(aes(x = gen, y = count, colour = population)) +
  geom_point() +
  geom_line() +
  scale_colour_manual(values = my_palette) +
  theme_classic() +
  theme(legend.position = "top")

# check seed data for proof of polyploids mating
seeds_0210 <- readRDS("data/_test/sim-001/seeds/seeds_0210.rds")
# summary shows we have some polyploids mating
seeds_0210 %>% summary()
# here are their offspring
seeds_0210[which(seeds_0210$ploidy_mum > 2), ]

# did those seeds manage to germinate?
seedlings_0211 <- readRDS("data/_test/sim-001/seedlings/seedlings_0211.rds")
# looks like some did, or some other transitions with polyploid pairings
seedlings_0211 %>% summary()
# here are the adults that made it...
seedlings_0211[which(seedlings_0211$ploidy_mum > 2), ]

# conclusion = polyploids can definitely mate.
# do they do it at lower mutation rates? 
# and with D and G set as we did in the real sims?
# sploidy(
#   pop_size = c(20, 20, 20),
#   generations = 250,
#   simulations = 1,
#   grid_size = 100,
#   ploidy_rate = 0.01, # put back down to max value in current data
#   name = "_test-2",
#   trans = mimulus %>%
#     dplyr::pull(matrix) %>%
#     magrittr::extract2(1),
#   D = 0.534,
#   G = mimulus %>%
#     dplyr::pull(G),
#   seed_longevity = 0,
#   output_gen_data = T
# )
# get a feel for the data
counts <- readRDS("data/_test-2/sim-001/_counts.rds") %>%
  tidyr::gather("population", "count", seeds:total) %>%
  dplyr::filter(population %in% c("diploid_adults", "polyploid_adults"))

counts %>%
  ggplot(aes(x = gen, y = count, colour = population)) +
  geom_point() +
  geom_line() +
  scale_colour_manual(values = my_palette) +
  theme_classic() +
  theme(legend.position = "top")

# check some seedlings
seedlings_0100 <- readRDS("data/_test-2/sim-001/seedlings/seedlings_0100.rds")
# looks like some did, or some other transitions with polyploid pairings
seedlings_0100 %>% summary()
# here are the adults that made it...
seedlings_0100[which(seedlings_0100$ploidy_mum > 2), ]


seedlings_0200 <- readRDS("data/_test-2/sim-001/seedlings/seedlings_0200.rds")
# looks like some did, or some other transitions with polyploid pairings
seedlings_0200 %>% summary()
# here are the adults that made it...
seedlings_0200[which(seedlings_0200$ploidy_mum > 2), ]

# conclusion = polyploids are definitely still mating at these levels, but just much less. 