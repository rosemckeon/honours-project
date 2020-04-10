rm(list=ls())
library(tidyverse)
my_palette = c("#000000", "#30a068")
diploid_colour = "#000000"
polyploid_colour = "#30a068"
ploidy_gradient = c("#10442a", "#3bbf7d")
# read in the data and make long
counts <- readRDS("thesis/_data/sim_counts.rds") %>%
  tidyr::gather("population", "count", seeds:total)

# what did the life stage composition look like
# VERY similar to null tests
life_stages <- counts %>%
  dplyr::filter(population %in% c("seeds", "seedlings", "rosettes")) %>%
  mutate(treatment = "ploidy_rate = 0.001-0.010") %>%
  bind_rows(
    readRDS("thesis/_data/null_counts.rds") %>%
      tidyr::gather("population", "count", seeds:total) %>%
      dplyr::filter(population %in% c("seeds", "seedlings", "rosettes")) %>%
      mutate(treatment = "ploidy_rate = 0", ploidy_rate = 0)
  )

life_stage_plot <- life_stages %>%
  ggplot(aes(x = gen, y = count, colour = population)) +
    geom_point(alpha = .2) +
    theme_classic() +
    theme(legend.position = "top") +
    facet_wrap(~treatment)

life_stage_plot

# how many diploids vs polyploids did we have, when it was turned on?
adults <- counts %>%
  dplyr::filter(population %in% c("diploid_adults", "polyploid_adults"))

adult_plot <- adults %>%
  ggplot(aes(x = gen, y = count, colour = population)) +
    geom_point(alpha = .2) +
    scale_colour_manual(values = my_palette) +
    theme_classic() +
    theme(legend.position = "top")

adult_plot

# zoom in on polyploidy
polyploids <- adults %>%
  dplyr::filter(population == "polyploid_adults") 

polyploid_plot <- polyploids %>%
  ggplot(aes(x = gen, y = count, colour = ploidy_rate)) +
  geom_point(alpha = .2) +
  scale_colour_gradient(low = ploidy_gradient[1], high = ploidy_gradient[2]) +
  theme_classic() +
  theme(legend.position = "top")

polyploid_plot

# and show the rate relationship
last_500yrs <- adults %>%
  dplyr::filter(gen > 500)

stable_rate_trends <- last_500yrs %>%
  ggplot(aes(x = ploidy_rate, y = count, colour = population)) +
    geom_point(alpha = .2) +
    scale_colour_manual(values = my_palette) +
    facet_wrap(~population, scales = "free_y") +
    theme_classic() +
    theme(legend.position = "top")

stable_rate_trends