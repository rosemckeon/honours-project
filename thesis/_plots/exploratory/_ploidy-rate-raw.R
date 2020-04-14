rm(list=ls())
library(tidyverse)
my_palette = c("#000000", "#30a068")
diploid_colour = "#000000"
polyploid_colour = "#30a068"
ploidy_gradient = c("#10442a", "#3bbf7d")
# read in the data and make long
counts <- readRDS("thesis/_data/sim_counts.rds") %>% dplyr::mutate(G_modifier = 1) 
counts <- counts %>%
  dplyr::bind_rows(
    readRDS("thesis/_data/increased_rate_counts.rds") %>%
      dplyr::mutate(ID = ID + max(counts$ID))
  ) %>%
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

life_stage_plot %>%
  saveRDS("thesis/_plots/exploratory/life-stages.rds")

rm(life_stage_plot, life_stages)

# how many diploids vs polyploids did we have, when it was turned on?
adults <- counts %>%
  dplyr::filter(population %in% c("diploid_adults", "polyploid_adults"))

rm(counts)

adult_plot <- adults %>%
  ggplot(aes(x = gen, y = count, colour = population)) +
    geom_point(alpha = .2) +
    scale_colour_manual(values = my_palette) +
    theme_classic() +
    theme(legend.position = "top")

adult_plot %>%
  saveRDS("thesis/_plots/exploratory/diploid-polyploid-counts.rds")

rm(adult_plot)

# zoom in on polyploidy
polyploids <- adults %>%
  dplyr::filter(population == "polyploid_adults") 

polyploid_plot <- polyploids %>%
  ggplot(aes(x = gen, y = count, colour = ploidy_rate)) +
  geom_point(alpha = .2) +
  scale_colour_gradient(low = ploidy_gradient[1], high = ploidy_gradient[2]) +
  theme_classic() +
  theme(legend.position = "right")

polyploid_plot %>%
  saveRDS("thesis/_plots/exploratory/polyploid-counts.rds")

rm(polyploid_plot)

# and explode that out into facets
polyploid_plot_2 <- polyploids %>%
  ggplot(aes(x = gen, y = count, colour = count)) +
  geom_point(alpha = .2) +
  scale_colour_gradient(low = ploidy_gradient[1], high = ploidy_gradient[2]) +
  theme_classic() +
  theme(legend.position = "none") +
  facet_wrap(~round(ploidy_rate, 3), ncol = 5)

polyploid_plot_2 %>%
  saveRDS("thesis/_plots/exploratory/polyploid-counts-by-rate.rds")

rm(polyploid_plot_2)

# and show the rate relationship
last_500yrs <- adults %>%
  dplyr::filter(gen > 500)

rm(adults)

stable_rate_trends <- last_500yrs %>%
  ggplot(aes(x = ploidy_rate, y = count, colour = population)) +
    geom_point(alpha = .2) +
    scale_colour_manual(values = my_palette) +
    facet_wrap(~population, scales = "free_y") +
    theme_classic() +
    theme(legend.position = "top")

stable_rate_trends %>%
  saveRDS("thesis/_plots/exploratory/diploid-polyploid-count~rate-trends.rds")

rm(last_500yrs, stable_rate_trends)

# what does the pattern look like for individual replicates?
random_replicates <- runif(2, 1, max(polyploids$ID)) %>% round()
random_replicates <- polyploids %>%
  dplyr::filter(ID %in% random_replicates)

random_replicates_plot <- random_replicates %>%
  ggplot(aes(x = gen, y = count, colour = count)) +
    scale_colour_gradient(low = ploidy_gradient[1], high = ploidy_gradient[2]) +
    geom_point() +
    geom_line() +
    theme_classic() +
    theme(legend.position = "right") +
    facet_wrap(~ID, nrow = 2)

random_replicates_plot %>%
  saveRDS("thesis/_plots/exploratory/2-random-replicates.rds")

rm(random_replicates, random_replicates_plot, polyploids)
