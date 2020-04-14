rm(list=ls())
library(tidyverse)
my_palette = c("#000000", "#30a068")
diploid_colour = "#000000"
polyploid_colour = "#30a068"
ploidy_gradient = c("#10442a", "#3bbf7d")
# read in the data and make long
counts <- readRDS("thesis/_data/germination_counts.rds") %>%
  tidyr::gather("population", "count", seeds:total) %>%
  dplyr::filter(ploidy_rate < 0.4)

# what did the life stage composition look like
# VERY similar to null tests
life_stages <- counts %>%
  dplyr::filter(population %in% c("seeds", "seedlings", "rosettes")) %>%
  mutate(treatment = "Ploidy rate > 0, G_modifier > 1") %>%
  bind_rows(
    readRDS("thesis/_data/sim_counts.rds") %>%
      tidyr::gather("population", "count", seeds:total) %>%
      dplyr::filter(population %in% c("seeds", "seedlings", "rosettes")) %>%
      dplyr::mutate(
        treatment = "Ploidy rate > 0, G_modifier = 1",
        G_modifier = 1
      ),
    readRDS("thesis/_data/null_counts.rds") %>%
      tidyr::gather("population", "count", seeds:total) %>%
      dplyr::filter(population %in% c("seeds", "seedlings", "rosettes")) %>%
      dplyr::mutate(
        treatment = "Ploidy rate = 0, G_modifier = 1", 
        G_modifier = 1, 
        ploidy_rate = as.numeric(ploidy_rate)
      )
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

adults <- counts %>%
  dplyr::filter(population %in% c("diploid_adults", "polyploid_adults"))

adult_plot <- adults %>%
  ggplot(aes(x = gen, y = count, colour = population)) +
  geom_point(alpha = .2) +
  scale_colour_manual(values = my_palette) +
  theme_classic() +
  theme(legend.position = "top")

polyploid_plot_4 <- adults %>%
  ggplot(aes(x = ploidy_rate, y = count/100000, colour = gen)) +
  geom_point(alpha = .2) +
  theme_classic()

polyploid_plot_4 <- polyploid_plot_4 +
  facet_wrap(~population) 

polyploid_plot_4 %>%
  saveRDS("thesis/_plots/exploratory/germination_frequency~rate-diploids-vs-polyploids.rds")
