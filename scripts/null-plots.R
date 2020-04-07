rm(list=ls())
library(tidyverse)
# read in the data and make long
counts <- readRDS("thesis/_data/null_counts.rds") %>%
  tidyr::gather("population", "count", seeds:total)
# plot the data
null_plot <- counts %>%
  dplyr::filter(population %in% c("seeds", "seedlings", "rosettes")) %>%
  ggplot(
    aes(x = gen, y = count, colour = population),
    alpha = .3
  ) +
  geom_point() +
  theme_classic() +
  theme(legend.position = "top")

saveRDS(null_plot, "thesis/_data/null_plot.rds")
