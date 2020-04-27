# objects needed for knitting results.Rmd into thesis_2417024.Rmd
rm(list=ls())
`%notin%` <- Negate(`%in%`)
library(tidyverse)
# split the trimmed sim data into groups that all have nrow number of sims
# successful stable simulations
stability <- simulations %>%
  tidyr::unnest(cols = c(data)) %>%
  dplyr::filter(gen == 1000, adults >= 0.99, diploid_adults > 0, polyploid_adults > 0) %>%
  dplyr::distinct()
# some seem to manage to persist with just one individual even though there's no seeds
# not sure why - may be some erroneous code? removing them incase this needs investigating.
polyploid_winners <- simulations %>%
  tidyr::unnest(cols = c(data)) %>%
  dplyr::filter(gen == 1000, diploid_adults == 0, polyploid_adults >= 0.00001) %>%
  dplyr::distinct()

stable_polyploid_winners <- polyploid_winners %>% filter(adults >= 0.99)
unstable_polyploid_winners <- polyploid_winners %>% filter(adults < 0.99)

diploid_winners <- simulations %>%
  tidyr::unnest(cols = c(data)) %>%
  dplyr::filter(gen == 1000, adults < 0.99, polyploid_adults == 0, diploid_adults >= 0.00001) %>%
  dplyr::distinct()

success <- c(stability$ID, polyploid_winners$ID, diploid_winners$ID) %>% unique() %>% sort()

# simulations that did not make it.
extinctions <- simulations[-which(simulations$ID %in% success), ] %>%
  tidyr::unnest(cols = c(data)) %>%
  dplyr::group_by(ID, ploidy_rate, starting_N) %>%
  dplyr::summarise(gen_extinction_occurred = max(gen))

# exploratory plots -----
# all the sims

# 
# simulations %>%
#   dplyr::filter(ID %in% extinctions$ID) %>%
#   tidyr::unnest(cols = c(data)) %>%
#   tidyr::gather(subset, count, diploid_adults:sterile_polyploid_adults) %>%
#   dplyr::filter(count > 0) %>%
#   ggplot(aes(x = gen, y = count, colour = subset)) + 
#   geom_point(alpha = .2, size = .2)
# 
# simulations %>%
#   dplyr::filter(ID %in% stable_polyploid_winners$ID) %>%
#   tidyr::unnest(cols = c(data)) %>%
#   tidyr::gather(subset, count, diploid_adults:sterile_polyploid_adults) %>%
#   dplyr::filter(count > 0) %>%
#   ggplot(aes(x = gen, y = count, colour = subset)) + 
#   geom_point(alpha = .2, size = .2)
# 
# simulations %>%
#   dplyr::filter(ID %in% unstable_polyploid_winners$ID) %>%
#   tidyr::unnest(cols = c(data)) %>%
#   tidyr::gather(subset, count, diploid_adults:sterile_polyploid_adults) %>%
#   dplyr::filter(count > 0) %>%
#   ggplot(aes(x = gen, y = count, colour = subset)) + 
#   geom_point(alpha = .2, size = .2)



# the rest ------
cost <- simulations %>% 
  dplyr::summarise(count = max(polyploid_adults)) %>%
  dplyr::mutate(diff = count/ploidy_rate) %>%
  dplyr::arrange(diff)

cost_summary <- cost %>%
  dplyr::ungroup() %>%
  dplyr::mutate(ploidy_rate = round(ploidy_rate, 2)) %>%
  dplyr::group_by(ploidy_rate) %>%
  dplyr::summarise(
    mean = mean(diff),
    sd = sd(diff),
    lower = mean - sd * 1.96,
    upper = mean + sd * 1.96
  )

range <- counts %>%
  dplyr::select(-seeds, -adults, -diploid_seeds, - polyploid_seeds, -total) %>%
  dplyr::filter(between(ploidy_rate, 0.001, .25)) %>%
  tidyr::gather(
    "subset", "count",
    diploid_adults:polyploid_adults
  )

range <- range %>%
  dplyr::mutate(
    panel = "whatever",
    panel = replace(
      panel, which(round(range$ploidy_rate, 3) <= 0.171),
      "Below threshold"
    ),
    panel = replace(
      panel, which(round(range$ploidy_rate, 3) > 0.171),
      "Over threshold"
    )
  )



counts %>%
  tidyr::gather(
    "subset", "count", 
    diploid_adults:polyploid_adults
  ) %>%
  filter(
    gen == 1000,
    between(ploidy_rate, 0, 0.25)  
  ) %>%
  ggplot(aes(x = ploidy_rate, y = count, colour = subset)) +
  scale_colour_manual(values = ploidylvl_colours) +
  geom_vline(xintercept = 0.171, color = "red", size = .2) +
  geom_point(alpha = .2) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 7, face = "bold"),
    plot.background = element_rect(fill = "#efefef"),
    strip.background = element_rect(colour = "white"),
    strip.text = element_text(size = 7, face = "bold")
  ) +
  xlab("Rate of genome-doubling") +
  ylab("Frequency of adults in population") +
  scale_x_continuous(
    breaks = seq(0, .2, .1)
    #limits = c(0, .25)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, .5),
    limits = c(0, 1.01)
  )