# objects needed for knitting results.Rmd into thesis_2417024.Rmd
#rm(list=ls())
#`%notin%` <- Negate(`%in%`)
#library(tidyverse)
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

outcomes <- dplyr::bind_rows(
  control %>%
    tidyr::unnest(cols = c(data)) %>%
    dplyr::filter(adults > 0.99) %>%
    dplyr::summarise(gen_reached = min(gen)) %>%
    dplyr::mutate(outcome = "control"),
  extinctions %>%
    dplyr::mutate(outcome = "total extinction") %>%
    dplyr::rename(gen_reached = gen_extinction_occurred),
  simulations %>%
    filter(ID %in% stability$ID) %>%
    unnest(cols = c(data)) %>%
    filter(adults > 0.99) %>%
    group_by(ID, ploidy_rate, starting_N) %>%
    summarise(gen_reached = min(gen)) %>%
    mutate(outcome = "stable coexistence"),
  simulations %>%
    filter(ID %in% unstable_polyploid_winners$ID) %>%
    unnest(cols = c(data)) %>%
    filter(polyploid_adults < 0.99, diploid_adults == 0) %>%
    group_by(ID, ploidy_rate, starting_N) %>%
    summarise(gen_reached = min(gen)) %>%
    mutate(outcome = "unstable polyploid fixation"),
  simulations %>%
    filter(ID %in% stable_polyploid_winners$ID) %>%
    unnest(cols = c(data)) %>%
    filter(polyploid_adults > 0.99, diploid_adults == 0) %>%
    group_by(ID, ploidy_rate, starting_N) %>%
    summarise(gen_reached = min(gen)) %>%
    mutate(outcome = "stable polyploid fixation")
)

outcomes$outcome <- factor(
  outcomes$outcome,
  levels = c(
    "control",
    "stable coexistence",
    "total extinction",
    "unstable polyploid fixation",
    "stable polyploid fixation"
  )
)

nonreduction <- tibble::tibble(
  ploidy_rate = seq(0, 0.5, length.out = 1000)
)

polyploid_winners <- dplyr::bind_rows(
  simulations %>%
    dplyr::filter(ID %in% stable_polyploid_winners$ID) %>%
    dplyr::mutate(outcome = "stable polyploid fixation"),
  simulations %>%
    dplyr::filter(ID %in% unstable_polyploid_winners$ID) %>%
    dplyr::mutate(outcome = "unstable polyploid fixation")
) %>%
  tidyr::unnest(cols = c(data)) %>%
  dplyr::mutate(
    viable_polyploid_adults = polyploid_adults - sterile_polyploid_adults
  ) %>%
  group_by(ID, ploidy_rate, starting_N, outcome) %>%
  nest()

# simulations %>%
#   unnest(cols = c(data)) %>%
#   dplyr::mutate(
#     viable_polyploid_adults = polyploid_adults - sterile_polyploid_adults
#   ) %>%
#   filter(between(viable_polyploid_adults, 0.4, 0.6), gen < 50) %>%
#   pull(gen) %>% mean()
#
# simulations %>%
#   unnest(cols = c(data)) %>%
#   filter(between(sterile_polyploid_adults, 0.4, 0.6), gen < 50) %>%
#   pull(gen) %>% mean()

# over_464 <- simulations %>%
#   tidyr::unnest(cols = c(data)) %>%
#   dplyr::mutate(viable_polyploid_adults = polyploid_adults - sterile_polyploid_adults) %>%
#   dplyr::filter(ploidy_rate > 0.464) %>%
#   group_by(ID, ploidy_rate, starting_N) %>%
#   nest()
#
# which(over_464$ID %in% extinctions$ID) %>% length()

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
