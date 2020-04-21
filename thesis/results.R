#library(sploidy)
#count_combine("data/results", "thesis/_data/results")
counts <- readRDS("_data/results.rds")

counts <- counts %>%
  dplyr::bind_rows(
    readRDS("_data/sim_counts.rds") %>%
      dplyr::mutate(ID = ID + max(counts$ID))
  )

counts <- counts %>%
  dplyr::bind_rows(
    readRDS("_data/increased_rate_counts.rds") %>%
      dplyr::mutate(ID = ID + max(counts$ID))
  )

# make all the counts proportional
K <- 100000
counts <- counts %>%
  dplyr::mutate(
    seeds = seeds / K,
    adults = adults / K,
    diploid_seeds = diploid_seeds / K,
    polyploid_seeds = polyploid_seeds / K,
    diploid_adults = diploid_adults / K,
    polyploid_adults = polyploid_adults / K
  ) %>%
  dplyr::select(gen, ploidy_rate, G_modifier, ID, seeds, adults, diploid_seeds, polyploid_seeds, diploid_adults, polyploid_adults, total)

# get number of simulations in various groups
rates <- counts %>%
  dplyr::group_by(ploidy_rate, ID) %>%
  nest() %>%
  group_by(ploidy_rate) %>%
  tally()

N <- tibble(
  control = rates %>%
    dplyr::filter(ploidy_rate == 0) %>%
    dplyr::pull(n) %>%
    sum(),
  WGD_on = rates %>%
    dplyr::filter(ploidy_rate > 0) %>%
    dplyr::pull(n) %>%
    sum(),
  WGD_1 = rates %>%
    dplyr::filter(between(ploidy_rate, 0.001, 0.010)) %>%
    dplyr::pull(n) %>%
    sum(),
  WGD_2 = rates %>%
    dplyr::filter(between(ploidy_rate, 0.01, 0.1)) %>%
    dplyr::pull(n) %>%
    sum(),
  WGD_3 = rates %>%
    dplyr::filter(between(ploidy_rate, 0.1, 0.25)) %>%
    dplyr::pull(n) %>%
    sum()
)

rm(rates)

cost <- counts %>%
  select(-seeds, -adults, -diploid_seeds, -polyploid_seeds, -diploid_adults, -total) %>%
  dplyr::filter(
    between(ploidy_rate, 0.001, 0.25),
    # polyploid_adults > ploidy_rate,
    gen == 1000
  ) %>%
  group_by(ID, ploidy_rate) %>%
  summarise(count = max(polyploid_adults)) %>%
  mutate(diff = count/ploidy_rate) %>%
  arrange(diff)

cost_summary <- cost %>%
  dplyr::filter(between(ploidy_rate, 0.001, 0.171)) %>%
  mutate(ploidy_rate = round(ploidy_rate, 2)) %>%
  group_by(ploidy_rate) %>%
  summarise(
    mean = mean(diff),
    sd = sd(diff),
    lower = mean - sd * 1.96,
    upper = mean + sd * 1.96
  )

range <- counts %>%
  select(-seeds, -adults, -diploid_seeds, - polyploid_seeds, -total) %>%
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
