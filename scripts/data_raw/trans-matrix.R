library(tidyverse)
# DATA -------------------------------
# from @Peterson2016
mimulus <- tibble(
  year = c(rep(2012, 7), rep(2013, 7)),
  group = as.factor(rep(c("Annuals", "Perennials", "Low-elevation perennials", "Montane perennials", "Silver Fork", "Silver Creek", "Eagle Meadows"), 2)),
  G = c(0.732, 0.549, 0.652, 0.478, 0.552, 0.410, 0.469, 0.732, 0.549, 0.652, 0.478, 0.552, 0.410, 0.469),
  O = c(363, 662, 621, 690, 803, 650, 614, 157, 523, 494, 542, 635, 525, 479),
  F = c(8.59, 3.12, 6.88, 0.56, 0.40, 0.63, 0.64, 4.91, 2.27, 5.34, 0.28, 0.29, 0.27, 0.29),
  S = c(0, 0.152, 0.162, 0.145, 0.100, 0.158, 0.179, 0, 0.052, 0, 0.086, 0.048, 0.083, 0.122),
  R = c(0, 1.03, 1.04, 1.02, 0.82, 0.66, 1.56, 0, 0.19, 0, 0.32, 0.17, 0.46, 0.31),
  n = c(239, 197, 80, 117, 40, 38, 39, 240, 199, 77, 122, 38, 42, 42)
)
A <- 6.7e-4 # from @Peterson2016
D <- 0.534 # from @Elder2006

# Calculate transition matrices ------
stages <- c("seed", "seedling", "rosette")
matrices <- list()
for(row in 1:nrow(mimulus)){
  G <- mimulus$G[row]
  O <- mimulus$O[row]
  F <- mimulus$F[row]
  S <- mimulus$S[row]
  R <- mimulus$R[row]
  # build the population matrix
  matrices[[row]] <- matrix(
    c(
      D * (1 - G),
      D * G,
      0,
      F * O * A * (1 - G),
      F * O * A * G,
      S * R,
      F * O * A * (1 - G),
      F * O * A * G,
      S * R
    ), 
    ncol = 3, nrow = 3
  )
}

# update data
mimulus <- mimulus %>% dplyr::mutate(matrix = matrices)
saveRDS(mimulus, "scripts/data/mimulus.RDS")

# calculate mean transisiton matrix -------------------
#trans <- apply(simplify2array(matrices), c(1, 2), mean)
#saveRDS(trans, "scripts/data/trans-matrix.RDS")
