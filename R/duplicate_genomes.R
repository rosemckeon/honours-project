#' @name duplicate_genomes
#' @title duplicate genomes
#' @description Double the ploidy value of random individuals within a population data frame at a specified rate.
#' @author Rose McKeon
#' @param pop A data frame containing individuals with a numerical column 'ploidy' that will be doubled (default = NULL).
#' @param ploidy_rate A number between 0 and 1 which represents the rate at which genome duplication occurs (default = 0.01).
#' @return a dataframe containing the updated population.
#' @export
duplicate_genomes <- function(pop = NULL, ploidy_rate = 0.01){
  stopifnot(
    is.numeric(ploidy_rate),
    dplyr::between(ploidy_rate, 0, 1)
  )
  # make sure this function still runs if passed empty seeds pop
  # may happen in the rare case that all parents are odd ploidy
  if(sum(nrow(pop)) > 0){
    # choose unreduced gametes
    f_unreduced <- stats::rbinom(nrow(pop), 1, ploidy_rate) == 1
    m_unreduced <- stats::rbinom(nrow(pop), 1, ploidy_rate) == 1
    # then double those gametes
    if(any(f_unreduced)){
      pop <- pop %>% dplyr::mutate(
        fn = replace(
          fn, which(f_unreduced), pop[which(f_unreduced), ]$fn * 2
        )
      ) 
      message("  Unreduced female gametes: ", which(f_unreduced == T) %>% length())
    }
    if(any(m_unreduced)){
      pop <- pop %>% dplyr::mutate(
        mn = replace(
          mn, which(m_unreduced), pop[which(m_unreduced), ]$mn * 2
        )
      ) 
      message("  Unreduced male gametes: ", which(f_unreduced == T) %>% length())
    }
    # work out new base ploidy level of seeds
    pop <- pop %>% dplyr::mutate(ploidy = fn + mn)
    # clean up 
    rm(f_unreduced, m_unreduced)
  }
  return(pop)
}
