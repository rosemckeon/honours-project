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
    is.data.frame(pop),
    is.numeric(ploidy_rate),
    dplyr::between(ploidy_rate, 0, 1)
  )
  # choose individuals that undergo whole-genome duplication
  duplication <- stats::rbinom(nrow(pop), 1, ploidy_rate) == 1
  if(any(duplication)){
    pop <- pop %>% dplyr::mutate(
      ploidy = replace(
        # double ploidy where duplication occurs
        ploidy, which(duplication), pop[which(duplication), ]$ploidy * 2
      )
    ) 
    message("  Duplication events: ", which(duplication == T) %>% length())
  }
  rm(duplication)
  return(pop)
}
