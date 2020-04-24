#' @name as.seeds
#' @title format data as seeds
#' @description Formats new seeds so they have all the right data regarding ploidy levels, generation and life_stage, while also removing seeds whose parents have ploidy levels that do not match.
#' @author Rose McKeon
#' @param seeds a dataframe containing stage one seeds created directly from parents (default = NULL).
#' @param parents a dataframe containing all the viable parents on the landscape (default = NULL).
#' @param generation integer representing generation that seeds were produced in.
#' @return a dataframe containing updated seeds.
#' @export
as.seeds <- function(seeds = NULL, parents = NULL, generation = NULL){
  stopifnot(
    is.data.frame(seeds),
    is.data.frame(parents),
    is.numeric(generation),
    generation%%1==0
  )
  #if(sum(nrow(parents)) > 0){
    seeds <- seeds %>%
      dplyr::mutate(
        life_stage = 1,
        ploidy_mum = ploidy,
        ploidy_dad = sample(parents$ploidy, nrow(seeds), replace = T),
        fn = ploidy_mum/2,
        mn = ploidy_dad/2,
        ploidy = fn + mn,
        gen_created = generation,
        # and rename with a new ID
        ID = paste(generation, 1:nrow(seeds), sep = "_")
      ) 
    return(seeds)
  #} else {
  #  return(NULL) # no valid parents so no seeds
  #}
}
