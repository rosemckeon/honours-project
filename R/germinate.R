#' @name germinate
#' @title germinate seeds into juveniles 
#' @description Changes some seeds within a dataframe into juveniles, based on the locations of adults as well as the probability of germination.
#' @author Rose McKeon
#' @param seeds a population dataframe that contains only seeds (life_stage = 0). These are the seeds that will germinate according to prob.
#' @param adults optional population dataframe that contains only adults (required life_stage 2 as well X and Y coordinates for both seeds and adults). If an adult is already established in a landscape cell, seeds in that cell will not germinate (default = NULL).
#' @param prob a number between 0 and 1 representing the probability of germination.
#' @return juveniles 
#' @export
germinate <- function(
  seeds = NULL,
  adults = NULL,
  prob = .5
){
  # make sure we have the right kind of parameters
  stopifnot(
    is.data.frame(seeds),
    "life_stage" %in% colnames(seeds),
    nrow(seeds) > 0,
    all(seeds$life_stage == 0),
    is.numeric(prob),
    between(prob, 0, 1)
  )
  # do extra steps if there are adults
  if(sum(nrow(adults)) > 0){
    message("  adults detected.")
    stopifnot(
      is.data.frame(adults),
      "life_stage" %in% colnames(adults),
      all(adults$life_stage == 2),
      "X" %in% colnames(seeds),
      "Y" %in% colnames(seeds),
      "X" %in% colnames(adults),
      "Y" %in% colnames(adults)
    )
    # stop redundant germinations on cells with adults
    seeds <- disturploidy::get_those_not_outcompeted(
      adults, seeds, "Seeds unable to germinate because of established adults: "
    ) 
  }
  message("  checking for adults completed.")
  juveniles <- seeds %>% 
    # do germination by updating life stages
    mutate(life_stage = rbinom(nrow(seeds), 1, prob)) %>% 
    # then just keep those that germinated
    filter(life_stage == 1)
  return(juveniles)
}
