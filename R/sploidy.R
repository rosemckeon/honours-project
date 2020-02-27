#' @name sploidy
#' @title What are the relative densities of polyploids vs diploids?
#' @description A spatially explicit individual-based model which runs a simulation, or repeated simulations, of a plant population over time.
#' @usage sploidy()
#' @author Rose McKeon
#' @param pop_size integer representing starting population size, all individuals begin as juveniles (default = 500).
#' @param grid_size integer representing the size of the landscape grid. Cells are numbered 0 to grid_size -1 along an X and Y axis (default = 10, so the grid is 10 x 10).
#' @param K integer representing K, the carrying capacity (max population size) of any given cell. Seeds and juveniles are not taken into account for K, only adults who compete for resouces after growth (which creates adults) but before reproduction (default = 1, so only 1 new adult per square can survive to reproduce).
#' @param germination_prob number between 0 and 1 representing the probability that any seed will germinate on cells which are not yet populated by adults (default = 0.3).
#' @param N_ovules integer representing the number of ovules any individual plant can create (default = 25).
#' @param pollen_range integer between 0 and grid_size - 1 representing the dispersal range of pollen (default = 9).
#' @param seed_dispersal_range whole number between 0 and grid_size - 1 representing the maximum distance a seed can travel (default = 9).
#' @param adult_survival_prob number between 0 and 1 representing survival probability of adults between generations (default = 0.5).
#' @param juvenile_survival_prob number representing the constant which converts trait values into probabilities. Used to select for plants wth higher growth rates by weighting survival chances of larger juveniles between generations (default = 0.1).
#' @param seed_survival_prob number between 0 and 1 representing survival probability of seeds between generations (default = 0, so there is no seedbank). New seeds are pooled with surviving seeds from previous generations after reproduction. Survival takes place before germination.
#' @param ploidy_rate number between 0 and 1 representing the chance that genome duplication will occur (default = 0, so no genome duplication).
#' @param generations integer representing the number of generations the model should attempt to run for (default = 10). The simulation will break early if extinction occurs.
#' @param simulations integer representing the number of simulations which should be run with these parameters (default = 2).
#' @param return logical value which indicates whether or not to return output at the end of the simulation/s.
#' @param filepath character string defining the file path where output files should be stored (relative to working directory). If this folder does not exist it will be created (default = "data").
#' @param name character string defining the name of the filepath subfolder which countains output RDS files. (default = unique random identifier).
#' @param log logical value which indicates whether a log file with verbose output messages should be stored in filepath/name (default = T).
#' @return if return == T, a dataframe of all simulations will be returned showing the population state at the end of each generation (immediately after reproduction, before survival). If return == F, data/dploidy.rda will be stored automatically and can be accessed with `data(sploidy)`.
#' @examples
#' # with default parameters
#' sploidy()
#' data(sploidy)
#' sploidy
#'
#' # with minimal console output
#' # (the rest logged to TXT files)
#' sploidy(logfilename = "whatever")
#'
#' # with stored data object as RDS file
#' sploidy(filename = "whatever")
#'
#' # assigning output to a new object
#' whatever <- sploidy(return = T)
#'
#' @export
sploidy <- function(
  pop_size = 500,
  grid_size = 10,
  K = 1,
  germination_prob = .3,
  N_ovules = 25,
  pollen_range = 9,
  seed_dispersal_range = 9,
  adult_survival_prob = .5,
  juvenile_survival_prob = .1,
  seed_survival_prob = 0,
  ploidy_rate = 0,
  generations = 10,
  simulations = 2,
  return = FALSE,
  filepath = "data",
  name = NULL,
  log = T
){
  start_time <- Sys.time()
  tic.clearlog()
  tic("Entire run time")
  # parameter checking
  stopifnot(
    is.numeric(
      c(
        pop_size,
        grid_size,
        K,
        germination_prob,
        N_ovules,
        pollen_range,
        seed_dispersal_range,
        adult_survival_prob,
        juvenile_survival_prob,
        seed_survival_prob,
        ploidy_rate,
        generations,
        simulations
      )
    ),
    is.logical(c(return, log)),
    is.character(filepath),
    c(
      pop_size,
      grid_size,
      K,
      N_ovules,
      pollen_range,
      seed_dispersal_range,
      generations,
      simulations
    )%%1==0,
    between(
      c(
        germination_prob,
        adult_survival_prob,
        juvenile_survival_prob,
        seed_survival_prob,
        ploidy_rate
      ),
      0, 1
    ),
    between(pollen_range, 0, grid_size - 1),
    between(seed_dispersal_range, 0, grid_size - 1)
  )
  # make sure there is a subfolder name for the set of simulations
  if(is_null(name)){
    name <- random_id(1, 10)
  }
  message("Parameters are all appropriate. Simulations can begin...")
  # store the function call for session info
  sploidy_call <- match.call()
  # setup objects for data storage
  seedbank <- NULL
  juveniles <- NULL
  adults <- NULL
  seedoutput <- NULL

  # Run the replicate simulations
  for(this_sim in 1:simulations){
    tic(paste0("Simulation ", this_sim, " complete"))
    # Start logging
    if(log){ log_info <- setup_log() }
    message("SIMULATION ", this_sim, ":")
    message("*************")
    message("Starting population of ", pop_size, " random diploid seeds.")
    # advance time
    for(generation in 1:generations){
      # initialise temp life stage files
      seedbank_tmp_file <- store_tmp_data(seedbank, "seedbank", generation)
      juvenile_tmp_file <- store_tmp_data(juveniles, "juveniles", generation)
      adult_tmp_file <- store_tmp_data(adults, "adults", generation)
      seedoutput_tmp_file <- store_tmp_data(seedoutput, "seedoutput", generation)
      tmp_files <- c(seedbank_tmp_file, juvenile_tmp_file, adult_tmp_file, seedoutput_tmp_file)
      # save data at the end of every gen and clear tmp file cache
      store_data(tmp_files, name, this_sim, filepath)
    }
    # stop logging
    if(log){ 
      store_data(log_info$path, name, this_sim, filepath)
      stop_log(log_info)
    }
    toc()
  }
  
  #get_session(sploidy_call, start_time, Sys.time(), filepath, name)
  toc()
}
