#' @name sploidy
#' @title What are the relative densities of polyploids vs diploids?
#' @description A spatially explicit individual-based model which runs a simulation, or repeated simulations, of a plant population over time.
#' @usage sploidy()
#' @author Rose McKeon
#' @param pop_size integer representing starting population size, all individuals begin as juveniles (default = 500).
#' @param grid_size integer representing the size of the landscape grid. Cells are numbered 0 to grid_size -1 along an X and Y axis (default = 10, so the grid is 10 x 10).
#' @param K integer representing K, the carrying capacity (max population size) of any given cell. Seeds and juveniles are not taken into account for K, only adults who compete for resouces after growth (which creates adults) but before reproduction (default = 1, so only 1 new adult per square can survive to reproduce).
#' @param germination_prob number between 0 and 1 representing the probability that any seed will germinate on cells which are not yet populated by adults (default = 0.3).
#' @param growth_prob number between 0 and 1 representing the probability that juveniles will become adults (default = 0.5).
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
  growth_prob = .5,
  N_ovules = 25,
  pollen_range = 9,
  seed_dispersal_range = 9,
  adult_survival_prob = .5,
  juvenile_survival_prob = .1,
  seed_survival_prob = .9,
  ploidy_rate = 0,
  generations = 10,
  simulations = 2,
  return = FALSE,
  filepath = "data",
  name = NULL,
  log = T
){
  tictoc::tic.clearlog()
  tictoc::tic("Entire run time")
  # parameter checking ----------
  stopifnot(
    is.numeric(
      c(
        pop_size,
        grid_size,
        K,
        germination_prob,
        growth_prob,
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
    dplyr::between(
      c(
        germination_prob,
        growth_prob,
        adult_survival_prob,
        juvenile_survival_prob,
        seed_survival_prob,
        ploidy_rate
      ),
      0, 1
    ),
    dplyr::between(pollen_range, 0, grid_size - 1),
    dplyr::between(seed_dispersal_range, 0, grid_size - 1)
  )
  # BEGIN --------------
  # make sure there is a subfolder name for the set of simulations
  if(is_null(name)){
    name <- ids::random_id(1, 10)
  }
  message("Parameters are all appropriate.")
  message("Simulation set ", name, " can begin...")
  # store the session info
  store_session(match.call(), name)
  # setup objects for data storage
  juveniles <- NULL; adults <- NULL; seeds <- NULL

  # Run the replicate simulations
  for(this_sim in 1:simulations){
    start_time <- Sys.time()
    tictoc::tic(paste0("Simulation ", this_sim, " complete"))
    folder_sim <- paste0("sim-", sprintf("%03d", this_sim))
    # Start logging
    if(log){ log_info <- setup_log() }
    message("SIMULATION ", this_sim, ":")
    # advance time
    for(generation in 0:generations){
      message("Generation: ", generation, " ----------")
      file_gen <- sprintf("%04d", generation)
      # initialise temp life stage files
      tmp_files <- setup_tmp_files(juveniles, adults, seeds, generation)
      # LOAD GEN DATA -----------
      if(generation == 0){
        # start with a cohort of diploids
        message("Populating ", grid_size, " by ", grid_size, " landscape with ", pop_size, " diploid juveniles.")
        juveniles <- disturploidy::create_pop(pop_size, grid_size, this_sim) %>%
          dplyr::mutate(
            ploidy_lvl = "diploid", # no need for genomes
            gen = 0, 
            # manual germination
            size = 1, 
            life_stage = 1 
          )
      } else {
        message("Loading data from last generation...")
        # or load data from the last generation into this one
        last_gen <- generation - 1
        last_file_gen <- sprintf("%04d", last_gen)
        juveniles <- readRDS(file.path(filepath, name, folder_sim, paste0("sploidy-juveniles-", last_file_gen, ".rds")))
        adults <- readRDS(file.path(filepath, name, folder_sim, paste0("sploidy-adults-", last_file_gen, ".rds")))
        seeds <- readRDS(file.path(filepath, name, folder_sim, paste0("sploidy-seeds-", last_file_gen, ".rds")))
      }
      # save data to tmp files
      store_tmp_data(juveniles, paste0("sploidy-juveniles-", file_gen))
      store_tmp_data(adults, paste0("sploidy-adults-", file_gen))
      store_tmp_data(seeds, paste0("sploidy-seeds-", file_gen))
      
      # SURVIVAL -----------------
      # only happens after initial generation of growth and competition
      if(generation > 0){
        message("Survival:")
        tictoc::tic("Survival")
        if(!is.null(juveniles)){
          n_juveniles <- nrow(juveniles)
          if(n_juveniles > 0){
            # keep only random survivors
            juveniles <- juveniles %>% 
              # make sure generation updated to this one
              mutate(gen = generation) %>% 
              disturploidy::survive(juvenile_survival_prob)
            message("  Surviving juveniles: ", nrow(juveniles), "/", n_juveniles)
          }
        }
        if(!is.null(adults)){
          n_adults <- nrow(adults)
          if(n_adults > 0){
            # keep only random survivors
            adults <- adults %>% 
              # make sure generation updated to this one
              mutate(gen = generation) %>% 
              disturploidy::survive(adult_survival_prob)
            message("  Surviving adults: ", nrow(adults), "/", n_adults)
          }
        }
        if(!is.null(seeds)){
          n_seeds <- nrow(seeds)
          if(n_seeds > 0){
            # keep only random survivors
            seeds <- seeds %>% 
              # make sure generation updated to this one
              mutate(gen = generation) %>% 
              disturploidy::survive(seed_survival_prob)
            message("  Surviving seeds: ", nrow(seeds), "/", n_seeds)
          }
        }
        # check for extinction
        if(sum(nrow(juveniles), nrow(adults), nrow(seeds)) == 0){
          message("  *** EXTINCTION ***")
          message("  Ending simulation.")
          break
        }
        tictoc::toc() # survival
      }
      # update tmp files
      store_tmp_data(juveniles, paste0("sploidy-juveniles-", file_gen))
      store_tmp_data(adults, paste0("sploidy-adults-", file_gen))
      store_tmp_data(seeds, paste0("sploidy-seeds-", file_gen))
      
      # GERMINATION ------------
      message("Germination:")
      tictoc::tic("Germination")
      # only germinate if there are seeds to transition
      if(sum(nrow(seeds)) > 0){
        juveniles <- bind_rows(
          juveniles, 
          disturploidy::germinate(seeds, adults, germination_prob) %>% filter(life_stage == 1)
        )
        seeds <- NULL # no seedbank
        message("  Seeds germinated at a rate of ", germination_prob)
      } else {
        message("  No seeds to germinate.")
      }
      # update tmp files
      store_tmp_data(juveniles, paste0("sploidy-juveniles-", file_gen))
      store_tmp_data(seeds, paste0("sploidy-seeds-", file_gen))
      tictoc::toc() # germination
      
      # GROWTH -----------------
      message("Growth:")
      tictoc::tic("Growth")
      # growth is just transition between life stages so only juveniles grow
      if(sum(nrow(juveniles)) > 0){
        # decide which ones will grow
        growth <- rbinom(nrow(juveniles), 1, growth_prob) == 0
        if(any(growth)){
          adults <- bind_rows(
            adults,
            juveniles[which(growth), ] %>% mutate(life_stage = 2)
          )
          juveniles <- juveniles[-which(growth), ]
          message("  New adults: ", length(which(growth)))
          # all those that have made it to adult are competative enough to clone
          if(sum(nrow(adults)) > 0){
            # clones can appear in any adjacent square
            # SHALL WE BOTHER CLONING?
            # SHALL WE REMOVE OTHER JUVENILES WHERE ADULTS HAVE EMERGED?
          } else {
            message("  No adults to clone.")
          }
        }
      } else {
        message("  No juveniles to grow.")
      }
      # update tmp files
      store_tmp_data(juveniles, paste0("sploidy-juveniles-", file_gen))
      store_tmp_data(adults, paste0("sploidy-adults-", file_gen))
      tictoc::toc() # growth
      
      # COMPETITION -------------
      message("Competition:")
      tictoc::tic("Competition")
      # Only K adults can survive on any one space
      if(sum(nrow(adults)) > 0){
        # get cell density counts
        N <- adults %>% dplyr::group_by(X,Y) %>% dplyr::arrange(X,Y) %>% dplyr::tally() %>% dplyr::pull(n)
        # nest and arrange adults by coordinates so rows match the vector above
        adults <- adults %>% disturploidy::nest_by_location()
        # subset those that need to compete 
        temp <- adults[which(N > K), ]
        # and keep those that didn't
        adults <- adults[which(N <= K), ] %>% tidyr::unnest(plants)
        message("  Adults that survive without competing: ", nrow(adults))
        # Does competition occur?
        if(sum(nrow(temp)) > 0){
          message("  Locations with competition between adults: ", nrow(temp))
          # decide who wins
          temp$plants <- apply(temp, 1, disturploidy::compete, K)
          adults <- bind_rows(adults, temp %>% tidyr::unnest(plants))
          message("  Adults after competition: ", nrow(adults))
          rm(temp)
        }
      } else {
        message("  No adults to compete.")
      }
      # update tmp files
      store_tmp_data(adults, paste0("sploidy-adults-", file_gen))
      tictoc::toc() # Competition
      
      # REPRODUCTION --------------
      message("Reproduction:")
      tictoc::tic("Reproduction")
      # only happens when there are adults
      if(sum(nrow(adults)) > 0){
        seeds <- reproduce(adults, N_ovules, pollen_range, ploidy_rate, grid_size, generation)
      }
      # save data to tmp files
      store_tmp_data(seeds, paste0("sploidy-seeds-", file_gen))
      tictoc::toc()
      
      # PROPER SAVE AND CLEAR CACHE --------------
      store_data(tmp_files, name, this_sim)
    }
    message("Simulation duration: ", start_time - Sys.time())
    # stop logging
    tictoc::toc() # sim time
    tictoc::toc() # sim time (not sure why there's an extra toc needed? do we have an erroneous tic?)
    if(log){ 
      store_data(log_info$path, name, this_sim, filepath)
      stop_log(log_info)
    }
  }
  message("Simulation set ", name, " with ", simulations, " replicate simulations complete.")
  tictoc::toc() # run time
}
