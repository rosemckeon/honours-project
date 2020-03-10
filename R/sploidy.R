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
#' @param fecundity integer representing the mean number of seeds output by any individual adult plant (default = 25).
#' @param fecundity_sd integer representing the standard deviation around the fecundity mean (default = fecundity/5).
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
  fecundity = 25,
  fecundity_sd = NULL,
  adult_survival_prob = .5,
  juvenile_survival_prob = .1,
  seed_survival_prob = .9,
  ploidy_rate = 0.01,
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
        fecundity,
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
      fecundity,
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
    )
  )
  # BEGIN --------------
  # make sure there is a subfolder name for the set of simulations
  if(is.null(name)){
    name <- ids::random_id(1, 10)
  }
  if(is.null(fecundity_sd)){
    fecundity_sd <- fecundity / 5
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
            ploidy = 2, # no need for genomes
            gen = 0, 
            # manual germination
            life_stage = 1 
          ) %>%
          dplyr::select(-size, -sim)
      } else {
        message("Loading data from last generation...")
        # or load data from the last generation into this one
        last_gen <- generation - 1
        last_file_gen <- sprintf("%04d", last_gen)
        juveniles <- readRDS(file.path(filepath, name, folder_sim, paste0("juveniles_", last_file_gen, ".rds")))
        adults <- readRDS(file.path(filepath, name, folder_sim, paste0("adults_", last_file_gen, ".rds")))
        seeds <- readRDS(file.path(filepath, name, folder_sim, paste0("seeds_", last_file_gen, ".rds")))
      }
      # save data to tmp files
      store_tmp_data(juveniles, paste0("juveniles_", file_gen))
      store_tmp_data(adults, paste0("adults_", file_gen))
      store_tmp_data(seeds, paste0("seeds_", file_gen))
      if(log){ store_data(log_info$path, name, this_sim, filepath, T) }
      
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
              dplyr::mutate(gen = generation) %>% 
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
              dplyr::mutate(gen = generation) %>% 
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
              dplyr::mutate(gen = generation) %>% 
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
      store_tmp_data(juveniles, paste0("juveniles_", file_gen))
      store_tmp_data(adults, paste0("adults_", file_gen))
      store_tmp_data(seeds, paste0("seeds_", file_gen))
      if(log){ store_data(log_info$path, name, this_sim, filepath, T) }
      
      # GERMINATION ------------
      message("Germination:")
      tictoc::tic("Germination")
      # only germinate if there are seeds to transition
      if(sum(nrow(seeds)) > 0){
        juveniles <- dplyr::bind_rows(juveniles, seeds)
        seeds <- NULL # no seedbank
        message("  Juveniles after germination ", nrow(juveniles))
      } else {
        message("  No seeds germinated.")
      }
      # update tmp files
      store_tmp_data(juveniles, paste0("juveniles_", file_gen))
      store_tmp_data(seeds, paste0("seeds_", file_gen))
      if(log){ store_data(log_info$path, name, this_sim, filepath, T) }
      tictoc::toc() # germination
      
      # GROWTH -----------------
      message("Growth:")
      tictoc::tic("Growth")
      # growth is just transition between life stages so only juveniles grow
      if(sum(nrow(juveniles)) > 0){
        # decide which ones will grow
        growth <- rbinom(nrow(juveniles), 1, growth_prob) == 0
        if(any(growth)){
          adults <- dplyr::bind_rows(
            adults,
            juveniles[which(growth), ] %>% dplyr::mutate(life_stage = 2)
          )
          juveniles <- juveniles[-which(growth), ]
          message("  New adults: ", length(which(growth)))
        }
      } else {
        message("  No juveniles to grow.")
      }
      # update tmp files
      store_tmp_data(juveniles, paste0("juveniles_", file_gen))
      store_tmp_data(adults, paste0("adults_", file_gen))
      if(log){ store_data(log_info$path, name, this_sim, filepath, T) }
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
          adults <- dplyr::bind_rows(adults, temp %>% tidyr::unnest(plants))
          message("  Adults after competition: ", nrow(adults))
          rm(temp)
        }
      } else {
        message("  No adults to compete.")
      }
      # update tmp files
      store_tmp_data(adults, paste0("adults_", file_gen))
      if(log){ store_data(log_info$path, name, this_sim, filepath, T) }
      tictoc::toc() # Competition
      
      # REPRODUCTION --------------
      message("Reproduction:")
      tictoc::tic("Reproduction")
      # only happens when there are adults
      if(sum(nrow(adults)) > 0){
        # replicate adult data to create seeds with some stochasticity around seed output
        seeds <- adults[rep(row.names(adults), rnorm(sum(nrow(adults)), fecundity, fecundity_sd)), ] %>% dplyr::ungroup(ID)
        message("  Fertilisation attempts: ", nrow(seeds))
        # exclude polyploid outcrossing with diploids
        # infact make ploidy levels only be able to mate with the exact same ploidy level.
        seeds <- seeds %>%
          dplyr::mutate(
            ploidy_mum = ploidy,
            ploidy_dad = sample(adults$ploidy, nrow(seeds), replace = T),
            ploidy = (ploidy_mum/2) + (ploidy_dad/2), # very basic
            gen = generation,
            life_stage = 0
          ) 
        seeds <- seeds[which(seeds$ploidy_mum == seeds$ploidy_dad), ]
        message("  Viable seeds: ", nrow(seeds))
        # decide which ones are fated to germinate so we store less data
        seeds <- seeds %>% 
          dplyr::mutate(life_stage = rbinom(nrow(seeds), 1, germination_prob)) %>% 
          dplyr::filter(life_stage == 1)
        # now give new IDS to those that make it
        seeds <- seeds %>% dplyr::mutate(ID = paste(generation + 1, 1:nrow(seeds), sep = "_"))
        # and make whole genome duplication occur at ploidy_rate specified 
        duplication <- rbinom(nrow(seeds), 1, ploidy_rate) == 1
        if(any(duplication)){
          seeds <- seeds %>% dplyr::mutate(
            ploidy = replace(
              # double ploidy where duplication occurs
              ploidy, which(duplication), seeds[which(duplication), ]$ploidy * 2
            )
          )
          message("  Duplication events: ", which(duplication == T) %>% length())
        }
        # disperse!
        seeds <- disturploidy::move(seeds, grid_size, FALSE, grid_size - 1)
      }
      # save data to tmp files
      store_tmp_data(seeds, paste0("seeds_", file_gen))
      if(log){ store_data(log_info$path, name, this_sim, filepath, T) }
      tictoc::toc()
      
      # PROPER SAVE AND CLEAR CACHE --------------
      store_data(tmp_files, name, this_sim)
      seeds <- NULL; juveniles <- NULL; adults <- NULL;
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
