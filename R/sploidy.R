#' @name sploidy
#' @title What are the relative densities of polyploids vs diploids?
#' @description A spatially explicit individual-based model which runs a simulation, or repeated simulations, of a plant population over time.
#' @usage sploidy()
#' @author Rose McKeon
#' @param pop_size integer representing starting population size, all individuals begin as seedlings (default = 500).
#' @param grid_size integer representing the size of the landscape grid. Cells are numbered 0 to grid_size -1 along an X and Y axis (default = 10, so the grid is 10 x 10).
#' @param ploidy_rate number between 0 and 1 representing the chance that genome duplication will occur (default = 0, so no genome duplication).
#' @param trans matrix (3x3) representing the tranisiton probabilities between life stages 1 to 3 (default = NULL).
#' @param generations integer representing the number of generations the model should attempt to run for (default = 10). The simulation will break early if extinction occurs.
#' @param simulations integer representing the number of simulations which should be run with these parameters (default = 2).
#' @param return logical value which indicates whether or not to return output at the end of the simulation/s.
#' @param filepath character string defining the file path where output files should be stored (relative to working directory). If this folder does not exist it will be created (default = "data").
#' @param name character string defining the name of the filepath subfolder which countains output RDS files. (default = unique random identifier).
#' @param log logical value which indicates whether a log file with verbose output messages should be stored in filepath/name (default = T).
#' @return if return == T, a dataframe of all simulations will be returned showing the population state at the end of each generation (immediately after reproduction, before survival). If return == F, data/dploidy.rda will be stored automatically and can be accessed with data(sploidy).
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
  ploidy_rate = 0.01,
  trans = NULL,
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
        ploidy_rate,
        trans,
        generations,
        simulations
      )
    ),
    is.logical(c(return, log)),
    is.character(filepath),
    c(
      pop_size,
      grid_size,
      generations,
      simulations
    )%%1==0,
    dplyr::between(
      c(ploidy_rate, trans),
      0, 1
    )
  )
  # BEGIN --------------
  # make sure there is a subfolder name for the set of simulations
  if(is.null(name)){
    name <- ids::random_id(1, 10)
  }
  message("Parameters are all appropriate.")
  message("Simulation set ", name, " can begin...")
  # store the session info
  store_session(match.call(), name)
  # setup objects for data storage
  seedlings <- NULL; rosettes <- NULL; seeds <- NULL

  # Run the replicate simulations
  for(this_sim in 1:simulations){
    start_time <- Sys.time()
    tictoc::tic(paste0("Simulation ", this_sim, " complete"))
    folder_sim <- paste0("sim-", sprintf("%03d", this_sim))
    # Start logging
    if(log){ log_info <- setup_log() }
    message("# SIMULATION ", this_sim, ": Ploidy rate = ", ploidy_rate)
    # advance time
    for(generation in 0:generations){
      message("## Generation: ", generation, " ----------")
      file_gen <- sprintf("%04d", generation)
      # initialise temp life stage files
      tmp_files <- setup_tmp_files(seedlings, rosettes, seeds, generation)
      # LOAD GEN DATA -----------
      if(generation == 0){
        # start with a cohort of diploids
        message("Populating ", grid_size, " by ", grid_size, " landscape with ", pop_size, " diploid seedlings.")
        seedlings <- disturploidy::create_pop(pop_size, grid_size, this_sim) %>%
          dplyr::mutate(
            ploidy = 2, # no need for genomes
            gen = 0, 
            # manual germination
            life_stage = 2 
          ) %>%
          dplyr::select(-size, -sim)
      } else {
        message("Loading data from last generation...")
        # or load data from the last generation into this one
        last_gen <- generation - 1
        last_file_gen <- sprintf("%04d", last_gen)
        seedlings <- readRDS(file.path(filepath, name, folder_sim, paste0("seedlings_", last_file_gen, ".rds")))
        rosettes <- readRDS(file.path(filepath, name, folder_sim, paste0("rosettes_", last_file_gen, ".rds")))
        seeds <- readRDS(file.path(filepath, name, folder_sim, paste0("seeds_", last_file_gen, ".rds")))
      }
      # save data to tmp files
      store_tmp_data(seedlings, paste0("seedlings_", file_gen))
      store_tmp_data(rosettes, paste0("rosettes_", file_gen))
      store_tmp_data(seeds, paste0("seeds_", file_gen))
      if(log){ store_data(log_info$path, name, this_sim, filepath, T) }
      
      # SURVIVAL -----------------
      # only happens after initial generation of growth and competition
      last_seedlings <- NULL; last_rosettes <- NULL
      if(generation > 0){
        message("Survival:")
        tictoc::tic("Survival")
        if(!is.null(seeds)){
          n_seeds <- nrow(seeds)
          if(n_seeds > 0){
            seeds <- seeds %>% survive(trans[1,1])
            message("  Surviving seeds: ", nrow(seeds), "/", n_seeds)
          }
        }
        last_seedlings <- seedlings
        seedlings <- NULL
        message("  Surviving seedlings: 0/", sum(nrow(last_seedlings))) 
        if(!is.null(rosettes)){
          n_rosettes <- nrow(rosettes)
          if(n_rosettes > 0){
            last_rosettes <- rosettes # save last gen rosettes for germination
            rosettes <- rosettes %>% survive(trans[3,3]) 
            message("  Surviving rosettes: ", nrow(rosettes), "/", n_rosettes)
          }
        }
        # check for extinction
        if(sum(nrow(seedlings), nrow(rosettes), nrow(seeds)) == 0){
          message("  *** EXTINCTION ***")
          message("  Ending simulation.")
          break
        }
        tictoc::toc() # survival
      }
      # update tmp files
      store_tmp_data(seedlings, paste0("seedlings_", file_gen))
      store_tmp_data(rosettes, paste0("rosettes_", file_gen))
      store_tmp_data(seeds, paste0("seeds_", file_gen))
      if(log){ store_data(log_info$path, name, this_sim, filepath, T) }
      
      # GERMINATION ------------
      message("Germination:")
      tictoc::tic("Germination")
      # only germinate if there are seeds to transition or rosettes to transition
      # @Pearson2016 supply transtion values for seed to seedling as well as from rosette to seedling
      # Both are given as processes of germination so the seedlings created from rosettes should
      # also be sexually produced with new parental ploidy levels etc.
      if(sum(nrow(seeds), nrow(last_rosettes), nrow(last_seedlings)) > 0){
        # germinate seeds created in the last generation
        # seed -> seedling transition
        message("  Seedlings before germination: ", sum(nrow(seedlings)))
        if(sum(nrow(seeds)) > 0){
          germination_results <- seeds %>% survive(trans[2,1], dead = T)
          if(germination_results %>% inherits("list")){
            # there were ungerminated seeds
            # were there also germinations?
            if(sum(nrow(germination_results$survivors)) > 0){
              seedlings <- germination_results$survivors %>% 
                  dplyr::mutate(life_stage = 2)
              message("  Seeds germinated: ", sum(nrow(germination_results$survivors)), "/", sum(nrow(seeds)))
            }
            message("  Seeds ungerminated: ", sum(nrow(germination_results$deaths)), "/", sum(nrow(seeds)))
            seeds <- germination_results$deaths
          } else {
            # all seeds were germinated
            seedlings <- germination_results %>% 
                dplyr::mutate(life_stage = 2)
            message("  Seeds germinated: ", sum(nrow(germination_results)), "/", sum(nrow(seeds)))
            seeds <- NULL
          }
          rm(germination_results)
          #message("  Seedlings after seed -> seedling transition: ", sum(nrow(seedlings)))
        }
        # germinate assumed additional seeds based on rosettes
        # rosette -> seedling transition
        if(sum(nrow(last_rosettes)) > 0){
          seedlings <- dplyr::bind_rows(
            seedlings,
            last_rosettes %>% 
              as.seeds(dplyr::bind_rows(last_seedlings, last_rosettes), generation - 1) %>%
              dplyr::mutate(life_stage = 2) %>%
              duplicate_genomes(ploidy_rate) %>%
              disturploidy::move(grid_size, F, grid_size - 1) %>%
              survive(trans[2,3]) 
          )
          #message("  Seedlings after rosette -> seedling transition: ", sum(nrow(seedlings)))
        }
        # germinate assumed additional seeds based on seedlings
        # seedling -> seedling transition
        if(sum(nrow(last_seedlings)) > 0){
          seedlings <- dplyr::bind_rows(
            seedlings,
            last_seedlings %>% 
              as.seeds(dplyr::bind_rows(last_seedlings, last_rosettes), generation - 1) %>%
              dplyr::mutate(life_stage = 2) %>%
              duplicate_genomes(ploidy_rate) %>%
              disturploidy::move(grid_size, F, grid_size - 1) %>%
              survive(trans[2,2]) 
          )
          #message("  Seedlings after seedling -> seedling transition: ", sum(nrow(seedlings)))
        }
        rm(last_seedlings, last_rosettes)
        message("  Seedlings after all transitions: ", sum(nrow(seedlings)))
      } else {
        message("  No seeds germinated.")
      }
      # update tmp files
      store_tmp_data(seedlings, paste0("seedlings_", file_gen))
      store_tmp_data(seeds, paste0("seeds_", file_gen))
      if(log){ store_data(log_info$path, name, this_sim, filepath, T) }
      tictoc::toc() # germination
      
      # A-SEXUAL REPRODUCTION -------------
      message("A-sexual Reproduction:")
      tictoc::tic("A-sexual Reproduction")
      # only happens when there are asexually reproducing life stages 
      if(sum(nrow(seedlings)) > 0){
        # add new clonal growth to existing rosettes
        rosettes <- dplyr::bind_rows(
          rosettes,
          seedlings %>% 
            disturploidy::move(grid_size, F, 2) %>%
            dplyr::mutate(life_stage = 3, ID = paste(ID, generation, "_")) %>% 
            survive(trans[3,2]) # mutate and move first incase no survivors
        )
        message("  Rosettes after reproduction: ", nrow(rosettes))
      } else {
        message("  No A-sexually reproducing plants on landscape.")
      }
      # save data to tmp files
      store_tmp_data(rosettes, paste0("rosettes_", file_gen))
      if(log){ store_data(log_info$path, name, this_sim, filepath, T) }
      tictoc::toc()
      
      # SEXUAL REPRODUCTION --------------
      message("Sexual Reproduction:")
      tictoc::tic("Sexual Reproduction")
      # only happens when there are sexually reproducing life stages
      if(sum(nrow(seedlings), nrow(rosettes)) > 0){
        # combine sexually reproducing stages to make a pool of pollen donors
        parents <- dplyr::bind_rows(seedlings, rosettes)
        message("  Sexually reproducing plants: ", nrow(parents))
        # choose the mothers and set size of seed pool with transition probabilities
        new_seeds <- NULL
        if(sum(nrow(seedlings)) > 0){
          new_seeds <- seedlings %>% survive(trans[1,2])
        }
        if(sum(nrow(rosettes)) > 0){
          new_seeds <- dplyr::bind_rows(
            new_seeds,
            rosettes %>% survive(trans[1,3])
          )
        }
        # if we actually have new seeds
        if(sum(nrow(new_seeds)) > 0){
          new_seeds <- new_seeds %>%
            disturploidy::move(grid_size, F, grid_size - 1) %>%
            as.seeds(parents, generation) %>% 
            duplicate_genomes(ploidy_rate)
        }
        message("  Viable new seeds: ", sum(nrow(new_seeds)))
        # combine with seedbank
        message("  Seeds in seedbank: ", sum(nrow(seeds)))
        seeds <- bind_rows(seeds, new_seeds)
        rm(parents, new_seeds)
        message("  Total seeds: ", sum(nrow(seeds)))
      } else {
        message("  No sexually reproducing plants on landscape.")
      }
      # save data to tmp files
      store_tmp_data(seeds, paste0("seeds_", file_gen))
      if(log){ store_data(log_info$path, name, this_sim, filepath, T) }
      tictoc::toc()
      
      # PROPER SAVE AND CLEAR CACHE --------------
      store_data(tmp_files, name, this_sim)
      seeds <- NULL; seedlings <- NULL; rosettes <- NULL;
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
