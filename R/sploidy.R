#' @name sploidy
#' @title What are the relative densities of polyploids vs diploids?
#' @description A spatially explicit individual-based model which runs a simulation, or repeated simulations, of a plant population over time.
#' @usage sploidy()
#' @author Rose McKeon
#' @param pop_size numerical vector of integers representing starting population size by life stage (default = c(5, 5, 5)).
#' @param grid_size integer representing the size of the landscape grid. Cells are numbered 0 to grid_size -1 along an X and Y axis (default = 10, so the grid is 10 x 10).
#' @param ploidy_rate number between 0 and 1 representing the chance that genome duplication will occur (default = 0, so no genome duplication).
#' @param trans matrix (3x3) representing the tranisiton probabilities between life stages 1 to 3. Values [1,1] and [2,1] are not used. These transtitons are a product of G*D instead (default = NULL).
#' @param G numerical value representing the rate of germination (default = 0.5).
#' @param G_modifier numerical value representing the factor by which to multiply G for polyploid seeds (default = 1, no change).
#' @param D numerical value representing the seed survival rate (default = 0.5).
#' @param K numerical value representing the carrying capacity of the entire landscape (default = grid_size x grid_size x 10).
#' @param seed_longevity integer representing the maximum seed life during dormancy (default = 1).
#' @param generations integer representing the number of generations the model should attempt to run for (default = 10). The simulation will break early if extinction occurs.
#' @param simulations integer representing the number of simulations which should be run with these parameters (default = 2).
#' @param filepath character string defining the file path where output files should be stored (relative to working directory). If this folder does not exist it will be created (default = "data").
#' @param name character string defining the name of the filepath subfolder which countains output RDS files. (default = unique random identifier).
#' @param log logical value which indicates whether a log file with verbose output messages should be stored in filepath/name (default = T).
#' @param output_gen_data logical value that defines whether population data contining info on all individuals at the end of every generation is stored (default = F, storage intensive).
#' @return Nothing is returned. Messages are output to the console so you can see each generation complete, and data output is stored according to the filepath and name paremeters.
#' @export
sploidy <- function(
  pop_size = c(5, 5, 5),
  grid_size = 10,
  ploidy_rate = 0.01,
  trans = NULL,
  G = 0.5,
  G_modifier = 1,
  D = 0.5,
  K = NULL,
  seed_longevity = 25,
  generations = 10,
  simulations = 2,
  return = FALSE,
  filepath = "data",
  name = NULL,
  log = T,
  output_gen_data = F
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
        G,
        G_modifier,
        D,
        seed_longevity,
        generations,
        simulations
      )
    ),
    is.logical(c(output_gen_data, log)),
    is.character(filepath),
    c(
      pop_size,
      grid_size,
      seed_longevity,
      generations,
      simulations
    )%%1==0
  )
  # BEGIN --------------
  # make sure there is a subfolder name for the set of simulations
  if(is.null(name)){
    name <- ids::random_id(1, 10)
  }
  if(is.null(K)){
    # default carrying capacity amounts to 10 individuals per cell
    K <- grid_size * grid_size * 10
  }
  message("Parameters are all appropriate.")
  message("Simulation set ", name, " can begin...")
  # store the session info
  store_session(match.call(), name, filepath)

  # Run the replicate simulations
  for(this_sim in 1:simulations){
    start_time <- Sys.time()
    tictoc::tic(paste0("Simulation ", this_sim, " complete"))
    folder_sim <- paste0("sim-", sprintf("%03d", this_sim))
    # Start logging
    if(log){ log_info <- setup_log() }
    message("# SIMULATION ", this_sim, ": Ploidy rate = ", ploidy_rate)
    # setup objects for data storage
    last_seedlings <- NULL; last_rosettes <- NULL; last_seeds <- NULL
    seedlings <- NULL; rosettes <- NULL; seeds <- NULL
    counts <- tibble::tibble(
      gen = numeric(),
      seeds = numeric(),
      diploid_seeds = numeric(),
      polyploid_seeds = numeric(),
      seedlings = numeric(),
      rosettes = numeric(),
      adults = numeric(),
      diploid_adults = numeric(),
      polyploid_adults = numeric(),
      total = numeric(),
      ploidy_rate = numeric(),
      G_modifier = numeric()
    )
    # initialise tmp count file
    counts_tmp <- store_tmp_data(counts, "_counts")
    # advance time
    for(generation in 0:generations){
      tictoc::tic(paste0("Generation ", generation, " complete"))
      message("## Generation: ", generation, " ----------")
      file_gen <- sprintf("%04d", generation)
      # initialise temp life stage files
      seeds_tmp <- store_tmp_data(seeds, paste0("seeds_", file_gen))
      seedlings_tmp <- store_tmp_data(seedlings, paste0("seedlings_", file_gen))
      rosettes_tmp <- store_tmp_data(rosettes, paste0("rosettes_", file_gen))
      # LOAD GEN DATA -----------
      if(generation == 0){
        message("Populating ", grid_size, " by ", grid_size, " landscape with:")
        # start with a cohort of diploids
        if(pop_size[1] > 0){
          message("  ", pop_size[1], " diploid seeds.")
          seeds <- disturploidy::create_pop(pop_size[1], grid_size, this_sim) %>%
            dplyr::mutate(
              ploidy = 2,
              gen = 0,
              gen_created = 0,
              life_stage = 1
            ) %>%
            dplyr::select(-size, -sim)
        }
        if(pop_size[2] > 0){
          message("  ", pop_size[2], " diploid seedlings.")
          seedlings <- disturploidy::create_pop(pop_size[2], grid_size, this_sim) %>%
            dplyr::mutate(
              ploidy = 2,
              gen = 0,
              gen_created = 0,
              life_stage = 2
            ) %>%
            dplyr::select(-size, -sim)
        }
        if(pop_size[3] > 0){
          message("  ", pop_size[3], " diploid rosettes.")
          rosettes <- disturploidy::create_pop(pop_size[3], grid_size, this_sim) %>%
            dplyr::mutate(
              ploidy = 2,
              gen = 0,
              gen_created = 0,
              life_stage = 3
            ) %>%
            dplyr::select(-size, -sim)
        }
        # update tmp files
        store_tmp_data(seedlings, paste0("seedlings_", file_gen))
        store_tmp_data(rosettes, paste0("rosettes_", file_gen))
        store_tmp_data(seeds, paste0("seeds_", file_gen))
        if(log){ store_data(log_info$path, name, this_sim, filepath, T) }
      } else {
        # or load data from the last generation (t) into this one
        message("Loading data from last generation...")
        last_gen <- generation - 1
        last_file_gen <- sprintf("%04d", last_gen)
        # store the tmp file paths for removal
        last_seeds_tmp <- file.path(tempdir(), paste0("seeds_", last_file_gen, ".rds"))
        last_seedlings_tmp <- file.path(tempdir(), paste0("seedlings_", last_file_gen, ".rds"))
        last_rosettes_tmp <- file.path(tempdir(), paste0("rosettes_", last_file_gen, ".rds"))
        # and load the data into objects
        last_seeds <- readRDS(last_seeds_tmp)
        last_seedlings <- readRDS(last_seedlings_tmp)
        last_rosettes <- readRDS(last_rosettes_tmp)
        # remove tmp files and objects no longer needed
        unlink(c(last_seeds_tmp, last_seedlings_tmp, last_rosettes_tmp))
        rm(last_seeds_tmp, last_seedlings_tmp, last_rosettes_tmp)
        # save log
        if(log){ store_data(log_info$path, name, this_sim, filepath, T) }
        # and use these data for transitions to t+1
        message("Transitioning to seeds from seeds...")
        # Transition 1,1 (Seed to Seed) -------------
        # transition value = seed survival * (1-germination)
        if(sum(nrow(last_seeds)) > 0){
          # make sure the seedbank can't get too crazy big by removing seeds older than seed_longevity
          last_seeds <- last_seeds %>%
            dplyr::filter(gen - gen_created <= seed_longevity)
          # then transition the remainder
          # Rather than use the transition value (trans[1,1]) here we're going to do seed survival and then
          # work out germination results so the correct germinated and ungerminated seeds go to the right place.
          # That way we're not just having the same seed pool being resampled at a probability rate.
          seeds <- last_seeds %>% survive(D)
          message("  Seeds that survive: ", sum(nrow(seeds)), "/", nrow(last_seeds))
          germination_results <- NULL
          if(sum(nrow(seeds)) > 0){
            # separate the diploid seeds
            diploids <- seeds %>%
              dplyr::filter(ploidy == 2)
            # do germination for diploid seeds
            if(sum(nrow(diploids)) > 0){
              diploid_germination_results <- seeds %>% survive(G, dead = T)
              if(diploid_germination_results %>% inherits("list")){
                message("  Surviving diploid seeds that don't germinate: ", sum(nrow(diploid_germination_results$deaths)), "/", sum(nrow(diploids)))
                diploids <- diploid_germination_results$deaths
                germination_results$survivors <- diploid_germination_results$survivors
              } else {
                message("  Surviving diploid seeds that don't germinate: 0/", sum(nrow(diploids)))
                diploids <- NULL
                germination_results <- diploid_germination_results
              }
              rm(diploid_germination_results)
            }
            # and then separate the polyploid seeds
            polyploids <- seeds %>%
              dplyr::filter(ploidy > 2)
            # so the germination rate can be modified
            if(sum(nrow(polyploids)) > 0){
              polyploid_germination_results <- seeds %>% survive(G * G_modifier, dead = T)
              if(polyploid_germination_results %>% inherits("list")){
                message("  Surviving polyploid seeds that don't germinate: ", sum(nrow(polyploid_germination_results$deaths)), "/", sum(nrow(polyploids)))
                polyploids <- polyploid_germination_results$deaths
                germination_results$survivors <- dplyr::bind_rows(
                  germination_results$survivors,
                  polyploid_germination_results$survivors
                )
              } else {
                message("  Surviving polyploid seeds that don't germinate: 0/", sum(nrow(polyploids)))
                polyploids <- NULL
                germination_results <- dplyr::bind_rows(
                  germination_results,
                  polyploid_germination_results
                )
              }
              rm(polyploid_germination_results)
            }
            # bind together all the seeds that didn't germinate and clean up objects
            seeds <- dplyr::bind_rows(diploids, polyploids)
            rm(diploids, polyploids)
          }
        }
        # store data and log
        store_tmp_data(seeds, paste0("seeds_", file_gen))
        if(log){ store_data(log_info$path, name, this_sim, filepath, T) }

        message("Transitioning to seeds from sexual life stages...")
        if(sum(nrow(last_seedlings), nrow(last_rosettes)) > 0){
          # combine sexually reproducing stages to make a pool of pollen donors
          last_parents <- dplyr::bind_rows(last_seedlings, last_rosettes)
          message("  Sexually reproducing plants last generation: ", nrow(last_parents))
          # Transition 1,2 (Seedling to Seed) -------------
          if(sum(nrow(last_seedlings)) > 0){
            new_seeds <- last_seedlings %>% survive(trans[1,2])
          }
          # Transition 1,2 (Rosette to Seed) -------------
          if(sum(nrow(rosettes)) > 0){
            new_seeds <- dplyr::bind_rows(
              new_seeds,
              rosettes %>% survive(trans[1,3])
            )
          }
          # if we have new seeds
          if(sum(nrow(new_seeds)) > 0){
            new_seeds <- new_seeds %>%
              disturploidy::move(grid_size, FALSE, grid_size - 1) %>%
              as.seeds(last_parents, generation) %>%
              duplicate_genomes(ploidy_rate)
          }
          message("  Viable new seeds: ", sum(nrow(new_seeds)))
          # combine with seedbank and relable as belonging to this gen
          seeds <- dplyr::bind_rows(seeds, new_seeds) %>%
            dplyr::mutate(gen = generation)
          rm(new_seeds)

          message("  Total seeds: ", sum(nrow(seeds)))
        } else {
          message("  No seeds created as no seedlings or rosettes last generation to mate.")
        }
        # store data and log
        store_tmp_data(seeds, paste0("seeds_", file_gen))
        if(log){ store_data(log_info$path, name, this_sim, filepath, T) }

        # Transition 2,1 (Seed to Seedling) -------------
        message("Transitioning to seedlings from seeds...")
        # Transition value = D * G
        # This part is also not using transition value, but using seed survival (D) and germination rate (G)
        # so that the germinated seeds are separated correctly from the ungerminated ones.
        if(germination_results %>% inherits("list")){
          # there were ungerminated seeds
          # were there also germinations?
          if(sum(nrow(germination_results$survivors)) > 0){
            seedlings <- germination_results$survivors %>%
              dplyr::mutate(life_stage = 2)
            message("  Surviving seeds that germinate: ", sum(nrow(germination_results$survivors)), "/", sum(nrow(seeds)))
          }
        } else if(!is.null(germination_results)) {
          # all seeds were germinated
          seedlings <- germination_results %>%
            dplyr::mutate(life_stage = 2)
          message("  Surviving seeds that germinate: ", sum(nrow(germination_results)), "/", sum(nrow(seeds)))
        }
        # store data and log
        store_tmp_data(seedlings, paste0("seedlings_", file_gen))
        if(log){ store_data(log_info$path, name, this_sim, filepath, T) }

        # Transition 2,2 (Seedling to Seedling) -------------
        # Transition value = F*O*A*G
        # There's no survival here, just sexual reproduction and germination
        message("Transitioning to seedlings from seedlings...")
        if(sum(nrow(last_seedlings)) > 0){
          if(trans[2,2] > 1){
            # growth
            new_seedlings <- last_seedlings %>%
              # loosing some stochasity around the number of seedlings produced here
              dplyr::sample_n(nrow(last_seedlings) * trans[2,2], replace = T)
          } else {
            # decline
            new_seedlings <- last_seedlings %>%
              survive(trans[2,2])
          }
          if(sum(nrow(new_seedlings)) > 0){
            # complete the transition
            new_seedlings <- new_seedlings %>% as.seeds(last_parents, generation - 1) 
            # make sure we didn't loos all the seeds to ploidy mismatching
            if(sum(nrow(new_seedlings)) > 0){
              new_seedlings <- new_seedlings %>%
                dplyr::mutate(life_stage = 2) %>%
                duplicate_genomes(ploidy_rate) %>%
                disturploidy::move(grid_size, FALSE, grid_size - 1)
            }
            # and combine
            seedlings <- dplyr::bind_rows(seedlings, new_seedlings)
          }
        }
        message("  New seedlings created: ", sum(nrow(new_seedlings)))
        new_seedlings  <- NULL
        # store data and log
        store_tmp_data(seedlings, paste0("seedlings_", file_gen))
        if(log){ store_data(log_info$path, name, this_sim, filepath, T) }

        # Transition 2,3 (Rosette to Seedling) -------------
        # Transition value = F*O*A*G
        # There's no survival here, just sexual reproduction and germination
        message("Transitioning to seedlings from rosettes...")
        if(sum(nrow(last_rosettes)) > 0){
          if(trans[2,3] > 1){
            # growth
            new_seedlings <- last_rosettes %>%
              # loosing some stochasity around the number of seedlings produced here
              dplyr::sample_n(nrow(last_rosettes) * trans[2,3], replace = T)
          } else {
            # decline
            new_seedlings <- last_rosettes %>%
              survive(trans[2,3])
          }
          if(sum(nrow(new_seedlings)) > 0){
            # complete the transition
            new_seedlings <- new_seedlings %>%
              as.seeds(last_parents, generation - 1) %>%
              dplyr::mutate(life_stage = 2) %>%
              duplicate_genomes(ploidy_rate) %>%
              disturploidy::move(grid_size, FALSE, grid_size - 1)
          }
        }
        message("  New seedlings created: ", sum(nrow(new_seedlings)))
        seedlings <- dplyr::bind_rows(seedlings, new_seedlings) %>%
          dplyr::mutate(gen = generation)
        rm(new_seedlings, last_parents)
        message("  Total seedlings: ", sum(nrow(seedlings)))
        # store data and log
        store_tmp_data(seedlings, paste0("seedlings_", file_gen))
        if(log){ store_data(log_info$path, name, this_sim, filepath, T) }

        # Transition 3,1 = 0 (may need to add in code for this at some point incase of different matrices)

        # Transition 3,2 (Seedling to Rosette)
        # Transition value = S*R
        # Survival and vegatative rosette production (clones are defined by lack of ID change)
        message("Transitioning to rosettes from seedlings...")
        if(sum(nrow(last_seedlings)) > 0){
          if(trans[3,2] > 1){
            # growth
            new_rosettes <- last_seedlings %>%
            # loosing some stochasity around the number of rosettes produced here
            dplyr::sample_n(nrow(last_seedlings) * trans[3,2], replace = T)
          } else {
            # decline
            new_rosettes <- last_seedlings %>%
              survive(trans[3,2])
          }
          if(sum(nrow(new_rosettes)) > 0){
            # complete the transition
            new_rosettes <- new_rosettes %>%
              disturploidy::move(grid_size, FALSE, 2) %>% # movement range limited to 2 cells
              dplyr::mutate(life_stage = 3, gen_created = generation)
            rosettes <- dplyr::bind_rows(rosettes, new_rosettes)
          }
        }
        message("  Rosettes that survive or are created: ", sum(nrow(new_rosettes)))
        new_rosettes <- NULL
        rm(last_seedlings)
        # store data and log
        store_tmp_data(rosettes, paste0("rosettes_", file_gen))
        if(log){ store_data(log_info$path, name, this_sim, filepath, T) }

        # Transition 3,3 (Rosette to Rosette)
        # Transition value = S*R
        # Survival and vegatative rosette production (clones are defined by lack of ID change)
        message("Transitioning to rosettes from rosettes...")
        if(sum(nrow(last_rosettes)) > 0){
          if(trans[3,3] > 1){
            # growth
            new_rosettes <- last_rosettes %>%
              # loosing some stochasity around the number of rosettes produced here
              dplyr::sample_n(nrow(last_rosettes) * trans[3,3], replace = T)
          } else {
            # decline
            new_rosettes <- last_rosettes %>%
              survive(trans[3,3])
          }
          if(sum(nrow(new_rosettes)) > 0){
            # complete the transition
            new_rosettes <- new_rosettes %>%
              disturploidy::move(grid_size, FALSE, 2) %>% # movement range limited to 2 cells
              dplyr::mutate(life_stage = 3, gen_created = generation)
          }
        }
        message("  Rosettes that survive or are created: ", sum(nrow(new_rosettes)))
        rosettes <- dplyr::bind_rows(rosettes, new_rosettes) %>%
          dplyr::mutate(gen = generation)
        rm(new_rosettes, last_rosettes)
        message("  Total rosettes: ", sum(nrow(rosettes)))
        # store data and log
        store_tmp_data(rosettes, paste0("rosettes_", file_gen))
        if(log){ store_data(log_info$path, name, this_sim, filepath, T) }
      }
      # make sure Carrying Capacity is honoured to stop exponential growth
      not_seeds <- dplyr::bind_rows(seedlings, rosettes)
      if(sum(nrow(not_seeds)) > K){
        message("Population exceeds K by ", K - sum(nrow(not_seeds)))
        not_seeds <- not_seeds %>% survive(K / sum(nrow(not_seeds)))
        message("Population reduced to ", sum(nrow(not_seeds)))
        seedlings <- not_seeds %>%
          dplyr::filter(life_stage ==2)
        rosettes <- not_seeds %>%
          dplyr::filter(life_stage == 3)
      }
      store_tmp_data(seedlings, paste0("seedlings_", file_gen))
      store_tmp_data(rosettes, paste0("rosettes_", file_gen))
      if(log){ store_data(log_info$path, name, this_sim, filepath, T) }
      # PROPER SAVE, COUNT AND CLEAR CACHE --------------
      adults <- dplyr::bind_rows(seedlings, rosettes)
      this_count <- tibble::tibble(
          gen = generation,
          seeds = sum(nrow(seeds)),
          diploid_seeds = NA,
          polyploid_seeds = NA,
          seedlings = sum(nrow(seedlings)),
          rosettes = sum(nrow(rosettes)),
          adults = sum(nrow(adults)),
          diploid_adults = NA,
          polyploid_adults = NA,
          total = sum(seeds, adults), # uses the tibble vars now
          ploidy_rate = ploidy_rate,
          G_modifier = G_modifier
        )
      # make sure we count diploid vs polyploid seeds
      if(sum(nrow(seeds)) > 0){
        this_count$diploid_seeds <- seeds %>%
          dplyr::filter(ploidy == 2) %>% nrow() %>% sum()
        this_count$polyploid_seeds <- seeds %>%
          dplyr::filter(ploidy > 2) %>% nrow() %>% sum()
      }
      # make sure we count diploid vs polyploid adults
      if(sum(nrow(adults)) > 0){
        this_count$diploid_adults <- adults %>% dplyr::filter(ploidy == 2) %>% nrow() %>% sum()
        this_count$polyploid_adults <- adults %>% dplyr::filter(ploidy > 2) %>% nrow() %>% sum()
      }
      # combine these counts with all previous generations
      counts <- dplyr::bind_rows(counts, this_count)
      rm(adults)
      store_tmp_data(counts, "_counts")
      store_data(counts_tmp, name, this_sim, filepath, T)
      # store generation population data if requested
      if(output_gen_data){
        # keep the tmp files intact for loading from next gen
        store_data(seeds_tmp, name, this_sim, filepath, T, "seeds")
        store_data(seedlings_tmp, name, this_sim, filepath, T, "seedlings")
        store_data(rosettes_tmp, name, this_sim, filepath, T, "rosettes")
        # but remove the objects holding the paths that re no longer needed.
        rm(seeds_tmp, seedlings_tmp, rosettes_tmp)
      }
      seeds <- NULL; seedlings <- NULL; rosettes <- NULL
      tictoc::toc() # gen time
      # now all the gen data is stored, check for extinction
      if(this_count$total == 0){
        message("EXTINCTION.")
        rm(this_count)
        break
      }
      rm(this_count)
    }
    message("Simulation duration: ", start_time - Sys.time())
    tictoc::toc() # sim time
    # stop logging
    if(log){
      store_data(log_info$path, name, this_sim, filepath)
      stop_log(log_info)
    }
  }
  message("Simulation set ", name, " with ", simulations, " replicate simulations complete.")
  tictoc::toc() # run time
}
