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
#' @param simulations integer representing the number of simulations which should be run with these parameters (default = 1).
#' @param return logical value which indicates whether or not to return output at the end of the simulation/s.
#' @param filepath character string defining the file path where output files should be stored. Only used if filename not NULL (default = "data/").
#' @param filename character string defining the name of the output file. Output files are RDS format and the file extension will be appended automatically (default = NULL).
#' @param logfilepath character string defining the file path where output log files should be stored. Only used if logfilename not NULL (default = "data/logs/").
#' @param logfilename character string defining the name of the output log file. Log files are txt format and the file extension will be appended automatically (default = NULL).
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
  simulations = 1,
  return = FALSE,
  filepath = "data",
  filename = NULL,
  logfilepath = "data/logs/",
  logfilename = NULL
){
  # tic.clearlog()
  # tic("Entire run time")
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
    is.logical(c(return)),
    is.character(c(filepath, logfilepath)),
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
  message("parameters are all appropriate.")
  message("setting up files...")
  # prep temp files (numbered with leading zeros)
  file_numbers <- sprintf("%03d", 0:generations)
  # function and life-stage prefix with rds format
  # (splitting the lifestages reduces mem usage)
  tmp_files_seedbank <- file.path(tempdir(), paste0("sploidy-seedbank-", file_numbers, ".rds"))
  tmp_files_juveniles <- file.path(tempdir(), paste0("sploidy-juveniles-", file_numbers, ".rds"))
  tmp_files_adults <- file.path(tempdir(), paste0("sploidy-adults-", file_numbers, ".rds"))
  tmp_files_seedoutput <- file.path(tempdir(), paste0("sploidy-seedoutput-", file_numbers, ".rds"))
  # create tmp files
  tmp_files <- c(tmp_files_seedbank, tmp_files_juveniles, tmp_files_adults, tmp_files_seedoutput)
  tmp_files %>% file.create(showWarnings = F)
  
  # Check that these files exist
  # dir(tempdir(), full.names = T)

  # fill these files with data! (run simulations)
  
  # setup folders to store final data
  # make sure filepath exists
  if(!dir.exists(filepath)){
    dir.create(filepath)
  }
  # make sure there is a filename
  if(is_null(filename)){
    filename <- random_id(1, 10)
  }
  # create subfolders for simulations
  dir.create(file.path(filepath, filename))
  dir_numbers <- sprintf("%03d", 0:simulations)
  sim_dirs <- file.path(filepath, filename, paste0("sim-", dir_numbers))
  for(dir in 1:length(sim_dirs)){
    dir.create(sim_dirs[[dir]])
  }
  # copy tmp files to folders
  file.copy(tmp_files, sim_dirs[1])
  # Remove tmp files
  unlink(tmp_files)

  # prepare an object for final output
  sploidy <- list(
    call = match.call(),
    time = list(
      start = Sys.time(),
      end = NULL,
      duration = NULL,
      sim_duration = NULL
    ),
    R = R.version.string,
    "sploidy" = "sploidy version 0.0.000",
    data = list(
      seedbank = NULL,
      juveniles = NULL,
      adults = NULL,
      seedoutput = NULL
    )
  )
  if(!is.null(logfilename)){
    sploidy$log = list()
  }
  toc()
}
