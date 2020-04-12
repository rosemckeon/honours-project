#' @name count_combine
#' @title Combine counts from simulations into one data file.
#' @description Takes all the simulation subfolders from a given data path_in and combines them into one rds file stored in path_out.
#' @usage count_combine(path_in, path_out)
#' @author Rose McKeon
#' @param path_in character string contining the path, based on the current working directory, where the named subfolders that contain the simulation data are held.
#' @param path_out character string caontaining the path abd filename where the combined count file should be stored. File extension (.rds) added automatically.
#' @examples 
#' count_combine("data/germination-benefit", "thesis/_data/germination_counts")
#' @export
count_combine <- function(path_in, path_out){
  message("All counts in ", path_in)
  # loop through the all the random ploidy_rate data
  wd <- getwd()
  setwd(path_in)
  # make sure we exclude the null data
  sim_dirs <- list.dirs(full.names = F, recursive = F)
  # setup some storage and replicate ID variables
  all_counts <- NULL
  i <- 1
  for(dir in sim_dirs){
    wd_data <- getwd()
    setwd(dir)
    # loop through any simulation replicate subfolders
    for(sim in list.dirs(full.names = F, recursive = F)){
      wd_dir <- getwd()
      setwd(sim)
      # get the count data
      counts <- readRDS("_counts.rds") %>%
        mutate(ID = i, ploidy_rate = as.numeric(ploidy_rate))
      # increase i for next replicate ID
      i <- i + 1
      # combine data
      all_counts <- dplyr::bind_rows(all_counts, counts)
      # free memory
      rm(counts)
      # reset working dir
      setwd(wd_dir)
    }
    setwd(wd_data)
  }
  setwd(wd)
  # store combined counts
  saveRDS(all_counts, paste0(path_out, ".rds"))
  message("  Combined into: ", paste0(path_out, ".rds"))
}
