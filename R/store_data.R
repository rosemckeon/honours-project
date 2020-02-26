#' @name store_data
#' @title Stores simulation data in working directory filepath
#' @description Copies data from temporary directory and clears temp files to free memory.
#' @usage store_data()
#' @author Rose McKeon
#' @param tmp_files charactor vector containing temp file path/s.
#' @param name character string defining the name of the filepath subfolder which countains output RDS files. (default = unique random identifier).
#' @param this_sim integer representing the simulation number.
#' @param filepath character string defining the file path where output files should be stored (relative to working directory). If this folder does not exist it will be created (default = "data").
#' @examples 
#' store_data()
#' @export
store_data <- function(tmp_files, name = NULL, this_sim, filepath = "data"){
  stopifnot(
    file.exists(tmp_files),
    is.character(filepath),
    is.numeric(this_sim),
    this_sim%%1==0,
    this_sim > 0
  )
  message("storing data...")
  # make sure filepath exists
  if(!dir.exists(filepath)){
    dir.create(filepath)
  }
  # make sure there is a filename
  if(is_null(name)){
    name <- random_id(1, 10)
  }
  # add leading zeros to maintain folder structure
  this_sim <- sprintf("%03d", this_sim)
  # make sure named subfolder exists
  if(!dir.exists(file.path(filepath, name))){
    dir.create(file.path(filepath, name))
  }
  # create this_sim subfolder
  sim_dir <- file.path(filepath, name, paste0("sim-", this_sim))
  if(!dir.exists(sim_dir)){
    dir.create(sim_dir)
  }
  # copy tmp_file to sim_dir
  file.copy(tmp_files, sim_dir)
  # Remove tmp_file
  message("removing tmp_files: ")
  message(tmp_files)
  unlink(tmp_files)
}
