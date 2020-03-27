#' @name setup_tmp_files
#' @title Ensure temp files are ready for simulation data
#' @description Creates temporary rds files for storing generation information separated by life stage and conditionally creates log files in text format.
#' @usage setup_temp_files()
#' @author Rose McKeon
#' @param generation integer representing the generation that this data represents
#' @examples 
#' setup_tmp_files(seedbank, juveniles, adults, seedoutput, 1)
#' @export
setup_tmp_files <- function(seedlings, rosettes, seeds, generation){
  stopifnot(
    # needs some checks for dataframe objects (colnames or whatever)
    is.numeric(generation),
    generation%%1==0,
    generation >= 0
  )
  generation <- sprintf("%04d", generation)
  seedling_tmp_file <- store_tmp_data(seedlings, paste0("seedlings_", generation))
  rosette_tmp_file <- store_tmp_data(rosettes, paste0("rosettes_", generation))
  seed_tmp_file <- store_tmp_data(seeds, paste0("seeds_", generation))
  return(c(seedling_tmp_file, rosette_tmp_file, seed_tmp_file))
}
