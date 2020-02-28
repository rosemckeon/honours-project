#' @name setup_tmp_files
#' @title Ensure temp files are ready for simulation data
#' @description Creates temporary rds files for storing generation information separated by life stage and conditionally creates log files in text format.
#' @usage setup_temp_files()
#' @author Rose McKeon
#' @param generation integer representing the generation that this data represents
#' @examples 
#' setup_tmp_files(seedbank, juveniles, adults, seedoutput, 1)
#' @export
setup_tmp_files <- function(seedbank, juveniles, adults, seedoutput, generation){
  stopifnot(
    # needs some checks for dataframe objects (colnames or whatever)
    is.numeric(generation),
    generation%%1==0,
    generation > 0
  )
  generation <- sprintf("%04d", generation)
  seedbank_tmp_file <- store_tmp_data(seedbank, paste0("sploidy-seedbank-", generation))
  juvenile_tmp_file <- store_tmp_data(juveniles, paste0("sploidy-juveniles-", generation))
  adult_tmp_file <- store_tmp_data(adults, paste0("sploidy-adults-", generation))
  seedoutput_tmp_file <- store_tmp_data(seedoutput, paste0("sploidy-seedoutput-", generation))
  return(c(seedbank_tmp_file, juvenile_tmp_file, adult_tmp_file, seedoutput_tmp_file))
}
