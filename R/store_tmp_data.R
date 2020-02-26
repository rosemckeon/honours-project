#' @name store_tmp_data
#' @title Store object in `tmpdir()`
#' @description Creates temporary rds file from object named `paste0("sploidy-", name, "-", generation, ".rds")`.
#' @usage store_tmp_data()
#' @author Rose McKeon
#' @param object any R data object to store.
#' @param name character string used in file naming.
#' @param generation integer representing the current generation for which the object data represents.
#' @return character string containing the location of the temporary file.
#' @examples 
#' seedbank <- NULL
#' seedbank_location <- store_tmp_data(seedbank, "seedbank", 1)
#' @export
store_tmp_data <- function(object, name, generation){
  stopifnot(
    is.character(name),
    is.numeric(generation),
    generation%%1==0,
    generation > 0
  )
  generation <- sprintf("%03d", generation)
  tmp_file <- file.path(tempdir(), paste0("sploidy-", name, "-", generation, ".rds"))
  # make sure tmp_file exists
  if(!file.exists(tmp_file)){
    tmp_file %>% file.create(showWarnings = F)
  }
  # save data
  saveRDS(object, tmp_file)
  message(name, " ", generation, " cached: ", tmp_file)
  return(tmp_file)
}
