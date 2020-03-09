#' @name store_tmp_data
#' @title Store object as RDS file in temp directory.
#' @description Creates temporary rds file from object.
#' @usage store_tmp_data()
#' @author Rose McKeon
#' @param object any R data object to store.
#' @param tmp_filename character string used in file naming.
#' @return character string containing the location of the temporary file.
#' @examples 
#' seedbank <- "whatever"
#' seedbank_location <- store_tmp_data(seedbank, "seedbank")
#' @export
store_tmp_data <- function(object, tmp_filename){
  stopifnot(
    is.character(tmp_filename)
  )
  # generate tmp file path
  tmp_file <- file.path(tempdir(), paste0(tmp_filename, ".rds"))
  
  # make sure tmp_file exists
  if(!file.exists(tmp_file)){
    file.create(tmp_file, showWarnings = F)
  }
  
  # save data in tmp file
  saveRDS(object, tmp_file)
  return(tmp_file)
}
