#' @name setup_log
#' @title Log output to `tempdir()`
#' @description Creates temporary text file containing simulation output.
#' @usage setup_log()
#' @author Rose McKeon
#' @return character string containing the location of the temporary file.
#' @examples 
#' setup_log("whatever")
#' @export
setup_log <- function(){
  log_path <- file.path(tempdir(), "_sploidy-log.txt")
  message("Output now being logged to: ", log_path)
  tmp_log_file <- file(log_path, open = "wt")
  sink(tmp_log_file, append = T, type = "message")
  return(list(path = log_path, connection = tmp_log_file))
}

stop_log <- function(log_info = list(path = NULL, connection = NULL)){
  stopifnot(
    !is.null(log_info$path),
    !is.null(log_info$connection)
  )
  sink(type = "message")
  close(log_info$connection)
  unlink(log_info$path)
}
