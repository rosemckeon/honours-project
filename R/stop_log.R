#' @name stop_log
#' @title Stop logging messages and clear log files from `tempdir()`.
#' @description Closes the connection to log file passed form log_info$connection and unlinks the log file passed from log_info$path.
#' @usage stop_log()
#' @author Rose McKeon
#' @return character string containing the location of the temporary file.
#' @examples 
#' log_info <- setup_log()
#' stop_log(log_info)
#' @export
stop_log <- function(log_info = list(path = NULL, connection = NULL)){
  stopifnot(
    !is.null(log_info$path),
    !is.null(log_info$connection)
  )
  sink(type = "message")
  close(log_info$connection)
  unlink(log_info$path)
}
