#' @name store_session
#' @title Store session info
#' @description Creates a file containing timestamp simulation was begun, the function call it ran with and the session info with package version info for all the loaded libraries.
#' @usage store_session()
#' @author Rose McKeon
#' @param sploidy_call mat.call object passed from main sploidy function.
#' @param name character string used in folder structure.
#' @examples 
#' store_session(match.call(), "whatever")
#' @export
store_session <- function(sploidy_call, name){
  stopifnot(
    is.character(name)
  )
  # save the session data in a tmp file
  session_tmp_file <- store_tmp_data(
    list(
      start_time = Sys.time(),
      call = sploidy_call,
      session = sessionInfo()
    ), 
    "session_info"
  )
  # copy the tmp file to the right sim subfolder
  store_data(session_tmp_file, name)
  message("Session info stored.")
}