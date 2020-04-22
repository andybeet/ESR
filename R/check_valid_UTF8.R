#' Check to make sure all character are valid UTF8 format
#'
#' Checks entire human readable file for any invalid UTF8 format
#'
#' @param filename Character vector. Path to file name
#'
#' @return Null
#' The code just exits with an error and prints the first offending line
#'

check_valid_UTF8 <- function(filename) {
  
  
  fileContent <- readLines(filename)
  
  # look for <h tags format with hashes
  for (aline in fileContent) {
    validLine <- validUTF8(aline)
    if(!validLine) {
      print(aline)
      stop()
    }

  }
  
}