#' @name searchFunctions
#' @export searchFunctions
#' 
#' @title Search Functions for Character Strings
#' @details At times, I have needed to find certain character strings within
#' a package.  For instance, I may need to know which functions contain
#' a particular argument.  This function scans all of the files in a directory
#' for a character string and returns the names of the files that contain it.
#' 
#' @param pattern The character string to be matched
#' @param directory The directory of functions to be scanned
#' 
#' @author Benjamin Nutter
#' @examples
#' \dontrun{
#' searchFunctions("survival",
#'                 "C:/Users/nutterb/Documents/GitHub/lazyWeave/R")
#' }
searchFunctions <- function(pattern, directory){
  files <- file.path(directory, list.files(directory))
  
  code <- lapply(files, function(x) paste(readLines(x), collapse="\n"))
  names(code) <- list.files(directory)
  
  match_found <- sapply(code, function(x, p) grepl(p, x), pattern)
  match_found <- names(match_found)[match_found]
  return(match_found)
}
