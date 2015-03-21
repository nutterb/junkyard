#' @name mostRecent
#' @export
#' 
#' @title Search a Path for the File with the Most Recent Time Stamp
#' @description Extracting analysis data sets from a dynamic database suggests
#'   that the data may change.  To maintain reproducibility, it is advised 
#'   that smaller files containing the analysis data sets be saved.  When 
#'   these data sets are created, they should be named with a date or time 
#'   stamp to indicate when they were made.  Often, however, it is preferable
#'   for a script to default to the most recently made data set.  In order to
#'   avoid manual interaction with the scripts, `mostRecent` looks for the
#'   file with the most recent time stamp.
#'   
#' @param filename The stem of the file name before the date stamp is added.
#' @param path The path to the data files
#' @param format The date format used for the time stamp
#' @param ext The file extension. These may start with a \code{.} or not.  The 
#'   first \code{.} will be deleted, since the function assumes a dot.
#'   
#' @details This is not a particularly intelligent function.  The user is 
#'   expected to know a fair amount about the file structure.  \code{mostRecent}
#'   also assumes that the only part of the file name to occur after the 
#'   date stamp is the file extension type.  Embedding the date format in the
#'   middle of a string will cause a failure (ie, \code{"file_date_string.Rdata"}).
#'   
#'   The user is also expected to know the date format.  We can't do all the 
#'   work for you.
#'   
#' @author Benjamin Nutter
#' 
#' @examples
#' \dontrun{
#' #* This is based off a case from my personal computer
#' #* It will not run if you execute it
#' path <- "/home/nutterb/Project/data"
#' filename <- "AnalysisData_"
#' 
#' list.files(path)
#' 
#' [1] "AnalysisData_2013-12-05.Rdata" "AnalysisData_2015-03-05.Rdata"
#' [3] "AnalysisData_2015-03-06.Rdata" "AnalysisData_2015-03-12.Rdata"
#' [5] "AnalysisData_2015-03-19.Rdata"
#' 
#' mostRecent(filename, path)
#' 
#' [1]"SleepReferral2015-03-19.Rdata"
#' }
mostRecent <- function(filename,
                       path=getwd(),
                       format="%Y-%m-%d",
                       ext = "Rdata"){
  
  ext <- sub("[.]", "", ext)
  Files <- data.frame(files = list.files(path)[grepl(filename, list.files(path))],
                      stringsAsFactors=FALSE)
  Files$date <- sub(filename, "", Files$files, fixed=TRUE)
  Files$date <- sub(paste0(".", ext), "", Files$date, fixed=TRUE)
  Files$date <- as.POSIXct(Files$date, format=format)
  Files <- Files[order(Files$date, decreasing=TRUE), ]
  Files$files[1]
}

