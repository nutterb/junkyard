#' @name findVariable
#' @export findVariable
#'
#' @title Find data frames containing a selected variable names
#'
#' @description The data frames in a workspace are searched for variable names 
#' of interest and a vector of names is returned.  The returned vector names
#' the data frames that contain all of the variables of interest.
#'
#' @param var A vector of variable names to search for.  Only data frames 
#'   containing \emph{all} of the variables given will be returned.
#'
#'@details This function was written in response to the following query:
#' 
#' "I have a workspace attached in R.  it's got objects in it.  some of the 
#' objects are data frames.  I'd like to search the dataframes in the 
#' workspace for a variable called THGRP and get a listing of dataframes 
#' that contain it.  how I do this?"
#'
#' @author Benjamin Nutter
#'
#' @examples
#' mtcarsA <- mtcars
#' mtcarsB <- mtcars
#' mtcarsB$newVar <- 1:nrow(mtcarsB)
#' IndomethA <- Indometh
#' 
#' findVariable("am")
#' findVariable(c("am", "mpg"))
#' findVariable(c("am", "newVar"))
#' findVariable("time")
#' 
#' # cleanup
#' rm(list=c('mtcarsA', 'mtcarsB', 'IndomethA'))

findVariable <- function(var){
  #* Get names of objects in the Global Environment
  X <- lapply(ls(envir=.GlobalEnv), get, envir=.GlobalEnv)
  names(X) <- ls(envir=.GlobalEnv)
  
  #* Identify objects having or inheriting class 'data.frame'
  X <- X[sapply(sapply(X, class), function(x) "data.frame" %in% x)]
  
  #* Return a vector of data frame names with the desired variable.
  names(X)[sapply(X, function(x, v) all(v %in% names(x)), v=var)]
}