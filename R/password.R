#' @name password
#' @export password
#' 
#' @title Random Password Generator
#' @description Generates a password consisting of a random set of characters.  
#'   Passwords are generated in a way that prevents any more than two consecutive 
#'   characters to be typed by the same hand.
#'   
#' @param length Character length of the password.
#' @param max.to.num maximum number of characters that may be selected before 
#'   choosing a numeric character.
#' @param use.num Logical, determines if numeric characters may be used.
#' 
#' @details At present, special (punctuation) characters and capital letters 
#'   are not among the sampled characters.
#'   
#' @author Benjamin Nutter
#' @examples
#'   password()
#'   

password <- function(length=8, max.to.num=length-1, use.num=TRUE){
  
  #* Set up the data frame of potential characters.
  char <- data.frame(
    char = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j",
             "k", "l", "m", "n", "o", "p", "q", "r", "s", "t",
             "u", "v", "w", "x", "y", "z",
             "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"),
    hand = c("l", "l", "l", "l", "l", "l", "l", "r", "r", "r",
             "r", "r", "r", "r", "r", "r", "l", "l", "l", "l",
             "r", "l", "l", "l", "r", "l", 
             "r", "l", "l", "l", "l", "l", "l", "r", "r", "r"),
    alpha = rep( c(TRUE,FALSE), c(26,10) ),
    stringsAsFactors=FALSE)
  
  #* Exclude numeric characters, if requested
  if(!use.num) char <- subset(char, alpha==TRUE)
  rownames(char) <- char$char
  
  #* Initialize the side counter.  The counter will add one for each left hand
  #* character and subtract one for each right hand character.  A bug exists 
  #* here in that there should be a counter for each hand.  As it stands, 
  #* up to four characters could be typed by a single hand while the counter moves
  #* from 2 to -2 (or vice versa)
  side <- 0
  
  #* set the number counter.  If numbers are not being used, the counter is set to 
  #* a value that will never become large enough to force the selection of a 
  #* numeric
  if(use.num) number <- 0 else number <- -length-1
  
  #* Initialize password
  pword <- ""
  
  repeat{
    #* If side is small, pick a left handed character,
    #* else if side is large, pick a right handed character,
    #* else, pick any character.
    if(side <= -2)      samp <- subset(char, hand == "l")
    else if(side >= 2) samp <- subset(char, hand == "r")
    else              samp <- char
    
    #* If too many alpha-characters have been chosen, force the
    #* selection of a numeric character.
    if(number >= max.to.num) samp <- subset(samp, alpha==FALSE)
    
    #* select the new character
    new.char <- samp[sample(1:nrow(samp),1),]
    
    #* Increment the counters.
    if(new.char$hand == "l") side <- side + 1 else side <- side - 1
    if(new.char$alpha) number <- number + 1
    
    #* Add the new character to the password
    pword <- paste(pword,new.char$char, sep="")
    
    if(nchar(pword)==length) break
  }
  
  return(pword)
}
