#' @name BlandAltman
#' @export BlandAltman
#'
#' @title Produce a Bland-Altman style plot for agreement between numeric measures.
#'
#' @description The Bland-Altman plot places the difference between two measures (such as observations between two 
#' readers) on the y-axis against the pair-wise means of the observations on the x-axis.  The confidence limits of the 
#' differences are plotted.  A plot of good agreement should have a random dispersal of points within the confidence 
#' limits, and at least 95% of the points should be within those limits.  Using \code{ggplot}, these plots can be 
#' generated in a grid with each panel having limits calculated independently of the others.
#' 
#' Additionally, plots can be generated that show the agreement among multiple raters in one graph.
#'
#' @param data A data frame.
#' @param x a numeric vector giving one set of values for comparison
#' @param y a numeric vector giving the second set of values for comparison
#' @param id a character vector indicating the identifiers for each experimental unit.
#' @param rater a character vector indicating the columns identifying unique raters.
#' @param alpha The significance level, or 1 - the confidence level.
#' @param gg_expr A list of additional layers to be added to the \code{ggplot} object.  For instance, 
#' \code{facet_grid()} can be an element of the list, or \code{ggtitle}, etc.
#'
#' @details The use of \code{colour}, \code{shape}, and other \code{ggplot} aesthetics is not recommended with this 
#' function.  Any data within the panel will be included in the calculation of the confidence limits.  If additional
#' aesthetics are being used to show strata of data, these confidence limits will not be correctly calculated. 
#' The one exception to this guideline is when using the \code{raters} argument, in which 
#' case you may want to pass \code{aes(colour=[rater])} in \code{gg_expr}.
#' 
#' @references
#' Bland JM, Altman DG (1986) "Statistical method for assessing agreement 
#' between two methods of clinical measurement." \emph{The Lancet} i:307-310.
#' 
#' Jones M, Dobson A, O'Brian S, "A graphical method for assessing agreement with
#' the mean between multiple observers using continuous measures," 
#' \emph{International Journal of Epidemiology}, 2011; 40: 1308-1313.
#'
#' @author Benjamin Nutter

BlandAltman <- function(data, x1, x2, id=NULL, rater=NULL, alpha=.05, gg_expr=list()){

  # Include alpha in data
  data$alpha <- alpha

  # Calculate pairwise mean
  data$mean <- (eval(substitute(x1), data) + eval(substitute(x2), data)) / 2
  # Calculate pairwise differences
  data$diff <- (eval(substitute(x1), data) - eval(substitute(x2), data))
  
  #* Plot the initial object
  p <- ggplot(data, aes(x=mean, y=diff)) + geom_point() + gg_expr
  
  #* Determine faceting groups
  group <- as.character(unlist(p$facet[c('rows', 'cols')]))
  
  if (is.null(id)){
    if (length(group) > 0){
      limits <- plyr::ddply(data,
                            group,
                            summarise,
                            md = mean(diff, na.rm=TRUE),
                            lcl = mean(diff, na.rm=TRUE) - qnorm(1-alpha[1]/2) * sd(diff, na.rm=TRUE),
                            ucl = mean(diff, na.rm=TRUE) + qnorm(1-alpha[1]/2) * sd(diff, na.rm=TRUE))
    }
    else{
      limits <- data.frame(md = mean(data$diff, na.rm=TRUE),
                           lcl = mean(data$diff, na.rm=TRUE) - qnorm(1-alpha/2) * sd(data$diff, na.rm=TRUE),
                           ucl = mean(data$diff, na.rm=TRUE) + qnorm(1-alpha/2) * sd(data$diff, na.rm=TRUE))
    }
  }
  else{
    data2 <- melt(data,
                c(id, rater, group),
                c(substitute(x1), substitute(x2)))
    
    data2 <- ddply(data2,
               c(id, group),
               transform,
               mean = mean(value),
               diff = value - mean(value))
    
#     return(data)
    
    limits <- ddply(data2,
               group,
               function(x) sqrt(anova(aov(as.formula(paste0("value ~ ", id, " + variable")), data=x))[3, 3]))
    names(limits)[length(limits)] <- "se"
    limits <- transform(limits, 
                   md = 0,
                   lcl = -se * qnorm(1-alpha/2),
                   ucl = se * qnorm(1-alpha/2))
    
    p <- ggplot(data, aes(x=mean, y=diff)) + geom_point() + gg_expr
    
  }
  
  p + geom_hline(data=limits, aes(yintercept=md), colour="blue", alpha=.4) + 
      geom_hline(data=limits, aes(yintercept=lcl), colour="red", alpha=.4) + 
      geom_hline(data=limits, aes(yintercept=ucl), colour="red", alpha=.4)
}
