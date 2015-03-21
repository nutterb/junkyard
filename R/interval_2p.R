#' @name interval_2p
#' @export interval_2p
#'
#' @title Sample Size for the confidence interval of the differnce of proportions.
#'
#' @description The $z$-based confidence interval is commonly used in estimating
#'     population mean using sample data.  It is based on the Standard Normal 
#'     distribution.  
#'     \code{interval_2p} provides tools for calculating and illustrating
#'     the sample size requirement and other design factors associated with 
#'     these intervals.
#' @param E Margin of error for the difference of proportions
#' @param p1 Proportion observed in group 1.
#' @param p2 Proportion observed in group 2.
#' @param n Total sample size for groups 1 and 2.
#' @param n1 Sample size for group 1.
#' @param n2 Sample size for group 2.
#' @param k proportion of the total samples size \code{n} that consitutes \code{n1}.
#'   the default assumptions is that \code{n1 = n2}.
#' @param alpha Significance level (or 1-confidence)
#' 
#' @details This function is currently under development.  I needed it for a 
#' study design.
#' 
#' This can be useful for designin equivalence trials.  According to Schumi and 
#' Wittes, ``Sometimes, the goal is not to show that the new treatment is better, 
#' but that the new treatment is 'equivalent' to the control...At the end of the 
#' trial, a CI is computed around the difference between two test statistics 
#' (equivalence trials typically use 90% CIs) and if the CI lies strictly within 
#' [-E, +E] the two treatments are called 'equivalent.'"
#' 
#' @author Benjamin Nutter
#' 
#' @references 
#' Jennifer Schumi and Janet T Wittes, `` Through the looking glass: understanding
#' non-inferiority," _Trials_, 2011, 12:106.
#' 

interval_2p <- function(E=NULL, p1=NULL, p2=NULL, 
                        n=NULL, n1=NULL, n2=NULL, k=0.5,
                        alpha=.05){
  
 #* Assume equal sample sizes
  if (is.null(n)) { 
    .params <- expand.grid(E = E,
                           p1 = p1,
                           p2 = p2,
                           alpha = alpha,
                           k = k,
                           n1_est = NA,
                           n1 = NA,
                           n2_est = NA,
                           n2 = NA,
                           n_est = NA,
                           n = NA)
    .params$n1_est <- with(.params, (qnorm(1-alpha/2)^2 * 
                                      (p1 * (1-p1) + p2 * (1-p2))) / E^2)
    .params$n2_est <- .params$n1_est
    .params$n_est <- .params$n1_est + .params$n2_est
    .params$n1 <- ceiling(.params$n1_est)
    .params$n2 <- ceiling(.params$n2_est)
    .params$n <- .params$n1 + .params$n2                           
  }
  
  return(.params)
}

interval_2p(E=c(.05), p1=.27, p2=seq(.22, .35, by=.01), alpha=c(0.05))
