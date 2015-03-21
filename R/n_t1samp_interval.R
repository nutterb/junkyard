#' @name n_t1samp_interval
#' @export n_t1samp_interval
#'
#' @title Sample Size for One Sample T Confidence Intervals
#'
#' @description The $t$-based confidence interval is commonly used in estimating
#'     population mean using sample data.  It is based on Student's $t$ 
#'     distribution, an adaptation of the standard normal distribution.  
#'     \code{n_t1samp_interval} provides tools for calculating and illustrating
#'     the sample size requirement and other design factors associated with 
#'     these intervals.
#' 
#' @param E Margin of error
#' @param n Sample size
#' @param s Sample standard deviation
#' @param alpha Significance level (or 1-confidence)
#' @param optim.max Maximum value for \code{optimze} when solving for \code{n}.  
#'     The default setting is 1,000,000,000, which is probably higher than
#'     anyone will ever need.  I may change this in the future for the sake
#'     of improving speed.
#' 
#' @details Exactly one of the parameters \code{n}, \code{E}, \code{s} and 
#' \code{alpha} must be passed as \code{NULL}, and that parameter will be 
#' calculated from the others.  Notice that \code{alpha} has a 
#' non-\code{NULL} default, so \code{NULL} must be explicitly passed 
#' if you want it computed.
#' 
#' @return Returns an object of class \code{HazPwr} with subclasses \code{t1samp}
#'  and \code{est} (actual appearance: \code{t1samp_est_HazPwr}.  The object
#'  contains a data frame of the parameters passed to the function
#'  (expanded using \code{expand.grid}) and the corresponding sample size 
#'  estimates.  
#'  
#'  \enumerate{
#'  \item \code{n_est}: Exact sample size.  When \code{n} is not 
#'      \code{NULL}, this will be identical to \code{n}.
#'  \item \code{n}: Estimated sample size; the next largest integer 
#'      after \code{n_est}.
#'  \item \code{E}: Margin of error
#'  \item \code{s}: Sample Standard Deviation
#'  \item \code{alpha}: Desired significance level.
#'  }
#' 
#' @author Benjamin Nutter
#' 
#' 



n_t1samp_interval <- function(E=NULL, n=NULL, s=NULL, alpha=.05,
                              optim.max = 1000000000){
  
  err.flag <- 0
  err.msg <- ""
  
  warn.flag <- 0
  warn.msg <- ""
  
  #*******************************************************
  #* Parameter checks
  #* 1.  only one of E, n, s, alpha may be NULL (error)
  #* 2a. for alpha, 0 < alpha < 1 (warning)
  #* 2b. Must be at least one valid value of alpha
  #* 3a. s must be a positive number
  #* 3b. Must be at least one valid value of s
  #* 4a. E must be a positive number
  #* 4b. Must be at least one valid value for E
  #*******************************************************
  
  #* 1.  only one of E, n, s, alpha may be NULL (error)
  if (sum(sapply(list(E, n, s, alpha), is.null)) != 1){
    err.flag <- err.flag + 1
    err.msg <- c(err.msg,
                 paste(err.flag, ": Exactly one of E, n, s, and alpha may be NULL"))
  }

  #* 2a. for alpha, 0 < alpha < 1 (warning)
  if (any(alpha <= 0 | alpha >=1)){
    warn.flag <- warn.flag + 1
    warn.msg <- c(warn.msg,
                  paste(warn.msg, ": alpha must be between 0 and 1, exclusive."))
    
    alpha <- alpha[alpha > 0 & alpha < 1]
    
    #* 2b. Must be at least one valid value of alpha
    if (length(alpha) == 0){
      err.flag <- err.flag + 1
      err.msg <- c(err.msg,
                   paste(err.flag, ": no valid values for alpha were given."))
    }
  }

  #* 3a. s must be a positive number
  if (any(s <= 0)){
    warn.flag <- warn.flag + 1
    warn.msg <- c(warn.msg,
                  paste(warn.msg, ": s must be a positive number.  Non positive numbers were removed"))
    s <- s[s > 0]
    
    #* 3b. Must be at least one valid value of s
    if (length(s) == 0){
      err.flag <- err.flag + 1
      err.msg <- c(err.msg,
                   paste(err.msg, ": No valid values for s were given."))
    }
  }

  #* 4a. E must be a positive number
  if (any(E <= 0)){
    warn.flag <- warn.flag + 1
    warn.msg <- c(warn.msg,
                  paste(warn.msg, ": E must be a positive number.  Non positive numbers were removed"))
    E <- E[E > 0]
    
    #* 4b. Must be at least one valid value for E
    if (length(E) == 0){
      err.flag <- err.flag + 1
      err.msg <- c(err.msg,
                   paste(err.msg, ": No valid values for E were given."))
    }
  }
  
  #* Print warnings and errors
  if (warn.flag){
    warning(paste(warn.msg, collapse="\n")) 
  }
  
  if (err.flag) {
    stop(paste(err.msg, collapse="\n"))
  }
  
  #* Solve for n
  if (is.null(n)){
    .param <- expand.grid(n_est = NA,
                          n = NA,
                          E = E,
                          s = s,
                          alpha = alpha)  
    fn <- function(n, E, s, alpha) (qt(1-alpha/2, n-1)^2 * s^2 / E^2 - n)^2
    
    for (i in 1:nrow(.param)){
      .param$n_est[i] <- with(.param,
                          optimize(function(n) (qt(1-alpha[i]/2, n-1)^2 * s[i]^2 / E[i]^2 - n)^2,
                                   c(0, optim.max))$minimum)
    }
    .param$n <- ceiling(.param$n_est)
    return(.param)
  }
  
  #* Solve for E
  if (is.null(E)){
    .param <- expand.grid(n_est = n,
                          n = n,
                          E = NA,
                          s = s,
                          alpha = alpha) 
    .param$E <- with(.param,
                     qt(1-alpha/2, n-1) * s / sqrt(n))
  }
  
  #* Solve for s
  if (is.null(s)){
    .param <- expand.grid(n_est = n,
                          n = n,
                          E = E,
                          s = NA,
                          alpha = alpha) 
    .param$s <- E * sqrt(n) / qt(1-alpha/2, n-1)
  }
  
  #* Solve for alpha
  if (is.null(alpha)){
    .param <- expand.grid(n_est = n,
                          n = n,
                          E = E,
                          s = s,
                          alpha = NA) 
    .param$alpha <- 2 * (1 - pt(E * sqrt(n) / s, n-1))
  }
  
  return(.param)
}

n_t1samp_interval(E=1.651119, s=c(5, 6, 7), alpha=.05)
  
