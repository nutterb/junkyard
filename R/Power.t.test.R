#' @name Power.t.test
#' @export Power.t.test
#' 
#' @title Modified T-test Power Calculations
#' @description Allows for calculating power, effect size, or sample size over multiple 
#'   conditions.  Returns a data frame instead of an 'htest' object
#'   
#' @param n Number of observations (per group)
#' @param delta True difference in means
#' @param sd Standard deviation
#' @param sig.level Significance level (Type I error probability)
#' @param power Power of test (1 minus Type II error probability)
#' @param type Type of test
#' @param alternative One or two-sided test
#' @param strict Use strict interpretation in two sided case
#' 
#' @details This is essentially a wrapper for a for loop over \code{power.t.test}.
#' 
#' 
Power.t.test <- function(n=NULL, delta=NULL, sd=1, sig.level=0.05,
                         power=NULL, type="two.sample",
                         alternative = "two.sided",
                         strict=FALSE){
  
  if (sum(sapply(list(n, delta, power), is.null)) != 1)
    stop("Exactly one of 'n', 'delta', and 'power' may be NULL")
  
  extract <- names(which(sapply(list(n=n, delta=delta, power=power), is.null)))
  
  Power <- expand.grid(n = if (is.null(n)) NA else n,
                       delta = if (is.null(delta)) NA else delta,
                       sd = sd,
                       sig.level = sig.level,
                       power = if (is.null(power)) NA else power,
                       type = type,
                       alternative = alternative,
                       strict = strict,
                       stringsAsFactors=FALSE)
  for (i in 1:nrow(Power)){
    Power[[extract]][i] <- with(Power, power.t.test(n = if (is.na(n[i])) NULL else n[i], 
                                                    delta = if (is.na(delta[i])) NULL else delta[i], 
                                                    sd = sd[i],
                                                    sig.level = sig.level[i], 
                                                    power = if (is.na(power[i])) NULL else power[i],
                                                    type = type[i],
                                                    alternative = alternative[i],
                                                    strict = strict[i]))[[extract]]
  }
  
  return(Power)
  
}

