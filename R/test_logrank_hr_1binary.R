#' @name test_logrank_hr_1binary
#' @export test_logrank_hr_1binary
#' 
#' @title Power Analysis for Log Rank Tests with a Single Binary Covariate
#' @description Calculates the power, number of deaths, proportion to be randomized,
#'   significance, or hazard ratio for a log-rank test of a Kaplan-Meier curve with a 
#'   single binary covariate.
#'   
#' @param hr Hazard ratio
#' @param deaths Number of deaths 
#' @param p proportion of subjects to be randomized to the first group.  The format
#'   of this argument will eventually change.
#' @param alpha significance level for the test
#' @param power Power of the test
#' 
#' @details This function is currently in development and will undergo serious changes.
#' Among the changes: 1) p will be changed to a 'weights' style argument; 
#' 2) follow up time and recruitment interval will be incorporated; 3) event rates may
#' be incorporated so as to calculate the sample size instead of the number of deaths.
#' 
#' @author Benjamin Nutter
#' @references Schoenfeld DA. Sample-size formula for the proportional-hazards regression 
#' model. \emph{Biometrics} 1983;39:499-503
#' 
#' @examples
#' #* Example from Schoenfeld (doesn't incorporate recruitment and follow-up periods)
#' test_logrank_hr_1binary(hr=1.5, p=.25)
#' 

test_logrank_hr_1binary <- 
  function(hr = NULL, deaths=NULL,
           p = 0.5, alpha=0.05, power=0.80){
  
  if (is.null(deaths)){
    .params <- expand.grid(hr = hr,
                           p = p,
                           alpha = alpha,
                           power = power,
                           deaths = NA)
    .params$deaths <- with(.params, 
                           (qnorm(1-alpha) + qnorm(power))^2 * 
                             (p * (1-p) * log(hr)^2) ^ -1)
    
    return(.params)
  }
  
}
