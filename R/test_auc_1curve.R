#' @name test_auc_1curve
#' @export test_auc_1curve
#' 
#' @title Power Analsis for Full Area Under the ROC Curve
#' @description Calculates the power, sample size, or other parameters for
#'   studies using Area Under the Curve (AUC) as the endpoint of an anlysis
#'   using a single continuous or ordinal predictor.
#'   
#' @param auc0 The hypothesize (null) value of AUC.  
#' @param auc1 The observed or desired AUC.
#' @param n The total sample size 
#' @param power The power of the test
#' @param alpha The significance level of the test.  
#' @param weights Weighting of subjects with the condition and those without.
#'   A vector \code{1, 3} indicates there is one subject with the condition
#'   for every three subjects without it.  Defaults to equal sizes.  The weights
#'   are normalized, so they do not have to sum to one.
#' @param one_tail Logical. Indicates if the test should be a one tailed test.
#' @param ordinal Logical.  Indicates if the independent variable in the study
#'   is ordinal.  This is currently ignored.
#'   
#' @details The Hanley and McNeil estimator is used for continuous predictors.
#'   Obuchowski's estimator for discrete ratings is used for ordinal predictors.
#'   
#' @author Benjamin Nutter
#' @references
#' Nancy A Obuchowski, "Sample size calculations in studies of test accuracy,"
#'   \emph{Statistical Methods in Medical Research} 1998; 7: 371-392
#'   
#' James A. Hanley and Barbara J. McNeil, "The Meaning and Use of the Area
#'   under a Receiver Operating Characteristic (ROC) Curve,"  
#'   \emph{Radiology}. Vol 143. No 1, Pages 29-36, April 1982.
#'   
#' The bulk of this code is compared against C Zepp's SAS Macro ROCPOWER.
#' See reference 25 in the Obuchowski paper above.
#' \url{http://www.bio.ri.ccf.org/doc/rocpower_help.txt}
#' \url{http://www.bio.ri.ccf.org/doc/rocpower.sas}
#' 
#' @examples
#' #* First example from \url{http://www.bio.ri.ccf.org/doc/rocpower_help.txt}
#' #* but solving for n
#' test_auc_1curve(auc0 = .75, auc1 = .92,
#'                 n = NULL, alpha=.02,
#'                 power=.893, weights = list(c(35, 50)))
#' #* First example from \url{http://www.bio.ri.ccf.org/doc/rocpower_help.txt}
#' test_auc_1curve(auc0 = .75, auc1 = .92,
#'                 n = 85, alpha=.02,
#'                 power=NULL, 
#'                 weights = list(c(35, 50)))

test_auc_1curve <- function(auc0=NULL, auc1=NULL, n=NULL, 
                            power=NULL, alpha=.05, 
                            weights=list(c(1, 1)),
                            one_tail = FALSE,
                            ordinal = FALSE){
  
  k <- sapply(weights, function(x) x[1] / sum(x))
  
  .params <- expand.grid(auc0 = if (is.null(auc0)) NA else auc0,
                         auc1 = if (is.null(auc1)) NA else auc1,
                         k = k,
                         power = if (is.null(power)) NA else power,
                         alpha = if (is.null(alpha)) NA else alpha,
                         ordinal = ordinal,
                         one_tail = unique(one_tail),
                         n1_est = NA,
                         n2_est = NA,
                         n_est = NA,
                         n1 = NA,
                         n2 = NA,
                         n = if (is.null(n)) NA else n)
  
  if (is.null(n)){
    #* Calculate variances to make the calculation of n1_est a little shorter
    .params$v0 <- with(.params,
                       (auc0/(2-auc0)) * k/(1-k) + 
                         2*auc0^2 / (1+auc0) - auc0^2 * (k/(1-k) + 1))
    .params$v1 <- with(.params,
                       (auc1/(2-auc1)) * k/(1-k) + 
                         2*auc1^2 / (1+auc1) - auc1^2 * (k/(1-k) + 1))

    .params$n1_est <- with(.params,
                           (qnorm(1-alpha/(2-one_tail)) * sqrt(v0) + qnorm(1-(1-power)) * sqrt(v1))^2 / 
                             (auc0 - auc1)^2)
    .params$n2_est <- with(.params, (1-k)/k * n1_est)
    .params$n_est <- with(.params, n1_est + n2_est)
    .params$n1 <- ceiling(.params$n1_est)
    .params$n2 <- ceiling(.params$n2_est)
    .params$n <- with(.params, n1 + n2)
    
    #* Remove variances from the data frame
    .params$v0 <- .params$v1 <- NULL
  }
  
  if (is.null(power)){
    .params$n1_est <- with(.params, n * k)
    .params$n2_est <- with(.params, n * (1-k))
    .params$n_est <- with(.params, n1_est + n2_est)
    .params$n1 <- ceiling(.params$n1_est)
    .params$n2 <- ceiling(.params$n2_est)
    .params$n <- with(.params, n1 + n2)
    
    #* Calculate variances to make the calculation of power a little shorter
    .params$v0 <- with(.params,
                       (auc0/(2-auc0)) * k/(1-k) + 
                         2*auc0^2 / (1+auc0) - auc0^2 * (k/(1-k) + 1))
    .params$v1 <- with(.params,
                       (auc1/(2-auc1)) * k/(1-k) + 
                         2*auc1^2 / (1+auc1) - auc1^2 * (k/(1-k) + 1))
    .params$power <- with(.params,
                          if (one_tail) pnorm((abs(auc0-auc1) * sqrt(n1) - 
                                                qnorm(1-alpha/(2-one_tail)) * sqrt(v0)) / sqrt(v1))
                          else sum(pnorm((c(-1, 1) * abs(auc0-auc1) * sqrt(n1) - 
                                        qnorm(1-alpha/(2-one_tail)) * sqrt(v0)) / sqrt(v1))))
    #* Remove variances from the data frame
    .params$v0 <- .params$v1 <- NULL
    
  }
  
  #* Estimate Significance
  if (is.null(alpha)){
    .params$n1_est <- with(.params, n * k)
    .params$n2_est <- with(.params, n * (1-k))
    .params$n_est <- with(.params, n1_est + n2_est)
    .params$n1 <- ceiling(.params$n1_est)
    .params$n2 <- ceiling(.params$n2_est)
    .params$n <- with(.params, n1 + n2)
    
    #* Calculate variances to make the calculation of power a little shorter
    .params$v0 <- with(.params,
                       (auc0/(2-auc0)) * k/(1-k) + 
                         2*auc0^2 / (1+auc0) - auc0^2 * (k/(1-k) + 1))
    .params$v1 <- with(.params,
                       (auc1/(2-auc1)) * k/(1-k) + 
                         2*auc1^2 / (1+auc1) - auc1^2 * (k/(1-k) + 1))
    
    .params$alpha <- with(.params,
                          sum(1 - pnorm(abs((c(-1, 1) * abs(auc0-auc1) * sqrt(n1) - 
                                       qnorm(1-power) * sqrt(v1)) / sqrt(v0)))) * (2-one_tail))
  }
  
  #* Estimate auc1
  if (is.null(auc1)){
    .params$n1_est <- with(.params, n * k)
    .params$n2_est <- with(.params, n * (1-k))
    .params$n_est <- with(.params, n1_est + n2_est)
    .params$n1 <- ceiling(.params$n1_est)
    .params$n2 <- ceiling(.params$n2_est)
    .params$n <- with(.params, n1 + n2)
    
    .params$v0 <- with(.params,
                       (auc0/(2-auc0)) * k/(1-k) + 
                         2*auc0^2 / (1+auc0) - auc0^2 * (k/(1-k) + 1))

    for (i in 1:nrow(.params)){
      .params$auc1[i] <- 
        with(.params,
             optimize(function(auc1)
               ((qnorm(1-alpha[i]/(2-one_tail[i])) * sqrt(v0[i]) + 
                qnorm(1-power[i]) * sqrt((auc1/(2-auc1)) * k[i]/(1-k[i]) + 
                                           2*auc1^2 / (1+auc1) - auc1^2 * (k[i]/(1-k[i]) + 1)))^2 / 
                 (auc0[i] - auc1)^2 - n1[i])^2,
               c(auc0[i], 1))$minimum)
    }
  }
  
  #* Estimate auc0
#   if (is.null(auc0)){
#     .params$n1_est <- with(.params, n * k)
#     .params$n2_est <- with(.params, n * (1-k))
#     .params$n_est <- with(.params, n1_est + n2_est)
#     .params$n1 <- ceiling(.params$n1_est)
#     .params$n2 <- ceiling(.params$n2_est)
#     .params$n <- with(.params, n1 + n2)
#     
#     .params$v1 <- with(.params,
#                        (auc1/(2-auc1)) * k/(1-k) + 
#                          2*auc1^2 / (1+auc1) - auc1^2 * (k/(1-k) + 1))
#     
#     for (i in 1:nrow(.params)){
#       .params$auc0[i] <- 
#         with(.params,
#              optimize(function(auc0)
#                ((qnorm(1-alpha[i]/(2-one_tail[i])) * sqrt((auc0[i]/(2-auc0[i])) * k/(1-k) + 
#                                                             2*auc0[i]^2 / (1+auc0[i]) - auc0[i]^2 * (k/(1-k) + 1)) + 
#                    qnorm(1-power[i]) * sqrt(v1)^2 / 
#                   (auc0[i] - auc1)^2 - n1[i])^2,
#                c(auc0[i], 1))$minimum)
#              
#              
#     }
#   }
  
  return(.params)
}
