#' @name interval_auc_1curve
#' @export interval_auc_1curve
#' 
#' @title Single Predictor, Single Curve ROC Confidence Interval Sample Sizes
#' @description Calculates the sample size, AUC, margin of error, or 
#' significance for the confidence interval of an Area Under the Curve (AUC).
#' Assumes that the AUC is based on a single numeric predictor and that the 
#' AUC is normally distributed.
#' 
#' @param auc Observed Area Under the Curve (AUC)
#' @param n Total sample size
#' @param E Desired margin of error
#' @param alpha Significance Level
#' @param weights a list of length-2 vectors giving the weights for the 
#'   diseased and non-diseased groups.  This is used to calculate \code{k}, the
#'   proportion of subjects belonging to the first group.
#' @param optim.max Maximum value for \code{optimze} when solving for \code{n}.  
#'     The default setting is 1,000,000,000, which is probably higher than
#'     anyone will ever need.  I may change this in the future for the sake
#'     of improving speed.
#'     
#' @details Exactly one of the parameters \code{n}, \code{E}, \code{auc} and 
#' \code{alpha} must be passed as \code{NULL}, and that parameter will be 
#' calculated from the others.  Notice that \code{alpha} has a 
#' non-\code{NULL} default, so \code{NULL} must be explicitly passed 
#' if you want it computed.
#' 
#' @return Returns a data frame of the parameters passed to the function
#'  (expanded using \code{expand.grid}) and the corresponding sample size 
#'  estimates.  
#'  
#'  \enumerate{
#'  \item \code{E}: Margin of Error
#'  \item \code{auc}: Area Under the Curve (AUC)
#'  \item \code{k}: Proportion of total sample size belonging to group 1
#'  \item \code{n1_est}: Exact sample size for group 1
#'  \item \code{n2_est}: Exact sample size for group 2
#'  \item \code{n_est}: Exact total sample size
#'  \item \code{n1}: Estimated sample size for group 1 (next largest integer)
#'  \item \code{n2}: Estimated sample size for group 2 (next largest integer)
#'  \item \code{n}: Estimated total sample size (sum of \code{n1} and \code{n2})
#'  }
#'  
#' @author Benjamin Nutter
#' 
#' @references 
#' Nancy A Obuchowski, "Sample size calculations in studies of test accuracy,"
#'   \emph{Statistical Methods in Medical Research} 1998; 7: 371-392
#'   
#' James A. Hanley and Barbara J. McNeil, "The Meaning and Use of the Area
#'   under a Receiver Operating Characteristic (ROC) Curve,"  
#'   \emph{Radiology}. Vol 143. No 1, Pages 29-36, April 1982.
#' 
#' @examples
#' # (Example 1 from PASS Sample Size Software)
#' # \url{http://ncss.wpengine.netdna-cdn.com/wp-content/themes/ncss/pdf/Procedures/PASS/Confidence_Intervals_for_the_Area_Under_an_ROC_Curve.pdf}
#' interval_auc_1curve(auc=c(0.6, 0.7, 0.8, 0.9),
#'                     E = c(.05, .10),
#'                     alpha=0.05)
#'                     

interval_auc_1curve <- function(auc=NULL, n=NULL, E = NULL, alpha=.05,
                                weights=list(c(1, 1)), 
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
  #* 3a. for auc, 0 < auc < 1 (warning)
  #* 3b. Must be at least one valid value of auc
  #* 4a. E must be a positive number
  #* 4b. Must be at least one valid value for E
  #*******************************************************
  
  #* 1.  only one of E, n, s, alpha may be NULL (error)
  if (sum(sapply(list(n, auc, E, alpha), is.null)) != 1){
    err.flag <- err.flag + 1
    err.msg <- c(err.msg,
                 paste(err.flag, ": Exactly one of E, n, auc, and alpha may be NULL"))
  }
  
  #* 2a. for alpha, 0 < alpha < 1 (warning)
  if (any(alpha <= 0 | alpha >=1)){
    warn.flag <- warn.flag + 1
    warn.msg <- c(warn.msg,
                  paste(warn.msg, ": alpha must be between 0 and 1, exclusive."))
    
    alpha <- alpha[alpha > 0 & alpha < 1]
    
    #* 2b. Must be at least one valid value of alpha 
    #*     (only applies when alpha is not NULL)
    if (length(alpha) == 0){
      err.flag <- err.flag + 1
      err.msg <- c(err.msg,
                   paste(err.flag, ": no valid values for alpha were given."))
    }
  }
  
  #* 3a. for auc, 0 < auc < 1 (warning)
  if (any(auc <= 0 | auc >=1)){
    warn.flag <- warn.flag + 1
    warn.msg <- c(warn.msg,
                  paste(warn.msg, ": auc must be between 0 and 1, exclusive."))
    
    auc <- auc[auc > 0 & auc < 1]
    
    #* 3b. Must be at least one valid value of auc 
    #*     (only applies when auc is not NULL)
    if (length(auc) == 0){
      err.flag <- err.flag + 1
      err.msg <- c(err.msg,
                   paste(err.flag, ": no valid values for auc were given."))
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
  
  
  #*******************************************************
  #* Solve for NULL parameters
  #*******************************************************

  #* Calculate proportion of sample belonging to n1
  k <- sapply(weights, function(x) x[1]/sum(x))
  
  #* Establish parameters data frame
  .params <- expand.grid(E = if (is.null(E)) NA else E,
                         auc = if (is.null(auc)) NA else auc,
                         k = k,
                         n1_est = NA,
                         n2_est = NA,
                         n_est = NA,
                         n1 = NA,
                         n2 = NA,
                         n = if(is.null(n)) NA else n,
                         alpha = if(is.null(alpha)) NA else alpha)
  
  #* Solve for n
  if (is.null(n)){
    for (i in 1:nrow(.params)){
      .params$n1_est[i] <- 
        with(.params,
             optimize(function(n1_est){ 
               (qnorm(1-alpha[i]/2) * 
                 sqrt((auc[i] * (1-auc[i]) + 
                         (n1_est - 1) * (auc[i]/(2-auc[i]) - auc[i]^2) + 
                         (k[i]/(1-k[i])*n1_est - 1) * ((2*auc[i])/(1+auc[i]) - auc[i]^2))/
                        (k[i]/(1-k[i])*n1_est^2)) - E[i])^2},
               c(0, optim.max))$minimum)
    }
    .params$n2_est <- with(.params, k/(1-k) * n1_est)
    .params$n_est <- with(.params, n1_est + n2_est)
    .params$n1 <- ceiling(.params$n1_est)
    .params$n2 <- ceiling(.params$n2_est)
    .params$n <- with(.params, n1 + n2)
  }
  
  #* Solve for AUC
  if (is.null(auc)){
    .params$n1_est <- with(.params, n * k)
    .params$n2_est <- with(.params, n * (1-k))
    .params$n_est <- with(.params, n1_est + n2_est)
    .params$n1 <- ceiling(.params$n1_est)
    .params$n2 <- ceiling(.params$n2_est)
    .params$n <- with(.params, n1 + n2)
    
    for (i in 1:nrow(.params)){
    .params$auc[i] <- 
      with(.params,
           optimize(function(auc) 
             (qnorm(1-alpha[i]/2) * 
                sqrt((auc * (1-auc) + 
                        (n1_est[i] - 1) * (auc/(2-auc) - auc^2) + 
                        (k[i]/(1-k[i])*n1_est[i] - 1) * ((2*auc)/(1+auc) - auc^2))/
                       (k[i]/(1-k[i])*n1_est[i]^2)) - E[i])^2,
             c(0, 1))$minimum)
    }
  }
  
  #* Solve for E
  if (is.null(E)){
    .params$n1_est <- with(.params, n * k)
    .params$n2_est <- with(.params, n * (1-k))
    .params$n_est <- with(.params, n1_est + n2_est)
    .params$n1 <- ceiling(.params$n1_est)
    .params$n2 <- ceiling(.params$n2_est)
    .params$n <- with(.params, n1 + n2)
    .params$E <- with(.params, 
                      qnorm(1-alpha/2) * sqrt((auc * (1-auc) + 
                                              (n1_est - 1) * (auc/(2-auc) - auc^2) + 
                                              (k/(1-k)*n1_est - 1) * ((2*auc)/(1+auc) - auc^2))/
                                              (k/(1-k)*n1_est^2)))
  }
  
  #* Solve for alpha
  if (is.null(alpha)){
    .params$n1_est <- with(.params, n * k)
    .params$n2_est <- with(.params, n * (1-k))
    .params$n_est <- with(.params, n1_est + n2_est)
    .params$n1 <- ceiling(.params$n1_est)
    .params$n2 <- ceiling(.params$n2_est)
    .params$n <- with(.params, n1 + n2)  
    .params$alpha <- with(.params,
                          2 * (1-pnorm(E / sqrt((auc * (1-auc) + 
                                                (n1_est - 1) * (auc/(2-auc) - auc^2) + 
                                                (k/(1-k)*n1_est - 1) * ((2*auc)/(1+auc) - auc^2))/
                                               (k/(1-k)*n1_est^2)))))
  }
  
  return(.params)
                         
}
