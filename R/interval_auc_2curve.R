#' @name interval_auc_2curve
#' @export interval_auc_2curve
#' 
#' @title Single Predictor, Difference Between Two ROC Curve Confidence Interval Sample Sizes
#' @description Calculates the sample size, AUC's, margin of error, or 
#' significance for the confidence interval of the difference of two 
#' Areas Under the Curve (AUC).
#' Assumes that the AUCs are based on a single numeric predictor and that the 
#' AUCs are normally distributed.
#' 
#' @param auc1 Observed Area Under the Curve 1 (AUC)
#' @param auc2 Observed Area Under the Curve 2 AUC)
#' @param n Total sample size
#' @param E Desired margin of error
#' @param alpha Significance Level
#' @param r The correlation introduced between the two AUCs by studying the 
#'   same sample of patients.
#' @param weights a list of length-2 vectors giving the weights for the 
#'   diseased and non-diseased groups.  This is used to calculate \code{k}, the
#'   proportion of subjects belonging to the first group.
#' @param optim.max Maximum value for \code{optimze} when solving for \code{n}.  
#'     The default setting is 1,000,000,000, which is probably higher than
#'     anyone will ever need.  I may change this in the future for the sake
#'     of improving speed.
#'     
#' @details The ROC curves for this comparison should be based on the same 
#' sample.  That is, it should be two tests performed on the same sample.  The
#' comparison assumes some dependence between the curves, with this dependence 
#' being defined by \code{r}.
#' 
#' Exactly one of the parameters \code{n}, \code{E}, \code{auc1},
#' \code{auc2} and 
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
#'  \item \code{auc1}: Area Under the Curve 1 (AUC)
#'  \item \code{auc2}: Area Under the Curve 2 (AUC)
#'  \item \code{k}: Proportion of total sample size belonging to group 1
#'  \item \code{r}: The correlation introduced between the two AUCs by studying the 
#'     same sample of patients.
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
#' James A Hanley and Barbara J McNeil, "A Method of Comparing the Areas Under
#'   Receiver Operating Characteristic Curves Derived from the Same Cases,"
#'   \emph{Radiology}, Vol. 148, No. 3, Pages 839-843, September 1983.
#' 
#' Nancy A Obuchowski, "Sample size calculations in studies of test accuracy,"
#'   \emph{Statistical Methods in Medical Research} 1998; 7: 371-392
#'   
#' James A. Hanley and Barbara J. McNeil, "The Meaning and Use of the Area
#'   under a Receiver Operating Characteristic (ROC) Curve,"  
#'   \emph{Radiology}. Vol 143. No 1, Pages 29-36, April 1982.
#'

interval_auc_2curve <- function(auc1=NULL, auc2=NULL, n=NULL,
                                E=NULL, alpha=.05, r,
                                weights=list(c(1, 1)),
                                optim.max=1000000000){
  
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
  
  #* 3a. for auc1, 0 < auc1 < 1 (warning)
  if (any(auc1 <= 0 | auc1 >=1)){
    warn.flag <- warn.flag + 1
    warn.msg <- c(warn.msg,
                  paste(warn.msg, ": auc1 must be between 0 and 1, exclusive."))
    
    auc1 <- auc1[auc1 > 0 & auc1 < 1]
    
    #* 3b. Must be at least one valid value of auc1 
    #*     (only applies when auc1 is not NULL)
    if (length(auc1) == 0){
      err.flag <- err.flag + 1
      err.msg <- c(err.msg,
                   paste(err.flag, ": no valid values for auc1 were given."))
    }
  }
  
  #* 3a. for auc2, 0 < auc2 < 1 (warning)
  if (any(auc2 <= 0 | auc2 >=1)){
    warn.flag <- warn.flag + 1
    warn.msg <- c(warn.msg,
                  paste(warn.msg, ": auc2 must be between 0 and 1, exclusive."))
    
    auc2 <- auc2[auc2 > 0 & auc2 < 1]
    
    #* 3b. Must be at least one valid value of auc2 
    #*     (only applies when auc2 is not NULL)
    if (length(auc2) == 0){
      err.flag <- err.flag + 1
      err.msg <- c(err.msg,
                   paste(err.flag, ": no valid values for auc2 were given."))
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
                         auc1 = if (is.null(auc1)) NA else auc1,
                         auc2 = if (is.null(auc1)) NA else auc2,
                         k = k,
                         r = r,
                         n1_est = NA,
                         n2_est = NA,
                         n_est = NA,
                         n1 = NA,
                         n2 = NA,
                         n = if(is.null(n)) NA else n,
                         alpha = if(is.null(alpha)) NA else alpha)
  
  if (is.null(n)){
    .params$n1_est <- 
      with(.params,
           optimize(function(n1_est)
             (qnorm(1-alpha/2) * 
               sqrt(((auc1 * (1-auc1) + (n1-1)*(auc1/(2-auc1) - auc1^2) + (k/(1-k)*n1-1)*((2*auc1)/(1+auc1)-auc1^2))^2 + 
                       (auc2 * (1-auc2) + (n1-1)*(auc2/(2-auc2) - auc2^2) + (k/(1-k)*n1-1)*((2*auc2)/(1+auc2)-auc2^2))^2 ) /
                      (-2 * r * 
                         ((auc1 * (1-auc1) + (n1-1)*(auc1/(2-auc1) - auc1^2) + (k/(1-k)*n1-1)*((2*auc1)/(1+auc1)-auc1^2))^2 * 
                            (auc2 * (1-auc2) + (n1-1)*(auc2/(2-auc2) - auc2^2) + (k/(1-k)*n1-1)*((2*auc2)/(1+auc2)-auc2^2))^2 ))) - E)^2,
             c(0, optim.max))$minimum)
    .params$n2_est <- with(.params, k/(1-k) * n1_est)
    .params$n_est <- with(.params, n1_est + n2_est)
    .params$n1 <- ceiling(.params$n1_est)
    .params$n2 <- ceiling(.params$n2_est)
    .params$n <- with(.params, n1 + n2)
  }
  
  if (is.null(auc1)){
    .params$n1_est <- with(.params, n * k)
    .params$n2_est <- with(.params, n * (1-k))
    .params$n_est <- with(.params, n1_est + n2_est)
    .params$n1 <- ceiling(.params$n1_est)
    .params$n2 <- ceiling(.params$n2_est)
    .params$n <- with(.params, n1 + n2)
    
    .params$auc1 <- 
      with(.params,
           optimize(function(auc1)
             (qnorm(1-alpha/2) * 
                sqrt(((auc1 * (1-auc1) + (n1-1)*(auc1/(2-auc1) - auc1^2) + (k/(1-k)*n1-1)*((2*auc1)/(1+auc1)-auc1^2))^2 + 
                        (auc2 * (1-auc2) + (n1-1)*(auc2/(2-auc2) - auc2^2) + (k/(1-k)*n1-1)*((2*auc2)/(1+auc2)-auc2^2))^2 ) /
                       (-2 * r * 
                          ((auc1 * (1-auc1) + (n1-1)*(auc1/(2-auc1) - auc1^2) + (k/(1-k)*n1-1)*((2*auc1)/(1+auc1)-auc1^2))^2 * 
                             (auc2 * (1-auc2) + (n1-1)*(auc2/(2-auc2) - auc2^2) + (k/(1-k)*n1-1)*((2*auc2)/(1+auc2)-auc2^2))^2 ))) - E)^2,
             c(0, 1))$minimum)
  }
  
  if (is.null(auc2)){
    .params$n1_est <- with(.params, n * k)
    .params$n2_est <- with(.params, n * (1-k))
    .params$n_est <- with(.params, n1_est + n2_est)
    .params$n1 <- ceiling(.params$n1_est)
    .params$n2 <- ceiling(.params$n2_est)
    .params$n <- with(.params, n1 + n2)
    
    .params$auc2 <- 
      with(.params,
           optimize(function(auc1)
             (qnorm(1-alpha/2) * 
                sqrt(((auc1 * (1-auc1) + (n1-1)*(auc1/(2-auc1) - auc1^2) + (k/(1-k)*n1-1)*((2*auc1)/(1+auc1)-auc1^2))^2 + 
                        (auc2 * (1-auc2) + (n1-1)*(auc2/(2-auc2) - auc2^2) + (k/(1-k)*n1-1)*((2*auc2)/(1+auc2)-auc2^2))^2 ) /
                       (-2 * r * 
                          ((auc1 * (1-auc1) + (n1-1)*(auc1/(2-auc1) - auc1^2) + (k/(1-k)*n1-1)*((2*auc1)/(1+auc1)-auc1^2))^2 * 
                             (auc2 * (1-auc2) + (n1-1)*(auc2/(2-auc2) - auc2^2) + (k/(1-k)*n1-1)*((2*auc2)/(1+auc2)-auc2^2))^2 ))) - E)^2,
             c(0, 1))$minimum)
  }
  
  if (is.null(E)){
    .params$n1_est <- with(.params, n * k)
    .params$n2_est <- with(.params, n * (1-k))
    .params$n_est <- with(.params, n1_est + n2_est)
    .params$n1 <- ceiling(.params$n1_est)
    .params$n2 <- ceiling(.params$n2_est)
    .params$n <- with(.params, n1 + n2)
    
    .params$E <- with(.params,
                      qnorm(1-alpha/2) * 
                        sqrt(((auc1 * (1-auc1) + (n1-1)*(auc1/(2-auc1) - auc1^2) + (k/(1-k)*n1-1)*((2*auc1)/(1+auc1)-auc1^2))^2 + 
                               (auc2 * (1-auc2) + (n1-1)*(auc2/(2-auc2) - auc2^2) + (k/(1-k)*n1-1)*((2*auc2)/(1+auc2)-auc2^2))^2 ) /
                               (-2 * r * 
                                  ((auc1 * (1-auc1) + (n1-1)*(auc1/(2-auc1) - auc1^2) + (k/(1-k)*n1-1)*((2*auc1)/(1+auc1)-auc1^2))^2 * 
                                     (auc2 * (1-auc2) + (n1-1)*(auc2/(2-auc2) - auc2^2) + (k/(1-k)*n1-1)*((2*auc2)/(1+auc2)-auc2^2))^2 ))))    
  }
  
  if (is.null(alpha)){
    .params$n1_est <- with(.params, n * k)
    .params$n2_est <- with(.params, n * (1-k))
    .params$n_est <- with(.params, n1_est + n2_est)
    .params$n1 <- ceiling(.params$n1_est)
    .params$n2 <- ceiling(.params$n2_est)
    .params$n <- with(.params, n1 + n2)
    
    .params$alpha <- 
      with(.params,
           2 * 1 - pnorm(E/
             sqrt(((auc1 * (1-auc1) + (n1-1)*(auc1/(2-auc1) - auc1^2) + (k/(1-k)*n1-1)*((2*auc1)/(1+auc1)-auc1^2))^2 + 
                    (auc2 * (1-auc2) + (n1-1)*(auc2/(2-auc2) - auc2^2) + (k/(1-k)*n1-1)*((2*auc2)/(1+auc2)-auc2^2))^2 ) /
                  (-2 * r * 
                    ((auc1 * (1-auc1) + (n1-1)*(auc1/(2-auc1) - auc1^2) + (k/(1-k)*n1-1)*((2*auc1)/(1+auc1)-auc1^2))^2 * 
                    (auc2 * (1-auc2) + (n1-1)*(auc2/(2-auc2) - auc2^2) + (k/(1-k)*n1-1)*((2*auc2)/(1+auc2)-auc2^2))^2 )))))
                                          
  }
  
  return(.params)
  
}