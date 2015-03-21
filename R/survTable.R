#' @name survTable
#' @export survTable
#' 
#' @title Univariable Survival Summary of Categorical Variables
#' @description Provides the median (and CI) survival, mean (and CI) survival, and hazard ratios (and CI) for 
#' time to event analyses related to categorical variables.
#' 
#' @param data A data frame with the variable to be summarized
#' @param vars A character vector of categorical variables to be used as predictors of survival
#' @param byVar A character(1) vector of 0's and 1's indicating whether the subject is censored 
#'   or experienced the event
#' @param timeVar A character(1) vector giving the name of the variable with the survival time
#' @param times A numeric(1) vector giving the time at which the mean survival should be calculated.
#' 
#' @details This function is under active development and may change without notice.
#' 
#' @author Benjamin Nutter

survTable <- function(data, vars, byVar, timeVar, times=60){
  
  if (!all(data[, byVar] %in% c(0, 1)))
    stop("'byVar' must be a vector of 0's and 1's")
  
  toFactor <- vars[!sapply(data[, vars, drop=FALSE], is.factor)]
  if (length(toFactor > 0)) data[toFactor] <- lapply(data[, toFactor, drop=FALSE], factor)
  
  SurvSumm <- function(v, data){
    form <- as.formula(paste0("Surv(", timeVar, ", ", byVar, ") ~ 1 + ", v))
    km <- survfit(form, data=data)
    logrank <- survdiff(form, data=data)
    logrank.p <- 1-pchisq(logrank$chisq, nlevels(data[, v]) - 1)
    cox <- coxph(form, data=data)
    cox.p <- if (nlevels(data[, v]) > 2) c(NA, coef(summary(cox))[, 5]) else c(coef(summary(cox))[, 5], NA)
    cox.hr <- confint(cox)
    
    data.frame(var.name = c(v, rep("", nlevels(data[, v]) - 1)),
               var.lab = c(label(data[, v]), rep("", nlevels(data[, v]) - 1)),
               levels = levels(data[, v]),
               n.obs = summary(km)$table[, 'records'],
               n.evt = summary(km)$table[, 'events'],
               stringsAsFactors=FALSE,
               median.surv = summary(km)$table[, 'median'],
               median.lower = summary(km)$table[, '0.95LCL'],
               median.upper = summary(km)$table[, '0.95UCL'],
               mean.surv = summary(km, times=times)$surv,
               mean.lower = summary(km, times=times)$lower,
               mean.upper = summary(km, times=times)$upper,
               logrank.p = c(logrank.p, rep("", nlevels(data[, v]) - 1)),
               hr = c(1, exp(coef(cox))),
               hr.lower = c(NA, exp(cox.hr[, 1])),
               hr.upper = c(NA, exp(cox.hr[, 2])),
               hr.p = cox.p)
  }
  survTable <- do.call("rbind", lapply(vars, SurvSumm, data))
  rownames(survTable) <- NULL
  return(survTable)
}
