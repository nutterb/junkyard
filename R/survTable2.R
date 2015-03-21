#' @name survTableComplex
#' @export survTableComplex
#' @import Hmisc
#' @importFrom lazyWeave pvalue.QHS
#' @importFrom plyr arrange
#' @importFrom plyr ddply
#' @importFrom plyr rbind.fill
#' @importFrom reshape2 melt
#' 
#' @title Univariable Summary of Survival Analyses
#' @description Provides a summary table of univariable survival metrics.  
#'   The default summaries include summary values similar to \code{cattable}
#'   and \code{conttable}, but also includes the hazard ratio, p-value,
#'   and the median survival.  Additionally, the survival proportions for
#'   user specified time points may be given.
#'   
#' @param data A data frame
#' @param vars Variables to be summarized
#' @param byVar The event variable.  May be 0/1 or a two category factor/character
#' @param time.var Variable giving the survival time
#' @param times The times argument passed to \code{summary.survfit}
#' @param vars.cat Numeric variables in 'vars' to force as categorical variables
#' @param event.level For non-numeric \code{byVar}, specifies the level to be 
#'   considered the event.
#' @param normal a character vector naming numeric variables to be summarized by
#'   mean and standard deviation.
#' @param quartile Should non-normal numeric variables be summarized with 
#'   the interquartile range. If \code{FALSE}, the range is printed.
#' @param row.p For categorical variables, should the percentage show the 
#'   row percentage?
#'   
#' @details The table produced is similar to the \code{cattable} format, but 
#'   the output may not be as flexible.
#'   
#' @author Benjamin Nutter

survTableComplex <- function(data, vars, byVar, time.var, times=NULL, 
                      vars.cat, event.level,
                      normal=NULL, quartile=TRUE, row.p=FALSE){
  require(lazyWeave)
  
  bracket <- if (quartile) c("(", ")") else c("[", "]")
  ptile <- if (quartile) c(.25, .75) else c(0, 1)
  
  
  #* byVar Checks
  if (is.numeric(data[, byVar])){
    if (!all(data[, byVar] %in% c(0, 1))) 
      stop("'byVar' must be a 0/1 vector")
    byVar_levs <- c("0 = No Event", "1 = Event")
  }
  else {
    if (!is.factor(data[, byVar])) data[, byVar] <- factor(data[, byVar])
    byVar_levs <- levels(data[, byVar])
    data[, byVar] <- as.numeric(data[, byVar])
  }
  
  if (!missing(vars.cat)){
    data[, vars.cat] <- lapply(data[, vars.cat, drop=FALSE],
                                           factor)
  }
    
  data[, vars] <- lapply(data[, vars],
                         function(x){
                           if (!is.numeric(x) & !is.factor(x))
                             factor(x)
                           else x
                         })
  
  numerics <- sapply(data[, vars], is.numeric)
    
  #* Formulae
  f <- paste0("Surv(", paste(time.var, collapse=", "), ", ", 
              byVar, ") ~ ", vars)
  f <- lapply(f, as.formula)
  names(f) <- vars
  
  #* Cox Models
  fits <- lapply(f, coxph, data=data)
  names(fits) <- vars

  #* Function to calculate the summaries for each variable
  SurvSummary <- function(v){
    if (numerics[v]){
      #* Normal numerics
      if (v %in% normal){
        summ <- paste(round(tapply(data[, v], data[, byVar], mean, na.rm=TRUE), 2),
                      round(tapply(data[, v], data[, byVar], sd, na.rm=TRUE), 2),
                      sep="pm")
      }
      #* Non normal numerics
      else{ 
        summ <- paste0(round(tapply(data[, v], data[, byVar], median, na.rm=TRUE), 2),
                       " ", bracket[1], 
                       round(tapply(data[, v], data[, byVar], quantile, probs=ptile[1], na.rm=TRUE), 2),
                       ", ", 
                       round(tapply(data[, v], data[, byVar], quantile, probs=ptile[2], na.rm=TRUE), 2),
                       bracket[2])
      }
      summ <- matrix(summ, ncol=2, byrow=FALSE)
      
      summ <- cbind(v, 
                    Hmisc::label(data[, v]),
                    "",
                    summ,
                    "",
                    "",
                    round(exp(coef(fits[[v]])), 1),
                    lazyWeave::pvalue.QHS(coef(summary(fits[[v]]))[[5]]))
      summ <- as.data.frame(summ, stringsAsFactors=FALSE)
      names(summ) <- c("var.name", "var.label", "level", "stat.noevent",
                       "stat.event", "time", "value", "hr", "p")
    }
    #* Categoricals
    else {
      summ <- paste0(table(data[, v], data[, byVar]),
                    " (", 
                    round(prop.table(table(data[, v], data[, byVar]), 2-row.p)* 100, 1),
                    ")")
      summ <- matrix(summ, ncol=2, byrow=FALSE)
      
      summ <- cbind(v,
                    Hmisc::label(data[, v]),
                    levels(data[, v]),
                    summ,
                    c("REF", round(exp(coef(fits[[v]])), 1)),
                    c("", lazyWeave::pvalue.QHS(coef(summary(fits[[v]]))[[5]])))
      summ <- as.data.frame(summ, stringsAsFactors=FALSE)
      names(summ) <- c("var.name", "var.label", "level", "stat.noevent", "stat.event",
                       "hr", "p")
      km <- summary(survfit(f[[v]], data=data), times=times)
      km <- {if (is.null(times)) matrix(km$table[, 'median'], ncol=2)
             else rbind(matrix(km$table[, 'median'], ncol=2),
                       matrix(km$surv, ncol=2, byrow=FALSE))}
      km <- round(km, 3)
      colnames(km) <- levels(data[, v])
      
      km <- as.data.frame(km, stringsAsFactors=FALSE)
      km$time <- c("median", times)
      km <- reshape2::melt(km, "time", variable.name = "level")
      
      summ <- merge(summ, km, by="level")
      summ$level <- factor(summ$level, levels(data[,v]))
      summ$time <- factor(summ$time, c('median', times))
      summ <- plyr::arrange(summ, var.name, var.label, level, time)
      rownames(summ) <- NULL
      summ <- plyr::ddply(summ, 
                          c("var.name", "level"),
                          transform,
                          stat.noevent = c(stat.noevent[1], rep("", length(level)-1)),
                          stat.event = c(stat.event[1], rep("", length(level)-1)),
                          hr = c(hr[1], rep("", length(level)-1)),
                          p = c(p[1], rep("", length(level)-1)),
                          level = c(as.character(level[1]), rep("", length(level) -1)))
      summ <- plyr::ddply(summ, 
                          c("var.name"),
                          transform,
                          var.name = c(var.name[1], rep("", length(level)-1)),
                          var.label = c(var.label[1], rep("", length(level)-1)))
      summ <- summ[, c("var.name", "var.label", "level", "stat.noevent",
                       "stat.event", "time", "value", "hr", "p")]
      
    }
    
    
    return(summ)
    
  }
  
  return(do.call("rbind", lapply(vars, SurvSummary)))
#   return(lapply(vars, SurvSummary))
  
}
