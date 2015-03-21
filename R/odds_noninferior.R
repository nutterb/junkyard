#' @name odds_noninferior
#' @export odds_noninferior
#'
#' @title Sample Size for a non-inferiority test of an Odds Ratio.
#'
#' @description Make a good description here.
#' @param pA
#' @param pB
#' 
#' @details This function is currently under development.  I needed it for a 
#' study design.
#' 
#' @author Benjamin Nutter
#' 
#' @references 
#' http://powerandsamplesize.com/Calculators/Test-Odds-Ratio/Non-Inferiority-or-Superiority
#' 

odds_noninferior <- function(pA = NULL, pB=NULL, delta=NULL, kappa=1, alpha=.05, power=NULL){
    return("NOT YET IMPLEMENTED")
#   pA=0.40
#   pB=0.25
#   delta=0.20
#   kappa=1
#   alpha=0.05
#   beta=0.20
#   (OR=pA*(1-pB)/pB/(1-pA)) # 2
#   (nB=(1/(kappa*pA*(1-pA))+1/(pB*(1-pB)))*((qnorm(1-alpha)+qnorm(1-beta))/(log(OR)-delta))^2)
#   ceiling(nB) # 242
#   z=(log(OR)-delta)*sqrt(nB)/sqrt(1/(kappa*pA*(1-pA))+1/(pB*(1-pB)))
#   (Power=pnorm(z-qnorm(1-alpha))+pnorm(-z-qnorm(1-alpha)))
}