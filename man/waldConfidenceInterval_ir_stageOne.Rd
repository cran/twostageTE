\name{waldConfidenceInterval_ir_stageOne}
\alias{waldConfidenceInterval_ir_stageOne}
\title{
Stage one IR-Wald confidence interval
}
\description{
This is an internal function not meant to be called directly.
Classical IR-Wald confidence interval that can be called at the first 
stage of a multistage procedure
}
\usage{
waldConfidenceInterval_ir_stageOne(explanatory, response, Y_0, level = NA)
}
\arguments{
  \item{explanatory}{
Explanatory sample points
}
  \item{response}{
Observed responses at the explanatory sample points
}
  \item{Y_0}{
Threshold of interest
}
  \item{level}{
Desired confidence level
}
}
\value{
\item{estimate}{Point estimate for d_0}
\item{lower}{Lower bound of the confidence interval}
\item{upper}{upper bound of the confidence interval}
\item{C_1}{Constant for computing the confidence interval -- required 
for second stage ir-wald analysis}
\item{sigmaSq}{Estimate of variance}
\item{deriv_d0}{Estimate of the derivative at d_0}
}
\author{
Shawn Mankad
}
\examples{
X=runif(25, 0,1)
Y=X^2+rnorm(n=length(X), sd=0.1)
oneStage_IR=stageOneAnalysis(X, Y, 0.25, type="IR-wald", 0.99)

## The function is currently defined as
function (explanatory, response, Y_0, level = NA) 
{
    if (is.na(level)) {
        level = 0.95
    }
    alpha = 1 - level
    ## Import previously computed Chernoff quantiles, provided by Groeneboom and Wellner
    chernoff_realizations <- NULL; rm(chernoff_realizations); 
    data("chernoff_realizations", envir =environment())
    ind = min(which(chernoff_realizations$DF - (1-alpha/2) >= 0))
    q = chernoff_realizations$xcoor[ind]
    n = length(response)
	
    fit = threshold_estimate_ir(explanatory, response, Y_0)
    sigmaSq = estimateSigmaSq(explanatory, response)$sigmaSq
    deriv_d0 = estimateDeriv(explanatory, response, 
        fit$threshold_estimate_explanatory, sigmaSq) 
    g_d0 = 1/n

    n = length(explanatory)
    C_di = (4 * sigmaSq/(deriv_d0^2))^(1/3)
    band = n^(-1/3) * C_di * g_d0^(-1/3) * q
    return(list(estimate = fit$threshold_estimate_explanatory, 
        lower = max(min(explanatory), fit$threshold_estimate_explanatory - 
            band), upper = min(max(explanatory), fit$threshold_estimate_explanatory + 
            band), C_1 = as.numeric(C_di * g_d0^(-1/3) * q), 
        sigmaSq = sigmaSq, deriv_d0 = deriv_d0))
  }
}


