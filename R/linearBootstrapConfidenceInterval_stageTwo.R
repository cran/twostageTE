linearBootstrapConfidenceInterval_stageTwo <-
function(explanatory, response, Y_0, level=NA) {
	numBootstrap = 1000
	if (is.na(level)) {
		level = 0.95
	}
	alpha = 1 - level
	n = length(response)
	
	fit = threshold_estimate_locLinear(explanatory, response, Y_0)
	Rn = rep(0, numBootstrap)
	for (i in 1:numBootstrap) {
		ind = sample(x=n, replace=TRUE)	 ## sample with replacement
		fit_bst = threshold_estimate_locLinear(explanatory[ind], response[ind], Y_0)
		Rn[i] = sqrt(n) * (fit_bst$threshold_estimate_explanatory - fit$threshold_estimate_explanatory)
	}
	qU = quantile(Rn, alpha/2)
	qL = quantile(Rn, level + alpha/2)	
	uBand = fit$threshold_estimate_explanatory  - n^(-1.0/2.0) * qU
	lBand = fit$threshold_estimate_explanatory  - n^(-1.0/2.0) * qL

	return(list(estimate=fit$threshold_estimate_explanatory,lower=max(lBand, min(explanatory)), upper=min(uBand, max(explanatory)), sigmaSq = NA, deriv_d0=NA))
}
