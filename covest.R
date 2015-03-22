require(robustbase) # required for the Qn scale estimator
require(Matrix) # required for forceSymmetric function
require(ICSNP) # required for the pair.sum C function
require(rococo) # required for Gaussian Rank correlation

pair.cov = function(X,scale.fn=Pn){
  if(any(is.na(X))) return("There are missing values!")
  n = nrow(X)
  p = ncol(X)
  s = apply(X,2,scale.fn)
  sigma = matrix(NA,ncol=p,nrow=p) 
  for (i in 2:p) { # construct the "correlation matrix"
    for (j in 1:(i-1)) {
      Y = cbind(X[,i], X[,j])
      x = Y[,1]
      y = Y[,2]
      alpha = s[i]
      beta = s[j]
      sigma[i, j] = alpha*beta/4*(scale.fn(x/alpha + y/beta)^2 - scale.fn(x/alpha - y/beta)^2)
    }
  }
  diag(sigma) = s^2
  S = as.matrix(nearPD(forceSymmetric(sigma,"L"))$mat)
  rownames(S) = colnames(S) = colnames(X)
  return(S)
}

### Pn estimator (13% breakdown point, 86% Gaussian efficiency)
Pn = function(y){
  n = length(y)
  if(n <= 2){
    warning("Your sample size is too small")
    return()
  }
  y.pairs = pair.sum(matrix(y))/2
  #y.pairs = outer(y,y,"+")
  #y.pairs = y.pairs[lower.tri(y.pairs)]/2
  constant = 1/0.9539 # asymptotic correction factor
  scale.est = constant*as.numeric(diff(quantile(y.pairs,c(0.25,0.75),type=1)))
  # Correction factors obtained through simulation over 1 million replications
  correction.factors =
    c(1.128,1.303,1.109,1.064,1.166,1.103,1.087,1.105,1.047,1.063,1.057,1.040, 
      1.061,1.047,1.043,1.048,1.031,1.037,1.035,1.028,1.036,1.030,1.029,1.032, 
      1.023,1.025,1.024,1.021,1.026,1.022,1.021,1.023,1.018,1.020,1.019,1.017, 
      1.020,1.018,1.017,1.018,1.015,1.016,1.016,1.014,1.016,1.015,1.014,1.015)
  if(n <= 40){
    scale.est = scale.est*correction.factors[n-2]
    # n-2 as the first element of the correction.factors vector is for n=3
  } else if(n > 40) scale.est = scale.est*n/(n-0.7)
  return(scale.est)
}

gaus.cov.Qn = function(X){
  s = apply(X,2,Qn)
  p = dim(X)[2]
  sigma = matrix(NA,ncol=p,nrow=p) 
  for (i in 2:p) { # construct the "correlation matrix"
    for (j in 1:(i-1)) {
      sigma[i, j] = gauss.cor(X[,i], X[,j])*s[i]*s[j]
    }
  }
  diag(sigma) = s^2
  return(as.matrix(forceSymmetric(sigma,"L")))
}
