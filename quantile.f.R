# Empirical Cumulative Distribution
#
# This function computes the quantiles of a distribution by using the generalized
# inverse function.
#
#' @param data Numeric vector where the quantiles will be computed.
#' @param alpha Numeric vector that represents the alpha.
#' @return
#' \code{data.q} A data frame that contains the empirical distirbution of the parameter a
#' \code{quantile.x} The requiered quantiles
#' #' @author Eduardo Alarcón-Bustamante. (esalarcon@mat.uc.cl).
#' Department of Statistics, Faculty of Mathematics, Pontificia Universidad Católica de Chile, Santiago, Chile
#' @export

options(scipen = 999)

## Quantile Function
quantile.f <-function(data,alpha)
{
  condition.alpha=alpha<0 | alpha>1
  if (class(data)!="numeric" | sum(condition.alpha)>0)
  stop("The input 'data' must be numeric class and each element of 'alpha' must be in between 0 and 1")
  else{
    data.q=cum.dist(data)[[1]]
    q.function=lapply(1:length(alpha),
                      function(a) min(which(data.q[,2]>=alpha[a])))
    values.q=unlist(q.function)
    quantile.x=data.frame(alpha=alpha,
                          quantile=data.q[values.q,][,1])
    quantile.x=quantile.x[order(quantile.x$alpha,
                                decreasing = FALSE),]
    list.return=list(data.q,
                     quantile.x)
    names(list.return)=c("Cumulative Distribution",
                         "Required quantiles")
    return(list.return)}
}
