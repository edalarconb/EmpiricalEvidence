# Empirical Cumulative Distribution
#
# This function computes the empirical distribution of a vector.
#
#' @param a Numeric vector where the cumulative distribution will be computed.
#' @return
#' \code{e_cdf} A data frame that contains the empirical distirbution of the parameter a
#' #' @author Eduardo Alarcón-Bustamante. (esalarcon@mat.uc.cl).
#'
#' Department of Statistics, Faculty of Mathematics, Pontificia Universidad Católica de Chile, Santiago, Chile
#' @export

options(scipen = 999)
cum.dist <- function(a, plot=FALSE)
{
  if (class(a)!=c('numeric'))
  stop('The input "a" must be a numeric class')
  else{
    a.sort <- sort(a)
    q.<-c()
    for(i in 1:length(a.sort))
    {
      q.[i]=sum(a.sort<=a.sort[i])/length(a.sort)
    }
    data.ordered=data.frame(a.sort,q.)
    colnames(data.ordered)=c("x","P(X<=x)")
    e_cdf=distinct(data.ordered,x,.keep_all = TRUE)
    p=ggplot()+
      geom_point(data=e_cdf, aes(x=x,y=`P(X<=x)`),col="blue",alpha=0.7)+
      theme_bw()
    if(plot==TRUE)
    {return(list(e_cdf,p))}
    else(return(e_cdf))}
}
