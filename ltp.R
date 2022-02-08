# Empirical Cumulative Distribution
#
# This function computes the quantiles of a distribution by using the generalized
# inverse function.
#
#' @param a Numeric vector with values in group A.
#' @param b Numeric vector with values in group b.
#' @param plot If TRUE, a plot with cumulative distribution of A, B and the mean distribution is returned (By default is FALSE).
#' @return
#' \code{cumulative x1} A data frame that contains the empirical distirbution of the parameter a.
#' \code{cumulative x2} A data frame that contains the empirical distirbution of the parameter b.
#' \code{cumulative mean} A data frame that contains the mean empirical distirbution.
#' \code{plot} A plot with the three distibutions.
#' #' @author Eduardo Alarcón-Bustamante. (esalarcon@mat.uc.cl).
#' Department of Statistics, Faculty of Mathematics, Pontificia Universidad Católica de Chile, Santiago, Chile
#' @export

options(scipen = 999)

ltp <- function(a,b,plot=FALSE)
{
  if (class(a)!=c('numeric') | class(b)!=c('numeric'))
    stop('Both "a" and "b" must be numeric class')
  else{
    q <- function(a,b)
    {
      a.sort <- sort(a)
      b.sort <- sort(b)
      q.<-c()
      q.a<-c()
      for(i in 1:length(a.sort))
      {q.[i]=sum(a.sort<=a.sort[i])/length(a.sort)}
      for(i in 1:length(b))
      {q.a[i]=sum(a.sort<=b.sort[i])/length(a)}
      return(list(q.,q.a))
    }

    ecdf.a=data.frame(sort(a),q(a,b)[[1]])
    colnames(ecdf.a)=c("y","P(Y<=y|Z=0)")
    ecdf.a=distinct(ecdf.a,y,.keep_all = TRUE)

    ecdf.b=data.frame(sort(b),q(b,a)[[1]])
    colnames(ecdf.b)=c("y","P(Y<=y|Z=1)")
    ecdf.b=distinct(ecdf.b,y,.keep_all = TRUE)

    rownames(ecdf.a)=NULL
    rownames(ecdf.b)=NULL

    df.q=data.frame(A=c(q(a,b)[[1]],q(a,b)[[2]]),
                    B=c(q(b,a)[[2]],q(b,a)[[1]]),
                    y=c(sort(a),sort(b)))

    prop=c(length(a)/dim(df.q)[1],1-length(a)/dim(df.q)[1])

    df.final=data.frame(x=df.q$y,
                        mean_curve=df.q$A*prop[1]+df.q$B*prop[2])
    colnames(df.final)=c("y","P(Y<=y)")
    df.final=df.final[order(df.final[,"y"]),]
    df.final=distinct(df.final,y,.keep_all = TRUE)
    rownames(df.final)=NULL

    p=ggplot() +
      xlim(min(min(ecdf.a$y),min(ecdf.b$y)),
           max(max(ecdf.a$y),max(ecdf.b$y)))+
      geom_point(data=ecdf.a, aes(x=y,y=`P(Y<=y|Z=0)`,col="blue"),alpha=0.7)+
      geom_point(data=ecdf.b, aes(x=y,y=`P(Y<=y|Z=1)`,col="darkgreen"),alpha=0.7) +
      geom_point(data=df.final, aes(x=y,y=`P(Y<=y)`,col="red"),alpha=0.7) +
      scale_color_manual("Distributions",
                         values = c("blue","darkgreen","red"),
                         labels=c("Group A","Group B","Mean Distribution"))+
      labs(title= "Cumulative distribution",
           x="y",
           y = "Cumulative proportion")+
      theme_bw()
    list.true=list(ecdf.a,
                   ecdf.b,
                   df.final,
                   p)
    names(list.true)=c("Cumulative distribution, Group A",
                       "Cumulative distribution, Group B",
                       "Cumulative distribution, population",
                       "Plot")

    list.false=list(ecdf.a,
                    ecdf.b,
                    df.final)
    names(list.false)=c("Cumulative distribution, Group A",
                        "Cumulative distribution, Group B",
                        "Cumulative distribution, population")
    if(plot==TRUE)
    {return(list.true)}
    else{
      return(list.false)
    }
  }
}

