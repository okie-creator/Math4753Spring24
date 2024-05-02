#' myclt
#'
#' @param n number of samples
#' @param a lower limit
#' @param b upper limit
#' @param iter number of iterations
#'
#' @return a histogram of the central tendency theorem
#' @export
#' @examples \dontrun{mycltb(n= 10, iter = 10000, a = 0, b = 5}
myclt=function(n = 100,iter = 1000,a=0,b=5){
  y=runif(n*iter,a,b)
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)
  sm=apply(data,2,sum)
  h=hist(sm,plot=FALSE)
  hist(sm,col=rainbow(length(h$mids)),freq=FALSE,main="CLT of sum of uniforms")
  curve(dnorm(x,mean=n*(a+b)/2,sd=sqrt(n*(b-a)^2/12)),add=TRUE,lwd=2,col="Blue")
}
