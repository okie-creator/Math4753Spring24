#' myncurve
#'
#' @param mu median
#' @param sigma standard deviation
#' @param a area
#'
#' @return a list of the median, standard deviation and area
#' @export
#'
#' @examples \dontrun{myquad(mu = 3, sigma = 5, a = 10)}
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma), ylab= "Normal Density", xlab="Y")
  list(mu = mu, sigma = sigma)
  xcurve=seq(-3*sigma,a,length=1000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)
  polygon(c(-3*sigma,xcurve,a),c(0,ycurve,0),col="red")
  prob=pnorm(a,mean=mu,sd=sigma)
  prob=round(prob,4)
  list(mu = mu, sigma = sigma, area = prob)
}
