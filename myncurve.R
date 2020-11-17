#' Myncurve Function
#'
#' @param mu
#' @param sigma
#' @param x
#'
#' @return A curve and the area between the curve and x-axis from negative infinity to x=a, returned as a list
#' @export
#'
#' @examples
myncurve = function(mu, sigma,x){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve=seq(mu-3*sigma,x,length=1000)
  ycurve=dnorm(xcurve,mu,sigma)
  polygon(c(mu-3*sigma,xcurve,x),c(0,ycurve,0),col="Blue")

  area=pnorm(x,mu,sigma)
  area<-round(area,4)

  text(mu,dnorm((mu),mu,sigma)/2,paste("Area=",area,sep=""))
  return(list(area))
}
