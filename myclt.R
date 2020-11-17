#' Central Limit Theorem
#'
#' @param n
#' @param iter
#' @param a
#' @param b
#'
#' @return histogram demonstrating central limit theorem
#' @export
#'
#' @examples w=myclt(n=50,iter=10000,a=5,b=10)
#'
myclt=function(n,iter,a=0,b=5){
  y=runif(n*iter,a,b)
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)
  sm=apply(data,2,sum)
  h=hist(sm,plot=FALSE)
  hist(sm,col=rainbow(length(h$mids)),freq=FALSE,main="Distribution of the sum of uniforms")
  curve(dnorm(x,mean=n*(a+b)/2,sd=sqrt(n*(b-a)^2/12)),add=TRUE,lwd=2,col="Blue")
  sm
}
