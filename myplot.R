#' @title Dr. Stewart's Function for Graphing a Quadratic Regression
#'
#' Creates a dot plot for a quadratic model
#'
#' @param x x is the independent variable
#' @param model model is the quadratic linear model object
#'
#' @return Y hat value
#' @export
#'
#' @examples
#'
#'\dontrun{myplot=(x=13,model=quad.lm)}
#
myplot=function(x,model){
  model$coef[1] +model$coef[2]*x  + model$coef[3]*x^2

}


