#' My Confidence interval
#'
#' @param d the data
#' @param conf.level the desired confidence interval
#'
#' @return confidence interval of mean
#' @export
#'
#' @examples myci(sam,conf.level=0.95)
myci=function(d,conf.level){
  obj=t.test(d,conf.level=0.95)
  ci=obj$conf.int
  print(ci)
}
