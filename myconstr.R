#' @title 2 sample test
#' This is my constructor function. This function will create a list. The constructor runs the hypotheses on two samples, and creates and object with the confidence interval and p statistics
#'
#' @param x sample 1 data
#' @param y sample 2 data
#' @param alpha significance level
#'
#' @return list of data, confidence interval and p value
#' @export
#'
#' @examples
#' \dontrun{obj = myconstr(x = x, y = y, alpha = 0.05)}
myconstr = function(x, y, alpha){

  test = stats::t.test(x, y, mu = 0, alternative = "two.sided", var.equal = TRUE, conf.level = 1-alpha)
  l = max(length(x), length(y))
  x = c(x, rep(NA, l - length(x)))
  y = c(y, rep(NA, l - length(y)))
  p.val = test$p.value
  conf.int = test$conf.int
  obj = list(data = data.frame(x, y), conf.int = conf.int, p.val = p.val)
  class(obj) = "Rttest"
  obj
}


