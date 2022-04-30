#' @title package Progress-3
#'
#' @param x x variable as vector
#' @param y y variable as vector
#' @param alpha
#'
#' @return
#' @export
#'
#' @examples
myconstr = function(x,y, alpha){
  test = t.test(x, y, mu = 0, alternative = "two.sided", var.equal = TRUE, conf.level = 1 - alpha)
  l = max(length(x), length(y))
  x = c(x, rep(NA, l - length(x)))
  y = c(y, rep(NA, l- length(y)))
  obj = list(data = data.frame(x,y), conf.int =test$conf.int, p.val = test$p.value)
  class(obj) = "Rttest"
  obj
}

set.seed(21)
x<- rnorm(30,5,2)
set.seed(23)
y <- rnorm(30,3,2)
alpha <- 0.05


obj<- myconstr(x =x , y = y, alpha = 0.05)
obj

print.Rttest = function(x){
  library(kableExtra)
  data = obj[["data"]]

  kable(data)

}
print(obj)
