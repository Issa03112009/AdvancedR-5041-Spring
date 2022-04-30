#' @title print
#' Prints out two samples, confidence interval and significance level to a table.
#'
#' @param x Rttest object
#' @param ... other
#' @importFrom kableExtra kable_styling kable
#' @return a table with two samples, confidence interval and significance level
#' @export
#' @examples
#' \dontrun{print(obj)}
#'
print.Rttest = function(x,...)
{

  options(knitr.kable.NA = '')
  data = x[["data"]]
  l = length(data$x)
  p.val = x[["p.val"]]
  p.val = c(p.val, rep(NA, l-1))
  conf.int = x[["conf.int"]]
  conf.int = c(conf.int, rep(NA, l - 2))
  df = data.frame(data, conf.int,p.val)
  kableExtra::kable_styling(kableExtra::kable(df, align = "c"))

}
