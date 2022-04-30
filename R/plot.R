#' @title plot
#' Generates a boxplot of two samples
#'
#' @param x Rttest object
#' @param ... other
#' @importFrom ggplot2 ggplot geom_boxplot aes labs theme_classic
#' @return boxplot of two samples
#' @export
#' @examples
#' \dontrun{plot(obj)}
#'
plot.Rttest <- function(x,...)
{

  L = NULL
  g = NULL
  data = x[["data"]]
  l = length(data$x)
  Cat <- rep(c("x", "y"), c(l,l))
  df = data.frame(L = c(data$x, data$y), g = Cat)
  ggplot(data = df, aes(x = g, y = L, fill = g )) +
    geom_boxplot() +
    labs(title="Boxplot of two samples",
         x ="samples", y = "data", fill='Sample name')+
    theme_classic()

}
