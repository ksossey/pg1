#' Normality test for one variable
#'
#' This function performs the Shapiro Wilk test for one single variable. The result can be accompanied by a graphical presentation (density and qqplot) giving a piece of visual information.
#'
#' @param x a `numerical` variable for which to test Normality.
#' @param output if 'none', nothing is displayed, if 'figure', a figure is displayed, if 'message' a message is displayed in the console and if 'all' a message and a figure are displayed.
#' @param var.name a `character` giving the name to be displayed. By default this will be the varname of `x`.
#'
#' @return A list with the following elements:
#' * `pval` the p-value obtained with the Shapiro Wilk test for `x`.
#' * `resul` a text (printed onto the console, if wanted) describing the result of the performed test.7
#'
#' @noRd
#'
#' @import graphics stats
# #' or, instead, it is better to use
# #' @importFrom graphics curve hist legend mtext par
# #' @importFrom stats dnorm qqline qqnorm sd shapiro.test
#'
#'

test.normality <- function(x, output = c("none", "figure", "message", "all"), var.name = deparse(substitute(x))) {
  output <- match.arg(output)
  if (!is.numeric(x)) {
    text <- paste0("The variable ", var.name, " is not numerical")
    if (output %in% c("message", "all")) {
      cat(text, "\n \n")
    }
    return(list(pval = NA, result = text))
  }
  if (output %in% c("figure", "all")) {
    par(mfrow = c(1, 2))
    test <- dnorm(x, mean = mean(x), sd = sd(x))
    histo <- hist(x, plot = FALSE)
    limiteY <- max(test, histo$density)
    hist(x, prob = TRUE, col = "lightgrey", main = "", plot = TRUE, ylim = c(0, limiteY), xlab = "", ylab = "Density")
    curve(dnorm(x, mean = mean(x), sd = sd(x)), col = "blue", add = TRUE)
    qqnorm(x, main = "")
    qqline(x)
    legend("topleft", legend = paste(c("Mean:", "Standard deviation:"), round(c(mean(x), sd(x)), 2)))
    mtext(paste("Normality test for the variable", var.name), 3, outer = TRUE, line = -2, cex = 1.5)
  }
  pvalue <- shapiro.test(x)$p.value
  if (pvalue > .05) {
    text <- paste0("The variable ", var.name, " is normal according to the Shapiro Wilk test (p-value=", round(pvalue, 5), ")")
  } else {
    text <- paste0("The variable ", var.name, " is not normal according to the Shapiro Wilk test (p-value=", round(pvalue, 5), ")")
  }
  if (output %in% c("message", "all")) {
    cat(text, "\n \n")
  }
  return(list(pval = pvalue, result = text))
}
