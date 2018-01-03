#------------------------------------------------------------------------------#
# Do deal with intrinsic null hypothesis
# Some references:
# - http://rcompanion.org/rcompanion/b_03.html
# - http://www.biostathandbook.com/chigof.html
#------------------------------------------------------------------------------#
#' Chi-squared goodness-of-fit with intrinsic null hypothesis
#'
#' This version of the chi-squared test for goodness-of-fit allows one to
#' perform intrinsic null hypothesis specifying the degree of freedom of the
#' X-squared distribution.
#'
#' Under the usual extrinsic null hypothesis where the expected numbers are
#' known before collecting data, and the degree of freedom corresponds to the
#' number of classes minus 1. Indeed,...
#'
#' However under a intrinsic null hypothesis, one or more parameters are
#' estimated from the data collected to estimate subsequently the expected
#' numbers. The degree of freedom needs to take that into account, and so it is
#' equal to the number of classes minus the number of assessed parameters minus
#' 1.
#'
#' If \code{df} is not given, the regular \code{\link[stats]{chisq.test}} is
#' called with all the same parameters and \code{rescale.p = TRUE}.
#'
#' @param x A numeric vector. Observed values.
#' @param p A vector of probabilities of the same length of x.
#' @param n_est Number of estimated parameters. Not yet implemented.
#' @param df Degree of freedom.
#' @param ... Extra parameters to be passed to \code{\link[stats]{chisq.test}}.
#'
#' @examples
#' set.seed(12345)
#' # Here we know the expected mean number:
#' p <- 0.3
#' n <- 10
#' N <- 30
#' obs <- rbinom(n = N, size = n, prob = p)
#' freq <- as.data.frame(table(obs))
#' names(freq) <- c("category","observed")
#' freq <- merge(x = data.frame(category = 0:n),
#'               y = freq,
#'               by = "category", all = TRUE)
#' freq[is.na(freq)] <- 0
#' freq[] <- lapply(freq, as.numeric) # Force each column to be numeric (and only numeric!)
#' freq$expected <- dbinom(x = 0:10, size = n, prob = p) * N
#' # Test
#' test1 <- chisq.test2(freq$observed, p = freq$expected, rescale.p = T)
#' test2 <- chisq.test(freq$observed, p = freq$expected, rescale.p = T)
#' identical(test1, test2)
#' test1
#'
#' # If we assess one parameter from the observed data set:
#' p_est <- mean(freq$observed) / n
#' n_est <- 1
#' test1 <- chisq.test2(freq$observed, p = freq$expected, n_est = n_est,
#'                      rescale.p = T)
#' test2 <- chisq.test2(freq$observed, p = freq$expected,
#'                      df = length(freq$observed) - n_est - 1, rescale.p = T)
#' identical(test1, test2)
#' test1
#'
#' @export
#------------------------------------------------------------------------------#
chisq.test2 <- function(x, p, n_est, df, rescale.p = FALSE, ...) {

    if (missing(n_est) && missing(df)) {
        res <- stats::chisq.test(x = x, p = p, rescale.p = rescale.p, ...)
        res$data.name <- deparse(substitute(x))
        res
    } else {
        if (!missing(n_est) && !missing(df)) {
            stop("n_est or df must be provided, not both at the same time.")
        }
        if (!missing(n_est)) df <- length(x) - n_est - 1
        n <- sum(x)
        if (abs(sum(p) - 1) > sqrt(.Machine$double.eps)) {
            if (rescale.p) p <- p/sum(p)
            else stop("Probabilities must sum to 1.")
        }
        E <- n * p
        V <- n * p * (1 - p)
        statistic <- sum((x - E)^2/E)
        pVal <- pchisq(statistic, df = df, lower.tail = FALSE)
        names(statistic) <- "X-squared"
        names(df) <- "df"
        names(E) <- names(x)
        if (any(E < 5)) warning("Chi-squared approximation may be incorrect.")
        structure(list(statistic = statistic,
                       parameter = df,
                       p.value = pVal,
                       method = "Chi-squared goodness-of-fit with intrinsic null hypothesis",
                       data.name = deparse(substitute(x)),
                       observed = x,
                       expected = E,
                       residuals = (x - E)/sqrt(E),
                       stdres = (x - E)/sqrt(V)),
                  class = "htest")
    }
}




