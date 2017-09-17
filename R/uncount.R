#------------------------------------------------------------------------------#
#' Generate raw data from count data
#'
#' Generate raw data from a count table or a count data frame. A count table
#' is a contingency table for only one factor. A count data frame is a data
#' frame consisting of two columns: the first column contains all the different
#' factor levels, and the second column, the corresponding counts.
#'
#' @param x Table (i.e. the output of \code{table} function) with only one
#'     dimension or a data frame with two columns, the second one being numeric.
#'
#' @return A vector whose length is equal to the sum of counts. It is a factor
#' if \code{x} is a table. Otherwise if \code{x} is a data frame, the class of
#' the return value is the same as the class of the first column of \code{x}.
#'
#' @examples
#' set.seed(12345)
#' x1 <- sort(rgeom(100, prob = 0.5))
#' y <- table(x1)
#' y
#' x2 <- uncount(y)
#' x2 <- as.integer(as.character(x2))
#' identical(x1, x2)
#'
#' require(MASS)
#' x <- data.frame(label = c(1, 2, 3, 4, 6, 9),
#'                 count = c(4, 7, 4, 1, 1, 1))
#' x <- uncount(x)
#' res <- fitdistr(x, "Poisson")
#' res
#'
#' @export
#------------------------------------------------------------------------------#
uncount <- function(x) {
    if (is.data.frame(x)) {
        stopifnot(ncol(x) == 2)
        stopifnot(is.numeric(x[, 2]))
    }
    if (is.table(x)) {
        stopifnot(length(dim(x)) == 1)
        x <- as.data.frame(x)
    }
    rep(x[, 1], times = x[, 2])
}


