#------------------------------------------------------------------------------#
#' Retrieve vector or array indices
#'
#' \code{ind2sub} is a synonym for \code{\link[base]{arrayInd}} in \code{base} package.
#'
#' \code{ind2sub} is just an alias for \code{\link[base]{arrayInd}}
#'
#' @param ind Vector indices.
#' @param sub Array/matrix indices.
#'
#' @examples
#' set.seed(12345)
#' mat <- matrix(round(runif(6, min = 0, max = 10)), nrow = 2, ncol = 3)
#' ind2sub(4, dim(mat))
#' sub2ind(c(2, 2), dim(mat))
#' subs <- as.matrix(expand.grid(1:2,2:3))
#' sub2ind(subs, dim(mat))
#'
#' @name indAndSub
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#' @rdname indAndSub
#' @inheritParams base::arrayInd
#' @export
#------------------------------------------------------------------------------#
ind2sub <- base::arrayInd
#ind2sub <- function(ind, .dim, .dimnames = NULL, useNames = FALSE)
#    base::arrayInd(ind, .dim, .dimnames, useNames)
# http://stackoverflow.com/questions/4452039/rs-equivalent-to-ind2sub-sub2ind-in-matlab
# For ?-dimensional matrices
# Inspired by:
# http://tipstrickshowtos.blogspot.com/2011/09/fast-replacement-for-ind2sub.html
#ind2sub <- function(...) {
#    r = rem(idx-1,nrows)+1;
#    c = (idx-r)/nrows + 1;
#    # Equivalent to: [r,c] = ind2sub([nrows ncols],idx);
#}

#------------------------------------------------------------------------------#
#' @rdname indAndSub
#' @inheritParams base::arrayInd
#' @export
#------------------------------------------------------------------------------#
# For N-dimensional matrices
# Inspired by:
# http://tipstrickshowtos.blogspot.com/2010/02/fast-replacement-for-sub2ind.html
sub2ind <- function(sub, .dim) {
    if (!is.matrix(sub)) {
        sub <- matrix(sub, nrow = 1L)
    }
    stopifnot(ncol(sub) == length(.dim))
    nr <- nrow(sub)
    if (any(sub < 1L) || (any(sub > matrix(rep(.dim, each = nr), nrow = nr)))) {
        stop("Error: subscript out of bounds.")
    }
    ind <- apply(sub, 1L, function(coord) {
        sum(vapply(seq_len(length(coord)), function(k) {
            if (k == 1L) x <- coord[k]
            else         x <- (coord[k] - 1L) * prod(.dim[1L:(k-1L)])
            x
        }, numeric(1L)))
    })
    storage.mode(ind) <- "integer"
    ind
}

