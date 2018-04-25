#------------------------------------------------------------------------------#
#' Interface to read and write attributes.
#'
#' Interface to easily read and write attributes.
#'
#' @param x An object whose attributes are to be accessed.
#' @param which A non-empty string specifying which attribute is to be accessed.
#' @param value The new value of the attribute, or \code{NULL} to remove the
#'     attribute.
#'
#' @seealso \code{\link{attr}}, \code{\link{attributes}}
#'
#' @examples
#' # Create a 2 by 5 matrix:
#' x <- 1:10
#' x%.%dim <- c(2, 5)
#' # The previous line is identical to: attr(x, "dim") <- c(2, 5)
#'
#' @name attr
#' @export
#------------------------------------------------------------------------------#
`%.%` <- function(x, which) {
    m <- match.call()
    attr(x, as.character(m["which"]))
}

#------------------------------------------------------------------------------#
#' @rdname attr
#' @export
#------------------------------------------------------------------------------#
`%.%<-` <- function(x, which, value) {
    m <- match.call()
    attr(x, as.character(m["which"])) <- value
    x
}
