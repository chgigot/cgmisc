#------------------------------------------------------------------------------#
#' Value matching (extra)
#'
#' @inheritParams base::`%in%`
#'
#' @name out
#' @export
#------------------------------------------------------------------------------#
`%out%` <- function(x, table) match(x, table, nomatch = 0L) == 0L

#------------------------------------------------------------------------------#
#' @rdname out
#' @export
#------------------------------------------------------------------------------#
`%!in%` <- `%out%`

#------------------------------------------------------------------------------#
#' Is a value within an interval?
#'
#' Is a value within an interval?
#'
#' @param x Vector of values to be tested.
#' @param bounds Data frame with two columns or vector with two values.
#' @param inclusive Are the bounds included within the interval.
#'
#' @examples
#' 2 %in_between% c(1, 3)
#'
#' @export
#------------------------------------------------------------------------------#
in_between <- function(x, bounds, inclusive_left = TRUE, inclusive_right = TRUE) {
    stopifnot(is.numeric(x))
    stopifnot(is.numeric(bounds))
    stopifnot(bounds[1] <= bounds[2])
    if (inclusive) {
        (x >= bounds[1] & x <= bounds[2])
    } else {
        (x > bounds[1] & x < bounds[2])
    }
}


#------------------------------------------------------------------------------#
#' @rdname in_between
#' @export
#------------------------------------------------------------------------------#
`%in_between%` <- function(x, bounds) {
    in_between(x, bounds, inclusive_left = TRUE, inclusive_right = TRUE)
}


