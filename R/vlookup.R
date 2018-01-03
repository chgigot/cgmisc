# Reference:
# https://sapa-project.org/blog/2013/06/28/vlookup-in-R/
#------------------------------------------------------------------------------#
#' vlookup function
#'
#' To do.
#'
#' @param x Search criterion.
#' @param table Array.
#' @param col_return Index.
#' @param near_val Not yet implemented.
#' @param sort_order Not yet implemented.
#'
#' @export
#------------------------------------------------------------------------------#
vlookup <- function(x, table, col_return, ..., col_search = 1) {
    sapply(x, function(xx) {
        i_row <- match(xx, table[, col_search])
        table[i_row, col_return]
    })
}
