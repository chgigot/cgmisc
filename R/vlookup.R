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
#' @examples
#' shopping <- data.frame(product  = c("Sports Almanac", "Frisbee",
#'                                     "Water bottles"),
#'                        quantity = c(1, 3, 6))
#' seller <- data.frame(product  = c("Frisbee", "Lava lamp", "Sports Almanac",
#'                                   "Video camera", "Water bottles"),
#'                      price    = c(7, 20, 35, 75, 3))
#' shopping$unit_price <- vlookup(shopping$product, seller, "price")
#' shopping$total_price <- with(shopping, unit_price * quantity)
#' seller
#' shopping
#' sum(shopping$total_price)
#'
#' @export
#------------------------------------------------------------------------------#
vlookup <- function(x, table, col_return, ..., col_search = 1) {
    sapply(x, function(xx) {
        i_row <- match(xx, table[, col_search])
        table[i_row, col_return]
    })
}
