#------------------------------------------------------------------------------#
#' Coerce a list of rows to a data frame
#'
#' Function to coerce a list (of rows) to a data frame.
#'
#' @param x A list. Each element corresponds to a row of the upcoming data frame.
#' @param col_names Optional. If not provided and if the first list element is
#' a named vector, this vector is used to name the columns of the data frame.
#'
#' @examples
#' my_list <- list(c(23, 21, 41), c(3, 55), 8, c(39, 31, 14))
#' list2df(my_list)
#'
#' # Name the columns:
#' # - Option #1:
#' list2df(my_list, col_names = letters[1:3])
#' # - Option #2:
#' my_list <- list(c(a = 23, b = 21, c = 41), c(d = 3, e = 55), c(f = 8),
#'                 c(g = 39, h = 31, i = 14))
#' list2df(my_list)
#'
#' # A more tricky case:
#' my_list <- list(r1 = c(a = 23, 21), r2 = c(c = 41, d = 3, e = 55),
#'                 r3 = c(f = 8), r4 = c(g = 39, h = 31, i = 14))
#' list2df(my_list)
#'
#' @export
#------------------------------------------------------------------------------#
list2df <- function(x, col_names, ...) {
    seq_max <- seq_len(max(lengths(x)))
    res     <- as.data.frame.matrix(t(sapply(x, "[", i = seq_max)), ...)
    if (!missing(col_names)) {
        setNames(res, col_names)
    } else if (!is.null(col_names <- names(x[[1]]))) {
        col_names <- c(col_names, rep(NA, ncol(res) - length(col_names)))
        id_empty  <- sort(c(which(is.na(col_names)),       # if name is NA
                            which(nchar(col_names) == 0))) # if name is ""
        col_names[id_empty] <- paste0("V", id_empty)
        setNames(res, col_names)
    } else {
        res
    }
}

