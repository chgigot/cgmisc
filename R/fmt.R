#------------------------------------------------------------------------------#
#' Format real numbers for pretty printing.
#'
#' Closure to format numeric numbers for pretty printing.
#'
#' @param digits How many digits to be kept?
#' @param lh_equ With leaf-hand side equal (or unequal) sign?
#'
#' @export
#------------------------------------------------------------------------------#
fmt <- function(digits = 3) {
    function(x, lh_equal = TRUE) {
        stopifnot(is.numeric(x))
        if ((x != 0) & (round(x, digits = digits) == 0)) {
            paste0("< ", 10^(-digits))
        } else {
            x <- format(round(x, digits = digits), nsmall = digits)
            if (lh_equal) {
                paste0("= ", x)
            } else {
                x
            }
        }
    }
}

#------------------------------------------------------------------------------#
#' @export
#------------------------------------------------------------------------------#
fmt0 <- fmt(0)

#------------------------------------------------------------------------------#
#' @export
#------------------------------------------------------------------------------#
fmt1 <- fmt(1)

#------------------------------------------------------------------------------#
#' @export
#------------------------------------------------------------------------------#
fmt2 <- fmt(2)

#------------------------------------------------------------------------------#
#' @export
#------------------------------------------------------------------------------#
fmt3 <- fmt(3)


