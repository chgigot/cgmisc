#------------------------------------------------------------------------------#
#' Manipulate file paths
#'
#' Like \code{basename}, but removes also the file extension (i.e. everything
#' after the first dot in a file name).
#'
#' @inheritParams base::basename
#'
#' @examples
#' verybasename(c("/test1/my_file.txt.bck", "/test2/another_file.txt"))
#'
#' @export
#------------------------------------------------------------------------------#
verybasename <- function(path) {
    vapply(strsplit(basename(path), "\\."), function(y) y[[1]], character(1))
}
