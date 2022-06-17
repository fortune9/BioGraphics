#' Plot of overlaps
#'
#' @description
#' This function creates plots to show the overlap
#' of the input data.
#'
#' @details
#' In default, it will generate
#' a Venn Diagram when the number of input sets is
#' smaller than 4, and generate an UpSet plot when
#' the set size is bigger.
#'
#' @param dat Input data, can be a [data.frame()] or
#'   [list()], with each column or element being a
#'   set of items, and the common items are counted
#'   as overlap.
#' @param counts Counts of common and set-specific items.
#'   Can be a named vector or list, each element being
#'   the count of items in different categories (given
#'   by the names).
#'
#' @return A `plot` object
#'
plot_overlaps<-function(dat=NULL, counts=NULL) {
  stopifnot(!(is.null(dat) && is.null(counts)))
}
