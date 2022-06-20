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
plot_overlaps<-function(dat=NULL, counts=NULL, ...) {
  stopifnot(!(is.null(dat) && is.null(counts)))
  if(!is.null(dat)) {
    plot_list_overlap(dat, ...)
  } else {
    plot_count_overlap(counts, ...)
  }
}

plot_list_overlap<-function(
    dat, ...
) {
  if(!(is.list(dat) || is.data.frame(dat)) ) {
    dat<-as.list(dat)
  }
  if(length(dat) > 3) {
    stop("UpSet plot will be made")
  } else {
    venn_diagram(dat=dat, ...)
  }
}

plot_count_overlap<-function(counts) {
  stop("Not implemented yet: plot_count_overlap")
}

#' Plot venn diagram
#'
#' @param dat A list with each element being a set
#' @param color A length-2 character vector to provide
#'   the colors for min and max values, forming a color
#'   gradient between them. Default is `c("white","blue")`.
#'
venn_diagram<-function(dat=NULL, color=NULL, ...) {
  if(is.null(color)) {
    color<-c("white","blue")
  }
  stopifnot(length(color) == 2)
  ggVennDiagram::ggVennDiagram(dat, ...) +
    ggplot2::scale_fill_gradient(low=color[1],high=color[2])
}


