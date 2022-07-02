#' Heatmap
#'
#' Create a heatmap based on the [pheatmap::pheatmap()] if the values
#' are numeric, or based on [ggplot2::geom_raster()] if the values are
#' categorical (i.e., character).
#'
#' @param mat A matrix, containing the values to be plotted.
plot_heatmap<-function(mat, ...) {
    if(mode(mat) == "numeric") {
        plot_continuous_heatmap(mat, ...)
    } else { # categorical
        plot_categorical_heatmap(mat, ...)
    }
}

plot_continuous_heatmap<-function(mat,
                                  ...) {
    pheatmap::pheatmap(mat, 
         color=colorPal, border_color=NA,
	     scale="none", legend=T,
		 cluster_rows=T, cluster_cols=T,
		 clustering_distance_rows = clusterMetric,
		 clustering_distance_cols = clusterMetric,
		 annotation_row = rowAnno, # row annotation
		 annotation_col = colAnno, # column annotation
		 annotation_colors=annoColors,
		 annotation_legend=T,
		 annotation_names_row=F,
		 annotation_names_col=F,
		 show_rownames=F, show_colnames=T,
		 fontsize=fontsize,
		 angle_col=270,
		 width=width, height=height,
		 filename=outfile,
         main=title,
		 na_col="black"
		 )
    rm(dat, rowAnno)
    # return the figname, if generated
    if(!is.na(outfile)) {
        return(outfile)
    }
}

#' Convert a matrix to long format
#' 
#' This is achieved via [data.table].
#' 
#' @param rowIds The row ids.
#'  * NULL: row ids are constructed in the form of `r<num>` where
#'   `num` is the order of rows.
#'  * A length-1 vector of character or integer: this specifies the column
#'    in the matrix which is used as row ids.
#'  * A character vector with length = `nrow(mat)`: this vector
#'    provides row ids directly.
#'
#' @keywords internal
melt_mat<-function(mat, rowIds=NULL, rowIdAsFactor=TRUE) {
    # assume that the input matrix has no column with name 'rn'
    stopifnot( (! 'rn' %in% colnames(mat)) || 
              (length(rowIds)==1 && rowIds == "rn") )
    dt<-data.table::data.table(mat, keep.rownames=T)
    # use user-provided rowids
    if(!is.null(rowIds)) {
        # a vector
        if(length(rowIds) == nrow(dt)) {
            dt[, rn:=rowIds]
        } else if(length(rowIds) ==  1) { # a column
            if(is.numeric(rowIds)) {
                rowIds<-colnames(dt)[rowIds]
            }
            stopifnot(rowIds %in% colnames(dt))
            data.table::setnames(dt, rowIds, "rn")
        } else {
            stop("`rowIds`'s length should be 1 or `nrow(mat)`")
        }
    } else { # construct rowids if not available
        if(! "rn" %in% colnames(dt)) {
            dt[, rn:=paste0("r",seq_len(nrow(dt)) )]
        }
    }
    # convert the column 'rn' to a factor so that they will show in
    # the original order
    if(rowIdAsFactor) {
        dt[, rn:=factor(rn, levels=rn)]
    }
    # melt data now
    dt<-data.table::melt(dt, id.vars="rn", variable.name="cn", value.name="value")
    return(dt)
}

#' Plot heatmap using categorical data
#' 
#' This function creates a heatmap from a matrix whose values are
#' categorical. It is based on [ggplot2::geom_tile()].
#'
#' @param mat The input matrix.
#' @param color Filling colors of the heatmap:
#'  * NULL: colors are picked using the function [pick_colors()]
#'  * named vector: element names are the values of the matrix and
#'    element values are colors, such as c("0"="red","1"="green").
#'  * a vector of colors: similarly to using named vector, but colors
#'    are assigned to matrix values in order.
#'  * a length-1 vector: this can be a palette name from
#'    [RColorBrewer], see [RColorBrewer::brewer.pal.info] for values.
#'    Then colors will be picked from the palette.
#' @inheritParams melt_mat
plot_categorical_heatmap<-function(mat, 
    color=NULL, borderColor="white", 
    borderLwd = 0.3,
    title=NA, xlab=NA, ylab=NA, 
    legendPos="top",
    rowIds=NULL,
    ...) {
    matL<-melt_mat(mat, rowIds=rowIds)
    uniqueVals<-unique(matL$value)
    # set colors for value
    if(is.null(color)) {
        color<-pick_colors(length(uniqueVals))
        names(color)<-uniqueVals
    } else {
        if(length(color) > 1) {
            stopifnot(length(uniqueVals) <= length(color))
            if(is.null(names(color))) { # not named
                names(color)<-uniqueVals
            }
        } else {
            # palette
            color<-pick_colors(length(uniqueVals), color)
            names(color)<-uniqueVals
        }
    }
    p<-ggplot(matL, aes(x=cn,y=rn,fill=value)) + 
        geom_tile(color=borderColor, lwd=borderLwd, linetype=1) +
        theme(panel.background = element_blank(), legend.position=legendPos) +
        scale_fill_manual(values=color)
    if(is.null(xlab)) { # no xlab
        p<-p+theme(axis.title.x = element_blank())
    } else if(!is.na(xlab)) {
        p<-p+xlab(xlab)
    }
    if(is.null(ylab)) { # no ylab
        p<-p+theme(axis.title.y = element_blank())
    } else if(!is.na(ylab)) {
        p<-p+ylab(ylab)
    }
    if(is.null(title)) { # no title
        p<-p+theme(plot.title = element_blank())
    } else if(!is.na(title)) {
        p<-p+ggtitle(title)
    }
    return(p)
}

