#' Pick colors
#'
#' Given a number `n`, the function returns a color vector
#' of length `n`.
#'
#' @param n The number of colors to return.
#' @param palName A character string specifying a color palette
#'  from R package [RColorBrewer]. Check
#'  [RColorBrewer::brewer.pal.info] for available palettes. If
#'  this parameter is `NA` (default), a palette will be chosen
#'  from c("Set2", "Set1", "Set3"), depending on `n`. When n is
#'  larger than the number of colors in the specified palette,
#'  [colorRampPalette()] is used to interpolate the colors.
#'
#' @return A character vector of colors.
pick_colors<-function(n, palName=NA) {
  paletteChoices<-c("Set1","Set2","Set3")
  if(is.na(palName)) {
    palName<-choose_palette(n, palettes=paletteChoices)
  }
  stopifnot(palName %in% get_available_palettes())
  palSize<-get_palette_size(palName)
  if(n > palSize) { # more than available, interpolate
    return(colorRampPalette(RColorBrewer::brewer.pal(palSize, palName))(n))
  } else {
    if(n >= 3) {
      return(RColorBrewer::brewer.pal(n, palName))
    } else {
      # minimal is 3 colors
      myPals<-RColorBrewer::brewer.pal(3, palName)
      if(n == 2) { return(myPals[1:2]) }
      return(myPals[1])
    }
  }

}

#' Choose color palette
#'
#' Given the number of colors needed, this function returns the name
#' of a color palette available in [RColorBrewer]. The returned
#' palette provides the most distinct colors as possible.
#
#' @param n The number of colors needed.
#' @param palettes The choices of the palettes, must be available in
#'  [RColorBrewer].
#' @param returnAll If TRUE, all the eligible palletes are returned.
#'  Otherwise only one is returned.
#' @return A character vector of color palette names.
choose_palette<-function(n, palettes=c("Dark2","Set1","Paired"), returnAll=F) {
  # for each size, choose one palette
  palInfo<-RColorBrewer::brewer.pal.info
  pals<-palInfo[row.names(palInfo) %in% palettes, ]
  #pals<-pals[pals$maxcolors >= n,]
  #stopifnot(nrow(pals) > 0)
  pals$dist<-pals$maxcolors - n
  if(any(pals$dist >= 0)) {
    pals<-pals[pals$dist >= 0,] # keep only pals having more colors than demand
  }
  minDist<-min(abs(pals$dist))
  pals<-pals[abs(pals$dist)==minDist,] # choose ones matching demand best
  if(nrow(pals) > 1) {
    warn("More than 1 palettes found")
  }
  if(returnAll) {
    return(row.names(pals))
  } else { # just return the first one
    idx<-which(palettes %in% row.names(pals))[1]
    return(palettes[idx])
  }
}

#' Get the number of colors in a palette
#' 
#' @keywords internal
get_palette_size<-function(palName) {
  return(RColorBrewer::brewer.pal.info[palName,"maxcolors"])
}

#' Get all the palette names
#'
#' @keywords internal
get_available_palettes<-function(palSpace="brewer") {
  return(rownames(RColorBrewer::brewer.pal.info))
}

#' Convert matrix to list of sets
#'
#' This function converts a matrix (or data.frame, data.table)
#' to a list of vectors, which can be used as input for
#' [VennDiagram::venn.diagram()]. The names of the list elements
#' are the column names of the input matrix.
#'
#' @param mat Input matrix, can also be [data.frame()] or [data.table-class]
#' @param selected A vector of numbers to select which columns are to
#'  be processed, default is `NULL`, i.e., all columns.
#' @param presence The values in each column indicating the presence of
#'  a column in a row. Default is `1`, so a value of `1` in that
#'  column will include that element in final set.
#' @return A list, with each element being a set
mat_to_set<-function(mat, selected=NULL, presence=1L, absence=0L) {
    if(is.null(selected)) {
        selected<-seq_len(ncol(mat))
    }
    stopifnot(all(selected <= ncol(mat)))
    # use row index as set ids
    setIds<-seq_len(nrow(mat))
    # get the list of sets
    isDT<-ifelse(data.table::is.data.table(mat), T, F)
    sets<-lapply(selected, function(i) {
                            if(isDT) {x<-mat[[i]]} else {x<-mat[,i]}
                            setIds[x %in% presence]
                            }
                )
    names(sets)<-colnames(mat)[selected]
    return(sets)
}

