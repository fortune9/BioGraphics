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

# return a palette name for a given number
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

get_palette_size<-function(palName) {
  return(RColorBrewer::brewer.pal.info[palName,"maxcolors"])
}

get_available_palettes<-function(palSpace="brewer") {
  return(rownames(RColorBrewer::brewer.pal.info))
}

