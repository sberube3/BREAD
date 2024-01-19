#' Filters measurements below the bead threshold.
#'
#' This function filters out measurements below the user specified
#' bead threshold and outputs a data frame containing only measurements
#' with bead counts above the user specified threshold.
#' This is done on an individual plate basis.
#'
#' @param bg_df the data frame output by rm_background function
#'
#'
#' @export
#'
#' @return \code{filter_low_beads} returns a data frame in the same
#' format as rm_background, but only with measurements above the
#' specified bead count threshold.
#'
#' @examples
#' R code examples of how to use your function
#'


filter_low_beads<- function(bg_df){

  filtered_bg_df<- bg_df|>
    filter(Low_Beads==0)

  return(filtered_bg_df)
}
