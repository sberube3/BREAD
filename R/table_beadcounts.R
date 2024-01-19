#' Returns a data frame summarizing low bead counts.
#'
#' This function takes in the output of rm_background and returns
#' a data frame summarizing how low bead counts are distributed
#' across sample types and antigens. This is done on an individual plate
#' basis.
#'
#' @param bg_df the data frame output by rm_background function
#'
#'
#' @export
#'
#' @return \code{table_beadcounts} returns a table of low beadcounts
#' for a particular plate.
#'
#' @examples
#' R code examples of how to use your function
#'

table_beadcounts<- function(bg_df){

  beadcount_table<- bg_df|>
    filter(Low_Beads==1)|>
    group_by(Sample_Type, Antigen)|>
    summarize(Samples_With_Low_Beads= n(), Plate= Plate[1])

  return(data.frame(beadcount_table))
}
