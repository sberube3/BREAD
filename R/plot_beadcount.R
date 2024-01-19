#' Shows a histogram of beadcounts.
#'
#' This function takes in the output of rm_background and plots the
#' beadcounts in the form of a histogram across all samples and antigens
#' for a particular plate. It also shows the cutoff for low bead counts
#' as specified by the user. This is done on an individual plate basis.
#'
#' @param bg_df the data frame output by rm_background function
#'
#' @param bead_threshold a numeric value indicating the minimum number of
#' beads for a measurement to be considered in downstream analysis.
#'
#'
#' @export
#'
#' @return \code{plot_bead_count} returns histograms of beadcounts for a
#' particular plate.
#'
#' @examples
#' R code examples of how to use your function
#'

plot_beadcount<- function(bg_df, bead_threshold){

  beadcount_plot<- ggplot(bg_df, aes(x= BeadCount))+
    geom_histogram()+xlab("Beads Per Sample")+ggtitle(paste0("Histogram of Bead Counts by Antigen for Plate ",bg_df$Plate[1]))+
    facet_wrap(~Antigen)+ geom_vline(xintercept = bead_threshold, color="red")+theme_bw()

  return(beadcount_plot)
}
