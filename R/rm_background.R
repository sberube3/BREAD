#' Performing background correction on raw measurements.
#'
#' This function takes the ouput of the read_and_tidy function
#' and performs background correction on all measurements either through
#' subtraction, which is median fluoresecnce intensity (MFI) minus
#' the median of MFI among background wells on the plate,
#' or division which is MFI divided by the median of MFI among
#' background wells on the plate. This is done on an individual plate basis.
#'
#'
#' @param clean_df data frame that is the output of read_and_tidy function
#'
#' @param method a string indicates whether background correction
#' should be performed by division or subtraction
#'
#' @export
#'
#' @return \code{rm_background} returns a data frame containing
#' an additional column in the original data frame from read_and_tidy
#' output called MFI_BG, which is the background corrected MFI.
#'

rm_background<- function(clean_df, method){

  bg_df<- clean_df%>%
    filter(Sample_Type=="BG")|>
    group_by(Antigen)|>
    summarize(Median_BG= median(MFI))


  if(method=="division"){
    bg_rm_df<- clean_df|>
      left_join(bg_df)|>
      mutate(MFI_BG= MFI/Median_BG)
  }

  if(method=="subtraction"){
    bg_rm_df<- clean_df|>
      left_join(bg_df)|>
      mutate(MFI_BG= MFI-Median_BG)
  }

  return(bg_rm_df)
}
