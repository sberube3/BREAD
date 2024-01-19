#' Plots standard curves.
#'
#' This function plots standard curves for individual plates on the
#' log-log scale (log dilution factor on the x-axis, and log
#' MFI on the y-axis).Users can specify whether they would like to
#' visualize the background corrected MFI or the raw MFI.
#'
#' @param plate_bg_df a dataframe that is the output of filter_low_beads.
#'
#' @param standard_curve_values  a data frame with two columns,
#' the first column should be named "Sample" and contain the name of
#' the standard curve wells as they are listed in the "Sample" column
#' of the original csv file. The second column  should be named
#' "Dilution" is the dilution factor of the standard curve sample
#' as a numeric fraction e.g. 1/1000.
#'
#' @param input a string either "bgMFI" or "MFI" indicating whether
#' to visualize background corrected MFI or raw MFI on the y-axis.
#'
#'
#'
#' @export
#'
#' @return \code{plot_std_curves} returns plots of standard curves.
#'
#' @examples
#' R code examples of how to use your function
#'

plot_std_curves<- function(plate_bg_df, std_curve_values, input){


  if(input== "bgMFI"){
    std_curve_df<- plate_bg_df|>
      filter(Sample_Type=="StdCurve")|>
      left_join(std_curve_values)|>
      group_by(Sample, Antigen)|>
      summarize(Dilution=Dilution[1], MFI_BG_val= median(MFI_BG), Plate=Plate[1])


    std_curve_plot<- ggplot(data= std_curve_df, aes(x=log(Dilution), y= log(MFI_BG_val)))+
      geom_point()+geom_line()+facet_wrap(~Antigen)+xlab("Log Dilution")+ylab("Log Background Corrected MFI")+
      ggtitle(paste0("Standard Curves for Plate ", std_curve_df$Plate))+theme_bw()
  }

  if(input=="MFI"){
    std_curve_df<- plate_norm_df|>
      filter(Sample_Type=="StdCurve")|>
      left_join(std_curve_values)|>
      group_by(Sample, Antigen)|>
      summarize(Dilution=Dilution[1], MFI_val= median(MFI), Plate=Plate[1])


    std_curve_plot<- ggplot(data= std_curve_df, aes(x=log(Dilution), y= log(MFI_val)))+
      geom_point()+geom_line()+facet_wrap(~Antigen)+xlab("Log Dilution")+ylab("Log Raw MFI")+
      ggtitle(paste0("Standard Curves for Plate ", std_curve_df$Plate))+theme_bw()

  }
  return(std_curve_plot)

}
