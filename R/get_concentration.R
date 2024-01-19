#' Get estimate of Log dilution factor of individual samples.
#'
#' This function fits standard curves by estimating the
#' parameters of a five point logistic curve then using these
#' estimated parameters, takes the inverse of this logistic curve
#' to estimate the log concentration of samples. The user can specify
#' whether the input should be raw or background corrected MFI values.
#'
#' @param plate_df_norm a data frame that is the output of the
#' filter_low_beads function.
#'
#'
#' @param std_curve_values  a data frame with two columns,
#' the first column should be named "Sample" and contain the name of
#' the standard curve wells as they are listed in the "Sample" column
#' of the original csv file. The second column  should be named
#' "Dilution" is the dilution factor of the standard curve sample
#' as a numeric fraction e.g. 1/1000.
#'
#' @param input a string either "MFI" or "bgMFI" to designate whether
#' the standard curve should be fit with raw MFI values or background
#' corrected MFI values
#'
#'
#' @export
#'
#' @return \code{get_concentration} returns a data frame containing
#' all variables from the filter_low_beads function plus a new variable
#' with the estimated log concentration obtained by fitting the standard
#' curve.
#'
#' @examples
#' R code examples of how to use your function
#'
#'



get_concentration<- function(plate_df_norm, std_curve_values, input){
  antigen_list<- unique(plate_df_norm$Antigen)

  std_curve_df<- plate_df_norm|>
    filter(Sample_Type=="StdCurve")|>
    left_join(std_curve_values)|>
    group_by(Sample, Antigen)|>
    summarize(Dilution=Dilution[1], Log_Dilution=log(Dilution[1]),
              MFI_val= median(MFI),Log_MFI_val= log(median(MFI)),
              MFI_bg_val= median(MFI_BG),Log_MFI_bg_val= log(median(MFI_BG)),
              Plate=Plate[1])

  sample_log_conc_df<- data.frame()

  if(input=="MFI"){
    for(i in 1:length(antigen_list)){
      antigen_curve_df<- std_curve_df|>
        filter(Antigen==antigen_list[i])

      antigen_curve_df<- as.data.frame(antigen_curve_df)

      std_curveFit_antigen<- flexfit::fitStd( std= antigen_curve_df,
                                              xvar="Log_Dilution", yvar="Log_MFI_val", interactive = F)

      if(is.null(std_curveFit_antigen$par)==F){
        sample_norm_df<- plate_df_norm|>
          filter(Sample_Type%in%c("Ctrl", "TestSample"), Antigen==antigen_list[i])|>
          mutate(Log_Conc=FUNinv(log(MFI),std_curveFit_antigen$par))

        sample_log_conc_df<- rbind(sample_log_conc_df, sample_norm_df)
      }

    }

  }

  if(input=="bgMFI"){
    for(i in 1:length(antigen_list)){
      antigen_curve_df<- std_curve_df|>
        filter(Antigen==antigen_list[i])

      antigen_curve_df<- as.data.frame(antigen_curve_df)

      std_curveFit_antigen<- flexfit::fitStd( std= antigen_curve_df,
                                              xvar="Log_Dilution", yvar="Log_MFI_bg_val", interactive = F)

      if(is.null(std_curveFit_antigen$par)==F){
        sample_norm_df<- plate_df_norm|>
          filter(Sample_Type%in%c("Ctrl", "TestSample"), Antigen==antigen_list[i])|>
          mutate(Log_Conc_BG=FUNinv(log(MFI_BG),std_curveFit_antigen$par))

        sample_log_conc_df<- rbind(sample_log_conc_df, sample_norm_df)
      }

    }

  }





  return(sample_log_conc_df)

}

