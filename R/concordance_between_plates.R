#' Computes Lin's Concordance Correlation Coefficient on repeated
#' measurements across plates
#'
#' This function takes in MFI or transformed MFI data from various stages
#' of the cleaning and normalization process and computes Lin's
#' Concordance Correlation Coefficient on repeated measurements across pairs
#' of plates.
#'
#' @param data_df a data frame is the output of either filter_low_beads,
#' get_concentration, or normalize_between_plates where the outputs for
#' all plates in the study are bound together (rbind) into one large data
#' frame.
#'

#' @param num_plates numeric value, indicating the number of plates
#' to be compared.
#'
#' @param stage  a string describing the stage of analysis being
#' analysed either "Background", "Std_Curve" or "Normalized".
#'
#'
#' @export
#'
#' @return \code{concordance_between_plates} returns a data frame
#' showing the concordance correlation coefficient (with 95% CI)
#' across all pairs of plates.
#'
#' @examples
#' R code examples of how to use your function
#'



concordance_between_plates<- function(data_df, num_plates, stage){
  plate_pairings<- combn(1:num_plates, 2)

  concordance_df<- data.frame(anaylsis_stage= character(),
                              Plate_pair=character(),
                              concordance_cor= numeric(),
                              upper95= numeric(),
                              lower95=numeric())

  for(i in 1:ncol(plate_pairings)){
    plate1<- data_df|>
      filter(Plate==plate_pairings[1,i]& Sample_Type%in%c("Ctrl"))
    plate2<- data_df|>
      filter(Plate==plate_pairings[2,i]& Sample_Type%in%c("Ctrl"))

    comparison_df<- left_join(plate1,plate2, by=c("Sample", "Antigen", "Location"))


    if(stage=="Background"){
      concordance<- DescTools::CCC(comparison_df$MFI_BG.x,comparison_df$MFI_BG.y, na.rm=T)
    }
    if(stage=="Std_Curve"){
      concordance<- DescTools::CCC(comparison_df$Log_Conc_BG.x,comparison_df$Log_Conc_BG.y, na.rm=T)
    }
    if(stage=="Normalized"){
      concordance<- DescTools::CCC(comparison_df$MFI_norm.x,comparison_df$MFI_norm.y, na.rm=T)
    }

    conc_df<- data.frame(analysis_stage=stage,
                         Plate_pair= paste0(plate_pairings[1,i], " vs ", plate_pairings[2,i]),
                         concordance_cor= concordance$rho.c$est,
                         upper95= concordance$rho.c$upr.ci,
                         lower95= concordance$rho.c$lwr.ci)

    concordance_df<- rbind(conc_df, concordance_df)

  }
  return(concordance_df)
}
