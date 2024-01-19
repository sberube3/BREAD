#'Performs simple between-plate normalization.
#'
#' Using measurements that are repeated between plates (control samples).
#' This function performs a linear regression with log estimate concentration,
#' the output of the get_concentration function, as the outcome and
#' plate, antigen, and sample name as regressors. The estimated plate effects
#' are then subtracted from the log concentrations of all samples
#' on respective plates to produce a normalized log concentration.
#'
#' @param all_plates_df the result of binding together (rbind)
#' all plate oututs from the get_concentration function into one
#' data frame.
#'
#'
#' @export
#'
#' @return \code{normalize_between_plates} returns a data frame containing
#' all the same outputs as the get_concentration function as well
#' as a new output of normalized log concentration, information
#' from all plates is combined into one large data frame.
#'
#' @examples
#' R code examples of how to use your function
#'



normalize_between_plates<- function(all_plates_df){

  fitting_matrix<- all_plates_df|>
    filter(Sample_Type%in%c("Ctrl"))

  model_1<- lm(Log_Conc_BG~Plate+Antigen+Sample, data= fitting_matrix)

  coefs<- model_1$coefficients[c(startsWith(names(model_1$coefficients), "Plate"))]

  df_plate_effects<- data.frame(Plate= c(1, substring(names(coefs), 6)),
                                Coef= c(0,coefs))

  all_plates_norm_df<- all_plates_df|>
    left_join(df_plate_effects)|>
    mutate(Conc_norm= Log_Conc_BG-Coef)
  return(all_plates_norm_df)
}
