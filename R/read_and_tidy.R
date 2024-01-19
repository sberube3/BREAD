#' Read in of output from Intelliflex or MagPix machines as .csv files
#' and basic tidying steps.
#'
#' This function takes in a raw CSV file from the MBA assay readout
#' as well as annotation information about samples including
#' which samples are controls, which are part of the standard curve, and
#' what dilution factors these correspond to, and which are background wells.
#' The function also requires the names of the antigens in the assay.
#' The function then converts the CSV file to long format with standardized
#' format that can be used as an input to other functions in the package.
#' This is done on an individual plate basis.
#'
#' @param file_name a string that indicates the name of the
#' csv file with raw Intelliflex or MagPix readout for a particular plate
#' in the study. The readout must have at minimum median or mean fulorescence
#' intensity, and bead counts  it should also have the location of each sample
#'  in the physical plate as a column labeled "Location",
#' and the name of the sample in each well including controls
#' as a column called "Sample",
#' then it should also have each antigen as separate columns
#' as well as a column called "Total Events".
#' Each row should correspond to a unique well on the original plate.
#'
#' @param plate_number a string
#' that labels each plate within a study e.g. Plate 1, Plate 2 etc.
#'
#' @param num_wells numeric value, indicating the number of wells in the
#' .csv file. Note that because of some readout issues this is not
#' always equal t the number of wells in the plate.
#'
#' @param antigen_names  a character vector of the antigen names as
#' they appear in the .csv file.
#'
#' @param control_samples  a character vector of names of control
#' samples as they appear in the "Sample" column of the .csv file. Note
#' these exclude standard curve samples and background wells.
#'
#' @param background_samples a character vector of names of
#' background wells as they appear in the "Sample" column of the .csv file.
#' Note that if background wells have different names throughout
#' the plate all variations should be included in this vector.
#'
#' @param standard_curve_values  a data frame with two columns,
#' the first column should be named "Sample" and contain the name of
#' the standard curve wells as they are listed in the "Sample" column
#' of the original csv file. The second column  should be named
#' "Dilution" is the dilution factor of the standard curve sample
#' as a numeric fraction e.g. 1/1000.
#'
#' @param bead_threshold a numeric value indicating the minimum number of
#' beads for a measurement to be considered in downstream analysis.
#'
#'
#' @export
#'
#' @return \code{read_and_tidy} returns a data frame containing
#' raw data from the MBA assay readout in a long format.
#'
#' @examples
#' R code examples of how to use your function
#'

read_and_tidy<- function(file_name, plate_number, num_wells, antigen_names, control_samples, background_samples, standard_curve_values,
                         bead_threshold){

  data<- read.csv(file_name)

  MFI_row<-which(data=="Median", arr.ind=TRUE)[1]

  CountVal_matrix<-which(data=="Count", arr.ind=TRUE)

  BeadCount_row<-CountVal_matrix[which(CountVal_matrix[,1]>MFI_row),1]

  MFI_matrix<- data[(MFI_row+2):(MFI_row+1+num_wells),]
  colnames(MFI_matrix)<- data[MFI_row+1,]

  MFI_long<- MFI_matrix|>
    select(-c("Total Events"))|>
    gather(key="Antigen", value="MFI", c(antigen_names))

  BeadCount_matrix<- data[(BeadCount_row+2):(BeadCount_row+1+num_wells),]
  colnames(BeadCount_matrix)<- data[BeadCount_row+1,]

  BeadCount_long<- BeadCount_matrix|>
    select(-c("Total Events"))|>
    gather(key="Antigen", value="BeadCount", c(antigen_names))

  output_df<- MFI_long|>
    left_join(BeadCount_long)|>
    mutate(Plate= plate_number,
           Sample_Type= ifelse(Sample%in% background_samples, "BG",
                               ifelse(Sample %in% control_samples, "Ctrl",
                                      ifelse(Sample%in% standard_curve_values$Sample, "StdCurve", "TestSample"))))


  output_df$MFI<- as.numeric(output_df$MFI)
  output_df$BeadCount<- as.numeric(output_df$BeadCount)

  output_df<- output_df|>
    mutate(Low_Beads= ifelse(BeadCount<bead_threshold,1,0))
  return(output_df)
}
