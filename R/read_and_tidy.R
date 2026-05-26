#' Read and Tidy
#'
#' Reads in raw luminex data in csv format and outputs a tidy data frame.
#'@param file_name A character string with the name of the csv file containing the raw Luminex data.
#' @param plate_number A character or numeric value with a unique plate identifier
#' @param num_wells The number of active wells on the plate -this includes all control samples.
#' @param antigen_names A vector of character strings with the names of each antigen in the assay exactly as they appear in the csv file.
#' @param control_samples A vector of character strings that contain the sample names of any control samples excluding those that measure background exactly as the appear in the csv file.
#' @param background_samples A vector of character strings that contain the sample names of any control samples that measure background exactly as they appear in the csv file.
#' @param standard_curve_values A data frame with four columns: 1- 'Sample' a vector of character strings with the sample names of any serial dilutions exactly as they appear in the csv file.
#' 2- 'Dilution': a numeric vector with the exact dilution factor of each serial dilution sample. 3- 'Location' the plate location of each serial dilution sample exactly as they appear in the csv file.
#' 4 - 'Replicate': a numeric vector with the replicate number of each serial dilution. If the serial dilution appears only once on the plate this is just a vector of 1's.
#' @param bead_threshold A numeric value with the minimum number of beads required for each antigen in each sample - 30 is a common value.
#'
#' @returns A tidy data frame with where each row represents the measurements assocated with one antigen in one sample. The data frame has 8 columns:
#' 1-"Location" the plate location of the sample.
#' 2-"Sample" the sample name.
#' 3-"Antigen" the name of the antigen.
#' 4-"MFI" the mean fluorescence intensity corresponding to
#'      the antigen in that sample.
#' 5-"BeadCount" the number of beads identified by
#'      the reader for each antigen in each sample.
#' 6- "Plate" the unique identifier for each plate.
#' 7- "Sample_Type" one of TestSample designating study samples,
#'      StdCurve designating serial dilutions,
#'      "BG" designating samples that measure background signal,
#'       or "Ctrl" designating control samples.
#'
#' @examples
#' csv_path<- system.file("extdata", "2023-08-31_Antoomwe_Plate.csv", package="BREAD")
#' read_and_tidy(csv_path, plate_number = "1", num_wells=384,
#' antigen_names=c("EBA175.RIII.V","Rh5.1","EBA181.RIII.V",
#' "gSG6","MSP2.Dd2","GST","Gexp","GLURP.R2","Tet.tox",
#' "PfAMA1","MSP2.CH150","EBA140.RIII.V","Rh4.2","PfMSP1.19",
#' "HSP40.Ag1","SBP1","PfSEA.1",
#' "Hyp2","Etramp5.Ag1","Etramp4.Ag2","CSP","Rh2.2030"),
#' control_samples = c("Control1","Control2","Control3","Control4",
#' "Control5","Control6","Control7"),
#' background_samples = c("Background0", "empty", "Empty"),
#' standard_curve_values = data.frame(Sample=rep(c("Standard1",
#' "Standard2","Standard3","Standard4","Standard5",
#'"Standard6","Standard7","Standard8","Standard9","Standard10"),times=2),
#'Dilution= rep(c(1/54, 1/162, 1/486, 1/1458, 1/4374, 1/13122,
#'           1/39366, 1/118098,1/354294,1/1062882), times=2),
#'Location= c("1(1,A1)",    "3(1,C1)",    "5(1,E1)",    "7(1,G1)",
#'            "9(1,I1)",    "11(1,K1)",   "13(1,M1)",   "15(1,O1)",
#'            "33(1,A3)",   "35(1,C3)",   "338(1,B22)", "340(1,D22)",
#'            "342(1,F22)", "344(1,H22)", "346(1,J22)", "348(1,L22)",
#'            "350(1,N22)", "352(1,P22)", "370(1,B24)", "372(1,D24)"),
#'Replicate= rep(c(1,2), each=10)),
#'bead_threshold = 30)
#'
#' @export
read_and_tidy<- function(file_name, plate_number, num_wells, antigen_names, control_samples, background_samples, standard_curve_values,
                         bead_threshold){

  data<- utils::read.csv(file_name)

  MFI_row<-which(data=="Median", arr.ind=TRUE)[1]

  CountVal_matrix<-which(data=="Count", arr.ind=TRUE)

  BeadCount_row<-CountVal_matrix[which(CountVal_matrix[,1]>MFI_row),1]

  MFI_matrix<- data[(MFI_row+2):(MFI_row+1+num_wells),1:(length(antigen_names)+3)]
  colnames(MFI_matrix)<- data[MFI_row+1,1:(length(antigen_names)+3)]

  MFI_long<- MFI_matrix|>
    dplyr::select(-c("Total Events"))|>
    tidyr::gather(key="Antigen", value="MFI", c(antigen_names))

  BeadCount_matrix<- data[(BeadCount_row+2):(BeadCount_row+1+num_wells),1:(length(antigen_names)+3)]
  colnames(BeadCount_matrix)<- data[BeadCount_row+1,1:(length(antigen_names)+3)]

  BeadCount_long<- BeadCount_matrix|>
    dplyr::select(-c("Total Events"))|>
    tidyr::gather(key="Antigen", value="BeadCount", c(antigen_names))

  output_df<- MFI_long|>
    dplyr::left_join(BeadCount_long)|>
    dplyr::mutate(Plate= plate_number,
           Sample_Type= ifelse(Sample%in% background_samples, "BG",
                               ifelse(Sample %in% control_samples, "Ctrl",
                                      ifelse(Sample%in% standard_curve_values$Sample, "StdCurve", "TestSample"))))


  output_df$MFI<- as.numeric(output_df$MFI)
  output_df$BeadCount<- as.numeric(output_df$BeadCount)

  output_df<- output_df|>
    dplyr::mutate(Low_Beads= ifelse(BeadCount<bead_threshold,1,0))
  return(output_df)
}
