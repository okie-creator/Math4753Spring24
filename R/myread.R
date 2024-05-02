#' @title myread
#'
#' @param csv a file from t
#' @param dird the file location
#'
#' @return a vector with the data from the csv file
#' @export
#'
#' @examples \dontrun{seed.df <- myread("SEEDLING.csv}
#'
myread = function(csv, dird){
  fl=paste(dird,csv,sep="")
  read.table(fl,header=TRUE,sep=",")
}
