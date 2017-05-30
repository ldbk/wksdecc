#' Build a data.frame based on a format
#' 
#' Read sheet of an xlsx file containing a format definition and generate a data
#' frame
#'
#' @param name the sheet name
#' @param nbrow the number of row of the dataframe
#' @return a data frame 
#' @author Laurent Dubroca
#' @name createdf 
#' 
createdf<-function(name="Design",nbrow=100){
	pathrdbformat<-system.file("extdata","RDB_CS_Data_Model_v1.3.xlsx",package="WKSDECC")
	aa<-readxl::read_excel(pathrdbformat,sheet=name)
	aa<-aa[!is.na(aa$`R-Object name`),]
	bb<-((matrix(nrow=nbrow,ncol=nrow(aa))))
	bb<-data.frame(t(apply(bb,1,as.character)))
	names(bb)<-as.vector(t(aa$`R-Object name`))
	return(bb)
}
