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
#' @export
createdf<-function(name="Design",nbrow=100){
	pathrdbformat<-system.file("extdata","RDB_CS_Data_Model_v1.3.xlsx",package="WKSDECC")
	aa<-readxl::read_excel(pathrdbformat,sheet=name)
	aa<-aa[!is.na(aa$`R-Object name`),]
	bb<-((matrix(nrow=nbrow,ncol=nrow(aa))))
	bb<-data.frame(t(apply(bb,1,as.character)))
	names(bb)<-as.vector(t(aa$`R-Object name`))
	return(bb)
}
#' Pick values from a data frame to another if the names match
#' 
#'
#' @param tab1 the dataframe to which the values are copied
#' @param tab2 the dataframe from which the values are coming from
#' @return a data frame 
#' @author Laurent Dubroca
#' @name pickvalue 
#' @export
pickvalue<-function(tab1,tab2){
	for(i in 1:ncol(tab1)){
			if(any(names(tab1)[i]%in%names(tab2))){
			#print(names(tab1)[i])
			tab1[,i]<-tab2[,names(tab2)==names(tab1)[i]]
			}
	}
	return(tab1)
}

