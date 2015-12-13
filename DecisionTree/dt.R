#PREPROCESSING
preprocess <- function(){

	args<-commandArgs(TRUE)
	fileName<-args[1]
	print(args[1])
	tdata<-read.table(fileName,header=TRUE)
        tdata<-data.matrix(tdata)
        print(is.matrix(tdata))
	tdata<-tdata[,-1]

#	print(tdata)
}
preprocess()
