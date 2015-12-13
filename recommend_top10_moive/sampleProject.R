######################################################
#Matric Calculation 
 metric_calc <-function(data) {
 usample1000=data[sample(1:nrow(data),300,),]
 count = table(usample1000[,1])
 uSample <- sample(count,5)
 cat("\nRandomly Selecting 5 users from data set:\n")
 uId50 = names(uSample)
 mCount = as.vector(uSample)
 uId50 = as.factor(uId50)
 cat("UserID:")
 print(uId50)
####################################################
 count_mv = length( unique( data[,2] ) )  
 usr_Mid<-c()
 for( i in uId50 ) {
    cat("\n==================================\n\n")
    cat("For User ID::",i,"\n\n")
    id_mv = data[which( data$Uid == i ),]
    mvSeen = id_mv[,2]
    cat("Length of seen Movie:")
    print(length(mvSeen))
    mvSeen.list =mvSeen
    full.list = seq( from=1, to=count_mv, by=1 )
    nsmv_id = setdiff( full.list, mvSeen.list )
    cat("Length of Unseen Movie:")
    leng_nsmvId = length(nsmv_id)
    print(leng_nsmvId)
    PrX = leng_nsmvId/count_mv
    PrbSet<-c()     
    nsmv_id=sample(nsmv_id,50,replace=FALSE)  
	for( j in nsmv_id) {
	      prob_list<-c()
              mvSeen =sample(mvSeen,50,replace=FALSE)     
	          for( k in mvSeen) {
                       l2mv = data[which(data$Mid == j & k),]
	               l2mv = length(l2mv$Uid)
	               PrAX = l2mv/count_mv
	               PrA = PrAX/PrX
	               if(PrA != 0) {
	                  prob_list<-c(prob_list,PrA)
                       }
                       Pr=prod(prob_list)
	               if(k ==mvSeen) {
	                  Pr<-Pr*PrX
		       }    
          
	         }
             PrbSet <- c(PrbSet,Pr)
        }
     usr_prb = data.frame(UnSeen_Movie = nsmv_id, Probability=PrbSet)
     top10=head(sort(usr_prb$UnSeen_Movie,decreasing=FALSE),n=10)
     cat("\nTop 10 Recommended Movie for user:",i,"\n\n")
     print(top10)
##########################################################
#precision calculation
  n = length(mvSeen)
  testN=0.25*n
  index =sample(1:n,testN)
  testMv =mvSeen[index]
  traindMV = mvSeen[-index]
  commonMv =length(which(testMv %in% top10))
  precision = (commonMv/length(top10)) * 100
  cat("\nPrecision:",precision,"\n")
  cat("\n")
  } 
}

########################################################
# preprocessing 
preprocessing <- function() {
 args <-commandArgs(TRUE)
 input <-args[1]
#pass Argument 2 as 1 if header exist in data file otherwise 0 
 if(args[2] == 1) {
   data = read.table(input,header = TRUE)
  } else {
   data = read.table(input,header = FALSE)
  }
#Pass Argument 3 as 1 if rownames exist in data file otherwise 0
 if(args[3] == 1) {
   data = data[,-1]
  }
# Removing Rating & TimeStamp Column from Data set.
 data = data[,c(-3,-4)]
 colnames(data)<-c("Uid","Mid")
# head(data)
 metric_calc(data)
}

#########################################################
preprocessing()
