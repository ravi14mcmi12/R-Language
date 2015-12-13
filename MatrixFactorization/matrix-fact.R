calc <- function(R, test){ 
	R = as.matrix(R)
		err_sum=0
		 err_mae = 0
		 rmse = c()
		 mae = c()
	         num =0
		cat("The R is:","\n") 
		print(R)
		m = nrow(R) 
		n = ncol(R) 
		k = min(m,n)
		U <-matrix(0,m,k)
		V <-matrix(0,n,k)
		U =apply(U, c(1,2), function(x) runif(1,0,1))
		cat("U Matrix:","\n")
		print(U)
		V=apply(V, c(1,2), function(x) runif(1,0,1))
		cat("V Matrix:","\n")
		print(V)
		X = U%*%t(V) 
		cat("The product is:","\n")
		print(X) 
		error = R-X
		cat("The error is:","\n")
		J_Err = c()
		R_Err = c()
		M_Err = c()
		E =  error(U, V, R)
		print(E)
		alpha = 0.0002
		beta = 0.02
		iter = c(1:10000)
		print("Code Running...")
		for ( w in iter) {
			U = U - alpha * ( -2 * ( R - U%*%t(V) ) %*% V + beta * U )
			V = V - alpha * ( -2 * t(( R - U %*%t(V)) ) %*% U + beta*V )
			E =  error(U, V, R)
			J_Err = c(J_Err, E$J)
			R_Err = c(R_Err, E$RMSE)
			M_Err = c(M_Err, E$MAE)
		} 
	cat ("U\n")
		print(U)
		cat ("V\n")
		print(V)
		cat("R\n")
		print(R)
		cat("UVt\n")
		X=(U%*%t(V))
	        print(X)
	       	print("Completed.")
		cat("Training Error\n")
		print(E)
		print("Testing on test data")
		
		jpeg('rplot.jpg')
		plot(iter, J_Err, type = "l", main = "J-Function", xlab = "iteration", ylab = "J")
		jpeg('rmse.jpg')
		plot(iter, R_Err, type = "l", main = "RMSE", xlab = "iteration", ylab = "rmse")
		jpeg('mae.jpg')
		plot(iter, M_Err, type = "l", main = "MAE", xlab = "iteration", ylab = "mae")
		dev.off()
		index = which(test != 0)
		cat("\nTest Data:","\n")
		print(test)
		tResult = test - (U%*%t(V))
		tResult = as.matrix(tResult)
		tRMSE = sqrt(mean(tResult[index]^2))
		tMAE = mean(abs(tResult[index]))
		cat("In Testing Data Value of RMSE:","\n")
		print(tRMSE)
	 	cat("In Testing Data Value  MAE:","\n")
		print(tMAE)

	
	
	
}

###############
# error function 
error <- function(U, V, R) {
	m = nrow(R)
		n = ncol(R)
		k = min(m,n)
		temp_sum = 0
		beta = 0.02
		E = R - U%*%t(V)
		MAE = apply(E, c(1,2), function (x) abs(x))
		E = apply( E, c(1,2), function(x) {x^2} )
		E_sum = sum(E)
		rmse = E_sum/prod(dim(E))
		mae = sum(MAE)/prod(dim(MAE))
		for( i in 1:k ) {
			temp_sum = (norm(U,type = "F")^2 + norm ( V, type = "F" )^2 ) + temp_sum }
		temp_sum = {beta/2} * temp_sum
		E_sum = E_sum + temp_sum
		ERR = list(J = E_sum, RMSE = rmse, MAE = mae)
		return(ERR)
}
################################
#########Preprocessing##########
#################################
preprocess <- function(){
	args <- commandArgs(TRUE)
	
	print(length(args))	
	if(length(args)!=1){
			cat("\n Command Line Arguement Error \n")
				cat("\nRscript <prog_name.R> <InputDataset> \n\n ")
				quit()
		}
	input<-args[1]

#Pass Arguement 2 as 1 if header exists else as 0 if header doen't exists
	dataset = read.table(input,header = FALSE)
	n = length(dataset)
	test_n = .2*n
	test_index = sample(1:n, test_n)
	training = dataset
	training[test_index] = 0
	test = dataset
	test[-test_index] = 0
	calc(training, test) 
}

preprocess() 

