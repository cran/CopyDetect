print.CopyDetectMany<- function(x, ...){

	cat("************************************************************************","\n")
	cat("CopyDetect - An R Package to Compute Response Similarity Indices for Multiple-Choice Tests","\n")
	cat("","\n")
	cat("Version 1.3, released on October 2018","\n")
	cat("","\n")
	cat("Cengiz Zopluoglu","\n")
	cat("","\n")
	cat("Assistant Professor","\n")
	cat("University of Miami - Department of Educational and Psychological Studies","\n")
	cat("Research, Measurement, and Evaluation Program","\n")
	cat("","\n")
	cat("c.zopluoglu@miami.edu","\n")
	cat("*************************************************************************","\n")
	cat("","\n")
	cat("Processing Date: ",date(),"\n")
	cat("","\n")
	cat("                     Probability Values Obtained from Various Response Similarity Indices       \n")
	cat("","\n")
	x$output.manypairs[,5:12] <- 	round(x$output.manypairs[,5:12],3)
	print(x$output.manypairs[,c(1,2,5:12)])
}



