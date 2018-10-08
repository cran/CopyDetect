print.CopyDetectCenter<- function(x, ...){

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
	cat("*************************************************************************","\n")
	cat("","\n")
		perc.cent <- matrix(nrow=length(x$output.centers),ncol=3)
		rownames(perc.cent) <- names(x$output.centers)
		colnames(perc.cent) <- c("W","GBT","M4")
		for(i in 1:length(x$output.centers)) {
			perc.cent[i,1] = sum(x$output.centers[[i]]$W.pvalue <.05)/nrow(x$output.centers[[i]])
			perc.cent[i,2] = sum(x$output.centers[[i]]$GBT.pvalue <.05)/nrow(x$output.centers[[i]])
			perc.cent[i,3] = sum(x$output.centers[[i]]$M4.pvalue <.05)/nrow(x$output.centers[[i]])
		}

		perc.cent2 <- matrix(nrow=length(x$output.centers),ncol=3)
		rownames(perc.cent2) <- names(x$output.centers)
		colnames(perc.cent2) <- c("W","GBT","M4")
		for(i in 1:length(x$output.centers)) {
			perc.cent2[i,1] = sum(x$output.centers[[i]]$W.pvalue <.01)/nrow(x$output.centers[[i]])
			perc.cent2[i,2] = sum(x$output.centers[[i]]$GBT.pvalue <.01)/nrow(x$output.centers[[i]])
			perc.cent2[i,3] = sum(x$output.centers[[i]]$M4.pvalue <.01)/nrow(x$output.centers[[i]])
		}

		perc.cent <- round(perc.cent*100,2)
		perc.cent2 <- round(perc.cent2*100,2)
		

	for(i in 1:length(x$output.centers)) {

		cat("CENTER ID ",names(x$output.centers)[i],":","\n")
		cat("","\n")
		cat("Number of Examinees: ",nrow(unique(x$output.centers[[i]][1])),"\n")
		cat("","\n")
		cat("Number of Pairs    : ",nrow(x$output.centers[[i]]),"\n")
		cat("","\n")
		cat("% of Identified Pairs at the Alpha Level of 0.05: ","\n")
		cat("    W-index   : ",paste0("%",perc.cent[i,1]),"\n")
		cat("    GBT-index : ",paste0("%",perc.cent[i,2]),"\n")
		cat("    M4-index  : ",paste0("%",perc.cent[i,3]),"\n")
		cat("","\n")
		cat("% of Identified Pairs at the Alpha Level of 0.01: ","\n")
		cat("    W-index   : ",paste0("%",perc.cent2[i,1]),"\n")
		cat("    GBT-index : ",paste0("%",perc.cent2[i,2]),"\n")
		cat("    M4-index  : ",paste0("%",perc.cent2[i,3]),"\n")
		cat("","\n")
		x$output.centers[[i]][,5:12] <- 	round(x$output.centers[[i]][,5:12] ,3)
		print(x$output.centers[[i]][,c(1,2,5:12)])
		cat("","\n")
		cat("**********************************************************************************************************************","\n")
		
	}


	

}



