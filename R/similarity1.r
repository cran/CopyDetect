
similarity1 <- function(data, item.par=NULL, model="1PL", prior = TRUE, person.id, center.id=NULL, item.loc, single.pair=NULL, many.pairs=NULL, centers=NULL) { #Start Function

#########################################################################################################

	# data,  N x n response data
		# "data" should be a data frame object
		# N is number of examinee and n is number of items
		# Binary responses 

	# item.par, n x 3 item parameter matrix, in the logistic metric
		#first column is item discrimination parameters
		#second column is item difficulty parameters
		#third column is item guessing parameters

		# If given, model and prior arguments are ignored.
		# If not given, the item parameters are estimated using the mirt package based on the model and prior arguments.

	# model, dichotomous item response model to use when computing probabilities. Can be "1PL", "2PL", or "3PL".

	# prior, if TRUE use a prior distribution for estimating guessing parameter in 3PL

	# person.id, the column label for the person id in the dataset

	# center.id, the column label for the center id in the dataset, not necessary if it doesn't exist

	# item.loc, a vector of numbers indicating which columns in the dataset contain item responses

	# single.pair, a vector of length two if the computation is requested for a single pair 
    		  
	  	#the first element of the vector is the id for the suspected copier examinee in in the data file
		#the second element of the vector is the id of the source examinee for the suspected copier examinee in the data file.
 
	# many.pairs, a matrix with two columns if the computation is requested for multiple pairs, not required

	      #the first column of the matrix is the id of the suspected copier examinee in in the data file
	      #the second column of the matrix is the id of the source examinee for the suspected copier examinee in the data file.
	

	# centers, a vector of centers if the computation is requested for all possible pairs in test center


		#CopyDetect computes w, GBT, S2, S1, K, K1, K2, and M4 indices internally, and print the information
			#regarding the likelihood of agreement between suspected copier and source examinees'
			#response vector.

#########################################################################################################

# INPUT CHECKS

if(is.data.frame(data)!=TRUE)  {stop("The input response data file is not a data frame object. Use as.data.frame() first to make your response data file a data frame object")}

if(length(which(colnames(data)==person.id))==0) { stop("The input label for the 'person.id' argument does not exist in the dataset. Please make sure you provide the correct label for the column that represents the person id in your data.") }

if(is.null(center.id)==FALSE) {
	if(length(which(colnames(data)==center.id))==0) { stop("The input label for the 'center.id' argument does not exist in the dataset. Please make sure you provide the correct label for the column that represents the center id in your data.") }
}

if(max(item.loc) > ncol(data)) {
stop("The elements of the item location vector is not aligned with the number of columns. Please make sure the column numbers you provide actually exist for your dataset, and include item responses." ) 
}

data[,person.id] <- as.character(data[,person.id])
data[,center.id] <- as.character(data[,center.id])

ncat <- c()
for(nnn in item.loc) { ncat[nnn] = length(table(data[,nnn]))}
if(length(which(ncat>2))!=0) {
stop("One of the columns you specified as an item has more than two categories. Please make sure the columns you specify for binary items include only two response options.") 
}


if(((exists("single.pair") & is.null(single.pair)==FALSE)  + (exists("many.pairs") & is.null(many.pairs)==FALSE)  + (exists("centers")&is.null(centers)==FALSE))==0) {
	stop("An input must be provided for one of the following arguments: 'single.pair', 'many.pairs', 'centers'. You did not provide input for any of these.")
}

if(((exists("single.pair") & is.null(single.pair)==FALSE) + (exists("many.pairs") & is.null(many.pairs)==FALSE) + (exists("centers")&is.null(centers)==FALSE))==2) {

	if((exists("single.pair") + exists("many.pairs"))==2) {
		stop("Please provide input for only one of the following arguments:'single.pair', 'many.pairs', 'centers'. You have provided inputs for both 'single.pair' and 'many.pairs'. Remove one of them.")
	}

	if((exists("single.pair") + exists("centers"))==2) {
		stop("Please provide input for only one of the following arguments:'single.pair', 'many.pairs', 'centers'. You have provided inputs for both 'single.pair' and 'centers'. Remove one of them.")
	}

	if((exists("centers") + exists("many.pairs"))==2) {
		stop("Please provide input for only one of the following arguments:'single.pair', 'many.pairs', 'centers'. You have provided inputs for both 'centers' and 'many.pairs'. Remove one of them.")
	}
}

if(((exists("single.pair") & is.null(single.pair)==FALSE)  + (exists("many.pairs") & is.null(many.pairs)==FALSE)  + (exists("centers")&is.null(centers)==FALSE))==3) {

		stop("Please provide input for only one of the following arguments:'single.pair', 'many.pairs', 'centers'. You have provided inputs for all of them. Please remove two of them.")

}


if(is.vector(single.pair)==FALSE & is.null(single.pair)==FALSE) { stop("'single.pair' argument is not a vector.If you are computing the response similarity indices for a single pair, please make sure 'single.pair' argument is a vector with two elements.") }

if(is.vector(single.pair)==TRUE) {
	if(length(single.pair)!=2){stop("'single.pair' vector has more than two numbers. It should be a vector of length two. The first element indicates the row number in the response data file for the suspected copier examinee, and the second element indicates the row number in the response data file for the suspected source examinee.")}	
      if(length(which(data[,person.id]==single.pair[1]))!=1 | 
         length(which(data[,person.id]==single.pair[2]))!=1) { stop("The IDs for the requested pair are not valid. Please check the IDs for the requested pair.")}
}

if(is.null(many.pairs)==FALSE) {
	if(is.matrix(many.pairs)==TRUE) {
		if(ncol(many.pairs)!=2){stop("'many.pairs' argument has more than two columns. It should be a matrix with two columns. The first column indicates the ID in the response data file for the suspected copier examinees, and the second column indicates the ID in the response data file for the suspected source examinees.")}	
		  for(yyy in 1:nrow(many.pairs)) {
		   if(length(which(data[,person.id]==many.pairs[yyy,1]))!=1 | 
                  length(which(data[,person.id]==many.pairs[yyy,2]))!=1 ) { stop(paste0("Check the row ",yyy," of the input matrix provided for the 'many.pairs' argument. At least one ID in this row is not a valid ID."))}
		  }		
	} else { 
		stop("The object provided for the 'many.pairs' argument is not a matrix.If you are computing the response similarity indices for multiple pairs, please make sure 'many.pairs' argument is a matrix with two columns. Each row represents a pair of examinees you want to compute the response similarity indices.") 
	}

}

center_ids <- unique(data[,center.id])

if(is.null(centers)==FALSE) {
	if(is.vector(centers)==TRUE) {
		  for(yyy in 1:length(centers)) {
		   if(length(which(center_ids==centers[yyy]))!=1 ) { 
			stop(paste0("Check the center ID ",centers[yyy]," in the vector provided for the 'centers' argument. Center ID " ,centers[yyy] ," does not appear in the dataset."))}
		  }		
	} else { 
		stop("The object provided for the 'centers' argument is not a vector. Please make sure 'centers' argument is a vector containing the labels for centers.") 
	}

}

if(is.null(item.par)==FALSE) {
		if(ncol(item.par)!=3){ stop("The item parameter file should have three columns. Check your input matrix")}
		if(nrow(item.par)!=length(item.loc)) {stop("The number of rows in item parameter file should be equal to the number of elements in the 'item.loc' argument. Check your input matrix for the item parameter file.")}		
}


#########################################################################################################


	row.names <- c("Examinee1");for(i in 2:nrow(data)){row.names <- c(row.names,paste("Examinee",i,sep="")) }
	rownames(data) <- row.names

#########################################

if(is.null(item.par)==TRUE) {

	model.check = (model!="1PL" & model!="2PL" & model!="3PL")
 
	if(model.check == TRUE){ stop("Please choose an IRT model for computing the indices. You can choose 1PL, 2PL, or 3PL.")}

   if(model=="1PL") {

            mod <- paste0("F = 1 - ",ncol(data[,item.loc]),"\n", " CONSTRAIN = (1 - ",ncol(data[,item.loc]),",a1)")

		onePL <- mirt(data  = data[,item.loc],
	      	        model = mod,
	            	  itemtype="2PL",
		              SE=FALSE)

		if(onePL@OptimInfo$converged == "TRUE") {
			item.par = as.matrix(coef(onePL,IRTpars=TRUE,simplify=TRUE)$items[,1:3])
		} else {
			stop("Item parameter estimation is not converged. Either use a different IRT model or check the plausibility of your data for fitting the IRT model")
		  }


    }

   if(model=="2PL") {

		twoPL <- mirt(data  = data[,item.loc],
	      	        model = 1,
	            	  itemtype="2PL",
		              SE=FALSE)

		

		if(twoPL@OptimInfo$converged == "TRUE") {
			item.par = as.matrix(coef(twoPL,IRTpars=TRUE,simplify=TRUE)$items[,1:3])
		} else {
			stop("Item parameter estimation is not converged. Either use a different IRT model or check the plausibility of your data for fitting the IRT model.")
		  }	

    }

   if(model=="3PL" & prior==FALSE) {

		threePL <- mirt(data  = data[,item.loc],
      	       	    model = 1,
            		    itemtype="3PL",
	                      SE=FALSE)

		if(threePL@OptimInfo$converged == "TRUE") {
			item.par = as.matrix(coef(threePL,IRTpars=TRUE,simplify=TRUE)$items[,1:3])
		} else {
			stop("Item parameter estimation is not converged. Either use a different IRT model or check the plausibility of your data for fitting the IRT model")
		  }		
    }


   if(model=="3PL" & prior==TRUE) {
	
		mod <- paste0("F = 1-",ncol(data[,item.loc]),"\n", " PRIOR = (1-",ncol(data[,item.loc]),",g,norm,-1.4,.1)")

		threePL <- mirt(data  = data[,item.loc],
      	       	    model = 1,
            		    itemtype="3PL",
	                      SE=FALSE)

		if(threePL@OptimInfo$converged == "TRUE") {
			item.par = as.matrix(coef(threePL,IRTpars=TRUE,simplify=TRUE)$items[,1:3])
		} else {
			stop("Item parameter estimation is not converged. Either use a different IRT model or check the plausibility of your data for fitting the IRT model")
		  }

    }

}

#####################################

#Other checks for inputs

		if(ncol(item.par)!=3){ stop("The item parameter file should have three columns. Check your input matrix")}
		if(nrow(item.par)!=length(item.loc)) {stop("The number of rows in item parameter file should be equal to the number of columns in the response data file. Check your input matrices")}
		




#Internal function to compute probability correct for a binary IRT model

		irtprob <- function(param,theta,d=1){ 
 				   return( param[,3] + (1 - param[,3])/( 1 + exp(-d*param[,1]*(theta-param[,2]))))
            		    }

#Estimate Theta


	if(exists("onePL")==TRUE) {
		#mle <- fscores(onePL,method="ML")
		mle <- fscores(onePL,method="MAP",cov = matrix(5))
		#mle[which(mle[,1]==Inf | mle[,1]==-Inf),1] = map[which(mle[,1]==Inf | mle[,1]==-Inf),1]
      }

	if(exists("twoPL")==TRUE) {
		#mle <- fscores(twoPL,method="ML")
		mle <- fscores(twoPL,method="MAP",cov = matrix(5))
		#mle[which(mle[,1]==Inf | mle[,1]==-Inf),1] = map[which(mle[,1]==Inf | mle[,1]==-Inf),1]
      }

	if(exists("threePL")==TRUE) {
		#mle <- fscores(threePL,method="ML")
		mle <- fscores(threePL,method="MAP",cov = matrix(5))
		#mle[which(mle[,1]==Inf | mle[,1]==-Inf),1] = map[which(mle[,1]==Inf | mle[,1]==-Inf),1]
      }


if(nrow(mle)!=nrow(data)){
  stop("Problem with ML estimation of person estimates. Please report this bug to package developer.")
}


###############################################################################

# Transform pair IDs to row numbers

if(exists("single.pair")==TRUE) {
 single.pair2 <- c()
 for(uuu in 1:2){
  single.pair2[uuu] = which(data[,person.id]==single.pair[uuu])
 }
}

if(exists("many.pairs")==TRUE & is.null(many.pairs)==FALSE) {

 many.pairs2 <- matrix(nrow=nrow(many.pairs),ncol=2)
 for(uuu in 1:nrow(many.pairs)){
  for(ttt in 1:2) {
	  many.pairs2[uuu,ttt] = which(data[,person.id]==many.pairs[uuu,ttt])
  }
 }

}


######################             Computing w index       #########################################################

	omega <- function(form,ip,pa,thetas=mle) { #start internal function
	
		theta.est1 <- thetas[pa[1],]      
 
		common.missing <- which(is.na(form[pa[1],])==TRUE & is.na(form[pa[2],])==TRUE)
            form[pa[1],common.missing]=0
            form[pa[2],common.missing]=0
	
		obs.match <- length(which(form[pa[1],]==form[pa[2],]))


	      prob.cor <- cbind(irtprob(param=ip,theta=theta.est1),1-irtprob(param=ip,theta=theta.est1))

			colnames(prob.cor) <- c(paste("Probability Correct for Examinee ",pa[1],sep=""),
							paste("Probability Incorrect for Examinee ",pa[1],sep=""))		
			row.names <- c("Item 1");for(i in 2:ncol(form)){row.names <- c(row.names,paste("Item ",i,sep="")) }
			rownames(prob.cor) <- row.names

				pvec <- c(prob.cor[which(form[pa[2],]==1),1],
                                 (1-prob.cor[which(form[pa[2],]==0),1]),
					   (1-prob.cor[which(is.na(form[pa[2],])==TRUE),1])
					   )

		exp.match <- sum(pvec)
		sd.match  <- sqrt(sum(pvec*(1-pvec)))

		w.value <- (obs.match-exp.match)/sd.match 
		p.value <- pnorm(w.value,0,1,lower.tail=FALSE)
		
		return(list(ability1=theta.est1,obs.match=obs.match,
				exp.match=exp.match,
				sd.match=sd.match,
				W.value=w.value,
			 	p.value=p.value))
	}#end internal function


##################                     Computing GBT index              ##################################################


	GBT <- function(form,ip,pa,thetas=mle) { #start internal function
	
		theta.est1 <- thetas[pa[1],]    
		theta.est2 <- thetas[pa[2],]        

		common.missing <- which(is.na(form[pa[1],])==TRUE & is.na(form[pa[2],])==TRUE)
            form[pa[1],common.missing]=0
            form[pa[2],common.missing]=0

		obs.match <- length(which(form[pa[1],]==form[pa[2],]))                                       

	      prob.cor <- cbind(irtprob(param=ip,theta=theta.est1),irtprob(param=ip,theta=theta.est2)) #probability correct
			
			colnames(prob.cor) <- c(paste("Examinee ",pa[1],sep=""),
							paste("Examinee ",pa[2],sep=""))	
			row.names <- c("Item 1");for(i in 2:ncol(form)){row.names <- c(row.names,paste("Item ",i,sep="")) }
			rownames(prob.cor) <- row.names

		prob.incor <- 1-prob.cor #probability incorrect

			colnames(prob.incor) <- c(paste("Examinee ",pa[1],sep=""),
							paste("Examinee ",pa[2],sep=""))	
			rownames(prob.incor) <- row.names


		Pi <- (prob.cor[,1]*prob.cor[,2])+(prob.incor[,1]*prob.incor[,2]) #probability of matching

  			Qi <-1-Pi
			   Cpi <- cumprod(Pi)
			     I <- length(Pi)
	
      			M <- matrix(1,(I + 1),I)
		      	M[1,] <- cumprod(Qi)
			       for(o in 1:I) {
				   M[(o+1),o]<- Cpi[o]
			       }

	      	for(m in 2:(I+1)) {
				for(o in 2:I) {
					if(m <= o)
						M[m,o] <- Qi[o]* M[m,(o-1)]+Pi[o]* M[(m-1),(o-1)]
					   else M[m,o] <- M[m,o]
				}
			}

	GBT.p.value <- sum(M[(obs.match+1):(I+1),I])

		matchings <- c("Prob. of 0 Match","Prob. of 1 Match");for(i in 2:ncol(form)){matchings<- c(matchings,paste("Prob. of ",i," Matches",sep="")) }
		prob.dist.match <- as.data.frame(cbind(matchings,round(M[,I],6)))

		return(list(ability1=theta.est1,
				ability2=theta.est2,
				prob.cor = prob.cor,
				prob.match=Pi,
				exact.prob.dist=prob.dist.match,
				p.value=GBT.p.value
			))
	
	}#end internal function


#####################                    Computing K-index                 #####################################################

	k <- function(form,pa) { #start internal function

			wc  <- sum(form[pa[1],]==0,na.rm=TRUE)                    #number-incorrect score for copier
			ws  <- sum(form[pa[2],]==0,na.rm=TRUE)                    #number-incorrect score for source
			
	            m   <- length(which(form[pa[1],]==0 & form[pa[2],]==0))+
                    length(which(is.na(form[pa[1],])==TRUE & is.na(form[pa[2],])==TRUE))  #number of identical incorrect responses between two vectors

			subgroup <- which((rowSums(form==0,na.rm=TRUE)==wc))

				if(length(subgroup)!=0) {

				smatrix <- as.data.frame(matrix(rep(as.matrix(form[pa[2],]),length(subgroup)),nrow=length(subgroup),byrow=TRUE))

				emp.agg <- rowSums((form[subgroup,]==0)&(smatrix==0),na.rm=TRUE)+
                                   rowSums((is.na(form[subgroup,])==TRUE & is.na(smatrix)==TRUE),na.rm=TRUE)
				p=mean(emp.agg)/ws 

				 } else p=NA

		if(is.na(p)!=TRUE) { k.index=1-pbinom(m-1,ws,p) } else k.index=NA

		return(list(
				subgroups=subgroup,
				emp.agg=emp.agg,
				k.index=k.index
			))

	
	}#end internal function

	

##############                       Computing K variants                   #######################################

	ks12 <- function(form,pa) { #start internal function

		options(warn=-1)
		subgroups <- vector("list",ncol(form)+1)

			for(j in 1:(ncol(form)+1)){
				subgroups.ind  <- which(rowSums(form==0,na.rm=TRUE)==j-1)
				subgroups.ind  <- subgroups.ind[subgroups.ind!=pa[2]]
			      subgroups[[j]] <- subgroups.ind
			}

	
			wc  <- rowSums(form[pa[1],]==0,na.rm=TRUE)
			qc  <- wc/ncol(form)
			ws  <- rowSums(form[pa[2],]==0,na.rm=TRUE)
	            m   <- length(which(form[pa[1],]==0 & form[pa[2],]==0))+
				 length(which(is.na(form[pa[1],])==TRUE & is.na(form[pa[2],])==TRUE))
			cm <- which(form[pa[1],]==1 & form[pa[2],]==1)

			pr     <- c()
			prob   <- matrix(nrow=(ncol(form)+1),ncol=ncol(form))
			weight <- matrix(nrow=(ncol(form)+1),ncol=ncol(form))
			pj <- c()

			g=.2
			d2=-(1+g)/g

				for(j in 1:(ncol(form)+1)){

					if(length(subgroups[[j]])!=0) {

						smatrix <- as.data.frame(matrix(rep(as.matrix(form[pa[2],]),length(subgroups[[j]])),nrow=length(subgroups[[j]]),byrow=TRUE))
						emp.agg <- rowSums((form[subgroups[[j]],]==0)&(smatrix==0),na.rm=TRUE)+
                                               rowSums((is.na(form[subgroups[[j]],])==TRUE & is.na(smatrix)==TRUE),na.rm=TRUE)
						pr[j]=mean(emp.agg,na.rm=TRUE)/ws 
						cor.match <- ((form[subgroups[[j]],]==1 & smatrix==1)*1)
						cor.match[which(is.na(cor.match)==TRUE)]=0
						prob[j,] <- colMeans(cor.match,na.rm=TRUE)
						weight[j,] <- (((1+g)/(1-g))*exp(1))^(prob[j,]*d2)
						pj[j] <-  mean(cor.match%*%t(t(weight[j,])),na.rm=TRUE)

					 } else 
				
					if(length(subgroups[[j]])==0) {
							pr[j]=NA 
							pj[j]=NA
					}
				}


			Qrs  <- (0:ncol(form))/ncol(form)
			Qrs2 <- Qrs^2
			Qrs3 <- 0:ncol(form)
			mm <- ceiling(sum(weight[wc+1,cm]))+m
				
				pred1 <- predict(lm(pr~1+Qrs))
				pred.1 <- c()
				for(i in 1:(ncol(form)+1)){ 
					if(length(which(as.character(1:(ncol(form)+1))[i]==names(pred1)))!=0){
						pred.1[i]=pred1[which(as.character(1:(ncol(form)+1))[i]==names(pred1))]} else pred.1[i]=NA
                        }
				pred2 <- predict(lm(pr~1+Qrs+Qrs2))
				pred.2 <- c()
				for(i in 1:(ncol(form)+1)){ 
					if(length(which(as.character(1:(ncol(form)+1))[i]==names(pred2)))!=0){
						pred.2[i]=pred2[which(as.character(1:(ncol(form)+1))[i]==names(pred2))]} else pred.2[i]=NA
                        }

				pred3 <- exp(predict(glm(ws*pr ~ Qrs3 ,family=poisson())))
				pred.3 <- c()
				for(i in 1:(ncol(form)+1)){ 
					if(length(which(as.character(1:(ncol(form)+1))[i]==names(pred3)))!=0){
						pred.3[i]=pred3[which(as.character(1:(ncol(form)+1))[i]==names(pred3))]} else pred.3[i]=NA
                        }
				pred4 <- exp(predict(glm(ws*pr+ceiling(pj) ~ Qrs3 ,family=poisson())))
				pred.4 <- c()
				for(i in 1:(ncol(form)+1)){ 
					if(length(which(as.character(1:(ncol(form)+1))[i]==names(pred4)))!=0){
						pred.4[i]=pred4[which(as.character(1:(ncol(form)+1))[i]==names(pred4))]} else pred.4[i]=NA
                        }


				p1 <- pred.1[which(Qrs==qc)]
				p2 <- pred.2[which(Qrs==qc)]
				s1 <- pred.3[which(Qrs==qc)]
				s2 <- pred.4[which(Qrs==qc)]


			if(is.na(p1)!=TRUE & p1>=1)  { p1=.999 };if(is.na(p1)!=TRUE & p1<=0) { p1=.001 }
			if(is.na(p2)!=TRUE & p2>=1)  { p2=.999 };if(is.na(p2)!=TRUE & p2<=0) { p2=.001 }
			if(is.na(s1)!=TRUE & s1>=ws) { s1=ws }  ;if(is.na(s2)!=TRUE & s2>=ncol(form)) { s2=ncol(form) }  ;

			if(is.na(p1)!=TRUE) { k1.index=1-pbinom(m-1,ws,p1) } else k1.index=NA
			if(is.na(p2)!=TRUE) { k2.index=1-pbinom(m-1,ws,p2) } else k2.index=NA
			if(is.na(s1)!=TRUE) { s1.index=(1-ppois(m-1,s1)) - (1 - ppois(ws,s1))       } else s1.index=NA
			if(is.na(s2)!=TRUE) { s2.index=(1-ppois(mm-1,s2)) - (1 - ppois(ncol(form),s2))      } else s2.index=NA


		return(list(mean.iden.incorrect = pr*ws,
				weighted.iden.correct=pj,
				subgroups=subgroups,
				pred1 = pred.1*ws,
				pred2 = pred.2*ws,
				pred3 = pred.3,
				pred4 = pred.4,
				K1.index =k1.index,
				K2.index =k2.index,
				S1.index =s1.index,
				S2.index =s2.index))

	}#end internal function


######################             Computing M4 index       #########################################################

	M4 <- function(form,ip,pa,thetas=mle) { #start internal function

		gtd <- function(P,Q,m,n) {

		  R <- 1-(P+Q)
		  I=length(P)
  
		  rec <- vector("list",I+1)
		  rec[[1]]=matrix(0,nrow=I+1,ncol=I+1)
		  rec[[1]][1,1] <- 1
		  for(k in 2:(I+1)){
		    rec[[k]] = R[k-1]*rec[[k-1]]+
		               rbind(0,P[k-1]*rec[[k-1]])[-(I+2),]+
		               cbind(0,Q[k-1]*rec[[k-1]])[,-(I+2)]
		  }
  
		  for(k in 1:(I+1)){ rec[[k]]=t(rec[[k]])}
  
		  upper <- matrix(nrow=I+1,ncol=I+1)
		  for(x in 1:(I+1)){
		    for(y in 1:(I+1)) {
		      upper[x,y] = sum(rec[[I+1]][x:(I+1),y:(I+1)])
		    }
		  }
  
		  prob.table <- expand.grid(0:I,0:I)
		  colnames(prob.table) <- c("IncorrectMatch","CorrectMatch")
		  prob.table <- prob.table[which(rowSums(prob.table)<=I),]
		  prob.table <- prob.table[order(prob.table[,1]),]
		  prob.table <- cbind(prob.table,0,0,0,0)
		  prob.table[,3] <- I-(rowSums(prob.table[,1:2]))
		  for(i in 1:(nrow(prob.table))){
		    x=prob.table[i,1]
		    y=prob.table[i,2]
		    prob.table[i,4] <- upper[x+1,y+1]
		    prob.table[i,5] <- rec[[I+1]][x+1,y+1]
		  }
  
		  for(i in 1:(nrow(prob.table))){
		    r = prob.table[i,4]
		    marked = which(prob.table[,4] <= r)
		    prob.table[i,6] <- sum(prob.table[marked,5])
		  }
	  
		  colnames(prob.table)[3:6] <- c("NonMatch","Upper",
		                                 "Probability","TailProbability")
		  p = prob.table[which(prob.table[,1]==n & prob.table[,2]==m),6]
		  list(prob.table[,-4],p)
		}

		theta.est1 <- thetas[pa[1],]    
		theta.est2 <- thetas[pa[2],]        

		P1 <- irtprob(param=ip,theta=theta.est1) 
		P2 <- irtprob(param=ip,theta=theta.est2) 
		P <- P1*P2
   		Q <- (1-P1)*(1-P2)
   		m <- length(which(form[pa[1],]==1 & form[pa[2],]==1))                                     
   		n <- length(which(form[pa[1],]==0 & form[pa[2],]==0))                                     
		m4 = gtd(P=P,Q=Q,m=m,n=n)

		return(list(ability1=theta.est1,
				ability2=theta.est2,
				prob.corr.match=P,
				prob.incorr.match=Q,			
				exact.prob.dist=m4[[1]],
				p.value=m4[[2]]
			))
	}
	

####################################################################################################################

if(exists("single.pair") & is.null(single.pair)==FALSE) {

# form, response data matrix
# ip, item parameter matrix
# pa, a vector of two, 1st row number for suspected copier, 2nd, row number for suspected source

	w.index <- omega(form=data[,item.loc],ip=item.par,pa=single.pair2)


	GBT.index <- GBT(form=data[,item.loc],ip=item.par,pa=single.pair2)

	m4.index <- M4(form=data[,item.loc],ip=item.par,pa=single.pair2)

	if(rowSums(data[single.pair2[2],item.loc],na.rm=TRUE)!=ncol(data[,item.loc])) { k.index <- k(form=data[,item.loc],pa=single.pair2) } else k.index <- NULL

	if(rowSums(data[single.pair2[2],item.loc],na.rm=TRUE)!=ncol(data[,item.loc])) { k.variants <- ks12(form=data[,item.loc],pa=single.pair2) } else k.variants <- NULL


	outCD <- list(data=data,
                    W.index=w.index,
                    GBT.index=GBT.index,
                    K.index=k.index,
                    K.variants=k.variants,
                    M4.index = m4.index,
			  item.loc = item.loc,
                    item.par = item.par,
                    center.id = center.id,
                    person.id = person.id,
			  single.pair  = single.pair,
			  single.pair2 = single.pair2,
			  thetas = mle)

	class(outCD) <- "CopyDetect"
	return(outCD)
}

if(exists("many.pairs") & is.null(many.pairs)==FALSE) {

	out.many <- as.data.frame(cbind(many.pairs,many.pairs2))
	colnames(out.many) <- c("Suspected Copier","Suspected Source","Suspected Copier Row #","Suspected Source Row #")
	
	out.many$W.pvalue 	<- NA
	out.many$GBT.pvalue 	<- NA
	out.many$M4.pvalue 	<- NA
	out.many$K.pvalue 	<- NA
	out.many$K1.pvalue 	<- NA
	out.many$K2.pvalue 	<- NA
	out.many$S1.pvalue 	<- NA
	out.many$S2.pvalue 	<- NA
	
	for(i in 1:nrow(out.many)) {
		copy.pair <- many.pairs2[i,]
		w.index <- omega(form=data[,item.loc],ip=item.par,pa=copy.pair)
		GBT.index <- GBT(form=data[,item.loc],ip=item.par,pa=copy.pair)
		m4.index <- M4(form=data[,item.loc],ip=item.par,pa=copy.pair)
		if(rowSums(data[copy.pair[2],item.loc],na.rm=TRUE)!=ncol(data[,item.loc])) { k.index <- k(form=data[,item.loc],pa=copy.pair) } else k.index <- NULL
		if(rowSums(data[copy.pair[2],item.loc],na.rm=TRUE)!=ncol(data[,item.loc])) { k.variants <- ks12(form=data[,item.loc],pa=copy.pair) } else k.variants <- NULL
		out.many[i,]$W.pvalue   <- w.index$p.value
		out.many[i,]$GBT.pvalue <- GBT.index$p.value
		out.many[i,]$M4.pvalue  <- m4.index$p.value
		out.many[i,]$K.pvalue   <- k.index$k.index
		out.many[i,]$K1.pvalue  <- k.variants$K1.index
		out.many[i,]$K2.pvalue  <- k.variants$K2.index
		out.many[i,]$S1.pvalue  <- k.variants$S1.index
		out.many[i,]$S2.pvalue  <- k.variants$S2.index
	}

	outCD <- list(data=data,
			  output.manypairs = out.many,	  
			  item.loc = item.loc,
                    item.par = item.par,
                    center.id = center.id,
                    person.id = person.id,
			  thetas = mle)

	class(outCD) <- "CopyDetectMany"
	return(outCD)


}


if(exists("centers") & is.null(centers)==FALSE) {

	out.centers <- vector("list",length(centers))
	names(out.centers) <- centers

	for(i in 1:length(centers)) {

		ps = data[which(data[,center.id]==centers[i]),person.id]

		if(length(ps)>1) {

			pt = expand.grid(ps,ps)
			pt <- pt[which(pt[,1]!=pt[,2]),]
			pt$row1 <- NA
			pt$row2 <- NA
			 for(uuu in 1:nrow(pt)){
			  for(ttt in 1:2) {
				  pt[uuu,ttt+2] = which(data[,person.id]==as.character(pt[uuu,ttt]))
			  }
			 }
			colnames(pt) <- c("Suspected Copier","Suspected Source","Suspected Copier Row #","Suspected Source Row #")
	
			pt$W.pvalue 	<- NA
			pt$GBT.pvalue 	<- NA
			pt$M4.pvalue 	<- NA
			pt$K.pvalue 	<- NA
			pt$K1.pvalue 	<- NA
			pt$K2.pvalue 	<- NA
			pt$S1.pvalue 	<- NA
			pt$S2.pvalue 	<- NA
	
			for(j in 1:nrow(pt)) {
				copy.pair <- as.numeric(pt[j,3:4])
				w.index <- omega(form=data[,item.loc],ip=item.par,pa=copy.pair)
				GBT.index <- GBT(form=data[,item.loc],ip=item.par,pa=copy.pair)
				m4.index <- M4(form=data[,item.loc],ip=item.par,pa=copy.pair)
				if(rowSums(data[copy.pair[2],item.loc],na.rm=TRUE)!=ncol(data[,item.loc])) { k.index <- k(form=data[,item.loc],pa=copy.pair) } else k.index <- NULL
				if(rowSums(data[copy.pair[2],item.loc],na.rm=TRUE)!=ncol(data[,item.loc])) { k.variants <- ks12(form=data[,item.loc],pa=copy.pair) } else k.variants <- NULL
				pt[j,]$W.pvalue   <- w.index$p.value
				pt[j,]$GBT.pvalue <- GBT.index$p.value
				pt[j,]$M4.pvalue  <- m4.index$p.value
				pt[j,]$K.pvalue   <- k.index$k.index
				pt[j,]$K1.pvalue  <- k.variants$K1.index
				pt[j,]$K2.pvalue  <- k.variants$K2.index
				pt[j,]$S1.pvalue  <- k.variants$S1.index
				pt[j,]$S2.pvalue  <- k.variants$S2.index
			}
	
			out.centers[[i]] <- pt
		} else {out.centers[[i]] <- NULL}
	
	}


	outCD <- list(data=data,
			  output.centers = out.centers,	  
			  item.loc = item.loc,
                    item.par = item.par,
                    center.id = center.id,
                    person.id = person.id,
			  thetas = mle)

	class(outCD) <- "CopyDetectCenter"
	return(outCD)


}


}















