\name{CopyDetect2}
\alias{CopyDetect2}
\title{Answer Copying Indices for Nominally Scored Items}
\description{
Computes the Omega index (Wollack, 1996), Generalized Binomial Test (van der Linden & Sotaridona (2006), K index (Holland, 1996), K1 and K2 indices (Sotaridona & Meijer, 2002), and S1 and S2 indices (Sotaridona & Meijer, 2003)
}
\usage{
CopyDetect2(data,item.par=NULL,pair,options,key=NULL)
}

\arguments{
  \item{data}{
a data frame with \emph{N} rows and \emph{n} columns, where \emph{N} denotes the number of subjects and \emph{n} denotes the number of items. All items should be scored using nominal response categories. All variables (columns) must be "character". Missing values ("NA") are allowed. Please see the details below for the treatment of missing data in the analysis.
}
  \item{item.par}{
a data matrix with \emph{n} rows and \emph{2*r} columns, where \emph{n} denotes the number of items and \emph{r} denotes the number of nominal response alternatives for an item. It is assumed that all items have the same number of response alternatives. The first \emph{r} columns are the Nominal Response Model (NRM) item slope parameters and the second \emph{r} columns are the NRM item intercept parameters. Please see \code{\link{ipar}} for a sample parameter matrix for items with five response alternatives. The NRM item parameters must be obtained from external software (e.g. MULTILOG) and provided to \code{\link{CopyDetect2}}. 
}
  \item{pair}{
a vector of length 2 to locate the row numbers for the suspected pair of examinees. The first element of the vector indicates the row number of the suspected copier examinee, and the second element of the vector indicates the row number of the suspected source examinee.
}
  \item{options}{a character vector of length \emph{r}, where \emph{r} denotes the number of response alternatives for an item. The order of the response alternatives in the vector must be the same as the column order of the response alternatives in the item parameter matrix. 
} 

  \item{key}{a character vector of length \emph{n}, where \emph{n} denotes the number of items. If an item parameter matrix is provided, the key does not have to be provided. If the key responses are not provided separately, they are internally determined from the item parameter matrix. In the NRM, the response alternative with the highest slope parameter for an item is the key response for the item. If an item parameter matrix is not provided, the key must be provided to compute the K index and K variants. 
} 


}
\details{
\code{\link{CopyDetect2}} uses nominally scored items. Therefore, the definition of "identical incorrect response" and "identical correct response" is slightly different from \code{\link{CopyDetect1}}. For example, let A, B, C, and D be response alternatives for items in a multiple-choice test, and let A be the key response for an item. There are 10 possible response combinations between two response vectors: (A,A), (A,B), (A,C), (A,D), (B,B), (B,C), (B,D), (C,C), (C,D), and (D,D). \code{\link{CopyDetect2}} counts the (A,A) response combination as an "identical correct response", and any of the (B,B), (C,C), and (D,D) response combinations as an "identical incorrect response". Similar to \code{\link{CopyDetect1}}, the (NA,NA) response combination is counted as an "identical incorrect response". All other response combinations (A,B), (A,C), (A,D), (B,C), (B,D), (C,D), (A,NA), (B,NA), (C,NA), and (D,NA) are counted as non-identical responses. When computing the number-correct/number-incorrect scores or estimating the IRT ability parameters, missing values (NA) in a response vector are counted as an incorrect response.

\subsection{Generalized Binomial Test}{\cr

The computational procedure is very similar to \code{\link{CopyDetect1}}. The probability of matching on item \emph{i} is computed assuming that the Nominal Response Model (Bock, 1972) is used to model the response data. \eqn{P_i}{Pi} is equal to
\deqn{\sum\limits_{j=1}^r{P_{jic}*P_{jis}},}{Please see the manual for the equation!}
where \eqn{P_{jic}}{Pjic} is the probability of choosing response alternative \emph{j} on item \emph{i} for suspected copier examinee and \eqn{P_{jis}}{Pjis} is the probability of choosing response alternative \emph{j} on item \emph{i} for suspected source examinee. In the NRM, the probability of choosing a response alternative \emph{j} on item \emph{i} given the ability and model parameters is equal to

\deqn{
P_{ji(c,s)}=P(x_{i(c,s)}=j|\hat{\mathrel\theta}_{c,s},\hat{\mathrel\xi_{i}})=\frac
{e(\hat{\mathrel\zeta}_{ji}+\hat{\mathrel\alpha}_{ji}*\hat{\mathrel\theta}_{(c,s)})}{\sum\limits_{j=1}^r{e(\hat{\mathrel\zeta}_{ji}+\hat{\mathrel\alpha}_{ji}*\hat{\mathrel\theta}_{(c,s)})}},
}{Please see the manual for the equation!}

where \eqn{\hat{\mathrel\zeta}_{ji}}{} and \eqn{\hat{\mathrel\alpha}_{ji}}{} are the NRM intercept and slope parameters respectively for response alternative \emph{j} on item \emph{i}.

The rest of the computations are identical to \code{\link{CopyDetect1}}. 

}


\subsection{ Omega Index}{\cr

The computations are identical to \code{\link{CopyDetect1}}. The only difference is that the NRM is used to compute the probabilities.

}

\subsection{ K Index and K variants}{\cr

The computations are identical to \code{\link{CopyDetect1}}. 

}


}

\value{

\code{CopyDetect2()} returns an object of class "\code{CopyDetect2}". An object of class "\code{CopyDetect2}" is a list containing the following components. 

    \item{data}{ original data file provided by user}
    \item{key}{ key response alternatives}
    \item{scored.data}{ dichotomously scored items based on the key responses}
    \item{theta.par}{ estimated IRT ability parameters}	
    \item{suspected.pair}{ row numbers in the data file for suspected pair}
    \item{W.index}{ Statistics for the W index}
    \item{GBT.index}{ Statistics for the GBT index}
    \item{K.index}{ Statistics for the K index}
    \item{K.variants}{ Statistics for the K1, K2, S1, and S2 indices}

}

\references{

Sotaridona, L.S., & Meijer, R.R.(2002). Statistical properties of the K-index for detecting answer copying. \emph{Journal of Educational Measurement, 39}, 115-132.\cr

Sotaridona, L.S., & Meijer, R.R.(2003). Two new statistics to detect answer copying. \emph{Journal of Educational Measurement, 40}, 53-69.\cr

van der Linden, W.J., & Sotaridona, L.S.(2006). Detecting answer copying when the regular response process follows a known response model. \emph{Journal of Educational and Behavioral Statistics, 31}, 283-304.\cr

Wollack, J.A.(1996). Detection of answer copying using item response theory. \emph{Dissertation Abstracts International, 57/05}, 2015.\cr

Wollack, J.A.(2003). Comparison of answer copying indices with real data. \emph{Journal of Educational Measurement, 40}, 189-205.\cr

Wollack, J.A.(2006). Simultaneous use of multiple answer copying indexes to improve detection rates. \emph{Applied Measurement in Education, 19}, 265-288.\cr

Wollack, J.A., & Cohen, A.S.(1998). Detection of answer copying with unknown item and trait parameters. \emph{Applied Psychological Measurement, 22}, 144-152.\cr

Zopluoglu, C., & Davenport, E.C.,Jr.(in press). The empirical power and type I error rates of the GBT and \eqn{\mathrel\omega} indices in detecting answer copying on multiple-choice tests. \emph{Educational and Psychological Measurement}.\cr

                                                                                                
}
\author{
Cengiz Zopluoglu
}


\examples{
data(simulated.data)
head(simulated.data)
str(simulated.data) #check that the variables are all "character"

data(ipar)
head(ipar)


# Due to the time constrains, I take a subset of the dataset
# You can ignore the following two lines in your run.

simulated.data <- simulated.data[,1:10]
ipar <- ipar[1:10,]

# Now, compute these indices for 100 random pairs of examinees
# a small type I error rate study

	replication=1 #set this number to 100 or 1000.One replication takes about 15 seconds
	
	pairs <- as.data.frame(matrix(replication,ncol=2))

		for(i in 1:replication){

			d <- sample(1:nrow(simulated.data),2,replace=FALSE)
			pairs[i,1]=d[1]
			pairs[i,2]=d[2]
		}

	pairs$W 	<- NA
	pairs$GBT 	<- NA
	pairs$K 	<- NA
	pairs$K1 	<- NA
	pairs$K2	<- NA
	pairs$S1 	<- NA
	pairs$S2 	<- NA

		for(i in 1:replication){

			x <- CopyDetect2(data=simulated.data,
                       item.par=ipar,
                       pair=c(pairs[i,1],pairs[i,2]),
				options=c("A","B","C","D","E"))

			pairs[i,]$W=x$W.index$p.value
			pairs[i,]$GBT=x$GBT.index$p.value
			pairs[i,]$K=x$K.index$k.index
			pairs[i,]$K1=x$K.variants$K1.index
			pairs[i,]$K2=x$K.variants$K2.index
			pairs[i,]$S1=x$K.variants$S1.index
			pairs[i,]$S2=x$K.variants$S2.index
		}

	#Check the false detection rates at alpha level of .05 
	#(empirical type I error rates)
	#We expect to see 5% of the pairs be detected just by chance

	length(which(pairs$W<.05))/nrow(pairs)
	length(which(pairs$GBT<.05))/nrow(pairs)
	length(which(pairs$K<.05))/nrow(pairs)
	length(which(pairs$K1<.05))/nrow(pairs)
	length(which(pairs$K2<.05))/nrow(pairs)
	length(which(pairs$S1<.05))/nrow(pairs)
	length(which(pairs$S2<.05))/nrow(pairs)


	#Now, compute these indices for 5 answer copying pairs
	#a tiny empirical power study
	#First we will randomly choose a cheater examinee
	#Second, we will randomly choose a corresponding source examinee 
	#Third, we will randomly select 10 items (25% copying)
	#Finally, we will overwrite the response vector of the source examinee
	#on the response vector of the cheater examinee
	#This mimicks the scenario that the cheater examinee looks at the 
	#source examinee's sheet and copies 5 items.

	replication=1 #set this number to 100 or 1000.One replication takes about 15 seconds
	
	copy.pairs <- as.data.frame(matrix(replication,ncol=2))
	
	for(i in 1:replication){
			d <- sample(1:nrow(simulated.data),2,replace=FALSE)
			copy.pairs[i,1]=d[1] #hypothetical cheater examinee
			copy.pairs[i,2]=d[2] #hypothetical source examinee
		}

	new.data <- simulated.data

	for(i in 1:replication){ #Simulate answer copying for each answer copying pair

		copy.items <- sample(1:ncol(simulated.data),5,replace=FALSE)
		new.data[copy.pairs[i,1],]=new.data[copy.pairs[i,2],]
	}

	#Compute indices on the original response vectors 

	copy.pairs$W1 	<- NA
	copy.pairs$GBT1 	<- NA
	copy.pairs$K_1 	<- NA
	copy.pairs$K1_1 	<- NA
	copy.pairs$K2_1	<- NA
	copy.pairs$S1_1 	<- NA
	copy.pairs$S2_1 	<- NA

		for(i in 1:replication){

			x <- CopyDetect2(data=simulated.data,
                       item.par=ipar,
                       pair=c(copy.pairs[i,1],copy.pairs[i,2]),
				options=c("A","B","C","D","E"))

			copy.pairs[i,]$W1=x$W.index$p.value
			copy.pairs[i,]$GBT1=x$GBT.index$p.value
			copy.pairs[i,]$K_1=x$K.index$k.index
			copy.pairs[i,]$K1_1=x$K.variants$K1.index
			copy.pairs[i,]$K2_1=x$K.variants$K2.index
			copy.pairs[i,]$S1_1=x$K.variants$S1.index
			copy.pairs[i,]$S2_1=x$K.variants$S2.index
		}

	
	#Compute indices for same pairs on the answer copying simulated response vectors

	
	copy.pairs$W2 	<- NA
	copy.pairs$GBT2 	<- NA
	copy.pairs$K_2 	<- NA
	copy.pairs$K1_2 	<- NA
	copy.pairs$K2_2	<- NA
	copy.pairs$S1_2 	<- NA
	copy.pairs$S2_2 	<- NA

		for(i in 1:replication){

			x <- CopyDetect2(data=new.data,
                       item.par=ipar,
                       pair=c(copy.pairs[i,1],copy.pairs[i,2]),
				options=c("A","B","C","D","E"))


			copy.pairs[i,]$W2=x$W.index$p.value
			copy.pairs[i,]$GBT2=x$GBT.index$p.value
			copy.pairs[i,]$K_2=x$K.index$k.index
			copy.pairs[i,]$K1_2=x$K.variants$K1.index
			copy.pairs[i,]$K2_2=x$K.variants$K2.index
			copy.pairs[i,]$S1_2=x$K.variants$S1.index
			copy.pairs[i,]$S2_2=x$K.variants$S2.index
		}


	#See what happens!

		print(copy.pairs,8)

}

