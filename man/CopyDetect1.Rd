\name{CopyDetect1}
\alias{CopyDetect1}
\title{Answer Copying Indices for Dichotomously Scored Items }
\description{
Computes the Omega index (Wollack, 1996), Generalized Binomial Test ([GBT], van der Linden & Sotaridona (2006), K index (Holland, 1996), K1 and K2 indices (Sotaridona & Meijer, 2002), and S1 and S2 indices (Sotaridona & Meijer, 2003)
}
\usage{
CopyDetect1(data,item.par=NULL,pair)
}

\arguments{
  \item{data}{
a data frame with \emph{N} rows and \emph{n} columns, where \emph{N} denotes the number of subjects and \emph{n} denotes the number of items. All items should be scored dichotomously, with 0 indicating an incorrect response and 1 indicating a correct response. All variables (columns) must be "numeric". Missing values (NA) are allowed. Please see the details below for the treatment of missing data in the analysis.
}
  \item{item.par}{
a data matrix with \emph{n} rows and three columns, where \emph{n} denotes the number of items. The first, second, and third columns represent item difficulty, item discrimination, and item guessing parameters, respectively. If item parameters are not provided by user, the \pkg{irtoys} package and ltm engine is internally called to estimate the 2PL IRT model item parameters. The rows in the item parameter matrix must be in the same order as the columns in the response data.
}
  \item{pair}{
a vector of length 2 to locate the row numbers for the suspected pair of examinees. The first element of the vector indicates the row number of the suspected copier examinee, and the second element of the vector indicates the row number of the suspected source examinee.
}
 
}
\details{

Test fraud has recently been receiving increased attention in the field of educational testing. The current R package provides a set of useful statistical indices recently proposed in the literature for detecting a specific type of test fraud - answer copying from a nearby examinee on multiple-choice examinations. The information obtained from these procedures may provide additional statistical evidence of answer copying, but they should be used cautiously. These statistical procedures should not be used as sole evidence of answer copying, especially when used for general screening purposes.\cr

There are more than twenty different statistical procedures recommended in the literature for detecting answer copying on multiple-choice examinations. However, the CopyDetect package includes the indices that have been shown as effective and reliable based on the simulation studies in the literature (Sotaridona & Meijer, 2002, 2003; van der Linden & Sotaridona, 2006; Wollack, 1996, 2003, 2006; Wollack & Cohen, 1998; Zopluoglu & Davenport, in press; Zopluoglu, Chen, Huang, & Mroch, in submission). Among these indices, \eqn{\mathrel\omega}{Omega} and GBT use IRT models, and \eqn{K} and \eqn{K} variants are the non-IRT counterparts.\cr

Since \code{\link{CopyDetect1}} uses dichotomous responses as input, any (0,0) response combination between two response vectors is counted as an "identical incorrect response", and any (1,1) response combination between two response vectors is counted as an "identical correct response". \code{\link{CopyDetect1}} also counts any (NA,NA) response combination between two response vectors as an "identical incorrect response". Other response combinations such as (0,1),(1,0),(0,NA),(1,NA) between two response vectors are not counted as identical responses. When computing the number-correct/number-incorrect scores or estimating the IRT ability parameters, missing values (NA) are counted as an incorrect response.\cr

For the the \eqn{\mathrel\omega}{Omega} and GBT indices, \code{\link{CopyDetect1}} uses dichotomous IRT models to estimate the probability of a correct response given the ability and item parameters, \eqn{P(x_{i}=1 |\hat{\mathrel\theta}, \hat{\mathrel\xi_{i}})}{the probability correct}. The user can manipulate the dichotomous IRT model used in the analysis by modifying the input item parameter matrix.


\subsection{Generalized Binomial Test}{\cr

GBT computes the exact probability distribution for the number of identical responses between two response vectors. Let \eqn{P_i}{Pi} be the probability of matching on item \emph{i} assuming that a dichotomous IRT model is used to model the response data, and be computed as 
\deqn{
P_{i}=\big[P(x_{ic}=1 |\hat{\mathrel\theta_{c}}, \hat{\mathrel\xi_{i}})*P(x_{is}=1 |\hat{\mathrel\theta_{s}},\hat{\mathrel\xi_{i}})\big]+
\big[P(x_{ic}=0 |\hat{\mathrel\theta_{c}}, \hat{\mathrel\xi_{i}})*P(x_{is}=0 |\hat{\mathrel\theta_{s}},\hat{\mathrel\xi_{i}})\big],}{Please see the manual for the equation!}

where \eqn{x_{ic}}{} and \eqn{x_{is}}{} are realizations of observed responses for the suspected copier and source examinees on item \emph{i} respectively, \eqn{\hat{\mathrel\theta_{c}}}{} is the ability estimate for the suspected copier examinee, \eqn{\hat{\mathrel\theta_{s}}}{} is the ability estimate for the suspected source examinee, and \eqn{\hat{\mathrel\xi_{i}}}{} is the model parameter estimates for item \emph{i}. 

Then, the probability of observing exactly \emph{m} matches on \emph{n} items between two response vectors is equal to

\deqn{ f_{n}(m)=\sum{\prod\limits_{i=1}^n{P_{i}^tQ_{i}^{1-t}}},}{Please see the manual for the equation!}

where \eqn{Q_{i}}{} is equal to \eqn{1-P_{i}}{}; \emph{t} is equal to one if source and copier examinees have identical responses on item \emph{i}, and zero otherwise; and the summation is over all possible combinations of \emph{m} matches on \emph{n} items. For instance, the probability of observing two matches on three items is equal to 
\eqn{f_{3}(2)=Q_{1}P_{2}P_{3}+P_{1}Q_{2}P_{3}+P_{1}P_{2}Q_{3}}{}. Finally, the probability of observing \eqn{O_{cs}}{} or more matches on \emph{n} items is equal to

\deqn{\sum\limits_{j=O_{cs}}^n{f_{n}(j)},}{Please see the manual for the equation!}

where \eqn{O_{cs}}{} is the observed number of identical responses between two response vectors. The probability is compared to a critical value such as .05, .01, or .001.
}

\subsection{ Omega Index}{\cr

The \eqn{\mathrel\omega}{Omega} index is a normal approximation to the exact probability distribution for the number of identical responses between two response vectors. The expected agreement between suspected source and copier examinees' response vectors is the sum of probabilities for the suspected copier examinee to give the suspected source examinee's responses, and is equal to

\deqn{
E_{cs}=\sum\limits_{i=1}^n{P(x_{ic}=U_{is}\big{|}\hat{\mathrel\theta_{c}}, \hat{\mathrel\xi_{i}})},
}{Please see the manual for the equation!}

where \eqn{U_{is}}{} is the observed response of the suspected source examinee (either 1 or 0) on item \emph{i}. 

The variance of this expectation is 

\deqn{
{\mathrel\sigma^2}=\sum\limits_{i=1}^n{\big(P(x_{ic}=U_{is}\big{|}\hat{\mathrel\theta_{c}}, \hat{\mathrel\xi_{i}})*\big[1-P(x_{ic}=U_{is}\big{|}\hat{\mathrel\theta_{c}}, \hat{\mathrel\xi_{i}})\big]\big)}.
}{Please see the manual for the equation!}

The expected agreement is compared to the observed agreement between two response vectors. The \eqn{\mathrel\omega}{Omega} index is equal to

\deqn{
\mathrel\omega = \frac{E_{cs}-O_{cs}}{\sqrt{\mathrel\sigma^2}}.
}{Please see the manual for the equation!}

The \eqn{\mathrel\omega}{Omega} index is compared to the critical values in a standard normal distribution for \eqn{\mathrel\alpha}{alpha} levels of .05, .01, or .001. 

}


\subsection{ K Index and K variants}{\cr

The \eqn{K-index} was originally developed by Frederick Kling (1979). However, there is no publication for the original development of the \eqn{K-index}. Later, Holland (1996) published the first study of its theoretical assumptions. Sotaridona and Meijer (2002) improved the \eqn{K-index} by developing \eqn{K_{1}} and \eqn{K_{2}}. Two other K variants (\eqn{S_{1}} and \eqn{S_{2}}), which use the Poisson distribution instead of binomial distribution, were also introduced by Sotaridona and Meijer (2003).\cr

\eqn{K}, \eqn{K_{1}}, and \eqn{K_{2}} use the binomial distribution to compute the likelihood of observing \eqn{W_{cs}} or more identical incorrect responses between two response vectors as the following:

\deqn{
\sum\limits_{j=W_{cs}}^{W_{s}}{W_{s} \choose j}P_{r}^j(1-P_{r})^{W_{s}-j}
}{Please see the manual for the equation!}

where \eqn{W_{s}} is the number-incorrect score for the suspected source examinee, \eqn{W_{cs}} is the observed number of identical incorrect responses between the suspected source and copier examinees, \eqn{W_{s} \choose j} is the number of all possible combinations for \eqn{j} matches on \eqn{W_{s}} identical responses, and \eqn{P_{r}} is the binomial probability of matching on an identical incorrect response with the suspected source examinee for the suspected copier examinee. In computational procedure, the examinees with the same number-incorrect scores are first put into the number-incorrect score subgroups. Second, the number of identical incorrect responses between each examinee in each subgroup and the suspected source examinee is computed. Let \eqn{M_{r}} be the average of the number of identical incorrect responses between each examinee in the \emph{r}th number-incorrect score group and the suspected source examinee. The \eqn{K-index} estimates \eqn{P_{r}} using the following equation:

\deqn{
P_{r}=\frac{M_{W_{c}}}{W_{s}},
}{Please see the manual for the equation!}

where \eqn{W_{c}} is the number-incorrect score for the suspected copier examinee.\cr

\eqn{K_{1}} and \eqn{K_{2}} use information from all number-incorrect score groups rather than using information only from the number-incorrect score group in which the suspected copier examinee belongs. \eqn{K_{1}} and \eqn{K_{2}} first regress the number-incorrect score on the average number of identical incorrect responses, using a linear and quadratic equations respectively. Then, \eqn{P_{r}} is estimated using \eqn{\hat{M_{r}}} from regression equations rather than using observed \eqn{M_{r}}. \cr

In a different approach, \eqn{S_{1}} uses a log-linear model to estimate\eqn{\hat{M_{r}}} from number-incorrect score group information and Poisson distribution to compute the likelihood of observing \eqn{W_{cs}} or more identical incorrect responses:

\deqn{
\sum\limits_{j=W_{cs}}^{W_{s}}{\frac{e^{-\hat{M_{r}}}\hat{M_{r}}^j}{j!},}
}{Please see the manual for the equation!}

where \eqn{r} is the number-incorrect score group of the suspected copier examinee. \cr

\eqn{S_{2}} is similar to \eqn{S_{1}}, but with one exception. Instead of using the predicted average number of identical incorrect responses for the \eqn{r}th subgroup, \eqn{S_{2}} uses the predicted average number of identical incorrect responses and predicted average weighted number of identical correct responses between the two response vectors.

}

}

\value{

\code{CopyDetect1()} returns an object of class "\code{CopyDetect}". An object of class "\code{CopyDetect}" is a list containing the following components. Each component is a further list with sub-elements.

    \item{data}{ original data file provided by user}
    \item{suspected.pair}{ row numbers in the data file for suspected pair}
    \item{W.index}{ statistics for the W index}
    \item{GBT.index}{ statistics for the GBT index}
    \item{K.index}{ statistics for the K index}
    \item{K.variants}{ statistics for the K1, K2, S1, and S2 indices}

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

\note{
* A recursive algorithm to compute the compound binomial probability distribution required for the GBT index is partially adapted from an S-plus code provided by Dr. Leonardo Sotaridona. The author acknowledges his contribution and permission.\cr

* The indices in the package rely on a sample of sufficient size to estimate CTT- or IRT-based parameters with enough precision for computational procedures. Users should be careful when using these indices with small samples such as those containing fewer than 100 examinees.

}

\examples{


	#Load irtoys package

	require(irtoys)

	#Set number of items and number of simulees

	n=20
	N=250

	#Generate item parameters to simulate data
	#First column is item discrimination, second column
	#is item difficulty, and third column is item guessing
	#parameters

	ipar <- cbind(rlnorm(n, meanlog = 0, sdlog = .5),
                    rnorm(n,0,1), 
                    rep(0,n))

	#Simulate dichotomous item responses

	responses <- as.data.frame(sim(ip=ipar, x=rnorm(N)))

	#Estimate item parameters

	est.ipar <- est(responses, model = "2PL", engine = "ltm")$est
      est.ipar

	#If the suspected copier examinee is Examinee 30, and suspected
	#source is Examinee 70

		CopyDetect1(data=responses,item.par=est.ipar,pair=c(30,70))


	#Now, compute these indices for 100 random pairs of examinees
	#a small type I error rate study

	k=2    # Due to the time constrains in package building, I set
		 # this to 2. Please set k equals to 100 in your run.

	pairs <- as.data.frame(matrix(k,ncol=2))

		for(i in 1:k){

			d <- sample(1:N,2,replace=FALSE)
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

		for(i in 1:k){

			x <- CopyDetect1(data=responses,item.par=est.ipar, pair=c(pairs[i,1],pairs[i,2]))

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


	length(which(pairs$W<.05))/k
	length(which(pairs$GBT<.05))/k
	length(which(pairs$K<.05))/k
	length(which(pairs$K1<.05))/k
	length(which(pairs$K2<.05))/k
	length(which(pairs$S1<.05))/k
	length(which(pairs$S2<.05))/k

	
	#Now, compute these indices for 5 answer copying pairs
	#a tiny empirical power study

	#First we will randomly choose a copier examinee
	#Second, we will randomly choose a corresponding source examinee 
	#Third, we will randomly select 10 items (50% copying)
	#Finally, we will overwrite the response vector of the source examinee
	#on the response vector of the copier examinee

	#This mimicks the scenario that the copier examinee looks at the 
	#source examinee's sheet and copies 10 items in a 20-item test.

	
	copy.pairs <- as.data.frame(matrix(nrow=5,ncol=2))
	
	for(i in 1:5){
			d <- sample(1:N,2,replace=FALSE)
			copy.pairs[i,1]=d[1] #hypothetical copier examinee
			copy.pairs[i,2]=d[2] #hypothetical source examinee
		}

	new.responses <- responses

	for(i in 1:5){ #Simulate answer copying for each answer copying pair

		copy.items <- sample(1:n,10,replace=FALSE)
		new.responses[copy.pairs[i,1],]=new.responses[copy.pairs[i,2],]
	}

	#Compute indices for pairs on the original response vectors 

	copy.pairs$W1 	<- NA
	copy.pairs$GBT1 	<- NA
	copy.pairs$K_1 	<- NA
	copy.pairs$K1_1 	<- NA
	copy.pairs$K2_1	<- NA
	copy.pairs$S1_1 	<- NA
	copy.pairs$S2_1 	<- NA

		for(i in 1:5){

			x <- CopyDetect1(data=responses,item.par=est.ipar, pair=c(copy.pairs[i,1],copy.pairs[i,2]))

			copy.pairs[i,]$W1=x$W.index$p.value
			copy.pairs[i,]$GBT1=x$GBT.index$p.value
			copy.pairs[i,]$K_1=x$K.index$k.index
			copy.pairs[i,]$K1_1=x$K.variants$K1.index
			copy.pairs[i,]$K2_1=x$K.variants$K2.index
			copy.pairs[i,]$S1_1=x$K.variants$S1.index
			copy.pairs[i,]$S2_1=x$K.variants$S2.index
		}

	
	#Compute indices for same pairs on answer copying simulated response vectors

	est.ipar2 <- est(new.responses, model = "2PL", engine = "ltm")$est
	
	copy.pairs$W2 	<- NA
	copy.pairs$GBT2 	<- NA
	copy.pairs$K_2 	<- NA
	copy.pairs$K1_2 	<- NA
	copy.pairs$K2_2	<- NA
	copy.pairs$S1_2 	<- NA
	copy.pairs$S2_2 	<- NA

		for(i in 1:5){

			x <- CopyDetect1(data=new.responses, item.par=est.ipar2,pair=c(copy.pairs[i,1],copy.pairs[i,2]))

			copy.pairs[i,]$W2=x$W.index$p.value
			copy.pairs[i,]$GBT2=x$GBT.index$p.value
			copy.pairs[i,]$K_2=x$K.index$k.index
			copy.pairs[i,]$K1_2=x$K.variants$K1.index
			copy.pairs[i,]$K2_2=x$K.variants$K2.index
			copy.pairs[i,]$S1_2=x$K.variants$S1.index
			copy.pairs[i,]$S2_2=x$K.variants$S2.index
		}


	#Let's see what happens!

		print(copy.pairs)
}

