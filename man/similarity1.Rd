\name{similarity1}
\alias{similarity1}
\title{Response Similarity for Dcihotomously Scored Items}
\description{
Computes the response similarity indices such as the Omega index (Wollack, 1996), Generalized Binomial Test ([GBT], van der Linden & Sotaridona (2006), K index (Holland, 1996), K1 and K2 indices (Sotaridona & Meijer, 2002), and S1 and S2 indices (Sotaridona & Meijer, 2003), and the M4 index (Maynes, 2014).
}
\usage{
similarity1(data, item.par=NULL, model="1PL", prior = TRUE, person.id, center.id=NULL, 
item.loc, single.pair=NULL, many.pairs=NULL, centers=NULL)

}

\arguments{
  \item{data}{
a data frame with \emph{N} rows and \emph{n} columns. The data must include at least one column for unique individual IDs and item responses. All items should be scored dichotomously, with 0 indicating an incorrect response and 1 indicating a correct response. All columns must be "numeric". Missing values (NA) are allowed. Please see the details below for the treatment of missing data in the analysis.
}
  \item{item.par}{
a data matrix with \emph{n} rows and three columns, where \emph{n} denotes the number of items. The first, second, and third columns representitem discrimination, item difficulty, and item guessing parameters, respectively. If item parameters are not provided by user, the \pkg{mirt} package is internally called to estimate the parameeters of a chosen IRT model. The rows in the item parameter matrix must be in the same order as the columns in the response data.
}

  \item{model}{
IRT model to be used for computing IRT-based indices (omega, GBT, and M4). The available options are "1PL", "2PL", and "3PL". Default is "1PL".
}

  \item{prior}{
a logical argument if the model is equal 3PL. If TRUE, then a prior distribution is specified for the guessing parameter when fitting the 3PL model. Otherwise, this argument is ignored.
}

  \item{person.id}{
column label in the dataset for the variable indicating unique person ids.
}

  \item{center.id}{
column label in the dataset for the variable indicating center ids. This is used if the user asks computing the indices for all pairs in some centers. For a single pair or multiple pair calculation, this argument is ignored.
}

  \item{item.loc}{
a numeric vector indicating the location of item response in the dataset. 
}

  \item{single.pair}{
a character vector of length 2 for the suspected pair of examinees. The first element of the vector indicates the unique ID of the suspected copier examinee, and the second element of the vector indicates the unique ID of the suspected source examinee.
}

  \item{many.pairs}{
a matrix with two columns. Users can request to compute the indices simultaneously as many pairs as they wish. Each row of this matrix represents a pair of examinees. The first column of each row indicates the unique ID of the suspected copier examinee, and the second column of each row indicates the unique ID of the suspected source examinee.
}

  \item{centers}{
a character vector including unique center ids. Users can request to compute the indices for all pairs in the centers listed using this argument. 
}
 
}
\details{

Test fraud has recently been receiving increased attention in the field of educational testing. The current R package provides a set of useful statistical indices recently proposed in the literature for detecting a specific type of test fraud - answer copying from a nearby examinee on multiple-choice examinations. The information obtained from these procedures may provide additional statistical evidence of answer copying, but they should be used cautiously. These statistical procedures should not be used as sole evidence of answer copying, especially when used for general screening purposes.\cr

There are more than twenty different statistical procedures recommended in the literature for detecting answer copying on multiple-choice examinations. However, the CopyDetect package includes the indices that have been shown as effective and reliable based on the simulation studies in the literature (Sotaridona & Meijer, 2002, 2003; van der Linden & Sotaridona, 2006; Wollack, 1996, 2003, 2006; Wollack & Cohen, 1998; Maynes, 2014). Among these indices, \eqn{\mathrel\omega}{Omega}, GBT, and M4 use IRT models, and \eqn{K} and \eqn{K} variants are the non-IRT counterparts.\cr

Since \code{\link{similarity1}} uses dichotomous responses as input, any (0,0) response combination between two response vectors is counted as an "identical incorrect response", and any (1,1) response combination between two response vectors is counted as an "identical correct response". \code{\link{similarity1}} also counts any (NA,NA) response combination between two response vectors as an "identical incorrect response". Other response combinations such as (0,1),(1,0),(0,NA),(1,NA) between two response vectors are not counted as identical responses. When computing the number-correct/number-incorrect scores or estimating the IRT ability parameters, missing values (NA) are counted as an incorrect response.\cr
}

\value{

If a single-pair is requested, \code{similarity1()} returns a list containing the following components. 

    \item{data}{ original data file provided by user}
    \item{W.index}{ statistics for the W index}
    \item{GBT.index}{ statistics for the GBT index}
    \item{K.index}{ statistics for the K index}
    \item{K.variants}{ statistics for the K1, K2, S1, and S2 indices}
    \item{M4.index}{ statistics for the M4 index}
    \item{item.loc}{columns in the dataset that stores the item responses}
    \item{item.par}{estimated item parameter matrix for a chosen IRT model}
    \item{center.id}{column label for the unique center IDs}
    \item{person.id}{column label for the unique person IDs}
    \item{single.pair}{Unique IDs for a pair of examinee requested}
    \item{single.pair2}{corresponding row numbers for the pair of examinee requested}
    \item{thetas}{maximum likelihood estimates for IRT ability parameter for each individual}
    
If a multiple pairs are requested in the matrix form, \code{similarity1()} returns a list containing the following components.


    \item{data}{ original data file provided by user}
    \item{output.manypairs}{a matrix including the IDs for each pair requested and corresponding indices computed for these pairs}
    \item{item.loc}{columns in the dataset that stores the item responses}
    \item{item.par}{estimated item parameter matrix for a chosen IRT model}
    \item{center.id}{column label for the unique center IDs}
    \item{person.id}{column label for the unique person IDs}
    \item{thetas}{maximum likelihood estimates for IRT ability parameter for each individual}
    
If all possible pairs in certain test centers are requested in the matrix form, \code{similarity1()} returns a list containing the following components.

    \item{data}{ original data file provided by user}
    \item{output.centers}{a matrix including the IDs for all pairs requested and corresponding indices computed for these pairs}
    \item{item.loc}{columns in the dataset that stores the item responses}
    \item{item.par}{estimated item parameter matrix for a chosen IRT model}
    \item{center.id}{column label for the unique center IDs}
    \item{person.id}{column label for the unique person IDs}
    \item{thetas}{maximum likelihood estimates for IRT ability parameter for each individual}
    

}

\references{

Sotaridona, L.S., & Meijer, R.R.(2002). Statistical properties of the K-index for detecting answer copying. \emph{Journal of Educational Measurement, 39}, 115-132.\cr

Sotaridona, L.S., & Meijer, R.R.(2003). Two new statistics to detect answer copying. \emph{Journal of Educational Measurement, 40}, 53-69.\cr

van der Linden, W.J., & Sotaridona, L.S.(2006). Detecting answer copying when the regular response process follows a known response model. \emph{Journal of Educational and Behavioral Statistics, 31}, 283-304.\cr

Wollack, J.A.(1996). Detection of answer copying using item response theory. \emph{Dissertation Abstracts International, 57/05}, 2015.\cr

Wollack, J.A.(2003). Comparison of answer copying indices with real data. \emph{Journal of Educational Measurement, 40}, 189-205.\cr

Wollack, J.A.(2006). Simultaneous use of multiple answer copying indexes to improve detection rates. \emph{Applied Measurement in Education, 19}, 265-288.\cr

Wollack, J.A., & Cohen, A.S.(1998). Detection of answer copying with unknown item and trait parameters. \emph{Applied Psychological Measurement, 22}, 144-152.\cr

}
\author{
Cengiz Zopluoglu
}

\note{
* A recursive algorithm to compute the compound binomial probability distribution required for the GBT index is partially adapted from an S-plus code provided by Dr. Leonardo Sotaridona. The author acknowledges his contribution and permission.\cr

* The indices in the package rely on a sample of sufficient size to estimate CTT- or IRT-based parameters with enough precision for computational procedures. Users should be careful when using these indices with small samples such as those containing fewer than 100 examinees.

}

\examples{

data(form1)
dim(form1)
head(form1)

  # the first column of this dataset is unique individual IDs
  # the second column of this dataset is unique center IDs
  # From Column 3 to Column 172, dichotomous item responses


# For the sake of reducing the computational time,
# I will analyze a subset of this dataset (first 20 items)

 subset <- form1[1:1000,1:22]
 
 dim(subset)
 head(subset)


# Computing similarity for a single pair

a <- similarity1(data       = subset, 
                 model      = "1PL", 
                 person.id  = "EID", 
                 item.loc   = 3:22, 
                 single.pair= c("e100287","e100869")) 

print(a)


\donttest{

# Computing for multiple pairs

pairs <- matrix(as.character(sample(subset$EID,20)),nrow=10,ncol=2)

a <- similarity1(data       = subset, 
                 model      = "1PL", 
                 person.id  = "EID", 
                 item.loc   = 3:20, 
                 many.pairs = pairs)

print(a)


# Computing all possible pairs in the requested centers

a <- similarity1(data       = subset, 
                 model      = "1PL", 
                 person.id  = "EID",
                 center.id  = "cent_id",
                 item.loc   = 3:20, 
                 centers    = c(1802,5130,67,9056)) 
                 
print(a)

}
	
}

