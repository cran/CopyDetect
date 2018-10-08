\name{similarity2}
\alias{similarity2}
\title{Response Similarity Indices for Nominal Response Items}
\description{
Computes the response similarity indices such as the Omega index (Wollack, 1996), Generalized Binomial Test ([GBT], van der Linden & Sotaridona (2006), K index (Holland, 1996), K1 and K2 indices (Sotaridona & Meijer, 2002), and S1 and S2 indices (Sotaridona & Meijer, 2003), and the M4 index (Maynes, 2014).
}
\usage{
similarity2(data,resp.options,key, person.id, center.id=NULL, item.loc, 
single.pair=NULL, many.pairs=NULL, centers=NULL)
}

\arguments{
  \item{data}{
a data frame with \emph{N} rows and \emph{n} columns. The data must include at least one column for unique individual IDs and item responses. All items should be scored dichotomously, with 0 indicating an incorrect response and 1 indicating a correct response. All columns must be "numeric". Missing values (NA) are allowed. Please see the details below for the treatment of missing data in the analysis.
}
  \item{resp.options}{
a vector of labels for the nominal response options in the dataset.  
}
  \item{key}{
a vector of key response options for the items in the dataset. The order of key responses should be the same as the order of columns that stores item responses given in the item.loc argument.
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

\code{\link{similarity2}} uses nominally scored items. Therefore, the definition of "identical incorrect response" and "identical correct response" is slightly different from \code{\link{similarity1}}. For example, let A, B, C, and D be the response alternatives for items in a multiple-choice test, and let A be the key response for an item. There are 10 possible response combinations between two response vectors: (A,A), (A,B), (A,C), (A,D), (B,B), (B,C), (B,D), (C,C), (C,D), and (D,D). \code{\link{similarity2}} counts the (A,A) response combination as an "identical correct response", and any of the (B,B), (C,C), and (D,D) response combinations as an "identical incorrect response". Similar to \code{\link{similarity1}}, the (NA,NA) response combination is counted as an "identical incorrect response". All other response combinations (A,B), (A,C), (A,D), (B,C), (B,D), (C,D), (A,NA), (B,NA), (C,NA), and (D,NA) are counted as non-identical responses. When computing the number-correct/number-incorrect scores or estimating the IRT ability parameters, missing values (NA) in a response vector are counted as an incorrect response.

}

\value{

If a single-pair is requested, \code{similarity1()} returns a list containing the following components. 

    \item{data}{ original data file provided by user}
    \item{scored.data}{ scored item response matrix based on the key response vector provided}
    \item{W.index}{ statistics for the W index}
    \item{GBT.index}{ statistics for the GBT index}
    \item{K.index}{ statistics for the K index}
    \item{K.variants}{ statistics for the K1, K2, S1, and S2 indices}
    \item{M4.index}{ statistics for the M4 index}
    \item{item.loc}{columns in the dataset that stores the item responses}
    \item{item.par}{estimated item parameter matrix for the Bock's Nominal Response IRT Model}
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


\examples{

data(form2)
dim(form2)
head(form2)

  # the first column of this dataset is unique individual IDs
  # the second column of this dataset is unique center IDs
  # From Column 3 to Column 172, nominal item responses. 1, 2, 3, and 4 represent different 
  # nominal response options.


# For the sake of reducing the computational time,
# I will analyze a subset of this dataset (first 10 items)

 subset <- form2[1:500,1:12]
 
 dim(subset)
 head(subset)

                        
# Computing similarity for a single pair

  key.resp <- c(2,3,1,4,1,2,2,1,1,1)

  a <- similarity2(data         = subset,
                   resp.options = c(1,2,3,4),
                   key          = key.resp, 
                   person.id    = "EID", 
                   item.loc     = 3:12, 
                   single.pair  = c("e200287","e200169"))
                   
  print(a)


\donttest{

  # Computing for multiple pairs

  pairs <- matrix(as.character(sample(subset$EID,20)),nrow=10,ncol=2)

  a <- similarity2(data         = subset,
                   resp.options = c(1,2,3,4),
                   key          = key.resp, 
                   person.id    = "EID", 
                   item.loc     = 3:12, 
                   many.pairs   = pairs)

  print(a)


# Computing all possible pairs in the requested centers

  a <- similarity2(data         = subset,
                   resp.options = c(1,2,3,4),
                   key          = key.resp, 
                   person.id    = "EID", 
                   center.id    = "cent_id",
                   item.loc     = 3:12, 
                   centers      = c(42,45,4114))

  print(a)
  
  
  # Key response vector for all 170 items for future reference

  key.resp <- c(2,3,1,4,1,2,2,1,1,1,4,1,3,1,3,3,1,2,1,3,3,4,1,
                3,3,2,3,2,2,3,1,4,1,2,3,3,2,3,4,1,2,1,1,4,3,3,
                1,1,4,2,2,1,4,1,2,3,3,1,2,4,1,4,2,4,1,1,2,3,4,
                4,1,4,2,1,2,2,2,2,4,4,3,2,1,3,2,3,2,2,1,2,4,3,
                2,1,2,1,2,3,1,1,4,3,4,3,4,3,1,3,3,4,2,1,1,4,3,
                2,4,4,1,1,1,2,2,1,3,1,2,3,3,3,4,4,1,4,4,3,4,2,
                3,1,4,1,4,1,3,2,2,4,4,4,1,2,2,3,4,1,2,1,4,4,4,
                1,3,1,2,1,2,3,2,2)
}
		
}

