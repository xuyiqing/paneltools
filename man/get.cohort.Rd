\name{get.cohort}
\alias{get.cohort}
\title{Get the Cohort Index}
\description{Gets the cohort index given a panel data.}
\usage{get.cohort(data, D, index, 
                  varname = NULL, entry.time = NULL)
}
\arguments{
    \item{data}{a data frame, can be a balanced or unbalanced panel data.}
    \item{D}{the treatment indicator. The treatment should be binary (0 and 1).}
    \item{index}{a two-element string vector specifying the unit and time indicators. Must be of length 2. Every observation should be uniquely defined by the pair of the unit and time indicator.}
    \item{varname}{a string specifying the name for the generated cohort index.}
    \item{entry.time}{a list of intervals for first get-treated time.}    
}
\details{
  \code{get.cohort} pre-processes the data and generates the index for different cohorts..
}
\value{
  \item{data}{a new data frame containing the cohort index.}
}
\author{
  Licheng Liu; Yiqing Xu, Ziyi Liu
}

