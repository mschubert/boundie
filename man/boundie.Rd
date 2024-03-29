% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/boundie-package.r, R/boundie.r
\docType{package}
\name{boundie}
\alias{boundie}
\alias{boundie-package}
\alias{boundie}
\title{Bounded differential expression using count data}
\usage{
boundie(x, design, weights = rep(1, ncol(x)), offset = rep(0, ncol(x)),
  lower = NULL, upper = NULL, control = list(), reduced = NULL,
  verbose = FALSE)
}
\arguments{
\item{x}{a gene expression matrix (genes x samples)}

\item{design}{the model to fit for each gene, e.g. '~ condition'}

\item{weights}{weight vector for all coefficients}

\item{offset}{vector of coefficient offsets of length 'n'}

\item{lower}{named vector of coefficient lower bounds (-Inf otherwise)}

\item{upper}{named vector of coefficient upper bounds (Inf otherwise)}

\item{control}{named list of control variables for fit (e.g. maxiter)}

\item{reduced}{the reduced model for a likelihood ratio test}

\item{verbose}{print debug messages}
}
\description{
Bounded differential expression using count data

Bounded coefficient differential expression
}
