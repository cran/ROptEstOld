\name{GammaFamily}
\alias{GammaFamily}

\title{Generating function for Gamma families}
\description{
  Generates an object of class \code{"L2ParamFamily"} which
  represents a Gamma family.
}
\usage{
GammaFamily(scale = 1, shape = 1, trafo)
}
\arguments{
  \item{scale}{ positive real: scale parameter }
  \item{shape}{ positive real: shape parameter }
  \item{trafo}{ matrix: transformation of the parameter }
}
\details{
  The slots of the corresponding L2 differentiable 
  parameteric family are filled.
}
\value{Object of class \code{"L2ParamFamily"}}
\references{
  Kohl, M. (2005) \emph{Numerical Contributions to 
  the Asymptotic Theory of Robustness}. Bayreuth: Dissertation.}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{\code{\link{L2ParamFamily-class}}, \code{\link[distr]{Gammad-class}}}
\examples{
distrExOptions("EupperTruncQuantile" = 1e-15) # problem with q(Gamma())(1) = NaN
(G1 <- GammaFamily())
FisherInfo(G1)
checkL2deriv(G1)
distrExOptions("EupperTruncQuantile" = 0) # default
}
\concept{Gamma model}
\keyword{models}
