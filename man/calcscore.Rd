\name{calcscore}
\alias{calcscore}
\title{
Calculate Scores Under A Specific Rule
}
\description{
Given parameters of a scoring rule family, calculate scores for probabilistic forecasts and associated outcomes.
}
\usage{
calcscore(forecast, outcome, fam = "pow",
          param = c(2,rep(1/NCOL(forecast), NCOL(forecast))),
          data, scaling = FALSE)
}
\arguments{
  \item{forecast}{vector of probabilistic forecasts in (0,1)}
  \item{outcome}{vector of outcomes corresponding to \code{forecast},
    coded as \code{0} and \code{1}.}
  \item{fam}{scoring rule family. \code{pow} (default) is the power
    family, \code{beta} is the beta family, \code{sph} is the
    pseudospherical family.}
  \item{param}{Numeric vector of length 2, containing the parameters for
    \code{fam}.  For family \code{beta}, these are the parameters
    commonly denoted alpha and beta.  For families \code{pow} and
    \code{sph}, these correspond to the family parameter gamma and the
    baseline parameter, respectively (see details).}
  \item{data}{An optional data frame or list containing the
          variables in the model.  If not found in \code{data}, the
          variables are taken from the environment from which
	  \code{calcscore} is called.}
  \item{scaling}{Should the scoring rule be specially scaled (see details)?  Defaults to \code{FALSE}}
}
\details{
  The beta family was originally proposed by Buja et al. (2005); the power and pseudospherical families with baseline are described by Jose et al. (2009).  A discussion of choosing specific rules from these families is provided by Merkle and Steyvers (2013).

Some notable special cases of these families are:

Beta family: Log score when parameters are (0,0); Brier score when parameters are (1,1).

Power family with baseline=.5: The family approaches the log score as gamma goes to 1 (but the family is undefined for gamma=1).  One obtains the Brier score for gamma=2.

Pseudospherical family with baseline=.5: The family approaches the log score as gamma goes to 1 (but the family is undefined for gamma=1).  One obtains the spherical score for gamma=2.


When \code{scaling=TRUE}, the code attempts to scale the scores so that the maximum (worst) score is 1 and the minimum (best) score is 0.  This scaling cannot be accomplished when the scoring rule allows scores of infinity (the log score is the most common case here).

When \code{scaling=FALSE}, the minimum and maximum score are not computed.  The user could obtain these manually by, e.g., scoring forecasts of 0 and 1 associated with outcomes of 1 and 0, respectively.
}
\value{
\code{calcscore} returns a numeric vector that is the same length as
\code{forecast}, containing scores under the selected scoring rule.
}
\references{
Buja, A., Stuetzle, W., & Shen, Y. (2005). Loss functions for binary
class probability estimation and classification: Structure and
applications. (Obtained from \url{http://stat.wharton.upenn.edu/~buja/PAPERS/})

Jose, V. R. R., Nau, R. F., & Winkler, R. L. (2008). Scoring rules, generalized entropy, and utility maximization. \emph{Operations Research, 56}, 1146--1157.

Jose, V. R. R., Nau, R. F., & Winkler, R. L. (2009). Sensitivity to distance and baseline distributions in forecast evaluation. \emph{Management Science, 55}, 582--590.

Merkle, E. C. & Steyvers, M. (in press). Choosing a strictly proper scoring rule. \emph{Decision Analysis}.
}
\author{
Ed Merkle
}

\seealso{\code{\link{plotscore}}}
\examples{
data("WorldEvents")

## Brier scores
scores <- calcscore(forecast, answer, fam="beta",
                    param=c(1,1), data=WorldEvents)

## Calculate Brier scores manually
scores.man <- with(WorldEvents, (forecast - answer)^2)

## Comparison; beta family scores are half the manual scores
## but equivalent (and scores of 0 return NaN here).
scores/scores.man

## Average Brier score for each forecaster
with(WorldEvents, tapply(scores, forecaster, mean))

## Logarithmic scores
scores.log <- calcscore(forecast, answer, fam="beta",
                        param=c(0,0), data=WorldEvents)

## Spherical scores
scores.sph <- calcscore(forecast, answer, fam="sph",
                        param=c(2,.5), data=WorldEvents)
}