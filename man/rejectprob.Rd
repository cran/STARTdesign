\name{rejectprob}
\alias{rejectprob}
\title{
Rejection Probability Calculation
}
\description{
Calculate the probability of rejecting the null hypothesis at the end of the second stage in the Single-to-double Arm Transition Design
}
\usage{
rejectprob(pe,ps,n1,n2,r1,z)
}
\arguments{
 \item{pe}{
The response rate of the experimental arm.
}
  \item{ps}{
The response rate of the standard arm.
}
  \item{n1}{
The number of subjects in the experiemntal arm in the single-arm stage.
}
  \item{n2}{
The number of subjects in each arm in the double-arm stage.
}
  \item{r1}{
The minimum number of responses to achieve in the single-arm stage in order for the trial to proceed into the next stage.
}
  \item{z}{
The threshold value for the Z test, i.e., the Z statistic should be greater than \eqn{z} in order to reject the null hypothesis at the end of the second stage.
}
}

\value{
 \item{n1}{The number of subjects in the experiemntal arm in the single-arm stage.}
  \item{n2}{The number of subjects in each arm in the double-arm stage.}
  \item{r1}{The minimum number of responses to achieve in the single-arm stage in order for the trial to proceed into the next stage.The number of responses observed at the end of single-arm stage should be greater than or equal to \eqn{r1} for the trial to proceed.}
  \item{ess0}{The expected sample size under the null hypothesis.}
  \item{ess1}{The expected sample size under the alternative hypothesis.}
  \item{asn}{The average sample number taken as the average of \eqn{ess0} and \eqn{ess1}.}
}
\references{
Shi H., Yin G. (2017), START: Single-to-double Arm Transition Design for Phase II Clinical Trials. 
}
\author{
Haolun Shi <shl2003@connect.hku.hk>,\cr
Guosheng Yin <gyin@hku.hk>
}
\examples{
rejectprob(pe=0.2,ps=0.4,n1=20,n2=40,r1=10,z=qnorm(0.9))
}

