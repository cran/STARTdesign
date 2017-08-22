\name{findparameter}
\alias{findparameter}
\title{
Parameter Calibration
}
\description{
Calibrate the design parameters for the Single-to-double Arm Transition Design
}
\usage{
findparameter(p0,p1,alpha1,beta1,alpha2,beta2)
}
\arguments{
 \item{p0}{
The minimally required level for the response rate to be clinically meaningful.
}
  \item{p1}{
The desirable target rate.
}
  \item{alpha1}{
The type I error rate in the first stage. 
}
  \item{beta1}{
The type II error rate in the first stage. 
}
  \item{alpha2}{
The type I error rate in the second stage. 
}
  \item{beta2}{
The type II error rate in the second stage. 
}
}

\value{
 \item{n1}{The number of subjects in the experimental arm in the single-arm stage.}
  \item{n2}{The number of subjects in each arm in the double-arm stage.}
  \item{r1}{The minimum number of responses to achieve in the single-arm stage in order for the trial to proceed into the next stage.The number of responses observed at the end of single-arm stage should be greater than or equal to \eqn{r1} for the trial to proceed.}
  \item{ess0}{The expected sample size under the null hypothesis.}
  \item{ess1}{The expected sample size under the alternative hypothesis.}
  \item{asn}{The average sample number taken as the average of \eqn{ess0} and \eqn{ess1}.}
}
\references{
Shi H., Yin G. (2017), START: Single-to-double Arm Transition Design for Phase II Clinical Trials. Submitted.
}
\author{
Haolun Shi <shl2003@connect.hku.hk>,\cr
Guosheng Yin <gyin@hku.hk>
}


\examples{
findparameter(p0=0.2,p1=0.5,alpha1=0.25,beta1=0.05,alpha2=0.2,beta2=0.25)
}